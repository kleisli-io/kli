;;; Playbook MCP Server - Session State Management
;;; Per-session state isolation and task-level pattern state persistence.

(in-package #:playbook)

(defvar *log-verbose*)  ; forward declaration — defined in package.lisp

;;; =========================================================================
;;; DEPOT ROOT DETECTION
;;; =========================================================================
;;; Local depot detection to avoid dependency on claude-hooks library.
;;; Uses git root for repository detection

(defun mcp-find-git-root ()
  "Find git repository root via git rev-parse.
   Returns absolute path string or NIL if not in a git repo."
  (let ((result (ignore-errors
                  (uiop:run-program '("git" "rev-parse" "--show-toplevel")
                                    :output :string
                                    :ignore-error-status t))))
    (when (and result (not (string= result "")))
      (string-trim '(#\Newline #\Return #\Space) result))))

(defun mcp-find-depot-root ()
  "Find repository root via git.
   Returns absolute path string with trailing slash, or NIL."
  (let ((git-root (mcp-find-git-root)))
    (when git-root
      (namestring (truename (uiop:ensure-directory-pathname git-root))))))


(defun kli-meta-path ()
  "Return the .kli/ metadata directory path, or NIL if not in a git repo
   or .kli/ doesn't exist."
  (let ((root (mcp-find-depot-root)))
    (when root
      (let ((kli-dir (namestring (merge-pathnames ".kli/" root))))
        (when (uiop:directory-exists-p kli-dir)
          kli-dir)))))

;;; =========================================================================
;;; HTTP MULTI-SESSION SUPPORT
;;; =========================================================================
;;; In stdio mode (default): one MCP subprocess per Claude Code session.
;;; Session discovered via .claude/latest-session file.
;;;
;;; In HTTP mode (daemon): single process serves all sessions.
;;; Session ID comes from Mcp-Session-Id HTTP header.
;;; Per-thread isolation via :around method in server.lisp.

(defvar *http-mode* nil
  "T when running in HTTP transport mode, NIL for stdio.")

(defun current-http-session-id ()
  "Extract Mcp-Session-Id from current Hunchentoot request.
   Returns session-id string or NIL if not in HTTP request context."
  (ignore-errors
    (let ((request (symbol-value (find-symbol "*REQUEST*" "HUNCHENTOOT"))))
      (when request
        (funcall (find-symbol "HEADER-IN" "HUNCHENTOOT")
                 "Mcp-Session-Id" request)))))

(defun current-claude-session-id ()
  "Extract Claude-Session-Id from the current Hunchentoot request.
   Returns the header value (string) or NIL when no request is active,
   when hunchentoot is not loaded, or when the client did not send the
   header.  The CLI populates the header from $CLAUDE_SESSION_ID; each
   Claude Code shell carries its own value."
  (ignore-errors
    (let ((request (symbol-value (find-symbol "*REQUEST*" "HUNCHENTOOT"))))
      (when request
        (let ((value (funcall (find-symbol "HEADER-IN" "HUNCHENTOOT")
                              "Claude-Session-Id" request)))
          (when (and value (plusp (length value))) value))))))

;;; Session state struct

(defstruct session-state
  "Per-session playbook state."
  (id "" :type string)
  (claude-session-id nil)                ; Claude Code session ID (differs from MCP session ID in HTTP mode)
  (activated-patterns nil :type list)    ; list of pattern-id strings
  (feedback-given nil :type list)        ; alist of (pattern-id . :helpful/:harmful)
  (active-domains nil :type list)        ; list of domain strings
  (task-type nil)                        ; keyword or nil
  (task-dir nil)                         ; string or nil
  (created-at (get-universal-time) :type integer)
  (last-active (get-universal-time) :type integer))

;;; PID → Claude session ID registry
;;; SessionStart hook calls /register-claude-session with PID + Claude session ID.
;;; On first MCP tool call, we look up the Claude session ID by scanning this registry.
;;;
;;; A single PID may map to MULTIPLE registered claude-sids when several
;;; Claude Code shells share UNIX PID lineage (same parent shell, same
;;; process tree).  Entries are stored newest-first so resolvers that
;;; need a single answer can pick the most recent.  The
;;; Claude-Session-Id HTTP header is the authoritative per-request
;;; signal; this registry exists only as a fallback for hook callers
;;; and older clients that do not carry the header.

(defvar *pid-claude-session-registry* (make-hash-table :test 'eql)
  "PID → list of (claude-session-id . internal-real-time) entries,
   newest-first. Populated by /register-claude-session endpoint.")

(defvar *pid-registry-lock* (make-lock "pid-registry")
  "Lock for thread-safe PID registry access.")

(defun %pid-registry-entries (pid)
  "Return the list of (sid . timestamp) entries for PID, newest-first.
   Caller must already hold *PID-REGISTRY-LOCK*."
  (gethash pid *pid-claude-session-registry*))

(defun register-claude-session-for-pid (pid claude-session-id)
  "Register a PID → Claude session ID mapping.
   Called by /register-claude-session HTTP endpoint.

   Multiple registrations for the same PID accumulate so parallel Claude
   Code shells with shared PID lineage do not silently coalesce onto a
   single sid.  A re-registration of the same sid promotes it to the
   newest position; older entries for that PID stay in the list."
  (with-lock-held (*pid-registry-lock*)
    (let* ((entries (gethash pid *pid-claude-session-registry*))
           (filtered (remove claude-session-id entries
                             :key #'car :test #'equal))
           (new-entry (cons claude-session-id (get-internal-real-time))))
      (setf (gethash pid *pid-claude-session-registry*)
            (cons new-entry filtered))))
  (format nil "Registered PID ~D → Claude session ~A" pid claude-session-id))

(defun lookup-claude-session-by-pid (pid)
  "Look up the most-recently registered Claude session ID for PID.
   Returns string or NIL.  When a PID has multiple registered sids,
   only the newest is returned — peer-PID resolution is fundamentally
   ambiguous under shared PID lineage and the per-request
   Claude-Session-Id header is the authoritative signal."
  (with-lock-held (*pid-registry-lock*)
    (let ((entries (%pid-registry-entries pid)))
      (when entries
        (caar entries)))))

;;; Session store

(defvar *session-states* (make-hash-table :test 'equal)
  "Per-session state: session-id → session-state.")

(defvar *session-states-lock* (make-lock "session-states")
  "Lock for thread-safe session state access.")

(define-condition unknown-session-error (error)
  ((session-id :initarg :session-id :reader unknown-session-error-session-id))
  (:report (lambda (condition stream)
             (format stream "Unknown playbook session: ~A"
                     (unknown-session-error-session-id condition))))
  (:documentation
   "Signaled by code paths that must reject calls against an unknown session
    rather than silently resurrect one. Activation and feedback go through
    GET-SESSION-OR-ERROR so that, after the unknown-session contract on the
    HTTP boundary, hitting an evicted or never-initialized session is treated
    as a programming error instead of a virgin-state creation."))

(defun get-or-create-session (session-id &key task-dir)
  "Get existing session state or create a new one.
   Use only on legitimate creation paths — initialize and PID registration.

   In stdio mode the SESSION-ID passed in IS the Claude session ID by
   construction (one MCP subprocess per Claude Code shell, session-id
   sourced from .claude/latest-session). Stamp it on the struct at
   creation so every callsite can rely on a single invariant: a
   session-state with a NIL claude-session-id is a not-yet-resolved
   HTTP session, never a stdio one.

   In HTTP mode SESSION-ID is the MCP-sid; APPLY-CLAUDE-SESSION-ID sets
   claude-session-id later from the per-request header."
  (with-lock-held (*session-states-lock*)
    (or (gethash session-id *session-states*)
        (let ((session (make-session-state :id session-id :task-dir task-dir)))
          (unless *http-mode*
            (setf (session-state-claude-session-id session) session-id))
          (setf (gethash session-id *session-states*) session)
          session))))

(defun get-session (session-id)
  "Get session state or NIL if not found."
  (with-lock-held (*session-states-lock*)
    (gethash session-id *session-states*)))

(defun get-session-or-error (session-id)
  "Get existing session state or signal UNKNOWN-SESSION-ERROR.
   Use on code paths where an unknown session indicates a programming
   error: activation and feedback recording, anywhere a virgin session
   would silently swallow state that the caller assumed already existed."
  (or (get-session session-id)
      (error 'unknown-session-error :session-id session-id)))

;;; Session operations

(defun record-activation (session-id pattern-id)
  "Record that a pattern was activated in this session.
   Signals UNKNOWN-SESSION-ERROR if the session does not exist —
   callers must initialize the session first via the MCP boundary."
  (let ((session (get-session-or-error session-id)))
    (pushnew pattern-id (session-state-activated-patterns session) :test #'string=)))

(defun record-feedback (session-id pattern-id feedback-type)
  "Record feedback given for a pattern in this session.
   Signals UNKNOWN-SESSION-ERROR if the session does not exist."
  (let ((session (get-session-or-error session-id)))
    (let ((existing (assoc pattern-id (session-state-feedback-given session) :test #'string=)))
      (if existing
          (setf (cdr existing) feedback-type)
          (push (cons pattern-id feedback-type) (session-state-feedback-given session))))))

(defun record-domain (session-id domain)
  "Record a domain detected in this session."
  (let ((session (get-or-create-session session-id)))
    (pushnew domain (session-state-active-domains session) :test #'string=)))

(defun session-summary (session-id)
  "Return summary for a session: (activated-count feedback-count pending-ids)."
  (let ((session (get-session session-id)))
    (if session
        (let* ((activated (session-state-activated-patterns session))
               (feedback (session-state-feedback-given session))
               (feedback-ids (mapcar #'car feedback))
               (pending (set-difference activated feedback-ids :test #'string=)))
          (values (length activated) (length feedback) pending))
        (values 0 0 nil))))

(defun cleanup-session (session-id)
  "Remove session state."
  (with-lock-held (*session-states-lock*)
    (remhash session-id *session-states*)))

;;; JSON serialization (for task-level state)

(defun session-to-json-alist (session)
  "Convert session state to an alist for JSON encoding."
  (list (cons "session_id" (session-state-id session))
        (cons "activated" (session-state-activated-patterns session))
        (cons "feedback" (mapcar (lambda (pair)
                                   (list (cons "id" (car pair))
                                         (cons "type" (string-downcase (symbol-name (cdr pair))))))
                                 (session-state-feedback-given session)))
        (cons "domains" (session-state-active-domains session))
        (cons "created_at" (session-state-created-at session))))

;;; Task-level persistence

(defun load-task-pattern-state (task-dir)
  "Load task's pattern state from playbook-state.json.
   Returns list of session entry hash-tables, or NIL."
  (let ((path (format nil "~A/playbook-state.json" task-dir)))
    (when (probe-file path)
      (yason:parse (alexandria:read-file-into-string path)))))

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash-table (for yason encoding)."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k . v) in alist
          do (setf (gethash k ht)
                   (cond ((and (consp v) (consp (car v)) (stringp (caar v)))
                          ;; Nested alist → hash-table
                          (alist-to-hash-table v))
                         ((and (listp v) (every (lambda (x) (and (consp x) (consp (car x)))) v))
                          ;; List of alists → list of hash-tables
                          (mapcar #'alist-to-hash-table v))
                         (t v))))
    ht))

(defun save-session-to-task (session task-dir)
  "Append session entry to task's playbook-state.json."
  (let* ((path (namestring (merge-pathnames
                            (format nil "~A/playbook-state.json" task-dir))))
         (existing (or (load-task-pattern-state task-dir) nil))
         (entry (alist-to-hash-table (session-to-json-alist session)))
         (all-entries (append existing (list entry)))
         (temp-path (format nil "~A.tmp" path)))
    (with-open-file (s temp-path :direction :output :if-exists :supersede)
      (yason:encode all-entries s))
    (rename-file temp-path path)))

;;; Aggregate view across sessions

(defun aggregate-task-state (session-entries)
  "Compute aggregate state from list of session entry hash-tables.
   Returns (values total-activated total-feedback pending-ids all-domains)."
  (let ((all-activated nil)
        (all-feedback-ids nil)
        (all-domains nil))
    (dolist (entry session-entries)
      (let ((activated (gethash "activated" entry))
            (feedback (gethash "feedback" entry))
            (domains (gethash "domains" entry)))
        (dolist (id activated)
          (pushnew id all-activated :test #'string=))
        (dolist (fb feedback)
          (pushnew (if (hash-table-p fb)
                       (gethash "id" fb)
                       (cdr (assoc "id" fb :test #'string=)))
                   all-feedback-ids :test #'string=))
        (dolist (d domains)
          (pushnew d all-domains :test #'string=))))
    (let ((pending (set-difference all-activated all-feedback-ids :test #'string=)))
      (values (length all-activated)
              (length all-feedback-ids)
              pending
              all-domains))))

(defun format-session-resume-message (total-activated total-feedback pending-ids domains)
  "Format a human-readable resume message for session start."
  (format nil "Playbook: ~D patterns activated across prior sessions (~{~A~^, ~} domains). ~D have feedback.~@[ Pending feedback: ~{~A~^, ~}.~]"
          total-activated domains total-feedback pending-ids))

;;; =========================================================================
;;; SESSION DISCOVERY
;;; =========================================================================
;;; MCP servers spawn before SessionStart hook runs, so they cannot receive
;;; session_id via environment. Instead, MCP discovers its session by:
;;; 1. Reading .claude/latest-session (written by SessionStart hook)
;;; 2. Atomically claiming the session with .claude/sessions/{id}/mcp.lock
;;; 3. Caching the claimed session for the MCP lifetime

(defvar *claimed-session* nil
  "Cached claimed session for this MCP instance. Lazily populated on first use.")

(defvar *claimed-session-cwd* nil
  "CWD used when claiming session. For cache invalidation if CWD changes.")

(defvar *mcp-lock-path* nil
  "Path to the mcp.lock file we created, if any.
   Set by try-claim-session on successful claim, used by cleanup-mcp-lock.")

(defun ensure-trailing-slash (path)
  "Ensure path ends with /."
  (if (and (plusp (length path))
           (char= (char path (1- (length path))) #\/))
      path
      (concatenate 'string path "/")))

(defun read-latest-session-announcement (cwd)
  "Read session_id from .claude/latest-session file.
   Checks CWD (depot root) first, then falls back to world root
   since hooks write latest-session to the coordination root.
   Returns session-id string or NIL if not found."
  (flet ((try-read (root)
           (when root
             (let ((path (merge-pathnames ".claude/latest-session"
                                          (ensure-trailing-slash root))))
               (when (probe-file path)
                 (handler-case
                     (let ((content (string-trim '(#\Space #\Newline #\Tab)
                                                 (uiop:read-file-string path))))
                       (when (plusp (length content))
                         content))
                   (error () nil)))))))
    (or (try-read cwd)
        (let ((world-root (mcp-find-depot-root)))
          (when (and world-root
                     (not (equal (ensure-trailing-slash world-root)
                                 (ensure-trailing-slash cwd))))
            (try-read world-root))))))

(defun session-claimed-p (session-dir)
  "Check if session has an active MCP claim (mcp.lock with running process).
   Returns T if claimed by a running process, NIL otherwise."
  (let ((lock-path (merge-pathnames "mcp.lock" session-dir)))
    (when (probe-file lock-path)
      (handler-case
          (let* ((content (uiop:read-file-string lock-path))
                 (pid (parse-integer content :junk-allowed t)))
            (when pid
              ;; Check if process is still running (signal 0 = existence check)
              (handler-case
                  (progn (sb-posix:kill pid 0) t)
                (sb-posix:syscall-error () nil))))
        (error () nil)))))

(defun try-claim-session (session-dir)
  "Attempt to atomically claim a session for this MCP.
   Creates mcp.lock with our PID. Returns T if claimed, NIL if already claimed.
   On success, sets *mcp-lock-path* for cleanup on shutdown."
  (let ((lock-path (merge-pathnames "mcp.lock" session-dir)))
    ;; First check for stale claim (process no longer running)
    (when (and (probe-file lock-path)
               (not (session-claimed-p session-dir)))
      ;; Stale claim - remove it
      (ignore-errors (delete-file lock-path)))
    ;; Try atomic creation
    (handler-case
        (with-open-file (s lock-path :direction :output
                                     :if-exists :error
                                     :if-does-not-exist :create)
          (format s "~A" (sb-posix:getpid))
          ;; Track lock path for cleanup on shutdown
          (setf *mcp-lock-path* (namestring lock-path))
          t)
      (file-error () nil))))

(defun session-alist-from-dir (session-dir)
  "Build session alist from a session directory.
   Extracts session-id from the directory name.
   Returns alist with :session-id, :branch, :commit, :task-dir (nils for missing data)."
  (let ((dir-name (first (last (pathname-directory
                                (uiop:ensure-directory-pathname session-dir))))))
    (list (cons :session-id dir-name)
          (cons :branch nil)
          (cons :commit nil)
          (cons :task-dir nil))))

(defun find-our-existing-claim (cwd)
  "Find a session we already claimed (mcp.lock with our PID).
   Returns session alist or NIL."
  (let* ((sessions-dir (merge-pathnames ".claude/sessions/" (ensure-trailing-slash cwd)))
         (our-pid (sb-posix:getpid)))
    (when (probe-file sessions-dir)
      (dolist (entry (directory (merge-pathnames "*/" sessions-dir)))
        (let ((lock-path (merge-pathnames "mcp.lock" entry)))
          (when (probe-file lock-path)
            (handler-case
                (let* ((content (uiop:read-file-string lock-path))
                       (pid (parse-integer content :junk-allowed t)))
                  (when (eql pid our-pid)
                    (return-from find-our-existing-claim
                      (session-alist-from-dir entry))))
              (error () nil))))))))

(defun discover-and-claim-session (cwd)
  "Discover current session and atomically claim it for this MCP.

   Strategy:
   1. Check if we already have a claim (from previous call)
   2. Read .claude/latest-session announcement (written by SessionStart)
   3. If announced session exists and unclaimed, claim it
   4. Fallback: claim most recent unclaimed session

   Returns alist with :session-id, :task-dir, :branch, :commit, or NIL."
  ;; 1. Check for existing claim
  (let ((existing (find-our-existing-claim cwd)))
    (when existing
      (return-from discover-and-claim-session existing)))

  (let* ((cwd-with-slash (ensure-trailing-slash cwd))
         (sessions-dir (merge-pathnames ".claude/sessions/" cwd-with-slash)))

    ;; 2. Try announced session first
    (let ((announced (read-latest-session-announcement cwd)))
      (when announced
        (let ((session-dir (merge-pathnames (concatenate 'string announced "/") sessions-dir)))
          (when (and (uiop:directory-exists-p session-dir)
                     (not (session-claimed-p session-dir)))
            (when (try-claim-session session-dir)
              (return-from discover-and-claim-session
                (session-alist-from-dir session-dir)))))))

    ;; 3. Fallback: most recent unclaimed (by session.lock mtime)
    (when (uiop:directory-exists-p sessions-dir)
      (let ((candidates nil))
        (dolist (entry (directory (merge-pathnames "*/" sessions-dir)))
          (let ((lock-file (merge-pathnames "session.lock" entry)))
            (when (probe-file lock-file)
              (push (cons entry (file-write-date lock-file)) candidates))))
        (setf candidates (sort candidates #'> :key #'cdr))
        (dolist (candidate candidates)
          (let ((session-dir (car candidate)))
            (unless (session-claimed-p session-dir)
              (when (try-claim-session session-dir)
                (return-from discover-and-claim-session
                  (session-alist-from-dir session-dir))))))))))

(defun get-current-session (&optional (cwd (or (mcp-find-depot-root)
                                                (namestring (uiop:getcwd)))))
  "Get current session, discovering and claiming lazily on first call.
   In HTTP mode: returns session from Mcp-Session-Id header (no file discovery).
   In stdio mode: discovers via .claude/latest-session file.
   Returns alist with :session-id, :task-dir, :branch, :commit.
   Returns NIL if no session available."
  (if *http-mode*
      ;; HTTP mode: session-id from request header, no file-based discovery
      (let ((http-sid (current-http-session-id)))
        (when http-sid
          (list (cons :session-id http-sid)
                (cons :branch nil)
                (cons :commit nil)
                (cons :task-dir nil))))
      ;; Stdio mode: file-based discovery (original behavior)
      (progn
        (when (or (null *claimed-session*)
                  (not (equal *claimed-session-cwd* cwd)))
          (setf *claimed-session-cwd* cwd
                *claimed-session* (discover-and-claim-session cwd)))
        *claimed-session*)))

(defun current-session-id (&optional (cwd (or (mcp-find-depot-root)
                                               (namestring (uiop:getcwd)))))
  "Get just the session-id from current session."
  (cdr (assoc :session-id (get-current-session cwd))))

(defun current-task-dir (&optional (cwd (or (mcp-find-depot-root)
                                             (namestring (uiop:getcwd)))))
  "Get just the task-dir from current session."
  (cdr (assoc :task-dir (get-current-session cwd))))

;;; =========================================================================
;;; HTTP SESSION CONTEXT MANAGEMENT
;;; =========================================================================
;;; In HTTP mode, each request needs the correct session context loaded.
;;; The :around method on mcp-acceptor (in server.lisp) rebinds *claimed-session*
;;; per-thread so concurrent requests don't race.

(defun resolve-claude-session-id (session)
  "Resolve Claude session ID for a session from the PID registry.

   Returns the Claude session ID string when the registry holds exactly
   one distinct claude-sid, NIL otherwise.

   Two or more distinct sids in the registry mean parallel Claude Code
   shells are alive concurrently.  Peer-PID resolution cannot tell us
   which shell sent this header-less request: a globally-most-recent
   guess routes the bridge write under the wrong claude-sid, where the
   activator session's Stop hook will never see it and keeps re-firing
   on patterns the agent has already given feedback on.  Refusing in
   the ambiguous case turns silent corruption into a fail-loud no-op.

   Callers that need a deterministic answer in parallel-shell scenarios
   must pass the Claude-Session-Id header (CLI populates it from
   $CLAUDE_SESSION_ID; APPLY-CLAUDE-SESSION-ID prefers the header over
   this fallback).  The fallback exists only for hook callers and older
   clients that do not carry the header; in single-shell deployments
   the registry has at most one distinct sid so this path resolves."
  (unless (session-state-claude-session-id session)
    (with-lock-held (*pid-registry-lock*)
      (let ((unique-sids (make-hash-table :test 'equal))
            (best-sid nil)
            (best-time 0))
        (flet ((consider (entry)
                 (when (and (consp entry)
                            (stringp (car entry))
                            (numberp (cdr entry)))
                   (setf (gethash (car entry) unique-sids) t)
                   (when (> (cdr entry) best-time)
                     (setf best-sid (car entry)
                           best-time (cdr entry))))))
          (maphash (lambda (pid value)
                     (declare (ignore pid))
                     (cond
                       ;; New shape: list of (sid . ts) newest-first.
                       ((and (consp value) (consp (car value)))
                        (dolist (entry value) (consider entry)))
                       ;; Legacy shape: a single (sid . ts) cell.
                       (t (consider value))))
                   *pid-claude-session-registry*))
        (when (and best-sid (= 1 (hash-table-count unique-sids)))
          (setf (session-state-claude-session-id session) best-sid)))))
  (session-state-claude-session-id session))

(defun apply-claude-session-id (session &key (header-sid (current-claude-session-id)))
  "Stamp the Claude session ID on SESSION using the most authoritative
   source available.

   Priority order:
   1. HEADER-SID — the explicit Claude-Session-Id header value from the
      current HTTP request (default: read via CURRENT-CLAUDE-SESSION-ID).
      The CLI populates the header from $CLAUDE_SESSION_ID, so each
      Claude Code shell carries its own distinct identity.  When non-nil
      this OVERWRITES any prior value on SESSION so a fresh header value
      recovers a session from a stale peer-PID-derived coalescing.
   2. RESOLVE-CLAUDE-SESSION-ID — the peer-PID-derived fallback used
      when no header is sent (hook callers and older CLI clients).  Only
      sets a value when the session does not already have one.

   The keyword argument exists for testability; production callers use
   the default and do not pass HEADER-SID.

   Returns the resolved Claude session ID (string or NIL)."
  (cond
    (header-sid
     (setf (session-state-claude-session-id session) header-sid))
    (t
     (resolve-claude-session-id session)))
  (session-state-claude-session-id session))

(defun ensure-playbook-session-context ()
  "Ensure session state exists for the current HTTP request.
   In HTTP mode: gets/creates session-state from Mcp-Session-Id header,
   updates last-active timestamp, and stamps the Claude session ID via
   APPLY-CLAUDE-SESSION-ID (header preferred, peer-PID fallback).
   In stdio mode: no-op (session discovered via files).
   Returns session-id or NIL."
  (if *http-mode*
      (let ((http-sid (current-http-session-id)))
        (when http-sid
          (let ((session (get-or-create-session http-sid)))
            (setf (session-state-last-active session) (get-universal-time))
            (apply-claude-session-id session))
          http-sid))
      (current-session-id)))

(defun cleanup-inactive-playbook-sessions (&optional (max-age-hours 4))
  "Remove session states inactive for more than MAX-AGE-HOURS.
   Called opportunistically in HTTP mode to prevent memory leaks.
   Returns count of removed sessions."
  (let* ((cutoff (- (get-universal-time) (* max-age-hours 60 60)))
         (to-remove nil))
    (with-lock-held (*session-states-lock*)
      (maphash (lambda (id state)
                 (when (< (session-state-last-active state) cutoff)
                   (push id to-remove)))
               *session-states*)
      (dolist (id to-remove)
        (remhash id *session-states*)))
    (length to-remove)))

;;; =========================================================================
;;; SESSION-SCOPED STATE FILES
;;; =========================================================================
;;; Pattern activations are written to the session directory so that
;;; SessionEnd hooks can find them for co-application mining.
;;; Path: .claude/sessions/{session_id}/playbook/activated.jsonl

(defun session-activated-path (cwd session-id)
  "Return path to session-scoped activated.jsonl.
   Path: {cwd}/.claude/sessions/{session_id}/playbook/activated.jsonl"
  (merge-pathnames (format nil ".claude/sessions/~A/playbook/activated.jsonl" session-id)
                   (ensure-trailing-slash cwd)))

(defun mcp-activated-path (cwd)
  "Return path to activated.jsonl for current session.
   Uses session discovery to find the correct session directory.
   Falls back to CWD-scoped path if no session discovered."
  (let ((session-id (current-session-id cwd)))
    (if session-id
        (session-activated-path cwd session-id)
        ;; Fallback for when session discovery fails
        (merge-pathnames ".claude/playbook/activated.jsonl"
                         (ensure-trailing-slash cwd)))))

(defun write-activation-to-mcp-state (cwd pattern-ids)
  "Write pattern IDs to .claude/playbook/activated.jsonl (append).
   Each call appends one JSON array line. Returns path if written, NIL otherwise."
  (when pattern-ids
    (let ((path (mcp-activated-path cwd)))
      (ensure-directories-exist path)
      (with-open-file (s path :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (yason:encode pattern-ids s)
        (terpri s))
      (namestring path))))

(defun read-mcp-activated-ids (cwd)
  "Read all pattern IDs from .claude/playbook/activated.jsonl.
   Returns deduplicated list of pattern ID strings."
  (let ((path (mcp-activated-path cwd)))
    (when (probe-file path)
      (let ((all-ids nil))
        (with-open-file (s path :direction :input)
          (loop for line = (read-line s nil nil)
                while line
                when (plusp (length (string-trim '(#\Space #\Tab) line)))
                do (let ((ids (yason:parse line)))
                     (dolist (id ids)
                       (pushnew id all-ids :test #'string=)))))
        (nreverse all-ids)))))

(defun clear-mcp-activated-state (cwd)
  "Delete .claude/playbook/activated.jsonl if it exists.
   Returns T if file was deleted, NIL otherwise."
  (let ((path (mcp-activated-path cwd)))
    (when (probe-file path)
      (delete-file path)
      t)))

;;; =========================================================================
;;; FEEDBACK STATE FILE (for Stop hook)
;;; =========================================================================
;;; The Stop hook needs to know which activated patterns have received feedback.
;;; Since hooks run as separate processes, they can't access MCP in-memory state.
;;; This file bridges the gap: MCP writes it, hook reads it.
;;; Path: .claude/sessions/{session_id}/playbook/feedback-state.json

(defun feedback-state-path (cwd session-id)
  "Return path to feedback-state.json for a session."
  (merge-pathnames (format nil ".claude/sessions/~A/playbook/feedback-state.json" session-id)
                   (ensure-trailing-slash cwd)))

(defun sort-pending-by-uncertainty (pending-ids)
  "Sort pattern IDs by Beta variance, highest first (most uncertain = most valuable to ask about).
   Patterns not found in store get variance 1.0 (treat as maximally uncertain)."
  (flet ((beta-variance-for-id (id)
           (let ((p (get-pattern id)))
             (if p
                 (let* ((h (pattern-helpful p)) (m (pattern-harmful p))
                        (a (1+ h)) (b (1+ m)) (n (+ a b)))
                   (/ (* (float a) (float b)) (* (float (* n n)) (float (1+ n)))))
                 1.0))))
    (sort (copy-list pending-ids) #'> :key #'beta-variance-for-id)))

(defun %feedback-state-list-field (state key)
  "Pull a list-of-strings field from a parsed feedback-state hash.
   Yason returns JSON arrays as lists by default, but be defensive against
   vectors in case a future writer changes that."
  (let ((v (and state (gethash key state))))
    (cond ((null v) nil)
          ((listp v) v)
          ((vectorp v) (coerce v 'list))
          (t (list v)))))

(defun write-feedback-state-file (cwd session-id)
  "Write the merged feedback state to the session directory.

   Two contracts beyond a plain dump of in-memory state:

   1. The path is always keyed by the Claude session ID. The Stop hook
      only knows how to look up files under
      .claude/sessions/<claude-sid>/playbook/feedback-state.json — any
      MCP-sid-keyed write would be invisible to it. Refuse (return NIL)
      when the session-state has no resolved claude-session-id.
      GET-OR-CREATE-SESSION populates it eagerly in stdio mode; in HTTP
      mode it must have been set by APPLY-CLAUDE-SESSION-ID at request
      entry.

   2. The on-disk file is the union across every MCP session that shares
      this Claude session ID — multiple parallel Claude Code shells under
      a single Claude-sid each hold their own activated / feedback_given
      lists. Read-modify-write under a file lock unions this caller's
      lists with whatever is already on disk and recomputes
      pending = activated ∖ feedback_given.

   Creates {\"activated\":[...],\"feedback_given\":[...],\"pending\":[...],\"count\":N}."
  (when (and cwd session-id)
    (let ((session (get-session session-id)))
      (when session
        (handler-case
            (let ((claude-sid (session-state-claude-session-id session)))
              (unless claude-sid
                (return-from write-feedback-state-file nil))
              (let* ((path (feedback-state-path cwd claude-sid))
                     (this-activated (session-state-activated-patterns session))
                     (this-feedback (mapcar #'car (session-state-feedback-given session))))
                (ensure-directories-exist path)
                (with-file-lock (path :wait t)
                  (let* ((existing (read-feedback-state-file path))
                         (merged-activated
                           (union (%feedback-state-list-field existing "activated")
                                  this-activated
                                  :test #'string=))
                         (merged-feedback
                           (union (%feedback-state-list-field existing "feedback_given")
                                  this-feedback
                                  :test #'string=))
                         (pending (sort-pending-by-uncertainty
                                   (set-difference merged-activated merged-feedback
                                                   :test #'string=)))
                         (ht (make-hash-table :test 'equal)))
                    (setf (gethash "activated" ht) (coerce merged-activated 'vector))
                    (setf (gethash "feedback_given" ht) (coerce merged-feedback 'vector))
                    (setf (gethash "pending" ht) (coerce pending 'vector))
                    (setf (gethash "count" ht) (length pending))
                    (with-open-file (s path :direction :output :if-exists :supersede)
                      (yason:encode ht s))
                    path))))
          (error () nil))))))

(defun read-feedback-state-file (path)
  "Read feedback state from JSON file. Returns hash-table or NIL."
  (when (probe-file path)
    (handler-case
        (yason:parse (uiop:read-file-string path))
      (error () nil))))

;;; =========================================================================
;;; MISKEYED FEEDBACK-STATE RECONCILIATION
;;; =========================================================================
;;; The Stop hook reads feedback-state.json under
;;; .claude/sessions/<claude-sid>/playbook/, where claude-sid is shaped
;;; like a UUID.  Files under non-UUID-shaped session directories (e.g.
;;; 32-hex-uppercase MCP-sid paths produced when peer-PID resolution
;;; misKeyed the write) are unreachable to the hook.
;;;
;;; The file payload carries no claude-sid back-pointer, so automatic
;;; remapping into the canonical path is not recoverable.  The
;;; reconciliation pass archives misKeyed payloads to
;;; .claude/sessions/.miskeyed/<dirname>/feedback-state.json so they
;;; remain available for forensics without polluting the live lookup
;;; surface.

(defparameter *uuid-shape-scanner*
  (cl-ppcre:create-scanner
   "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")
  "Compiled regex matching a session directory whose name is shaped
   like a UUID — the canonical Claude-Session-Id form.")

(defun %uuid-shaped-p (name)
  "Return T when NAME is a UUID-shaped session directory name."
  (and (stringp name)
       (cl-ppcre:scan *uuid-shape-scanner* name)
       t))

(defun %session-dir-name (session-dir)
  "Return the trailing directory component of SESSION-DIR as a string."
  (first (last (pathname-directory
                (uiop:ensure-directory-pathname session-dir)))))

(defun reconcile-miskeyed-feedback-state-files (cwd)
  "Archive feedback-state.json files that live under non-UUID-shaped
   session directories at CWD.  These are leftovers from peer-PID
   coalescing under MCP-sid keys; the Stop hook cannot find them.

   The archive root is <cwd>/.claude/sessions/.miskeyed/<dirname>/
   feedback-state.json — the original directory name is preserved so
   forensic correlation back to MCP-sid logs stays possible.

   Returns an alist summary suitable for a single structured log line:
     ((:scanned . N) (:archived . M) (:archive-root . path-or-nil))"
  (let* ((sessions-dir (merge-pathnames ".claude/sessions/"
                                        (ensure-trailing-slash cwd)))
         (archive-root (merge-pathnames ".miskeyed/" sessions-dir))
         (scanned 0)
         (archived 0))
    (when (uiop:directory-exists-p sessions-dir)
      (dolist (entry (directory (merge-pathnames "*/" sessions-dir)))
        (let ((dir-name (%session-dir-name entry)))
          ;; Skip our own archive root and any UUID-shaped directories.
          (unless (or (string= dir-name ".miskeyed")
                      (%uuid-shaped-p dir-name))
            (let ((fb-file (merge-pathnames "playbook/feedback-state.json"
                                            entry)))
              (when (probe-file fb-file)
                (incf scanned)
                (let ((dest-dir (merge-pathnames
                                 (concatenate 'string dir-name "/")
                                 archive-root)))
                  (handler-case
                      (progn
                        (ensure-directories-exist
                         (merge-pathnames "feedback-state.json" dest-dir))
                        (rename-file
                         fb-file
                         (merge-pathnames "feedback-state.json" dest-dir))
                        (incf archived))
                    (error (e)
                      (format *error-output*
                              "~&;; playbook-miskeyed-archive: failed for ~A: ~A~%"
                              dir-name e))))))))))
    (when (plusp scanned)
      (format *error-output*
              "~&;; playbook-miskeyed-archive: scanned=~D archived=~D root=~A~%"
              scanned archived (when (plusp archived) (namestring archive-root))))
    (list (cons :scanned scanned)
          (cons :archived archived)
          (cons :archive-root (when (plusp archived) (namestring archive-root))))))

;;; =========================================================================
;;; SIGNAL HANDLER CLEANUP
;;; =========================================================================
;;; Clean up mcp.lock file on graceful shutdown (SIGTERM/SIGINT).
;;; This prevents stale lock files from blocking future session claims.
;;; Note: *mcp-lock-path* is defined above near *claimed-session*.

(defun cleanup-mcp-lock ()
  "Remove mcp.lock file if we created one.
   Safe to call multiple times - only deletes if path is set and file exists."
  (when *mcp-lock-path*
    (handler-case
        (when (probe-file *mcp-lock-path*)
          (delete-file *mcp-lock-path*)
          (format *error-output* "Cleaned up ~A~%" *mcp-lock-path*))
      (error (e)
        (format *error-output* "Warning: failed to clean up ~A: ~A~%" *mcp-lock-path* e)))
    (setf *mcp-lock-path* nil)))

(defun graceful-shutdown (signal info context)
  "Signal handler for SIGTERM/SIGINT: cleanup and exit.
   Removes mcp.lock to allow future sessions to claim."
  (declare (ignore info context))
  (format *error-output* "~&Received signal ~A, shutting down...~%" signal)
  (cleanup-mcp-lock)
  (sb-ext:exit :code 0))

(defun install-signal-handlers ()
  "Install signal handlers for graceful shutdown.
   SIGTERM (15): Standard termination signal from process managers
   SIGINT (2): Interrupt signal (Ctrl+C)"
  (sb-sys:enable-interrupt sb-posix:sigterm #'graceful-shutdown)
  (sb-sys:enable-interrupt sb-posix:sigint #'graceful-shutdown)
  (when *log-verbose*
    (format *error-output* "Installed signal handlers for graceful shutdown~%")))
