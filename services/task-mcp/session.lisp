(in-package #:task-mcp)

;;; Session state (globals for stdio mode, rebindable for HTTP mode)

(defvar *current-task-id* nil
  "Current task ID for this session.")

(defvar *session-id* nil
  "Unique session identifier.")

(defvar *session-vc* nil
  "Session vector clock, incremented with each event.")

(defvar *claude-pid* nil
  "Claude Code's PID, obtained from PARENT_PID env var passed through bwrap.")

(defvar *event-counter* 0
  "Monotonic counter for unique event IDs within a session.")

;;; Per-request caches (bound per HTTP request in server.lisp :around method)
;;; These eliminate redundant computation within a single bootstrap/get call.

(defvar *active-sessions-cache* nil
  "Per-request cache for read-active-sessions result.
   Key: max-age-hours, Value: result list.  NIL outside HTTP request scope.")

(defvar *active-fingerprints-cache* nil
  "Per-request cache for compute-active-fingerprints result.
   Key: max-age-hours, Value: fingerprint list.  NIL outside HTTP request scope.")

;;; ==========================================================================
;;; HTTP Multi-Session Support: Session Context Registry
;;; ==========================================================================
;;; For HTTP transport, multiple clients connect to a single server process.
;;; Each client has its own session context (task-id, vector-clock, etc.).
;;;
;;; Architecture:
;;; - *session-contexts*: Hash table mapping session-id -> session-context
;;; - with-session-context: Macro to rebind globals for request scope
;;; - Stdio mode: Uses globals directly (single session)
;;; - HTTP mode: Each request rebinds globals from/to registry

(defstruct (session-context (:conc-name ctx-))
  "Per-session context for HTTP multi-client support."
  (id "" :type string)
  (task-id nil)
  (vector-clock nil)
  (claude-pid nil)
  ;; Agent team metadata (set via register-pid when session is a teammate)
  (team-name nil :type (or null string))
  (agent-name nil :type (or null string))
  (agent-type nil :type (or null string))
  (event-counter 0 :type fixnum)
  (created-at (get-universal-time) :type integer)
  (last-active (get-universal-time) :type integer))

(defvar *session-contexts* (make-hash-table :test 'equal)
  "Registry of session contexts, keyed by session-id.
   Used for HTTP multi-session support.")

(defvar *session-contexts-lock* (bt:make-lock "session-contexts")
  "Lock for *session-contexts* hash table.
   SBCL hash tables are not thread-safe; concurrent gethash/setf from
   Hunchentoot request threads can return wrong entries.  All gethash,
   setf-gethash, maphash, and remhash on *session-contexts* must be
   wrapped in bt:with-lock-held on this lock.")

(defvar *http-mode* nil
  "T when running in HTTP transport mode, NIL for stdio.")

(defvar *in-mutation* nil
  "T during tq-mutation-handler execution.
   Suppresses session registry reads (ensure-session-context) and writes
   (finalize-session-context) so the mutation handler can directly control
   *current-task-id* without polluting the shared registry with transient
   task IDs.  The handler's unwind-protect cleanup restores the registry
   after the mutation completes.")


(define-condition unknown-session (error)
  ((session-id :initarg :session-id :reader unknown-session-session-id))
  (:report
   (lambda (c stream)
     (format stream "session ~S not found in registry — client must reinitialize"
             (unknown-session-session-id c))))
  (:documentation
   "Signalled when a request arrives carrying an Mcp-Session-Id header
    that does not correspond to a context in *SESSION-CONTEXTS*.  The
    HTTP layer translates this to 404 with code=session-not-found so
    the CLI can drop its persisted sid and re-init.

    Before this condition existed, GET-SESSION-CONTEXT silently
    auto-created a virgin context for any unknown sid.  When task-mcp's
    4h cleanup-inactive-sessions evicted a context that the framework's
    no-TTL session-store still considered valid, the next request from
    the same client would resurrect a virgin (task-id=NIL) context with
    no indication to the client that any state had been lost."))

(defun get-session-context (session-id)
  "Look up the session context for SESSION-ID.  Returns NIL on miss.

   This is a pure read.  See CREATE-SESSION-CONTEXT for the legitimate
   creation paths (post-/initialize hook in the :AROUND method, and
   REGISTER-CLAUDE-PID); see ENSURE-SESSION-CONTEXT for the validation
   gate that signals UNKNOWN-SESSION when a request supplies an
   Mcp-Session-Id whose context has been evicted."
  (with-timed-lock (*session-contexts-lock* :get-ctx)
    (gethash session-id *session-contexts*)))

(defun create-session-context (session-id)
  "Insert a fresh session-context for SESSION-ID and return it.
   Idempotent: if a context already exists for SESSION-ID, return it
   unchanged.

   Legitimate creation paths:
   - The :AROUND method's post-call-next-method hook, when the
     framework allocated a new Mcp-Session-Id during /initialize.
   - REGISTER-CLAUDE-PID, when the SessionStart hook stitches a
     Claude PID onto an MCP session that has not yet seen its first
     tool call."
  (with-timed-lock (*session-contexts-lock* :create-ctx)
    (or (gethash session-id *session-contexts*)
        (let ((ctx (make-session-context
                    :id session-id
                    :vector-clock (crdt:make-vector-clock))))
          (setf (gethash session-id *session-contexts*) ctx)))))

(defun save-session-context (session-id)
  "Save current globals to session context registry.
   Called after request processing in HTTP mode.
   Lock held during struct modification to prevent interleaved writes
   from concurrent requests on the same session.

   CTX-CLAUDE-PID is only overwritten when the thread-local
   *CLAUDE-PID* is non-NIL.  This protects a freshly registered PID
   (written by REGISTER-CLAUDE-PID via /register-pid) from being
   clobbered by an in-flight request whose :around captured
   *CLAUDE-PID* = NIL before /register-pid landed."
  (with-timed-lock (*session-contexts-lock* :save-ctx)
    (let ((ctx (or (gethash session-id *session-contexts*)
                   (let ((new (make-session-context
                               :id session-id
                               :vector-clock (crdt:make-vector-clock))))
                     (setf (gethash session-id *session-contexts*) new)))))
      (setf (ctx-task-id ctx) *current-task-id*)
      ;; Merge the thread-local VC into the registry's stored VC
      ;; (pointwise max) so we never go backward when a concurrent
      ;; request on the same session has already advanced any entry.
      (setf (ctx-vector-clock ctx)
            (if (ctx-vector-clock ctx)
                (crdt:vc-merge (ctx-vector-clock ctx) *session-vc*)
                *session-vc*))
      (when *claude-pid*
        (setf (ctx-claude-pid ctx) *claude-pid*))
      (setf (ctx-event-counter ctx) *event-counter*)
      (setf (ctx-last-active ctx) (get-universal-time))
      ctx)))

(defun maybe-save-session-context (session-id snapshot-task-id snapshot-event-counter)
  "Save session context only when this request actually mutated it.
   Called from the HTTP :around method on unwind.

   Compares the current thread-local *CURRENT-TASK-ID* and *EVENT-COUNTER*
   to the snapshot taken right after ENSURE-SESSION-CONTEXT.  If either
   differs, the request produced persistable session state (event emitted,
   current task switched) and a full save is performed.  Otherwise only
   LAST-ACTIVE is updated so CLEANUP-INACTIVE-SESSIONS sees the request
   as a liveness signal.

   This discipline prevents long-lived read-only SSE streams from
   resurrecting stale CTX-TASK-ID values: their thread-local
   *CURRENT-TASK-ID* is whatever LOAD-SESSION-CONTEXT read at request
   start, and on unwind they would otherwise save that stale value back
   over any concurrent reset."
  (with-timed-lock (*session-contexts-lock* :maybe-save-ctx)
    (let ((ctx (or (gethash session-id *session-contexts*)
                   (let ((new (make-session-context
                               :id session-id
                               :vector-clock (crdt:make-vector-clock))))
                     (setf (gethash session-id *session-contexts*) new)))))
      (setf (ctx-last-active ctx) (get-universal-time))
      (when (or (not (equal *current-task-id* snapshot-task-id))
                (not (eql *event-counter* snapshot-event-counter)))
        (setf (ctx-task-id ctx) *current-task-id*)
        ;; See SAVE-SESSION-CONTEXT: pointwise-max merge so concurrent
        ;; advances to the registry VC are not rolled back.
        (setf (ctx-vector-clock ctx)
              (if (ctx-vector-clock ctx)
                  (crdt:vc-merge (ctx-vector-clock ctx) *session-vc*)
                  *session-vc*))
        ;; See SAVE-SESSION-CONTEXT: only overwrite CTX-CLAUDE-PID when
        ;; the thread-local has a value, so a concurrent /register-pid
        ;; cannot be clobbered by a request that loaded NIL pre-register.
        (when *claude-pid*
          (setf (ctx-claude-pid ctx) *claude-pid*))
        (setf (ctx-event-counter ctx) *event-counter*))
      ctx)))

(defun load-session-context (session-id)
  "Load session context into globals.
   Called before request processing in HTTP mode.

   Signals UNKNOWN-SESSION when SESSION-ID has no context in the
   registry — callers must have validated existence (typically via
   ENSURE-SESSION-CONTEXT, which runs first in the :AROUND method).

   The vector clock is COPIED into a fresh struct so concurrent
   requests on the same MCP session each operate on an independent
   hash table.  Otherwise two threads' VC-INCREMENT calls would race
   on the shared *SESSION-CONTEXTS* entry's hash table — SBCL hash
   tables are not safe for concurrent mutation.  The save path
   merges the per-thread VC back into the registry pointwise-max
   under the lock, preserving monotonicity across the join."
  (let ((ctx (get-session-context session-id)))
    (unless ctx
      (error 'unknown-session :session-id session-id))
    (setf *session-id* session-id)
    (setf *current-task-id* (ctx-task-id ctx))
    (setf *session-vc*
          (if (ctx-vector-clock ctx)
              (crdt:vc-copy (ctx-vector-clock ctx))
              (crdt:make-vector-clock)))
    (setf *claude-pid* (ctx-claude-pid ctx))
    (setf *event-counter* (ctx-event-counter ctx))
    ctx))

(defmacro with-session-context ((session-id) &body body)
  "Execute BODY with session context for SESSION-ID.
   Rebinds globals to this session's values, then saves back on exit.
   Used by HTTP handler to isolate sessions."
  (let ((sid (gensym "SESSION-ID-")))
    `(let ((,sid ,session-id))
       (load-session-context ,sid)
       (unwind-protect
            (progn ,@body)
         (save-session-context ,sid)))))

(defun cleanup-inactive-sessions (&optional (max-age-hours 4))
  "Remove session contexts inactive for more than MAX-AGE-HOURS.
   Called opportunistically to prevent memory leaks."
  (let* ((cutoff (- (get-universal-time) (* max-age-hours 60 60)))
         (to-remove nil))
    (with-timed-lock (*session-contexts-lock* :cleanup-inactive)
      (maphash (lambda (id ctx)
                 (when (< (ctx-last-active ctx) cutoff)
                   (push id to-remove)))
               *session-contexts*)
      (dolist (id to-remove)
        (remhash id *session-contexts*)))
    (length to-remove)))

(defun register-claude-pid (session-id pid &key team-name agent-name agent-type)
  "Register Claude Code's PID for a session.
   Called by SessionStart hook to correlate HTTP session with Claude process.
   When TEAM-NAME is non-nil, also stores agent team metadata.

   The SessionStart hook may fire before any tool call has loaded the
   MCP session into the task-mcp registry, so this is a legitimate
   creation path: CREATE-SESSION-CONTEXT is idempotent — it returns
   the existing context unchanged if one already exists."
  (let ((ctx (create-session-context session-id)))
    (setf (ctx-claude-pid ctx) pid)
    (when team-name
      (setf (ctx-team-name ctx) team-name)
      (setf (ctx-agent-name ctx) agent-name)
      (setf (ctx-agent-type ctx) agent-type))
    (if team-name
        (format nil "Registered PID ~D for session ~A (team: ~A, agent: ~A)"
                pid session-id team-name agent-name)
        (format nil "Registered PID ~D for session ~A"
                pid session-id))))

;;; ==========================================================================
;;; TCP Peer PID Resolution (Linux /proc-based)
;;; ==========================================================================
;;; Resolves which Claude Code process owns an MCP HTTP connection by tracing
;;; the TCP connection back to its owning PID.
;;;
;;; Linux: remote-port → /proc/net/tcp (inode) → /proc/<pid>/fd/ (socket match)
;;; macOS: remote-port → lsof -i TCP (PID owning connection)
;;;
;;; Called once per MCP session in ensure-session-context.  Result is latched
;;; on ctx-claude-pid so subsequent requests skip the scan.  Only registered
;;; Claude PIDs are checked (typically 1-3), so overhead is minimal.
;;;
;;; Correctness argument: each TCP connection is owned by exactly one PID.
;;; Parallel sessions have different PIDs and different TCP connections,
;;; so the mapping is unambiguous on both platforms.

(defun find-client-socket-inode (client-port server-port)
  "Find the socket inode for the CLIENT side of a localhost TCP connection.
   Reads /proc/net/tcp for the entry where:
     - local_port = CLIENT-PORT (the client's ephemeral port)
     - remote_port = SERVER-PORT (our daemon port)
     - state = 01 (ESTABLISHED)
   Returns the inode as an integer, or NIL if not found."
  (handler-case
      (with-open-file (s "/proc/net/tcp" :if-does-not-exist nil)
        (when s
          (read-line s nil nil)  ; skip header
          (loop for line = (read-line s nil nil)
                while line
                do (let* ((trimmed (string-trim '(#\Space #\Tab) line))
                          (parts (remove "" (uiop:split-string trimmed)
                                         :test #'string=)))
                     ;; Fields: [0]=sl: [1]=local_addr:port [2]=rem_addr:port
                     ;;         [3]=state ... [9]=inode
                     (when (>= (length parts) 10)
                       (let* ((local-str (nth 1 parts))
                              (remote-str (nth 2 parts))
                              (state (nth 3 parts))
                              (colon-l (position #\: local-str :from-end t))
                              (colon-r (position #\: remote-str :from-end t)))
                         (when (and (string= state "01") colon-l colon-r)
                           (let ((lport (ignore-errors
                                          (parse-integer local-str
                                                         :start (1+ colon-l)
                                                         :radix 16)))
                                 (rport (ignore-errors
                                          (parse-integer remote-str
                                                         :start (1+ colon-r)
                                                         :radix 16))))
                             (when (and (eql lport client-port)
                                        (eql rport server-port))
                               (return (ignore-errors
                                         (parse-integer
                                          (nth 9 parts)))))))))))))
    (error () nil)))

(defun pid-owns-socket-p (pid inode)
  "Check if PID owns a socket with the given INODE.
   Scans /proc/<pid>/fd/0 through /proc/<pid>/fd/255 for a symlink
   matching socket:[INODE].  Returns T if found.  Returns NIL
   immediately when /proc/<pid> does not exist (dead process)."
  (handler-case
      (when (probe-file (format nil "/proc/~D" pid))
        (let ((target (format nil "socket:[~D]" inode)))
          (loop for fd from 0 below 256
                for link = (ignore-errors
                             (sb-posix:readlink
                              (format nil "/proc/~D/fd/~D" pid fd)))
                when (and link (string= link target))
                return t)))
    (error () nil)))

(defun snapshot-registered-pids ()
  "Return a fresh list of all non-NIL claude PIDs currently in
   *SESSION-CONTEXTS*.  Takes *SESSION-CONTEXTS-LOCK* for the
   duration of the MAPHASH only; callers iterate /proc lock-free."
  (let ((pids nil))
    (with-timed-lock (*session-contexts-lock* :snapshot-pids)
      (maphash (lambda (sid ctx)
                 (declare (ignore sid))
                 (let ((cpid (ctx-claude-pid ctx)))
                   (when cpid (push cpid pids))))
               *session-contexts*))
    (delete-duplicates pids)))

(defun resolve-peer-pid/linux (remote-port server-port)
  "Linux implementation: trace TCP connection via /proc/net/tcp inodes.
   Returns the registered Claude PID owning the connection, or NIL.

   Lock discipline: snapshot the registered PID list under
   *SESSION-CONTEXTS-LOCK*, then release the lock and probe /proc
   lock-free.  The O(256) readlink loop per PID therefore cannot
   block any other request's SAVE-SESSION-CONTEXT or
   GET-SESSION-CONTEXT call.  PID-OWNS-SOCKET-P's PROBE-FILE
   short-circuits dead PIDs in a single syscall."
  (let ((inode (find-client-socket-inode remote-port server-port)))
    (when inode
      (loop for cpid in (snapshot-registered-pids)
            when (pid-owns-socket-p cpid inode)
              return cpid))))

(defun resolve-peer-pid/macos (remote-port server-port)
  "macOS implementation: use lsof to find PID owning a TCP connection.
   Runs: lsof -i TCP@127.0.0.1:<remote-port> -nP -sTCP:ESTABLISHED -t
   Returns the PID if it matches a registered Claude PID, or NIL.
   The -t flag outputs bare PIDs (one per line), -nP suppresses name
   resolution, -sTCP:ESTABLISHED filters to active connections."
  (declare (ignore server-port))
  (handler-case
      (let ((output (uiop:run-program
                     (list "lsof" "-i"
                           (format nil "TCP@127.0.0.1:~D" remote-port)
                           "-nP" "-sTCP:ESTABLISHED" "-t")
                     :output :string
                     :error-output nil
                     :ignore-error-status t)))
        (when (and output (plusp (length output)))
          (let ((candidate-pids
                  (loop for line in (uiop:split-string output :separator '(#\Newline))
                        for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
                        when (plusp (length trimmed))
                          collect (ignore-errors (parse-integer trimmed))))
                (registered (snapshot-registered-pids)))
            (loop for pid in candidate-pids
                  when (and pid (member pid registered :test #'eql))
                    return pid))))
    (error () nil)))

(defun resolve-peer-pid ()
  "Resolve the PID of the process making the current HTTP request.
   Dispatches to platform-specific implementation:
   - Linux: /proc/net/tcp socket inode tracing
   - macOS: lsof -i TCP for connection ownership
   Returns integer PID or NIL.  Only called in HTTP mode."
  (handler-case
      (when *http-mode*
        (let ((remote-port
                (ignore-errors
                  (funcall (find-symbol "REMOTE-PORT*" "HUNCHENTOOT"))))
              (server-port
                (let ((p (uiop:getenv "TASK_MCP_PORT")))
                  (if (and p (plusp (length p)))
                      (parse-integer p :junk-allowed t)
                      8090))))
          (when (and remote-port server-port)
            (if (probe-file "/proc/net/tcp")
                (resolve-peer-pid/linux remote-port server-port)
                (resolve-peer-pid/macos remote-port server-port)))))
    (error () nil)))

(defun current-http-session-id ()
  "Get the session ID from the current HTTP request's Mcp-Session-Id header.
   Returns NIL if not in HTTP mode or no request is active.
   Safe to call even if hunchentoot isn't loaded."
  (when *http-mode*
    (ignore-errors
     ;; Access hunchentoot symbols safely - they're available when mcp-http is loaded
     (let ((request (symbol-value (find-symbol "*REQUEST*" "HUNCHENTOOT"))))
       (when request
         (funcall (find-symbol "HEADER-IN" "HUNCHENTOOT")
                  "Mcp-Session-Id" request))))))

(defun ensure-session-context ()
  "Ensure the correct session context is loaded for the current request.
   In HTTP mode: loads context from registry based on Mcp-Session-Id header.
   Also resolves peer PID for MCP sessions that lack one (once per session,
   via /proc/net/tcp socket inode tracing).
   In stdio mode: no-op (globals already correct).

   Call this at the start of any operation that depends on session state.
   Returns the session-id being used."
  (if *http-mode*
      (let ((http-session-id (current-http-session-id)))
        (when http-session-id
          ;; Skip reload during mutations — *current-task-id* is managed
          ;; directly by tq-mutation-handler and must not be overwritten
          ;; by stale or intermediate values from the shared registry.
          (unless *in-mutation*
            ;; Always load — per-thread LET bindings (from :around method on
            ;; acceptor-dispatch-request) mean each request starts fresh.
            ;; The old skip optimization was racy: another thread could SETF
            ;; *session-id* between our comparison and our read of *current-task-id*.
            (load-session-context http-session-id))
          ;; Resolve peer PID for MCP sessions that don't have one yet.
          ;; This bridges MCP sessions to registered Claude sessions by tracing
          ;; the TCP connection through /proc/net/tcp → /proc/<pid>/fd/.
          (unless *claude-pid*
            (let ((peer-pid (resolve-peer-pid)))
              (when peer-pid
                (setf *claude-pid* peer-pid)
                (with-timed-lock (*session-contexts-lock* :set-peer-pid)
                  (let ((ctx (gethash http-session-id *session-contexts*)))
                    (when ctx
                      (setf (ctx-claude-pid ctx) peer-pid))))))))
        (or http-session-id *session-id*))
      *session-id*))

(defun finalize-session-context ()
  "Save current session context back to registry after request processing.
   In HTTP mode: persists context for this session.
   In stdio mode: no-op.
   Suppressed during mutations (*in-mutation*) to prevent transient task IDs
   from polluting the shared registry.

   Call this at the end of operations that modify session state."
  (when (and *http-mode* *session-id* (not *in-mutation*))
    (save-session-context *session-id*)))

(defun initialize-session ()
  "Initialize session from environment.
   Detects tasks root directory."
  (setf *session-id*
        (or (uiop:getenv "CLAUDE_SESSION_ID")
            (format nil "s-~A" (get-universal-time))))
  (setf *session-vc* (crdt:make-vector-clock))
  (let ((current (uiop:getenv "TASK_MCP_CURRENT")))
    (when (and current (> (length current) 0))
      (setf *current-task-id* current)))
  (task:detect-tasks-root))

(defun require-current-task ()
  "Signal error if no current task is set.
   In HTTP mode, ensures session context is loaded first."
  (ensure-session-context)
  (unless *current-task-id*
    (error "No current task. Use task_set_current or task_create first.")))

(defun make-event-id ()
  "Generate a unique event ID with monotonic counter."
  (format nil "~A-~A-~A" *session-id* (get-universal-time) (incf *event-counter*)))

(defparameter *completed-allowed-events*
  '(:task.update-status :session.join :session.team-join :task.create
    :pattern.activate :task.fork :task.link)
  "Event types permitted on completed tasks.
   Additive provenance (fork, link) allowed; subtractive (sever) requires reopen.")

(defun current-task-completed-p (&optional (task-id *current-task-id*))
  "Check whether TASK-ID has status completed.
   Defaults to *current-task-id* for the common case.  Callers that
   route an emit to a different target (task_complete, task_reopen,
   task_release) pass the target explicitly so the completed-gate
   checks the same task the emit will write to."
  (let* ((path (task:task-events-path task-id))
         (log (task:elog-load path))
         (events (reverse (task:event-log-events log))))
    (when events
      (let ((state (task:compute-state events)))
        (string= "completed"
                 (crdt:lww-value (task:task-state-status state)))))))

(defparameter *event-required-fields*
  '((:task.create        :name)
    (:task.link          :target-id :edge-type)
    (:task.sever         :target-id :edge-type)
    (:task.fork          :child-id  :edge-type)
    (:task.set-metadata  :key)
    (:observation        :text))
  "Per-event-type list of plist keys whose values must be present and non-nil
   in the event DATA passed to EMIT-EVENT.  Event types not listed here have
   no required fields enforced at this layer.

   Rationale: the event log is a system boundary for serialized data; a JSON
   null surviving to disk silently propagates through replay and renders as
   the literal string \"NIL\" in derived views.  This guard catches any
   caller — direct MCP tool, TQ mutation, or internal — that forgot to
   validate at its own boundary.

   Note: :task.set-metadata :value is intentionally exempt — a nil value is
   the legitimate way to express 'no value bound for this key'.  Only :key is
   required.  If a future event type needs to clear a key explicitly, prefer
   a distinct :clear-meta event type over weakening this guard.")

(defun check-required-fields (type data)
  "Return the list of required field keys missing or nil in DATA for event TYPE.
   Returns NIL if every required field for TYPE is present and non-nil."
  (loop for field in (cdr (assoc type *event-required-fields*))
        for value = (getf data field 'missing)
        when (or (eq value 'missing) (null value))
        collect field))

(defun emit-event (type &optional data &key (task-id *current-task-id*))
  "Emit an event to TASK-ID's event log.
   TASK-ID defaults to *current-task-id*.  Callers that need to write
   to a task other than the session's current focus (task_complete,
   task_reopen, task_release) pass :task-id explicitly.

   Rejects mutating events when TASK-ID is completed.
   Rejects events whose required fields (per *event-required-fields*) are
   missing or nil — this is the boundary check that prevents JSON nulls
   from surviving to disk.
   Increments vector clock, creates event, appends to file.
   Uses append-only writes for concurrent safety — multiple sessions
   can emit events to the same task without lost updates.

   Session context (Mcp-Session-Id → ctx-task-id, vector clock, event
   counter) is loaded once per request by the HTTP :AROUND method on
   ACCEPTOR-DISPATCH-REQUEST and by DEFINE-TASK-TOOL's injected
   REQUIRE-CURRENT-TASK.  EMIT-EVENT does not re-load it: re-loading
   would overwrite *CURRENT-TASK-ID* from the registry mid-body and
   defeat the explicit :TASK-ID argument."
  (unless task-id
    (error "emit-event: no task-id supplied and *current-task-id* is nil. ~
            Either pass :task-id, or run task_set_current / task_create first."))
  (when (and (current-task-completed-p task-id)
             (not (member type *completed-allowed-events*)))
    (error "Task ~A is completed. Reopen with task_reopen before making changes."
           task-id))
  (let ((missing (check-required-fields type data)))
    (when missing
      (error "emit-event: event type ~A missing/nil required field(s): ~{~A~^, ~}"
             type missing)))
  (crdt:vc-increment *session-vc* *session-id*)
  (let* ((ev (task:make-event
              :id (make-event-id)
              :timestamp (get-universal-time)
              :session *session-id*
              :clock *session-vc*
              :type type
              :data data))
         (path (task:task-events-path task-id)))
    (task:elog-append-event path ev)
    ;; Invalidate per-request elog cache for this task (we just appended an event)
    (when task:*elog-cache*
      (remhash (namestring path) task:*elog-cache*))
    ;; Invalidate per-task state cache (we just modified this task's events)
    (task:invalidate-task-state-cache task-id)
    ;; Invalidate graph/infos caches on edge-mutating events only.
    ;; task.create adds a node but no edges — the existing graph is still valid
    ;; (new task will be discovered on next full rebuild or TTL expiry).
    ;; task.update-status doesn't change topology either — skip it.
    ;; Only events that add/remove edges need immediate invalidation.
    (when (member type '(:task.fork :task.link :task.sever :task.reclassify))
      (task:clear-graph-cache)
      (task:clear-infos-cache))
    ;; Auto-index observations into *obs-graph* (best-effort)
    (when (eq type :observation)
      (ignore-errors
       (index-observation-event ev task-id)))
    ;; Session context (vector clock, event counter) is persisted by the
    ;; HTTP dispatch :around method on request exit.  No per-event save
    ;; needed — eliminates mid-mutation registry write risk.
    ev))

(defun current-task-state ()
  "Compute current task state from event log."
  (require-current-task)
  (let* ((path (task:task-events-path *current-task-id*))
         (log (task:elog-load path))
         (events (reverse (task:event-log-events log))))
    (task:compute-state events)))

(defun merge-vector-clocks-from-sessions (session-ids)
  "Merge vector clocks from SESSION-IDS into current session's VC.
   Enables happened-before queries across sessions that share observations.
   Returns the number of new VC entries merged."
  (let ((before-count 0)
        (after-count 0))
    (maphash (lambda (k v) (declare (ignore k v)) (incf before-count))
             (crdt:vc-entries *session-vc*))
    (with-timed-lock (*session-contexts-lock* :merge-vc-read)
      (dolist (sid session-ids)
        (let ((ctx (gethash sid *session-contexts*)))
          (when (and ctx (ctx-vector-clock ctx))
            (setf *session-vc* (crdt:vc-merge *session-vc* (ctx-vector-clock ctx)))))))
    (maphash (lambda (k v) (declare (ignore k v)) (incf after-count))
             (crdt:vc-entries *session-vc*))
    ;; Update context with merged VC.  Pointwise-max into the registry
    ;; so a concurrent request that already advanced any entry is not
    ;; rolled back by our overwrite.
    (with-timed-lock (*session-contexts-lock* :merge-vc-save)
      (let ((ctx (gethash *session-id* *session-contexts*)))
        (when ctx
          (setf (ctx-vector-clock ctx)
                (if (ctx-vector-clock ctx)
                    (crdt:vc-merge (ctx-vector-clock ctx) *session-vc*)
                    *session-vc*)))))
    (- after-count before-count)))

;;; Session file management for PostToolUse hooks
;;; Hooks receive session_id (Claude Code's UUID) which differs from task-mcp's
;;; internal session ID. We index session files by Claude Code's PID so hooks
;;; can look up their parent's session file.
;;;
;;; PID Namespace Solution:
;;; task-mcp's sandbox uses the `daemon` combinator which OMITS --unshare-pid,
;;; so the daemon shares the host PID namespace and can access /proc/<pid>/fd/.
;;; PARENT_PID is still passed through as a fallback for stdio mode.
;;; The sandbox launcher (program.nix) captures PARENT_PID=$PPID before
;;; entering bwrap and passes it through via --setenv. We read that env var here.

(defun get-parent-pid-from-env ()
  "Get parent PID from PARENT_PID environment variable.
   This is set by the sandbox launcher before entering bwrap's PID namespace.
   Returns integer PID or NIL if not available."
  (let ((pid-str (uiop:getenv "PARENT_PID")))
    (when (and pid-str (> (length pid-str) 0))
      (ignore-errors (parse-integer pid-str)))))

(defun get-claude-pid-for-session ()
  "Get Claude PID for current session from filesystem or session context.

   Uses .claude/active-pids/ as source of truth (written by SessionStart hook).
   In multi-session scenarios, the PostToolUse hook (session-task-write) writes
   the correct PID via kernel-guaranteed getppid(), so this is a fallback.

   Priority:
   1. If only one live PID in .claude/active-pids/, use it (common single-session case)
   2. If multiple live PIDs, prefer one in current session context
   3. If multiple live PIDs and none in context, use most recent (by file mtime)
   4. PARENT_PID environment variable (fallback for stdio mode)

   Returns integer PID or NIL."
  (or
   ;; HTTP mode: use filesystem as source of truth
   (when *http-mode*
     (let* ((coord-root (depot:coordination-root))
            (active-pids-dir (merge-pathnames ".claude/active-pids/"
                                               (uiop:ensure-directory-pathname coord-root)))
            (live-pid-files nil))
       ;; Collect all live PID files with their mtimes
       (when (probe-file active-pids-dir)
         (dolist (pid-file (ignore-errors (directory (merge-pathnames "*" active-pids-dir))))
           (let ((pid (ignore-errors (parse-integer (pathname-name pid-file)))))
             (when pid
               (push (cons pid (file-write-date pid-file)) live-pid-files)))))
       (cond
         ;; Single live PID: use it directly (common case)
         ((= (length live-pid-files) 1)
          (caar live-pid-files))
         ;; Multiple live PIDs: prefer one in current session context
         ((> (length live-pid-files) 1)
          (let* ((session-id (or *session-id* (current-http-session-id)))
                 (ctx (when session-id
                        (with-timed-lock (*session-contexts-lock* :get-claude-pid-ctx)
                          (gethash session-id *session-contexts*))))
                 (ctx-pid (when ctx (ctx-claude-pid ctx)))
                 (live-pids-set (make-hash-table)))
            ;; Build lookup set
            (dolist (pf live-pid-files)
              (setf (gethash (car pf) live-pids-set) t))
            (if (and ctx-pid (gethash ctx-pid live-pids-set))
                ;; Current context has a live PID, use it
                ctx-pid
                ;; Otherwise use most recent by mtime, preferring PIDs with session files
                (let* ((sorted (sort live-pid-files #'> :key #'cdr))
                       (sessions-dir (merge-pathnames ".claude/sessions/"
                                                       (uiop:ensure-directory-pathname
                                                        (depot:coordination-root)))))
                  (or (car (find-if (lambda (pf)
                                      (probe-file (merge-pathnames
                                                   (format nil "claude-~A.json" (car pf))
                                                   sessions-dir)))
                                    sorted))
                      (caar sorted))))))
         ;; No live PIDs found
         (t nil))))
   ;; Fallback: env var (stdio mode or no active-pids directory)
   (get-parent-pid-from-env)))

(defun sessions-directory ()
  "Return the .sessions directory path under coordination root.
   Uses depot:coordination-root (git root).
   Creates the directory if it doesn't exist."
  (let* ((coord-root (depot:coordination-root))
         (dir (merge-pathnames ".claude/sessions/"
                               (uiop:ensure-directory-pathname coord-root))))
    (ensure-directories-exist dir)
    dir))

(defun session-file-name ()
  "Return the session file name. Uses Claude PID if known, else internal session ID."
  (format nil "~A.json"
          (if *claude-pid*
              (format nil "claude-~A" *claude-pid*)
              *session-id*)))

(defun write-session-task-file ()
  "Write current session's task info to .sessions/{identifier}.json.
   Uses Claude Code's PID for filename so hooks can look up by their PPID.
   Called when task_set_current succeeds."
  (when (and *current-task-id* *session-id* task:*tasks-root*)
    ;; In HTTP mode, *claude-pid* is already loaded from session context by
    ;; ensure-session-context (called by define-session-tool macro).
    ;; The filesystem lookup via get-claude-pid-for-session is WRONG in HTTP mode:
    ;; it picks the most-recent-by-mtime PID which may belong to a different session.
    ;; In stdio mode, refresh from filesystem (single session, no collision possible).
    (unless *http-mode*
      (setf *claude-pid* (or (get-claude-pid-for-session) *claude-pid*)))
    (let* ((sessions-dir (sessions-directory))
           (session-file (merge-pathnames (session-file-name) sessions-dir))
           (events-path (task:task-events-path *current-task-id*))
           (ht (make-hash-table :test #'equal)))
      ;; Build JSON object
      (setf (gethash "task_id" ht) *current-task-id*)
      (setf (gethash "events_path" ht) (namestring events-path))
      (setf (gethash "session_id" ht) *session-id*)
      (setf (gethash "claude_pid" ht) *claude-pid*)
      (setf (gethash "timestamp" ht) (get-universal-time))
      ;; Team metadata from session context (set by P1 register-claude-pid)
      (let ((ctx (with-timed-lock (*session-contexts-lock* :team-lookup)
                   (gethash *session-id* *session-contexts*))))
        (when (and ctx (ctx-team-name ctx))
          (setf (gethash "team_name" ht) (ctx-team-name ctx))
          (setf (gethash "agent_name" ht) (ctx-agent-name ctx))
          (setf (gethash "agent_type" ht) (ctx-agent-type ctx))))
      ;; Write atomically via temp file
      (let ((temp-file (merge-pathnames
                        (format nil "~A.tmp" *session-id*)
                        sessions-dir)))
        (with-open-file (s temp-file
                           :direction :output
                           :if-exists :supersede)
          (yason:encode ht s))
        (rename-file temp-file session-file)))))

(defun clear-session-task-file ()
  "Clear task_id in session file when releasing task context.
   Session file remains (session is still alive) but task_id becomes empty.
   Called when task_release succeeds."
  (when (and *session-id* task:*tasks-root*)
    (let* ((sessions-dir (sessions-directory))
           (session-file (merge-pathnames (session-file-name) sessions-dir)))
      (when (probe-file session-file)
        (handler-case
            (let ((ht (with-open-file (s session-file)
                        (yason:parse s))))
              ;; Clear task_id but keep other fields
              (setf (gethash "task_id" ht) "")
              (setf (gethash "events_path" ht) "")
              (setf (gethash "timestamp" ht) (get-universal-time))
              ;; Write atomically via temp file
              (let ((temp-file (merge-pathnames
                                (format nil "~A.tmp" *session-id*)
                                sessions-dir)))
                (with-open-file (s temp-file
                                   :direction :output
                                   :if-exists :supersede)
                  (yason:encode ht s))
                (rename-file temp-file session-file)))
          (error () nil))))))

(defun cleanup-stale-sessions (&optional (max-age-hours 24))
  "Remove session files older than MAX-AGE-HOURS.
   Called opportunistically during task_set_current."
  (when task:*tasks-root*
    (let* ((sessions-dir (sessions-directory))
           (cutoff (- (get-universal-time) (* max-age-hours 60 60)))
           (pattern (merge-pathnames "*.json" sessions-dir)))
      (dolist (file (directory pattern))
        (handler-case
            (when (< (file-write-date file) cutoff)
              (delete-file file))
          (error () nil))))))

;;; ==========================================================================
;;; Forward declarations for cross-file references
;;; ==========================================================================
;;; session.lisp is compiled before graph.lisp (circular dep via macros.lisp).
;;; Declare forward references so SBCL doesn't emit undefined warnings.

(declaim (ftype (function () t) get-or-build-graph))

;;; ==========================================================================
;;; Session Behavioral Fingerprints (struct + defvar)
;;; ==========================================================================
;;; Defined here (before format-swarm-awareness) to avoid forward-reference
;;; warnings. The full fingerprint computation code follows later in this file.

(defstruct (session-fingerprint (:conc-name sfp-))
  "Per-session behavioral fingerprint for swarm coordination."
  (session-id "" :type string)
  (task-id "" :type string)
  (team-name nil :type (or null string))  ; team membership (nil for solo sessions)
  (tools nil :type list)           ; alist of (tool-name . count)
  (files nil :type list)           ; alist of (file-path . count)
  (obs-embedding nil)              ; mean observation vector (or nil)
  (archetype :unknown :type keyword)  ; :builder or :observer (from Markov classification)
  (timestamp 0 :type integer))     ; when fingerprint was computed

(defvar *session-fingerprints* (make-hash-table :test 'equal)
  "Cache of session fingerprints, keyed by session-id.")

(defvar *session-fingerprints-lock* (bt:make-lock "session-fingerprints")
  "Lock for *session-fingerprints* hash table.")

;;; ==========================================================================
;;; Swarm Awareness: Discover Related Sessions
;;; ==========================================================================
;;; Stigmergic coordination via session discovery. Sessions working on related
;;; tasks can discover each other without explicit communication - they find
;;; each other through the shared task graph (the "environment" in swarm terms).
;;;
;;; Mathematical basis: Bisimulation approximation via graph topology.
;;; Sessions on tasks with shared edges are implicitly collaborating.
;;;
;;; PID Liveness Solution:
;;; task-mcp runs inside bwrap with --unshare-pid, so it cannot verify process
;;; liveness via /proc. Solution: Cross-reference .claude/active-pids/ directory
;;; which contains files named by PID for each live Claude process.

(defun read-active-pids-from-directory ()
  "Read all live PIDs from .claude/active-pids/ directory.
   Returns hash-table of PID strings for O(1) lookup.
   This solves the PID namespace isolation problem where task-mcp cannot
   verify Claude process liveness via /proc.

   Uses coordination-root (world root if depot-of-depots, else depot root)
   so that all depots share a single active-pids directory."
  (let* ((coord-root (depot:coordination-root))
         (active-pids-dir (merge-pathnames ".claude/active-pids/"
                                            (uiop:ensure-directory-pathname coord-root)))
         (live-pids (make-hash-table :test 'equal)))
    (when (probe-file active-pids-dir)
      (dolist (pid-file (ignore-errors (directory (merge-pathnames "*" active-pids-dir))))
        (setf (gethash (pathname-name pid-file) live-pids) t)))
    live-pids))

(defun dedup-sessions (sessions)
  "Dedup session list by :claude-pid, keeping entry with highest :timestamp.
   A single Claude process may acquire multiple MCP session IDs (from parallel
   connections or reconnects). Quotienting by PID collapses them to one entry
   per physical process — the canonical agent identity."
  (let ((best (make-hash-table :test 'eql)))
    (dolist (sess sessions)
      (let* ((pid (getf sess :claude-pid))
             (ts (getf sess :timestamp))
             (existing (when pid (gethash pid best))))
        (when (and pid (or (null existing) (> ts (getf existing :timestamp))))
          (setf (gethash pid best) sess))))
    (let (result)
      (maphash (lambda (k v) (declare (ignore k)) (push v result)) best)
      (sort result #'> :key (lambda (s) (getf s :timestamp))))))

(defun read-active-sessions (&optional (max-age-hours 1))
  "Read all active session files (within MAX-AGE-HOURS) with live PIDs.
   Cross-references .claude/active-pids/ to filter out stale sessions from
   dead processes. Deduplicates by PID (keeps most recent per process).
   Per-request cached: subsequent calls with the same max-age-hours within
   one HTTP request return the cached result (called 5+ times per bootstrap).
   Returns list of plists with keys: :session-id :task-id :claude-pid :timestamp
   :age-minutes :team-name :agent-name :agent-type (last three nil for solo sessions)"
  ;; Per-request cache: avoid re-reading all session files within one request
  (when *active-sessions-cache*
    (multiple-value-bind (cached found) (gethash max-age-hours *active-sessions-cache*)
      (when found (return-from read-active-sessions cached))))
  (let ((result (read-active-sessions-uncached max-age-hours)))
    (when *active-sessions-cache*
      (setf (gethash max-age-hours *active-sessions-cache*) result))
    result))

(defun read-active-sessions-uncached (&optional (max-age-hours 1))
  "Implementation of read-active-sessions without caching."
  (when task:*tasks-root*
    (let* ((live-pids (read-active-pids-from-directory))
           (sessions-dir (sessions-directory))
           (now (get-universal-time))
           (cutoff (- now (* max-age-hours 60 60)))
           (results nil))
      (dolist (file (ignore-errors (directory (merge-pathnames "*.json" sessions-dir))))
        (handler-case
            (with-open-file (s file)
              (let* ((json (yason:parse s))
                     (ts (gethash "timestamp" json))
                     (pid (gethash "claude_pid" json))
                     (pid-str (when pid (format nil "~D" pid))))
                ;; Session must: be within time window AND have a PID AND PID is live
                (when (and ts (> ts cutoff)
                           pid-str
                           (gethash pid-str live-pids))
                  (push (list :session-id (gethash "session_id" json)
                              :task-id (gethash "task_id" json)
                              :claude-pid pid
                              :timestamp ts
                              :age-minutes (round (/ (- now ts) 60))
                              :team-name (gethash "team_name" json)
                              :agent-name (gethash "agent_name" json)
                              :agent-type (gethash "agent_type" json))
                        results))))
          (error () nil)))
      (dedup-sessions (nreverse results)))))

(defun task-id-bare (id)
  "Strip any legacy depot prefix from task ID.
   Kept as convenience wrapper; all call sites could use task:strip-depot-prefix directly."
  (task:strip-depot-prefix id))

(defun read-task-outgoing-edges (task-id)
  "Read outgoing edges from a task's events.jsonl.
   Returns list of (target-id . edge-type) pairs.
   Accounts for task.sever events that remove edges."
  (let* ((events-path (task:task-events-path task-id)))
    (when (probe-file events-path)
      (handler-case
          (let* ((log (task:elog-load events-path))
                 (events (reverse (task:event-log-events log)))
                 (adds nil)
                 (severs nil))
            (dolist (ev events)
              (let ((type (task:event-type ev))
                    (d (task:event-data ev)))
                (cond
                  ((member type '(:task.fork :task.link))
                   (push (cons (or (getf d :child-id)
                                   (getf d :target-id))
                               (getf d :edge-type))
                         adds))
                  ((eq type :task.sever)
                   (push (cons (getf d :target-id)
                               (getf d :edge-type))
                         severs)))))
            ;; Remove severed edges from adds
            (if severs
                (remove-if (lambda (edge)
                             (member edge severs :test #'equal))
                           (nreverse adds))
                (nreverse adds)))
        (error () nil)))))

(defun find-related-sessions (current-task-id &key (max-age-hours 1))
  "Find sessions working on tasks related to CURRENT-TASK-ID.
   Uses bidirectional edge traversal: tasks that point to us OR we point to.

   Returns list of plists:
     (:session-id :task-id :relation :direction :age-minutes
      :team-name :agent-name :agent-type)

   Relation types: :same-task, :phase-of, :related-to, :depends-on, etc.
   Direction: :incoming (they point to us) or :outgoing (we point to them)
   Team fields are nil for solo sessions."
  (let* ((current-bare (task-id-bare current-task-id))
         (our-edges (read-task-outgoing-edges current-task-id))
         (active-sessions (read-active-sessions max-age-hours))
         ;; Cache edges per task-id: many sessions may share a task
         (edges-cache (make-hash-table :test 'equal))
         (results nil))
    (dolist (sess active-sessions)
      (let* ((their-task (getf sess :task-id))
             (their-bare (task-id-bare their-task))
             (session-id (getf sess :session-id))
             (age (getf sess :age-minutes))
             (team-name (getf sess :team-name))
             (agent-name (getf sess :agent-name))
             (agent-type (getf sess :agent-type)))
        ;; Skip our own session (by PID equivalence class, not session-id;
        ;; a single process may have multiple MCP session IDs)
        (unless (or (and *claude-pid* (getf sess :claude-pid)
                         (eql *claude-pid* (getf sess :claude-pid)))
                    (string= session-id *session-id*))
          ;; Check: same task
          (when (or (string= their-task current-task-id)
                    (string= their-bare current-bare))
            (push (list :session-id session-id
                        :task-id their-task
                        :relation :same-task
                        :direction :same
                        :age-minutes age
                        :team-name team-name
                        :agent-name agent-name
                        :agent-type agent-type)
                  results))
          ;; Check: we point to their task (outgoing edge)
          (let ((edge (find their-bare our-edges
                            :key (lambda (e) (task-id-bare (car e)))
                            :test #'string=)))
            (when edge
              (push (list :session-id session-id
                          :task-id their-task
                          :relation (intern (string-upcase (cdr edge)) :keyword)
                          :direction :outgoing
                          :age-minutes age
                          :team-name team-name
                          :agent-name agent-name
                          :agent-type agent-type)
                    results)))
          ;; Check: they point to us (incoming edge)
          ;; Cache edge reads per task-id to avoid N file reads
          (let ((their-edges (or (gethash their-task edges-cache)
                                 (setf (gethash their-task edges-cache)
                                       (or (read-task-outgoing-edges their-task) :none)))))
            (unless (eq their-edges :none)
              (dolist (e their-edges)
                (when (or (string= (task-id-bare (car e)) current-bare)
                          (string= (car e) current-task-id))
                  (push (list :session-id session-id
                              :task-id their-task
                              :relation (intern (string-upcase (cdr e)) :keyword)
                              :direction :incoming
                              :age-minutes age
                              :team-name team-name
                              :agent-name agent-name
                              :agent-type agent-type)
                        results))))))))
    ;; Deduplicate by session-id (keep first occurrence)
    (remove-duplicates results :test #'equal :key (lambda (r) (getf r :session-id)))))

(defun format-related-sessions (related-sessions)
  "Format related sessions for display in bootstrap output."
  (if (null related-sessions)
      ""
      (with-output-to-string (s)
        (format s "~%## Swarm Awareness~%~%")
        (format s "Related sessions (~D active):~%" (length related-sessions))
        (dolist (sess related-sessions)
          (format s "  - ~A on ~A (~A, ~A, ~Dm ago)~%"
                  (getf sess :session-id)
                  (getf sess :task-id)
                  (getf sess :relation)
                  (getf sess :direction)
                  (getf sess :age-minutes))))))

(defun format-swarm-awareness (task-id related-sessions &key (departure-hours 24))
  "Format comprehensive swarm awareness for task_bootstrap output.
   Includes: related sessions + recent departures + orphaned phases.

   TASK-ID: Current task for departure/orphan queries
   RELATED-SESSIONS: Pre-computed related sessions from find-related-sessions
   DEPARTURE-HOURS: Time window for departure scanning (default 24h)"
  (let ((departures (handler-case
                        (find-recent-departures task-id :max-age-hours departure-hours)
                      (error () nil)))
        (orphans (handler-case
                     (check-orphaned-phases task-id)
                   (error () nil))))
    ;; Return (values formatted-string orphans) so bootstrap-task can pass
    ;; the orphans to format-orphan-warnings (avoiding duplicate computation).
    (values
     (if (and (null related-sessions) (null departures) (null orphans))
         ""
         (with-output-to-string (s)
           (format s "~%## Swarm Awareness~%")
           ;; Related sessions — grouped by team
           (when related-sessions
             (let ((teams (make-hash-table :test 'equal))  ; team-name -> list of sessions
                   (solo nil))                              ; sessions without team
               ;; Partition into teams vs solo
               (dolist (sess related-sessions)
                 (let ((team (getf sess :team-name)))
                   (if team
                       (push sess (gethash team teams))
                       (push sess solo))))
               (let ((team-count (hash-table-count teams)))
                 (format s "~%Related sessions (~D active~@[, ~D team~:P~]):~%"
                         (length related-sessions)
                         (when (plusp team-count) team-count))
                 ;; Display teams as grouped units
                 (maphash
                  (lambda (team-name members)
                    (let* ((members (nreverse members))
                           (tasks (remove-duplicates
                                   (mapcar (lambda (m) (getf m :task-id)) members)
                                   :test #'string=))
                           (agents (mapcar (lambda (m)
                                            (or (getf m :agent-name) "?"))
                                          members)))
                      (format s "  Team '~A': ~D member~:P (~{~A~^, ~}) on ~{~A~^, ~}~%"
                              team-name (length members) agents
                              (mapcar #'task-id-bare tasks))))
                  teams)
                 ;; Display solo sessions as before
                 (dolist (sess (nreverse solo))
                   (let* ((sid (getf sess :session-id))
                          (fp (bt:with-lock-held (*session-fingerprints-lock*)
                                (gethash sid *session-fingerprints*)))
                          (archetype (if fp
                                         (string-downcase (symbol-name (sfp-archetype fp)))
                                         "session")))
                     (format s "  - ~A ~A on ~A (~A, ~A, ~Dm ago)~%"
                             sid archetype
                             (getf sess :task-id)
                             (getf sess :relation)
                             (getf sess :direction)
                             (getf sess :age-minutes)))))))
           ;; Recent departures — annotate teammate departures
           (when departures
             (format s "~%Recent departures (~D):~%" (length departures))
             ;; Build session->team lookup from active session files and in-memory contexts
             (let ((session-teams (make-hash-table :test 'equal)))
               ;; From in-memory contexts (covers current process)
               (with-timed-lock (*session-contexts-lock* :swarm-teams)
                 (maphash (lambda (sid ctx)
                            (when (ctx-team-name ctx)
                              (setf (gethash sid session-teams)
                                    (ctx-team-name ctx))))
                          *session-contexts*))
               ;; From related-sessions (covers sessions discovered via files)
               (dolist (sess related-sessions)
                 (when (getf sess :team-name)
                   (setf (gethash (getf sess :session-id) session-teams)
                         (getf sess :team-name))))
               (dolist (dep departures)
                 (let* ((dep-sid (getf dep :session))
                        (team (gethash dep-sid session-teams)))
                   (if team
                       (format s "  - ~A left ~Dm ago (~A, teammate in '~A')~%"
                               dep-sid
                               (getf dep :age-minutes)
                               (getf dep :reason)
                               team)
                       (format s "  - ~A left ~Dm ago (~A)~%"
                               dep-sid
                               (getf dep :age-minutes)
                               (getf dep :reason))))))
             (format s "  Consider: Check if any work needs pickup~%"))
           ;; Orphaned phases
           (when orphans
             (format s "~%Orphaned phases (~D):~%" (length orphans))
             (dolist (orph orphans)
               (format s "  - ~A (claimed by departed ~A, status: ~A)~%"
                       (getf orph :phase-id)
                       (getf orph :claimed-by)
                       (getf orph :status)))
             (format s "  Consider: Claim with task_claim to continue work~%"))))
     orphans)))

;;; ==========================================================================
;;; Session Behavioral Fingerprint Computation
;;; ==========================================================================
;;; Coalgebraic behavioral traces captured as fingerprints for similarity
;;; computation. Each session's behavioral signature enables swarm coordination
;;; through behavioral similarity rather than explicit communication.
;;;
;;; Fingerprint components:
;;; - tools: G-Set of tool names used (from :tool.call events)
;;; - files: OR-Set of files touched (from :tool.call Edit/Write events)
;;; - obs-embedding: Mean vector of observation embeddings
;;;
;;; Mathematical basis: Bisimulation approximation via behavioral traces.
;;; Sessions with similar fingerprints are likely working on related problems.
;;;
;;; Note: session-fingerprint struct and *session-fingerprints* defvar are
;;; defined earlier in this file (before format-swarm-awareness).

(defun compute-mean-embedding (texts)
  "Compute mean embedding vector from list of texts.
   Uses batch API for cache misses to minimize HTTP round-trips.
   Returns nil if no embeddings could be computed."
  ;; Separate cached from uncached texts
  (let ((cached-pairs nil)    ; (text . embedding) for cache hits
        (miss-texts nil))     ; texts needing embedding
    (dolist (text texts)
      (let ((emb (bt:with-lock-held (*embedding-cache-lock*)
                   (gethash text *embedding-cache*))))
        (if emb
            (push (cons text emb) cached-pairs)
            (push text miss-texts))))
    ;; Batch-embed cache misses in one API call
    (setf miss-texts (nreverse miss-texts))
    (let ((miss-embeddings
            (when (and miss-texts (ollama-available-p))
              (let ((result (ollama-embed-batch miss-texts)))
                (if result
                    (progn (ollama-record-success) result)
                    (progn (ollama-record-failure) nil))))))
      ;; Store new embeddings in cache
      (when miss-embeddings
        (bt:with-lock-held (*embedding-cache-lock*)
          (loop for text in miss-texts
                for emb in miss-embeddings
                when emb
                do (evict-embedding-cache-if-full)
                   (setf (gethash text *embedding-cache*) emb)
                   (incf *embedding-cache-dirty*)))
        (maybe-save-embedding-cache))
      ;; Combine all embeddings
      (let ((all-embeddings
              (nconc (mapcar #'cdr (nreverse cached-pairs))
                     (when miss-embeddings
                       (remove nil miss-embeddings)))))
        (when all-embeddings
          (let* ((dim (length (first all-embeddings)))
                 (mean (make-array dim :element-type 'double-float
                                       :initial-element 0.0d0))
                 (n (length all-embeddings)))
            (dolist (emb all-embeddings)
              (dotimes (i dim)
                (incf (aref mean i) (aref emb i))))
            (dotimes (i dim)
              (setf (aref mean i) (/ (aref mean i) n)))
            mean))))))

(defvar *phase-embedding-cache* (make-hash-table :test 'equal)
  "Cache for phase embeddings, keyed by (task-id . obs-count).
   G-Set monotonicity guarantees obs-count only increases, so this key
   is a valid cache invalidation strategy.")

(defvar *phase-embedding-cache-lock* (bt:make-lock "phase-embedding-cache")
  "Lock for *phase-embedding-cache* hash table.")

(defun get-phase-embedding-cached (task-id)
  "Get embedding for a phase's observations, cached by obs count.
   Returns nil if phase has no observations."
  (let* ((elog (task:elog-load (task:task-events-path task-id)))
         (state (task:compute-state (task:event-log-events elog)))
         (obs (crdt:gs-members (task:task-state-observations state)))
         (obs-count (length obs))
         (cache-key (cons task-id obs-count)))
    (when (zerop obs-count)
      (return-from get-phase-embedding-cached nil))
    (or (bt:with-lock-held (*phase-embedding-cache-lock*)
          (gethash cache-key *phase-embedding-cache*))
        (let ((emb (handler-case
                    (get-embedding (format nil "~{~A~^ | ~}" obs))
                    (error () nil))))
          (when emb
            (bt:with-lock-held (*phase-embedding-cache-lock*)
              (setf (gethash cache-key *phase-embedding-cache*) emb)))
          emb))))

(defun count-to-alist (table)
  "Convert hash-table of (string . count) to sorted alist."
  (sort (loop for k being the hash-keys of table using (hash-value v)
              collect (cons k v))
        #'string< :key #'car))

(defun compute-session-fingerprint (task-id session-id)
  "Compute behavioral fingerprint for SESSION-ID working on TASK-ID.
   Extracts tool frequencies, file frequencies, and observation embeddings.
   Returns session-fingerprint struct."
  (let* ((events-path (task:task-events-path task-id))
         (log (task:elog-load events-path))
         (events (reverse (task:event-log-events log)))
         (session-events (remove-if-not
                          (lambda (e) (string= (task:event-session e) session-id))
                          events))
         (tool-counts (make-hash-table :test 'equal))
         (file-counts (make-hash-table :test 'equal))
         (obs-texts nil))
    ;; Single pass over session events to collect all dimensions
    (dolist (e session-events)
      (let ((type (task:event-type e))
            (data (task:event-data e)))
        (case type
          (:tool.call
           (let ((tool (getf data :tool)))
             (when tool (incf (gethash tool tool-counts 0)))
             ;; Extract file paths from Edit/Write/Read args (normalized)
             (when (member tool '("Edit" "Write" "Read") :test #'string=)
               (let ((path (normalize-file-path (getf (getf data :args) :file_path))))
                 (when path (incf (gethash path file-counts 0)))))))
          (:file.touch  ; legacy compat
           (let ((path (normalize-file-path (getf data :path))))
             (when path (incf (gethash path file-counts 0)))))
          (:observation
           (let ((text (getf data :text)))
             (when text (push text obs-texts)))))))
    (let* ((tools-alist (count-to-alist tool-counts))
           (total-events (length session-events))
           (tool-call-count (loop for (nil . cnt) in tools-alist sum cnt))
           (tool-ratio (if (plusp total-events)
                           (/ tool-call-count (float total-events))
                           0.0))
           (archetype (if (> tool-ratio 0.30) :builder :observer)))
      (make-session-fingerprint
       :session-id session-id
       :task-id task-id
       :tools tools-alist
       :files (count-to-alist file-counts)
       :obs-embedding (when obs-texts
                        (compute-mean-embedding (nreverse obs-texts)))
       :archetype archetype
       :timestamp (get-universal-time)))))

(defun get-session-fingerprint (session-id &optional task-id)
  "Get cached fingerprint or compute fresh if stale (>5min).
   TASK-ID required if not cached."
  (let ((cached (bt:with-lock-held (*session-fingerprints-lock*)
                  (gethash session-id *session-fingerprints*))))
    (if (and cached
             (< (- (get-universal-time) (sfp-timestamp cached)) 300))
        cached
        (when task-id
          (let ((fp (compute-session-fingerprint task-id session-id)))
            (bt:with-lock-held (*session-fingerprints-lock*)
              (setf (gethash session-id *session-fingerprints*) fp))
            fp)))))

(defun compute-active-fingerprints (&optional (max-age-hours 1))
  "Compute fingerprints for all active sessions.
   Per-request cached: called 2+ times per bootstrap (find-similar-sessions
   and format-swarm-frontier), each call re-reads sessions and replays event
   logs without this cache.
   Returns list of session-fingerprint structs."
  ;; Per-request cache: avoid recomputing fingerprints within one request
  (when *active-fingerprints-cache*
    (multiple-value-bind (cached found) (gethash max-age-hours *active-fingerprints-cache*)
      (when found (return-from compute-active-fingerprints cached))))
  (let ((result (compute-active-fingerprints-uncached max-age-hours)))
    (when *active-fingerprints-cache*
      (setf (gethash max-age-hours *active-fingerprints-cache*) result))
    result))

(defun compute-active-fingerprints-uncached (&optional (max-age-hours 1))
  "Implementation of compute-active-fingerprints without per-request caching."
  (let ((sessions (read-active-sessions max-age-hours))
        (results nil))
    (dolist (sess sessions)
      (let* ((session-id (getf sess :session-id))
             (task-id (getf sess :task-id))
             (fp (ignore-errors
                  (compute-session-fingerprint task-id session-id))))
        (when fp
          (setf (sfp-team-name fp) (getf sess :team-name))
          (bt:with-lock-held (*session-fingerprints-lock*)
            (setf (gethash session-id *session-fingerprints*) fp))
          (push fp results))))
    (nreverse results)))

(defun format-session-fingerprint (fp)
  "Format a session fingerprint for display."
  (format nil "~A (~A): tools={~{~A:~D~^, ~}} files=~D obs=~A"
          (sfp-session-id fp)
          (string-downcase (symbol-name (sfp-archetype fp)))
          (loop for (name . count) in (sfp-tools fp)
                collect name collect count)
          (length (sfp-files fp))
          (if (sfp-obs-embedding fp) "yes" "no")))

;;; ==========================================================================
;;; Session Similarity: Bisimulation-Inspired Metrics
;;; ==========================================================================
;;; Compute behavioral similarity between sessions using their fingerprints.
;;; Sessions with similar tools, files, and observations are likely working
;;; on related problems - enabling swarm coordination without explicit messaging.
;;;
;;; Mathematical basis: Bisimulation approximation via behavioral traces.
;;; Two sessions are "similar" if they exhibit similar observable behaviors.

(defun frequency-cosine-similarity (freq1 freq2)
  "Cosine similarity between two frequency alists.
   FREQ1, FREQ2 are alists of (key . count).
   Treats each alist as a sparse vector over the union of keys.
   Returns 0.0d0-1.0d0. Both empty = 0.0d0 (no signal)."
  (let ((all-keys (union (mapcar #'car freq1) (mapcar #'car freq2) :test #'equal))
        (dot 0.0d0) (mag1 0.0d0) (mag2 0.0d0))
    (dolist (key all-keys)
      (let ((v1 (or (cdr (assoc key freq1 :test #'equal)) 0))
            (v2 (or (cdr (assoc key freq2 :test #'equal)) 0)))
        (incf dot (* v1 v2))
        (incf mag1 (* v1 v1))
        (incf mag2 (* v2 v2))))
    (if (or (zerop mag1) (zerop mag2))
        0.0d0
        (/ dot (* (sqrt mag1) (sqrt mag2))))))

(defun structural-graph-distance (id-a id-b &optional (max-depth 5))
  "BFS graph distance using only structural edges (phase-of, depends-on, related-to).
   Returns NIL if no path within MAX-DEPTH.
   Filters out TOPIC and REFERENCES which create dense graphs where most pairs
   are reachable. With structural edges only, distance is meaningful."
  (when (string= id-a id-b) (return-from structural-graph-distance 0))
  (let* ((graph (get-or-build-graph))
         (fwd (task:task-graph-forward graph))
         (rev (task:task-graph-reverse graph))
         (structural-types '(:phase-of :depends-on :related-to))
         (visited (make-hash-table :test 'equal))
         (queue (list (cons id-a 0))))
    (setf (gethash id-a visited) t)
    (loop while queue
          for (current . depth) = (pop queue)
          when (string= current id-b) return depth
          when (< depth max-depth)
          do (flet ((process-edges (edges)
                      (dolist (e edges)
                        (when (member (second e) structural-types)
                          (let ((target (first e)))
                            (unless (gethash target visited)
                              (setf (gethash target visited) t)
                              (setq queue (nconc queue (list (cons target (1+ depth)))))))))))
               (process-edges (gethash current fwd))
               (process-edges (gethash current rev))))))

(defun graph-proximity-score (task-a task-b)
  "Compute graph proximity score [0,1] from structural distance.
   Distance 0 → 1.0, distance 1 → 0.5, distance 2 → 0.25, NIL → 0.0"
  (let ((dist (structural-graph-distance task-a task-b 4)))
    (if dist
        (/ 1.0d0 (1+ dist))
        0.0d0)))

(defun exponential-decay (hours-ago &key (half-life 4.0))
  "Exponential decay with configurable half-life (default 4 hours).
   Returns 1.0 for current, 0.5 at half-life, approaches 0 for old."
  (exp (* (- (/ (log 2.0d0) half-life)) hours-ago)))

(defun session-freshness (session-fingerprint)
  "Compute freshness of a session fingerprint [0,1].
   Based on time since fingerprint was computed."
  (let* ((age-secs (- (get-universal-time) (sfp-timestamp session-fingerprint)))
         (age-hours (/ age-secs 3600.0d0)))
    (exponential-decay (max 0.0d0 age-hours))))

(defun enhanced-fingerprint-similarity (fp1 fp2)
  "5-component similarity: tools + files + embeddings + graph proximity + freshness.
   Returns two values: score and degraded warning.

   Full weights: 0.2 tools + 0.2 files + 0.3 embeddings + 0.2 graph + 0.1 freshness.
   Degraded (no embeddings): 0.3 tools + 0.3 files + 0.25 graph + 0.15 freshness."
  (let* ((tool-sim (frequency-cosine-similarity (sfp-tools fp1) (sfp-tools fp2)))
         (file-sim (frequency-cosine-similarity (sfp-files fp1) (sfp-files fp2)))
         (emb1 (sfp-obs-embedding fp1))
         (emb2 (sfp-obs-embedding fp2))
         (obs-sim (when (and emb1 emb2) (max 0.0d0 (cosine-sim emb1 emb2))))
         (graph-prox (graph-proximity-score (sfp-task-id fp1) (sfp-task-id fp2)))
         (freshness (min (session-freshness fp1) (session-freshness fp2))))
    (if obs-sim
        ;; Full 5-component
        (values (+ (* 0.2d0 tool-sim) (* 0.2d0 file-sim) (* 0.3d0 obs-sim)
                   (* 0.2d0 graph-prox) (* 0.1d0 freshness))
                nil)
        ;; Degraded: redistribute obs weight to graph + freshness
        (values (+ (* 0.3d0 tool-sim) (* 0.3d0 file-sim)
                   (* 0.25d0 graph-prox) (* 0.15d0 freshness))
                "embeddings-unavailable"))))

(defun fingerprint-similarity (fp1 fp2 &key (tool-weight 0.3) (file-weight 0.3) (obs-weight 0.4))
  "Compute similarity between two session fingerprints.
   Returns two values:
   1. Weighted similarity score (0.0-1.0)
   2. Warning string if degraded (nil if full quality)

   Weights: tools (0.3), files (0.3), observation embeddings (0.4).
   Falls back to equal weights for tools/files if embeddings unavailable."
  (let* ((tool-sim (frequency-cosine-similarity (sfp-tools fp1) (sfp-tools fp2)))
         (file-sim (frequency-cosine-similarity (sfp-files fp1) (sfp-files fp2)))
         (emb1 (sfp-obs-embedding fp1))
         (emb2 (sfp-obs-embedding fp2))
         (obs-sim (when (and emb1 emb2)
                    (cosine-sim emb1 emb2))))
    (if obs-sim
        ;; Full weighted combination
        (values (+ (* tool-weight tool-sim)
                   (* file-weight file-sim)
                   (* obs-weight obs-sim))
                nil)
        ;; No embeddings: equal weight tools and files
        (values (/ (+ tool-sim file-sim) 2.0d0)
                "embeddings-unavailable"))))

(defun find-similar-sessions (session-id &key (threshold 0.3) (max-age-hours 1))
  "Find sessions with fingerprints similar to SESSION-ID.
   Excludes same-team sessions (teammates are shown in swarm awareness instead).
   Returns two values:
   1. List of plists (:session-id :task-id :similarity :team-name) sorted by similarity
   2. Warning string if similarity is degraded (e.g. embeddings unavailable)

   THRESHOLD guide (with default 30/30/40 tool/file/obs weights):
     0.8+  Strong match — sessions likely working on same sub-problem
     0.5+  Moderate match — overlapping tools/files, related work
     0.3+  Weak match — some shared tooling (default, catches broad overlap)
     <0.3  Noise — different behavioral patterns

   MAX-AGE-HOURS: Time window for active session scan (default 1h)."
  (let* ((all-fps (compute-active-fingerprints max-age-hours))
         (our-fp (find session-id all-fps
                       :key #'sfp-session-id
                       :test #'string=))
         (our-team (when our-fp (sfp-team-name our-fp)))
         (results nil)
         (degraded nil))
    (when our-fp
      (dolist (fp all-fps)
        (unless (or (string= (sfp-session-id fp) session-id)
                    ;; Skip same-team sessions — they appear in swarm awareness
                    (and our-team (sfp-team-name fp)
                         (string= our-team (sfp-team-name fp))))
          (multiple-value-bind (sim warning) (enhanced-fingerprint-similarity our-fp fp)
            (when warning (setf degraded warning))
            (when (>= sim threshold)
              (push (list :session-id (sfp-session-id fp)
                          :task-id (sfp-task-id fp)
                          :similarity sim
                          :team-name (sfp-team-name fp))
                    results)))))
      (values (sort results #'> :key (lambda (r) (getf r :similarity)))
              degraded))))

(defun similarity-guidance (score)
  "Convert similarity score to natural language guidance."
  (cond
    ((>= score 0.8) "doing very similar work — coordinate to avoid conflicts")
    ((>= score 0.5) "doing related work — check before editing shared files")
    (t "some overlap in tooling")))

(defun format-similar-sessions (similar-sessions &key degraded)
  "Format similar sessions for display in swarm awareness output.
   Similar sessions are those with matching behavioral fingerprints
   (shared tools, files, or observation embeddings) - different from
   related sessions which are found via task graph edges.
   Same-team sessions are excluded by find-similar-sessions; cross-team
   sessions are annotated with their team name.

   SIMILAR-SESSIONS: list of plists (:session-id :task-id :similarity :team-name)
   DEGRADED: warning string from find-similar-sessions (e.g. embeddings unavailable)"
  (if (null similar-sessions)
      ""
      (with-output-to-string (s)
        (format s "Similar sessions (~D found):~%" (length similar-sessions))
        (dolist (entry similar-sessions)
          (let* ((session-id (getf entry :session-id))
                 (task-id (getf entry :task-id))
                 (similarity (getf entry :similarity))
                 (team-name (getf entry :team-name))
                 (guidance (similarity-guidance similarity))
                 (team-label (if team-name
                                 (format nil " [team '~A']" team-name)
                                 "")))
            (if (and task-id (not (string= task-id "")))
                (format s "  - ~A~A on ~A — ~A~%" session-id team-label task-id guidance)
                (format s "  - ~A~A (no task) — ~A~%" session-id team-label guidance)))))))

;;; ==========================================================================
;;; File Path Normalization
;;; ==========================================================================
;;; Canonicalize file paths for consistent string= comparison. Resolves
;;; symlinks, relative paths, and double slashes. Duplicated from claude-hooks
;;; because task-mcp and claude-hooks share no common library.

(defun normalize-file-path (path)
  "Canonicalize a file path for consistent string= comparison.
   Resolves symlinks, relative paths, trailing slashes, and double slashes
   via CL:TRUENAME when the file exists. For non-existent files, normalizes
   syntactically via MERGE-PATHNAMES.
   Returns NIL for NIL or empty-string input."
  (when (and path (stringp path) (plusp (length path)))
    (handler-case
        (namestring (truename path))
      (error ()
        (let* ((absolute-p (char= (char path 0) #\/))
               (pn (if absolute-p
                       (pathname path)
                       (merge-pathnames path (uiop:getcwd)))))
          (namestring pn))))))

;;; ==========================================================================
;;; Emergent Coordination Signals: File Activity Queries
;;; ==========================================================================
;;; Events serve as implicit coordination signals. Sessions can query who
;;; recently touched a file to avoid conflicts. No explicit locking - just
;;; awareness that enables self-organization.
;;;
;;; Key insight from swarm research: Stigmergic coordination works through
;;; environment modification (pheromone trails). tool.call events for Edit/Write
;;; ARE the pheromone deposits. This function reads the trails.

(defun scan-file-activity (task-id &key (max-age-hours 1))
  "Scan a task's events for file modifications within MAX-AGE-HOURS.
   Scans tool.call events where tool is Edit or Write, extracting file_path
   from args. Also checks legacy file.touch events for backward compat.
   Returns list of plists: (:file :session :timestamp :age-minutes)"
  (let* ((events-path (task:task-events-path task-id))
         (now (get-universal-time))
         (cutoff (- now (* max-age-hours 60 60)))
         (results nil))
    (when (probe-file events-path)
      (handler-case
          (let* ((log (task:elog-load events-path))
                 (events (reverse (task:event-log-events log))))
            (dolist (ev events)
              (let* ((type (task:event-type ev))
                     (ts (task:event-timestamp ev))
                     (session (task:event-session ev))
                     (d (task:event-data ev)))
                (when (and ts (> ts cutoff))
                  (let ((path
                          (cond
                            ;; tool.call with Edit/Write → args.file_path
                            ((and (eq type :tool.call)
                                  d
                                  (member (getf d :tool)
                                          '("Edit" "Write") :test #'string=))
                             (let ((args (getf d :args)))
                               (when args (getf args :file_path))))
                            ;; Legacy file.touch → data.path
                            ((and (eq type :file.touch) d)
                             (getf d :path))
                            (t nil))))
                    (when path
                      (let ((npath (normalize-file-path path)))
                        (when npath
                          (push (list :file npath
                                      :session session
                                      :timestamp ts
                                      :age-minutes (round (/ (- now ts) 60)))
                                results)))))))))
        (error () nil)))
    (nreverse results)))

(defun task-neighborhood-ids ()
  "Return list of task IDs to scan for file activity: current task + graph neighbors.
   Falls back to active sessions' tasks if no current task is set."
  (if *current-task-id*
      (handler-case
          (let* ((graph (get-or-build-graph))
                 (fwd (gethash *current-task-id* (task:task-graph-forward graph)))
                 (rev (gethash *current-task-id* (task:task-graph-reverse graph)))
                 (ids (make-hash-table :test 'equal)))
            (setf (gethash *current-task-id* ids) t)
            (dolist (e fwd) (setf (gethash (first e) ids) t))
            (dolist (e rev) (setf (gethash (first e) ids) t))
            (let ((result nil))
              (maphash (lambda (k v) (declare (ignore v)) (push k result)) ids)
              result))
        (error ()
          ;; Fallback: just current task
          (list *current-task-id*)))
      ;; No task context — fall back to active sessions
      (let ((seen (make-hash-table :test 'equal))
            (result nil))
        (dolist (sess (read-active-sessions 1))
          (let ((tid (getf sess :task-id)))
            (when (and tid (not (gethash tid seen)))
              (setf (gethash tid seen) t)
              (push tid result))))
        result)))

(defun recent-activity-on-file (file-path &key task-id (max-age-hours 1))
  "Query who recently touched FILE-PATH for conflict avoidance.
   If TASK-ID is provided, only search that task. Otherwise, search current task
   + graph neighbors (bounded scan instead of all active sessions).

   Returns list of plists: (:session :task :timestamp :age-minutes)

   Usage for conflict avoidance:
     (when (recent-activity-on-file \"src/foo.lisp\")
       (warn \"Another session recently touched this file\"))"
  (let* ((file-path (or (normalize-file-path file-path) (return-from recent-activity-on-file nil)))
         (results nil)
         (task-ids (if task-id (list task-id) (task-neighborhood-ids))))
    (dolist (tid task-ids)
      (dolist (touch (scan-file-activity tid :max-age-hours max-age-hours))
        (when (string= (getf touch :file) file-path)
          (push (list :session (getf touch :session)
                      :task tid
                      :timestamp (getf touch :timestamp)
                      :age-minutes (getf touch :age-minutes))
                results))))
    ;; Sort by recency, exclude self
    (sort (remove-if (lambda (r) (string= (getf r :session) *session-id*))
                     results)
          #'< :key (lambda (r) (getf r :age-minutes)))))

(defun format-file-activity (activity)
  "Format file activity for display."
  (if (null activity)
      "No recent activity by other sessions."
      (with-output-to-string (s)
        (format s "Recent activity (~D sessions):~%" (length activity))
        (dolist (a activity)
          (format s "  - ~A on ~A (~Dm ago)~%"
                  (getf a :session)
                  (getf a :task)
                  (getf a :age-minutes))))))

(defun check-file-conflicts (file-paths &key task-id (max-age-hours 1))
  "Check multiple files for potential conflicts.
   Scans current task + graph neighbors (bounded) instead of all active sessions.
   Returns alist of (file . activity-list)."
  (let* ((norm-paths (remove nil (mapcar #'normalize-file-path file-paths)))
         (query-set (make-hash-table :test 'equal))
         (conflicts nil))
    ;; Build lookup set of normalized query paths
    (dolist (np norm-paths) (setf (gethash np query-set) t))
    (when (zerop (hash-table-count query-set))
      (return-from check-file-conflicts nil))
    ;; Bounded scan: specific task or neighborhood
    (let ((task-ids (if task-id (list task-id) (task-neighborhood-ids))))
      ;; Single pass per task: scan events, check against query set
      (dolist (tid task-ids)
        (dolist (touch (scan-file-activity tid :max-age-hours max-age-hours))
          (let ((file (getf touch :file)))
            (when (gethash file query-set)
              (unless (string= (getf touch :session) *session-id*)
                (push (list :file file
                            :session (getf touch :session)
                            :task tid
                            :timestamp (getf touch :timestamp)
                            :age-minutes (getf touch :age-minutes))
                      conflicts)))))))
    ;; Group by file
    (let ((grouped (make-hash-table :test 'equal)))
      (dolist (c conflicts)
        (push c (gethash (getf c :file) grouped)))
      (let ((result nil))
        (maphash (lambda (file touches)
                   (push (cons file (sort touches #'< :key (lambda (r) (getf r :age-minutes))))
                         result))
                 grouped)
        (nreverse result)))))

;;; ==========================================================================
;;; Hook File-Conflict Endpoint
;;; ==========================================================================
;;; Single source of truth for file-conflict detection, called by the
;;; PostToolUse:Edit hook via HTTP. Eliminates duplicated session logic
;;; in the hook by centralizing liveness checks, session-aware event scanning,
;;; and self-exclusion here.
;;;
;;; Session Attribution Invariant:
;;;   Session S is attributed as editing file F iff:
;;;     1. PID(S) ∈ active-pids/ (process is alive)       [read-active-sessions]
;;;     2. timestamp(S) > now - max_age (session is recent) [read-active-sessions]
;;;     3. ∃ event with session=S, file=F, recent           [scan-file-activity]
;;;     4. S ≠ caller                                       [caller-session-id param]

(defun scan-task-enrichment (task-id session-id &key (max-age-hours 1))
  "Scan a task's events for enrichment context, filtered by SESSION-ID.
   Returns plist: (:task-description :last-observation :edited-files)
   where :edited-files is an alist of (normalized-path . count)."
  (let* ((events-path (task:task-events-path task-id))
         (now (get-universal-time))
         (cutoff (- now (* max-age-hours 60 60)))
         (task-desc nil)
         (last-obs nil)
         (file-counts (make-hash-table :test #'equal)))
    (when (probe-file events-path)
      (handler-case
          (let* ((log (task:elog-load events-path))
                 (events (reverse (task:event-log-events log))))
            (dolist (ev events)
              (let* ((type (task:event-type ev))
                     (ts (task:event-timestamp ev))
                     (evt-session (task:event-session ev))
                     (d (task:event-data ev)))
                (when d
                  ;; Task description from birth certificate (any session)
                  (when (and (eq type :task.create) (null task-desc))
                    (setf task-desc (getf d :description)))
                  ;; Only count activity from the specific session
                  (when (and evt-session session-id
                             (string= evt-session session-id))
                    ;; Last observation
                    (when (and (eq type :observation) ts (> ts cutoff))
                      (let ((text (getf d :text)))
                        (when text (setf last-obs text))))
                    ;; File edit counts
                    (when (and (eq type :tool.call)
                               ts (> ts cutoff)
                               (member (getf d :tool)
                                       '("Edit" "Write") :test #'string=))
                      (let* ((args (getf d :args))
                             (fp (when args
                                   (normalize-file-path
                                    (getf args :file_path)))))
                        (when fp
                          (incf (gethash fp file-counts 0))))))))))
        (error () nil)))
    ;; Truncate long observations
    (when (and last-obs (> (length last-obs) 120))
      (setf last-obs (concatenate 'string (subseq last-obs 0 120) "...")))
    ;; Convert file-counts to sorted alist
    (let ((file-alist nil))
      (maphash (lambda (k v) (push (cons k v) file-alist)) file-counts)
      (setf file-alist (sort file-alist #'> :key #'cdr))
      (list :task-description task-desc
            :last-observation last-obs
            :edited-files file-alist))))

(defun file-conflict-for-caller (file-path caller-session-id
                                 &key (max-age-minutes 30))
  "Check for file conflicts from the perspective of CALLER-SESSION-ID.
   Uses read-active-sessions (PID-checked) and scan-file-activity (session-aware)
   to enforce the Session Attribution Invariant.

   Returns list of plists suitable for JSON serialization:
     ((:session S :task T :age-minutes N :edit-count N
       :task-description D :last-observation O :other-files ((path . count) ...))
      ...)"
  (let* ((norm-path (normalize-file-path file-path))
         (max-age-hours (/ max-age-minutes 60.0))
         ;; Invariant clauses 1+2: only alive sessions with recent timestamps
         (sessions (read-active-sessions (ceiling max-age-hours)))
         (conflicts nil))
    (unless norm-path (return-from file-conflict-for-caller nil))
    (let ((seen-tasks (make-hash-table :test 'equal)))
      (dolist (sess sessions)
        (let ((sid (getf sess :session-id))
              (tid (getf sess :task-id)))
          ;; Invariant clause 4: exclude caller
          (when (and sid tid
                     (not (string= sid caller-session-id))
                     ;; Only scan each task once
                     (not (gethash tid seen-tasks)))
            (setf (gethash tid seen-tasks) t)
            ;; Invariant clause 3: session-aware file activity scan
            (let ((touches (scan-file-activity tid :max-age-hours max-age-hours)))
              ;; Filter: events from THIS session that touch THIS file
              (let ((file-touches
                      (remove-if-not
                       (lambda (touch)
                         (and (string= (getf touch :file) norm-path)
                              (string= (getf touch :session) sid)))
                       touches)))
                (when file-touches
                  ;; Enrich with task context
                  (let* ((enrichment (scan-task-enrichment tid sid
                                      :max-age-hours max-age-hours))
                         (edited-files (getf enrichment :edited-files))
                         (edit-count (or (cdr (assoc norm-path edited-files
                                                     :test #'string=))
                                         (length file-touches)))
                         (other-files (remove norm-path edited-files
                                              :key #'car :test #'string=))
                         (most-recent-ts (getf (car (last file-touches)) :timestamp))
                         (age-minutes (when most-recent-ts
                                        (round (/ (- (get-universal-time)
                                                     most-recent-ts)
                                                  60)))))
                    (push (list :session sid
                                :task tid
                                :age-minutes age-minutes
                                :edit-count edit-count
                                :task-description (getf enrichment :task-description)
                                :last-observation (getf enrichment :last-observation)
                                :other-files other-files)
                          conflicts)))))))))
    (nreverse conflicts)))

;;; ==========================================================================
;;; Session Departure Detection
;;; ==========================================================================
;;; Detect when sessions have left tasks via :session.leave events.
;;; Enables swarm coordination: detect departed collaborators and orphaned work.
;;;
;;; session.leave events are emitted by the SessionEnd hook when Claude Code
;;; sessions terminate (graceful exit, timeout, or crash).

(defun find-recent-departures (task-id &key (max-age-hours 1))
  "Find sessions that recently departed from TASK-ID.
   Scans events.jsonl for :session.leave events within time window.

   Returns list of plists: (:session :timestamp :age-minutes :reason)

   Usage for swarm coordination:
     (find-recent-departures *current-task-id*)
     => ((:session \"s-123\" :timestamp 4028456789 :age-minutes 15 :reason \"graceful\"))"
  (let* ((events-path (task:task-events-path task-id))
         (now (get-universal-time))
         (cutoff (- now (* max-age-hours 60 60)))
         (results nil))
    (when (probe-file events-path)
      (handler-case
          (let* ((log (task:elog-load events-path))
                 (events (reverse (task:event-log-events log))))
            (dolist (ev events)
              (when (eq (task:event-type ev) :session.leave)
                (let* ((ts (task:event-timestamp ev))
                       (session (task:event-session ev))
                       (reason (getf (task:event-data ev) :reason)))
                  (when (and ts (> ts cutoff))
                    (push (list :session session
                                :timestamp ts
                                :age-minutes (round (/ (- now ts) 60))
                                :reason (or reason "unknown"))
                          results))))))
        (error () nil)))
    (nreverse results)))

(defun session-has-left-p (task-id session-id)
  "Check if SESSION-ID has emitted a session.leave event for TASK-ID.
   Returns the departure plist if found, NIL otherwise.

   Note: This scans the full event log (not time-bounded) to catch any
   historical departure. Use find-recent-departures for time-bounded queries."
  (let ((events-path (task:task-events-path task-id)))
    (when (probe-file events-path)
      (handler-case
          (let* ((log (task:elog-load events-path))
                 (events (task:event-log-events log)))
            (dolist (ev events)
              (when (and (eq (task:event-type ev) :session.leave)
                         (string= (task:event-session ev) session-id))
                (return-from session-has-left-p
                  (list :session session-id
                        :timestamp (task:event-timestamp ev)
                        :reason (or (getf (task:event-data ev) :reason)
                                    "unknown"))))))
        (error () nil)))))

(defun check-orphaned-phases (parent-task-id)
  "Find phases of PARENT-TASK-ID that are orphaned (claimed by departed sessions).
   A phase is orphaned when:
   1. It has a claim (session claimed ownership)
   2. The claiming session has emitted session.leave
   3. The phase is not completed

   Returns list of plists: (:phase-id :claimed-by :status :departed-at)

   Usage for swarm coordination:
     (check-orphaned-phases *current-task-id*)
     => ((:phase-id \"phase-1\" :claimed-by \"s-123\" :status \"active\" :departed-at 3979194161))"
  (let* ((parent-events-path (task:task-events-path parent-task-id))
         (results nil))
    (when (probe-file parent-events-path)
      ;; Get parent's state to find phase children
      (handler-case
          (let* ((log (task:elog-load parent-events-path))
                 (events (reverse (task:event-log-events log)))
                 (parent-state (task:compute-state events))
                 ;; Get phase children (tasks that have :phase-of edge to parent)
                 (phase-ids (task:edge-targets (task:task-state-edges parent-state)
                                               :phase-of)))
            (dolist (phase-id phase-ids)
              ;; Get each phase's state
              (let ((phase-path (task:task-events-path phase-id)))
                (when (probe-file phase-path)
                  (handler-case
                      (let* ((phase-log (task:elog-load phase-path))
                             (phase-events (task:event-log-events phase-log))
                             (phase-state (task:compute-state (reverse phase-events)))
                             (status (crdt:lww-value (task:task-state-status phase-state)))
                             (claim (crdt:lww-value (task:task-state-claim phase-state))))
                        ;; Check if phase is claimed, not completed, and claimer departed
                        (when (and claim
                                   (> (length claim) 0)
                                   (not (string= status "completed")))
                          ;; Inline session-has-left-p: search already-loaded events
                          ;; for session.leave from the claimer (avoids re-loading elog)
                          (let ((departure
                                  (dolist (ev phase-events)
                                    (when (and (eq (task:event-type ev) :session.leave)
                                               (string= (task:event-session ev) claim))
                                      (return
                                        (list :timestamp (task:event-timestamp ev)))))))
                            (when departure
                              (push (list :phase-id phase-id
                                          :claimed-by claim
                                          :status (or status "active")
                                          :departed-at (getf departure :timestamp))
                                    results)))))
                    (error () nil))))))
        (error () nil)))
    (nreverse results)))

;;; ==========================================================================
;;; Swarm-Aware Frontier
;;; ==========================================================================
;;; Enriches the plan frontier (antichain of ready phases) with session
;;; affinity scores, claim state, and active session data. Enables swarm
;;; coordination by showing which sessions are best suited for which phases.
;;;
;;; Mathematical basis (CALM theorem):
;;;   F = antichain of ready phases (monotonic, no coordination needed)
;;;   score: F × S → [0,1] (read-only, monotonic)
;;;   Only claim mutation needs LWW synchronization

(defun find-parent-task (task-id)
  "Find the parent task ID from the creation event."
  (let* ((elog (task:elog-load (task:task-events-path task-id)))
         (events (task:event-log-events elog)))
    (dolist (ev events)
      (when (eq (task:event-type ev) :task.create)
        (let ((data (task:event-data ev)))
          (return (getf data :parent)))))))

(defun session-phase-affinity-v2 (session-fingerprint phase-info phase-embedding)
  "Compute affinity score [0,1] between session and frontier phase.
   Uses file overlap (Jaccard), observation embedding similarity (cosine),
   and claim/activity bonuses.

   affinity(s, p) = 0.3 · J(files(s), files(p))
                  + 0.45 · cos(emb(s), emb(p))
                  + bonus(active) + bonus(unclaimed)"
  (let* ((session-files (mapcar #'car (sfp-files session-fingerprint)))
         (phase-files (getf phase-info :files-touched))
         (file-sim
          (if (and session-files phase-files)
              (let ((inter (length (intersection session-files phase-files
                                                 :test #'string=)))
                    (uni (length (union session-files phase-files
                                       :test #'string=))))
                (if (zerop uni) 0.0d0 (/ (float inter 1.0d0) uni)))
              0.0d0))
         (emb-sim
          (if (and (sfp-obs-embedding session-fingerprint) phase-embedding)
              (max 0.0d0
                   (cosine-sim (sfp-obs-embedding session-fingerprint)
                               phase-embedding))
              0.0d0))
         (active-bonus
          (if (plusp (getf phase-info :active-sessions-on-phase)) 0.2d0 0.0d0))
         (unclaimed-bonus
          (if (null (getf phase-info :claim)) 0.05d0 0.0d0)))
    (+ (* 0.3d0 file-sim) (* 0.45d0 emb-sim) active-bonus unclaimed-bonus)))

(defun swarm-aware-frontier (parent-task-id)
  "Compute frontier annotated with session interest and claim state.
   Returns list of plists with :id :status :total-sessions :claim :obs-count
   :active-sessions-on-phase :files-touched."
  (let* ((frontier (task:plan-frontier parent-task-id))
         (active-sessions (read-active-sessions))
         (active-sids (mapcar (lambda (s) (getf s :session-id)) active-sessions)))
    (mapcar
     (lambda (tid)
       (let* ((elog (task:elog-load (task:task-events-path tid)))
              (state (task:compute-state (task:event-log-events elog)))
              (phase-sessions (crdt:gs-members (task:task-state-sessions state)))
              (active-on-phase
               (intersection active-sids phase-sessions :test #'string=)))
         (list :id tid
               :status (crdt:lww-value (task:task-state-status state))
               :total-sessions (length phase-sessions)
               :claim (crdt:lww-value (task:task-state-claim state))
               :obs-count (crdt:gs-count (task:task-state-observations state))
               :active-sessions-on-phase (length active-on-phase)
               :files-touched (crdt:ors-members (task:task-state-files-touched state)))))
     frontier)))

(defun swarm-frontier-v2 (parent-task-id)
  "Compute frontier annotated with embedding-based session affinity.
   Returns phases sorted by max affinity, each with :session-affinities
   :max-affinity and :best-session."
  (let* ((frontier-phases (swarm-aware-frontier parent-task-id))
         (fingerprints (compute-active-fingerprints))
         (phase-embeddings
          (mapcar (lambda (phase)
                    (get-phase-embedding-cached (getf phase :id)))
                  frontier-phases)))
    (let ((annotated
           (mapcar
            (lambda (phase phase-emb)
              (let ((affinities
                     (mapcar
                      (lambda (fp)
                        (list :session (subseq (sfp-session-id fp) 0 8)
                              :task (sfp-task-id fp)
                              :affinity (float
                                         (session-phase-affinity-v2
                                          fp phase phase-emb))))
                      fingerprints)))
                (append phase
                        (list :session-affinities affinities
                              :max-affinity
                              (if affinities
                                  (reduce #'max (mapcar (lambda (a) (getf a :affinity))
                                                        affinities))
                                  0.0)
                              :best-session
                              (when affinities
                                (getf (first (sort (copy-list affinities) #'>
                                                   :key (lambda (a) (getf a :affinity))))
                                      :session))))))
            frontier-phases phase-embeddings)))
      (sort annotated #'> :key (lambda (p) (getf p :max-affinity))))))

(defun format-swarm-frontier (parent-task-id &optional calling-session-id)
  "Format swarm-aware frontier for bootstrap output.
   CALLING-SESSION-ID identifies 'your' session for personalized affinity.
   Returns empty string if no frontier."
  (handler-case
   (let ((frontier (task:plan-frontier parent-task-id)))
     (if (null frontier)
         ""
         (let* ((fingerprints (compute-active-fingerprints))
                (our-sid (or calling-session-id *session-id*))
                (our-fp (find our-sid fingerprints
                              :key #'sfp-session-id :test #'string=))
                (phase-data nil))
           (dolist (tid frontier)
             (let* ((elog (task:elog-load (task:task-events-path tid)))
                    (state (task:compute-state (task:event-log-events elog)))
                    (claim (crdt:lww-value (task:task-state-claim state)))
                    (obs-count (crdt:gs-count (task:task-state-observations state)))
                    (sessions-count (crdt:gs-count (task:task-state-sessions state)))
                    (emb (get-phase-embedding-cached tid)))
               (push (list :id tid :claim claim :obs-count obs-count
                           :sessions-count sessions-count :embedding emb)
                     phase-data)))
           (setf phase-data (nreverse phase-data))
           (with-output-to-string (s)
             (format s "~%## Plan Frontier (~D ready phases)~%~%"
                     (length frontier))
             (dolist (pd phase-data)
               (format s "- ~A  [~D obs, ~D sessions~A]~%"
                       (task-id-bare (getf pd :id))
                       (getf pd :obs-count) (getf pd :sessions-count)
                       (if (getf pd :claim)
                           (format nil ", claimed: ~A" (getf pd :claim))
                           "")))
             (when our-fp
               (let ((scored nil))
                 (dolist (pd phase-data)
                   (let* ((phase-info
                           (list :claim (getf pd :claim)
                                 :active-sessions-on-phase 0
                                 :files-touched nil))
                          (score (session-phase-affinity-v2
                                  our-fp phase-info (getf pd :embedding))))
                     (push (cons (getf pd :id) score) scored)))
                 (setf scored (sort scored #'> :key #'cdr))
                 (when scored
                   (format s "~%Your session affinity:~%")
                   (dolist (pair scored)
                     (format s "  ~A: ~,0F%~%"
                             (task-id-bare (car pair))
                             (* 100 (cdr pair)))))))
             ;; Other sessions' affinities: skip if too many sessions × phases
             ;; to avoid O(S×P) computation in the swarm deadline window
             (let ((others (remove our-sid fingerprints
                                   :key #'sfp-session-id :test #'string=)))
               (when (and others (<= (* (length others) (length phase-data)) 100))
                 (format s "~%Other active sessions:~%")
                 (dolist (fp others)
                   (let* ((best-phase nil) (best-score 0.0))
                     (dolist (pd phase-data)
                       (let* ((phase-info
                               (list :claim (getf pd :claim)
                                     :active-sessions-on-phase 0
                                     :files-touched nil))
                              (score (session-phase-affinity-v2
                                      fp phase-info (getf pd :embedding))))
                         (when (> score best-score)
                           (setf best-score score
                                 best-phase (getf pd :id)))))
                     (format s "  ~A (~A): best->~A (~,0F%)~%"
                             (subseq (sfp-session-id fp) 0 8)
                             (task-id-bare (sfp-task-id fp))
                             (if best-phase (task-id-bare best-phase) "?")
                             (* 100 best-score))))))))))
   (error (e)
     (format nil "~%[Swarm frontier unavailable: ~A]~%" e))))

(defun detect-orphaned-claims (parent-task-id)
  "Find frontier phases with claims by departed sessions."
  (let* ((frontier (task:plan-frontier parent-task-id))
         (active-sessions (read-active-sessions))
         (active-sids (mapcar (lambda (s) (getf s :session-id)) active-sessions)))
    (loop for tid in frontier
          for elog = (task:elog-load (task:task-events-path tid))
          for state = (task:compute-state (task:event-log-events elog))
          for claim = (crdt:lww-value (task:task-state-claim state))
          when (and claim (not (member claim active-sids :test #'string=)))
          collect (list :phase (task-id-bare tid)
                        :claimed-by claim
                        :status "orphaned — claimer departed"))))

(defun format-orphan-warnings (task-id &key pre-computed-orphans)
  "Format orphan detection results for bootstrap output.
   Combines detect-orphaned-claims (frontier phases) and check-orphaned-phases
   (all phases). Returns empty string if no orphans found.
   PRE-COMPUTED-ORPHANS: if provided, reuses the check-orphaned-phases result
   from format-swarm-awareness instead of calling it a second time."
  (handler-case
      (let* ((frontier-orphans (detect-orphaned-claims task-id))
             (all-orphans (or pre-computed-orphans
                              (check-orphaned-phases task-id)))
             ;; Merge: frontier-orphans have :phase/:claimed-by/:status
             ;; all-orphans have :phase-id/:claimed-by/:status/:departed-at
             ;; Deduplicate by phase ID
             (seen (make-hash-table :test 'equal))
             (merged nil))
        (dolist (o frontier-orphans)
          (let ((phase (getf o :phase)))
            (unless (gethash phase seen)
              (setf (gethash phase seen) t)
              (push o merged))))
        (dolist (o all-orphans)
          (let ((phase (task-id-bare (getf o :phase-id))))
            (unless (gethash phase seen)
              (setf (gethash phase seen) t)
              (push (list :phase phase
                          :claimed-by (getf o :claimed-by)
                          :status (format nil "~A — departed" (getf o :status)))
                    merged))))
        (if merged
            (with-output-to-string (s)
              (format s "~%## Orphaned Phases (~D found)~%~%" (length merged))
              (format s "These phases were claimed by sessions that have since departed:~%")
              (dolist (o (nreverse merged))
                (format s "  - ~A: claimed by ~A (~A)~%"
                        (getf o :phase)
                        (getf o :claimed-by)
                        (getf o :status)))
              (format s "~%Consider: Use task_claim to take over orphaned work.~%"))
            ""))
    (error () "")))

(defun estimate-info-flows (task-id)
  "Estimate information flows between sessions working on TASK-ID.
   A flow A->B exists when session B joined after session A wrote observations,
   meaning B's bootstrap likely surfaced A's observations.
   Returns list of (:writer writer-sid :reader reader-sid :obs-count N)."
  (handler-case
      (let* ((path (task:task-events-path task-id))
             (log (task:elog-load path))
             (events (task:event-log-events log))
             (obs-events (remove-if-not
                          (lambda (ev) (eq :observation (task:event-type ev)))
                          events))
             (join-events (remove-if-not
                           (lambda (ev) (member (task:event-type ev)
                                                '(:session.join :session.team-join)))
                           events))
             (flows nil))
        (dolist (join join-events)
          (let ((reader (task:event-session join))
                (join-time (task:event-timestamp join))
                (writers (make-hash-table :test 'equal)))
            (dolist (obs obs-events)
              (when (and (< (task:event-timestamp obs) join-time)
                         (not (string= (task:event-session obs) reader)))
                (incf (gethash (task:event-session obs) writers 0))))
            (maphash (lambda (writer count)
                       (push (list :writer writer :reader reader :obs-count count)
                             flows))
                     writers)))
        ;; Deduplicate: keep max obs-count per writer->reader pair
        (let ((deduped (make-hash-table :test 'equal)))
          (dolist (f flows)
            (let* ((key (format nil "~A->~A" (getf f :writer) (getf f :reader)))
                   (existing (gethash key deduped)))
              (when (or (null existing)
                        (> (getf f :obs-count) (getf existing :obs-count)))
                (setf (gethash key deduped) f))))
          (let ((result nil))
            (maphash (lambda (k v) (declare (ignore k)) (push v result)) deduped)
            result)))
    (error () nil)))

(defun format-info-flow-summary (task-id)
  "Format info flow summary for task_get output.
   Shows how many sessions influenced each other via observations."
  (handler-case
      (let ((flows (estimate-info-flows task-id)))
        (if (null flows)
            ""
            (let* ((writers (remove-duplicates
                             (mapcar (lambda (f) (getf f :writer)) flows)
                             :test #'string=))
                   (readers (remove-duplicates
                             (mapcar (lambda (f) (getf f :reader)) flows)
                             :test #'string=)))
              (with-output-to-string (s)
                (format s "~%Info flows: ~D flows (~D writers -> ~D readers)~%"
                        (length flows) (length writers) (length readers))))))
    (error () "")))

(defun completion-frontier-update (task-id)
  "After completing a phase, compute newly available phases in parent plan."
  (let ((parent-id (find-parent-task task-id)))
    (when parent-id
      (let ((frontier (task:plan-frontier parent-id)))
        (when frontier
          (format nil "Newly available phases in ~A:~%~{  - ~A~%~}"
                  (task-id-bare parent-id)
                  (mapcar #'task-id-bare frontier)))))))
