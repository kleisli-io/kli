(in-package #:task-mcp)

(defvar *log-verbose* nil
  "When true, emit informational log messages to *error-output*.
   Set from KLI_LOG environment variable at startup.")

;;; Build-id (runtime lookup to avoid compile-time dep on kli package)

(defun get-build-id ()
  "Get build-id from KLI package if loaded, else return unknown."
  (let ((pkg (find-package "KLI")))
    (if pkg
        (let ((sym (find-symbol "ENSURE-BUILD-ID" pkg)))
          (if (and sym (fboundp sym))
              (funcall sym)
              "unknown"))
        "unknown")))

;;; Transport selection

(defun get-http-port ()
  "Get HTTP port from TASK_MCP_PORT env var, default 8090."
  (let ((port-str (uiop:getenv "TASK_MCP_PORT")))
    (if (and port-str (> (length port-str) 0))
        (parse-integer port-str :junk-allowed t)
        8090)))

(defun select-transport ()
  "Select transport based on TASK_MCP_TRANSPORT env var.
   Values: 'stdio' for STDIO transport, anything else for http (default).
   Also sets *http-mode* for session context management."
  (let ((transport-type (uiop:getenv "TASK_MCP_TRANSPORT")))
    (if (and transport-type (string-equal transport-type "stdio"))
        (progn
          (setf *http-mode* nil)
          (when *log-verbose*
            (format *error-output* "  Transport: stdio~%")
            (format *error-output* "  Mode: single-session~%"))
          (mcp-framework:make-stdio-transport))
        (let ((port (get-http-port)))
          (setf *http-mode* t)
          (when *log-verbose*
            (format *error-output* "  Transport: HTTP on port ~D~%" port)
            (format *error-output* "  Mode: multi-session (contexts isolated per Mcp-Session-Id)~%"))
          ;; Register non-MCP HTTP endpoints for hook integration
          (register-http-endpoints)
          (mcp-http:make-http-transport :port port :host "127.0.0.1")))))

;;; Per-request hash table pool
;;;
;;; Avoids allocating 3 fresh hash tables per HTTP request. Tables are
;;; cleared with clrhash and returned to the pool after each request.
;;; Lock-free via sb-ext:atomic-push / atomic-pop.

(defvar *hash-table-pool* nil
  "Lock-free pool of (elog-ht sessions-ht fingerprints-ht) triples.
   Each entry is a list of 3 pre-allocated hash tables.")

(defun acquire-request-tables ()
  "Pop a triple from the pool, or allocate fresh tables."
  (or (sb-ext:atomic-pop *hash-table-pool*)
      (list (make-hash-table :test 'equal)    ; elog cache
            (make-hash-table :test 'eql)      ; active sessions
            (make-hash-table :test 'eql))))   ; active fingerprints

(defun release-request-tables (tables)
  "Clear and return tables to the pool."
  (dolist (ht tables)
    (clrhash ht))
  (sb-ext:atomic-push tables *hash-table-pool*))

;;; Thread-safe session isolation for HTTP mode
;;;
;;; Hunchentoot processes requests in a thread pool. Session state globals
;;; (*current-task-id*, *session-id*, etc.) are shared across all threads.
;;; This :around method creates per-thread dynamic bindings so concurrent
;;; requests from different sessions don't clobber each other's context.
;;;
;;; Also binds playbook session globals since the task daemon serves both
;;; task and playbook tools (unified kli binary, single MCP server).

(defmethod hunchentoot:acceptor-dispatch-request :around
    ((acceptor mcp-http::mcp-acceptor) request)
  "Wrap each HTTP request with thread-local session state bindings
   and a wall-clock deadline.  The deadline protects the handler
   thread from pathological blocking paths: on timeout we unwind
   cleanly through the unwind-protect and return HTTP 503 rather
   than leaking the thread and its TCP socket."
  (let* ((tables (acquire-request-tables))
         (script-name (ignore-errors (hunchentoot:script-name request)))
         ;; Per-request caches from pool (prevents redundant disk reads/computation)
         (task:*elog-cache* (first tables))
         (*active-sessions-cache* (second tables))
         (*active-fingerprints-cache* (third tables))
         ;; Task session isolation
         (*current-task-id* *current-task-id*)
         (*session-id* *session-id*)
         (*session-vc* *session-vc*)
         (*event-counter* *event-counter*)
         (*claude-pid* *claude-pid*)
         (*http-mode* *http-mode*)
         ;; Mutation isolation (ensure fresh nil per request)
         (*in-mutation* nil)
         ;; Playbook session isolation (unified daemon serves both)
         (playbook::*claimed-session* playbook::*claimed-session*)
         (playbook::*claimed-session-cwd* playbook::*claimed-session-cwd*)
         (playbook::*http-mode* *http-mode*))
    ;; Snapshot vars are bound at the outer scope so the unwind-protect
    ;; cleanup can read them, but populated inside the handler-case after
    ;; ENSURE-SESSION-CONTEXT has loaded the registry's task-id /
    ;; event-counter.  If ENSURE-SESSION-CONTEXT signals UNKNOWN-SESSION
    ;; the snapshots stay NIL/0 and MAYBE-SAVE-SESSION-CONTEXT skips
    ;; (gated by *SESSION-ID*, which load-session-context never set).
    (let ((snapshot-task-id nil)
          (snapshot-event-counter 0))
      (unwind-protect
           (handler-case
               (progn
                 ;; ENSURE-SESSION-CONTEXT is now inside the handler-case
                 ;; so UNKNOWN-SESSION (signalled when the inbound sid
                 ;; has no context in *SESSION-CONTEXTS*) translates to
                 ;; HTTP 404 + code=session-not-found instead of escaping
                 ;; to hunchentoot's default 500 handler.
                 (ensure-session-context)
                 (playbook::ensure-playbook-session-context)
                 (setf snapshot-task-id *current-task-id*
                       snapshot-event-counter *event-counter*)
                 (with-request-deadline (:method (hunchentoot:request-method*)
                                         :path script-name
                                         :session-id *session-id*)
                   (call-next-method)))
             (unknown-session (c)
               ;; Mcp-Session-Id header named a sid not in *SESSION-CONTEXTS*.
               ;; The framework's session-store has no TTL but task-mcp's
               ;; cleanup-inactive-sessions evicts after 4h, so the two
               ;; stores can diverge.  Returning 404 with a typed code
               ;; lets the CLI drop its persisted sid and re-initialize
               ;; rather than silently resurrecting a virgin context.
               (setf (hunchentoot:return-code*) 404)
               (setf (hunchentoot:content-type*) "application/json")
               (format nil "{\"error\": \"session not found\", ~
                            \"code\": \"session-not-found\", ~
                            \"session_id\": ~S}"
                       (or (unknown-session-session-id c) "")))
             (request-deadline-exceeded (c)
               (format *error-output*
                       "~&;; WARNING: ~A~%" c)
               (setf (hunchentoot:return-code*) 503)
               (setf (hunchentoot:content-type*) "application/json")
               (format nil "{\"error\": \"request deadline exceeded\", ~
                            \"path\": ~S, \"deadline_seconds\": ~D}"
                       (or script-name "")
                       (or (request-deadline-deadline-seconds c)
                           *request-deadline-seconds*))))
        ;; If the framework allocated a fresh Mcp-Session-Id during
        ;; this request (i.e. handle-post processed an /initialize and
        ;; called CREATE-SESSION on the framework's store), create the
        ;; matching task-mcp context now so the next request from the
        ;; client lands on a real ctx instead of UNKNOWN-SESSION.
        ;;
        ;; Hunchentoot stores outgoing header keys as keywords once
        ;; the framework's `(setf header-out "Mcp-Session-Id" ...)`
        ;; round-trips them, but the reader form `(header-out NAME)`
        ;; does not normalise NAME, so we have to look at the raw
        ;; alist to find both case variants reliably.
        (let* ((headers (ignore-errors (hunchentoot:headers-out*)))
               (new-sid
                 (cdr (assoc "Mcp-Session-Id" headers
                             :test (lambda (a b)
                                     (string-equal a (string b))))))
               (current-sid *session-id*))
          (when (and new-sid
                     (stringp new-sid)
                     (plusp (length new-sid))
                     (not (equal new-sid current-sid)))
            (ignore-errors (create-session-context new-sid))))
        ;; Persist session context on exit only when the request mutated
        ;; it.  Always touches LAST-ACTIVE so idle-session GC still sees
        ;; the request as a liveness signal.
        (when *session-id*
          (ignore-errors
           (maybe-save-session-context *session-id*
                                       snapshot-task-id
                                       snapshot-event-counter)))
        (release-request-tables tables)))))

;;; HTTP API endpoints (non-MCP, for hook integration)

(defun register-http-endpoints ()
  "Register HTTP endpoints for hook integration.
   These endpoints bypass MCP session validation since hooks run before
   Claude Code has established an MCP session."
  ;; Health check for daemon liveness detection
  (hunchentoot:define-easy-handler (health-handler :uri "/health") ()
    (setf (hunchentoot:content-type*) "application/json")
    (format nil "{\"status\":\"ok\",\"build_id\":~S}" (get-build-id)))

  ;; Diagnostic endpoint: session counts, thread count, CLOSE_WAIT count,
  ;; and a snapshot of the recent lock-event ring buffer.
  (register-admin-endpoints)

  (hunchentoot:define-easy-handler (register-pid-handler :uri "/register-pid")
      (session-id pid team-name agent-name agent-type)
    "Register Claude Code's PID for a session.
     Called by SessionStart hook to enable session file correlation.
     Query params: session-id (string), pid (integer),
       team-name (string, optional), agent-name (string, optional),
       agent-type (string, optional)."
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (let ((pid-int (when pid (parse-integer pid :junk-allowed nil)))
              ;; Normalize empty strings to nil
              (team (when (and team-name (plusp (length team-name))) team-name))
              (agent (when (and agent-name (plusp (length agent-name))) agent-name))
              (atype (when (and agent-type (plusp (length agent-type))) agent-type)))
          (if (and session-id (plusp (length session-id)) pid-int (plusp pid-int))
              (let ((result (register-claude-pid session-id pid-int
                                                :team-name team
                                                :agent-name agent
                                                :agent-type atype)))
                ;; Also register in playbook PID registry so feedback-state.json
                ;; gets written to the Claude session path, not MCP session path.
                (handler-case
                    (playbook::register-claude-session-for-pid pid-int session-id)
                  (error (e)
                    (format *error-output*
                            "~&;; WARNING: playbook PID registration failed for session ~A: ~A~%"
                            session-id e)))
                (format nil "{\"success\": true, \"message\": ~S}" result))
              (progn
                (setf (hunchentoot:return-code*) 400)
                "{\"success\": false, \"error\": \"Missing or invalid session-id or pid\"}")))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"success\": false, \"error\": ~S}" (princ-to-string e)))))

  ;; Tool call endpoint for PostToolUse hook integration
  (hunchentoot:define-easy-handler (tool-call-handler :uri "/tool/call")
      (session-id tool)
    "Record a tool.call event with proper vector clock.
     Called by PostToolUse hook to emit first-class tool events.

     Query params:
       session-id (string): task-mcp session ID (from session file)
       tool (string): Tool name (e.g. 'Read', 'Edit', 'Bash')

     POST body (optional JSON):
       {\"args\": {\"file_path\": \"/src/foo.lisp\", ...}}

     Returns JSON with success status and event ID."
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (progn
          ;; Validate required params
          (unless (and session-id (plusp (length session-id)))
            (setf (hunchentoot:return-code*) 400)
            (return-from tool-call-handler
              "{\"success\": false, \"error\": \"Missing session-id\"}"))
          (unless (and tool (plusp (length tool)))
            (setf (hunchentoot:return-code*) 400)
            (return-from tool-call-handler
              "{\"success\": false, \"error\": \"Missing tool\"}"))
          ;; Load session context (gets *current-task-id*, *session-vc*)
          (load-session-context session-id)
          ;; Check if session has a current task
          (unless *current-task-id*
            (setf (hunchentoot:return-code*) 400)
            (return-from tool-call-handler
              "{\"success\": false, \"error\": \"Session has no current task\"}"))
          ;; Parse optional args from POST body
          (let* ((body (hunchentoot:raw-post-data :force-text t))
                 (args-ht (when (and body (plusp (length body)))
                            (ignore-errors
                             (let ((json (yason:parse body)))
                               (when (hash-table-p json)
                                 (gethash "args" json))))))
                 ;; Convert hash-table args to plist with keyword keys
                 (args-plist (when (hash-table-p args-ht)
                               (loop for k being the hash-keys of args-ht
                                     using (hash-value v)
                                     collect (intern (string-upcase k) :keyword)
                                     collect v)))
                 ;; Build event data as plist (first-class format)
                 (data (if args-plist
                           (list :tool tool :args args-plist)
                           (list :tool tool)))
                 ;; Emit event with proper vector clock
                 (event (emit-event :tool.call data)))
            ;; Save session context (vector clock updated)
            (save-session-context session-id)
            (format nil "{\"success\": true, \"event_id\": ~S, \"task\": ~S}"
                    (task:event-id event) *current-task-id*)))
      (unknown-session (c)
        ;; PostToolUse hook arrived with a stale task-mcp sid.  Return
        ;; 404 with the typed code so the hook can detect the recovery
        ;; case explicitly rather than treating it as a server error.
        (setf (hunchentoot:return-code*) 404)
        (format nil "{\"success\": false, \"error\": \"session not found\", ~
                     \"code\": \"session-not-found\", \"session_id\": ~S}"
                (or (unknown-session-session-id c) "")))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"success\": false, \"error\": ~S}" (princ-to-string e)))))

  ;; File conflict endpoint for PostToolUse:Edit hook integration
  (hunchentoot:define-easy-handler (file-conflict-endpoint :uri "/file-conflict")
      (session-id file-path max-age-minutes)
    "Check for file conflicts from the perspective of a session.
     Called by PostToolUse:Edit hook to detect concurrent edits.
     Uses read-active-sessions (PID-checked) for liveness and
     scan-file-activity (session-aware) for attribution.

     Query params:
       session-id (string): caller's MCP session ID (for self-exclusion)
       file-path (string): normalized file path that was edited
       max-age-minutes (integer, optional): conflict window (default 30)"
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (let* ((max-age (if (and max-age-minutes (plusp (length max-age-minutes)))
                            (parse-integer max-age-minutes :junk-allowed t)
                            30))
               (conflicts (file-conflict-for-caller
                           file-path
                           (or session-id "")
                           :max-age-minutes max-age)))
          (if conflicts
              ;; Encode conflicts as JSON array
              (with-output-to-string (s)
                (let ((arr (mapcar
                            (lambda (c)
                              (let ((ht (make-hash-table :test #'equal)))
                                (setf (gethash "session" ht) (getf c :session)
                                      (gethash "task" ht) (getf c :task)
                                      (gethash "age_minutes" ht) (getf c :age-minutes)
                                      (gethash "edit_count" ht) (getf c :edit-count))
                                (when (getf c :task-description)
                                  (setf (gethash "task_description" ht)
                                        (getf c :task-description)))
                                (when (getf c :last-observation)
                                  (setf (gethash "last_observation" ht)
                                        (getf c :last-observation)))
                                (when (getf c :other-files)
                                  (setf (gethash "other_files" ht)
                                        (mapcar (lambda (pair)
                                                  (let ((f (make-hash-table :test #'equal)))
                                                    (setf (gethash "path" f) (car pair)
                                                          (gethash "count" f) (cdr pair))
                                                    f))
                                                (getf c :other-files))))
                                ht))
                            conflicts)))
                  (yason:encode arr s)))
              "[]"))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"error\": ~S}" (princ-to-string e))))))

;;; Server lifecycle

(defun initialize-server ()
  "Initialize the task MCP server."
  (initialize-session)
  ;; Load embedding cache from disk (survives daemon restarts)
  (initialize-embedding-cache-path)
  (when *embedding-cache-path*
    (load-embedding-cache)
    (when (and *log-verbose* (plusp (embedding-cache-size)))
      (format *error-output* "  Embedding cache: ~D entries from disk~%"
              (embedding-cache-size))))
  (when *log-verbose*
    (format *error-output* "Task MCP server initialized~%")
    (format *error-output* "  Session: ~A~%" *session-id*)
    (format *error-output* "  Tasks root: ~A~%" task:*tasks-root*)
    (when *current-task-id*
      (format *error-output* "  Current task: ~A~%" *current-task-id*))))

(defun start-task-server (&key background)
  "Create and start the task MCP server.
   In HTTP mode, also spawns the periodic session-context GC thread
   so idle daemons don't accumulate dead-Claude contexts, and runs a
   one-shot pass that archives feedback-state.json files written under
   non-UUID-shaped session directories (which the Stop hook cannot
   find)."
  (initialize-server)
  (let* ((transport (select-transport))
         (server (mcp-framework:make-server
                  :name "task-mcp"
                  :version "1.0.0"
                  :transport transport)))
    (when *http-mode*
      (start-session-gc-thread)
      (when *log-verbose*
        (format *error-output*
                "  Session GC: every ~Ds, max-age ~Dh~%"
                *session-gc-interval-seconds*
                *session-gc-max-age-hours*))
      (let ((cwd (or (playbook:mcp-find-depot-root)
                     (namestring (uiop:getcwd)))))
        (handler-case
            (playbook:reconcile-miskeyed-feedback-state-files cwd)
          (error (e)
            (format *error-output*
                    "~&;; playbook-miskeyed-archive: scan failed: ~A~%" e)))))
    (mcp-framework:run-server server :background background)))

(defun main ()
  "Main entry point for standalone server."
  (handler-case
      (progn
        (when *log-verbose*
          (format *error-output* "Starting Task MCP Server...~%"))
        (start-task-server :background nil))
    (error (e)
      (format *error-output* "Fatal error: ~A~%" e)
      (uiop:quit 1))))
