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
  "Wrap each HTTP request with thread-local session state bindings."
  (declare (ignore request))
  (let (;; Per-request elog cache (prevents redundant disk reads within a request)
        (task:*elog-cache* (make-hash-table :test 'equal))
        ;; Task session isolation
        (*current-task-id* *current-task-id*)
        (*session-id* *session-id*)
        (*session-vc* *session-vc*)
        (*claude-pid* *claude-pid*)
        (*http-mode* *http-mode*)
        ;; Playbook session isolation (unified daemon serves both)
        (playbook-mcp::*claimed-session* playbook-mcp::*claimed-session*)
        (playbook-mcp::*claimed-session-cwd* playbook-mcp::*claimed-session-cwd*)
        (playbook-mcp::*http-mode* *http-mode*))
    (ensure-session-context)
    (playbook-mcp::ensure-playbook-session-context)
    (call-next-method)))

;;; HTTP API endpoints (non-MCP, for hook integration)

(defun register-http-endpoints ()
  "Register HTTP endpoints for hook integration.
   These endpoints bypass MCP session validation since hooks run before
   Claude Code has established an MCP session."
  ;; Health check for daemon liveness detection
  (hunchentoot:define-easy-handler (health-handler :uri "/health") ()
    (setf (hunchentoot:content-type*) "application/json")
    (format nil "{\"status\":\"ok\",\"build_id\":~S}" (get-build-id)))

  (hunchentoot:define-easy-handler (register-pid-handler :uri "/register-pid")
      (session-id pid depot team-name agent-name agent-type)
    "Register Claude Code's PID for a session.
     Called by SessionStart hook to enable session file correlation.
     Query params: session-id (string), pid (integer),
       depot (string, optional - client's current depot name),
       team-name (string, optional), agent-name (string, optional),
       agent-type (string, optional)."
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (let ((pid-int (when pid (parse-integer pid :junk-allowed nil)))
              ;; Normalize empty strings to nil
              (dep (when (and depot (plusp (length depot))) depot))
              (team (when (and team-name (plusp (length team-name))) team-name))
              (agent (when (and agent-name (plusp (length agent-name))) agent-name))
              (atype (when (and agent-type (plusp (length agent-type))) agent-type)))
          (if (and session-id (plusp (length session-id)) pid-int (plusp pid-int))
              (let ((result (register-claude-pid session-id pid-int
                                                :depot dep
                                                :team-name team
                                                :agent-name agent
                                                :agent-type atype)))
                ;; Also register in playbook PID registry so feedback-state.json
                ;; gets written to the Claude session path, not MCP session path.
                (ignore-errors
                  (playbook-mcp::register-claude-session-for-pid pid-int session-id))
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
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"success\": false, \"error\": ~S}" (princ-to-string e))))))

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
    (if (> (hash-table-count task:*depot-tasks-roots*) 0)
        (maphash (lambda (depot root)
                   (format *error-output* "  Depot ~A: ~A~%" depot root))
                 task:*depot-tasks-roots*)
        (format *error-output* "  Tasks root: ~A~%" task:*tasks-root*))
    (when *current-task-id*
      (format *error-output* "  Current task: ~A~%" *current-task-id*))))

(defun start-task-server (&key background)
  "Create and start the task MCP server."
  (initialize-server)
  (let* ((transport (select-transport))
         (server (mcp-framework:make-server
                  :name "task-mcp"
                  :version "1.0.0"
                  :transport transport)))
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
