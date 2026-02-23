;;; kli — CLI dispatch entry point
;;;
;;; The kli binary bundles both task-mcp and playbook-mcp servers into a
;;; single SBCL image. Claude Code invokes it as an MCP server; the CLI
;;; selects which server to run.

(in-package #:kli)

(defun getenv-nonempty (var)
  "Get environment variable VAR, returning NIL if unset or empty."
  (let ((val (uiop:getenv var)))
    (when (and val (plusp (length val))) val)))

(defvar *build-id* nil
  "Build identifier, computed at startup from executable path.
   Same binary = same build-id. Different Nix derivation = different build-id.
   For non-Nix builds, uses executable mtime as proxy.")

(defun compute-build-id ()
  "Compute build-id from running SBCL executable.
   Nix: extracts 32-char store hash from /nix/store/<hash>-<name>/...
   Non-Nix: uses executable file-write-date as version proxy."
  (or (ignore-errors
       (let* ((exe #+sbcl sb-ext:*runtime-pathname*
                   #-sbcl (first (uiop:raw-command-line-arguments)))
              (truepath (when exe (namestring (truename exe))))
              (store-prefix "/nix/store/"))
         (when truepath
           (if (and (>= (length truepath) (+ (length store-prefix) 32))
                    (string= store-prefix truepath
                             :end2 (length store-prefix)))
               ;; Nix: extract the 32-char hash
               (subseq truepath (length store-prefix)
                       (position #\- truepath :start (length store-prefix)))
               ;; Non-Nix: executable mtime
               (format nil "dev-~A" (file-write-date truepath))))))
      (format nil "unknown-~A" (get-universal-time))))

(defun ensure-build-id ()
  "Initialize *build-id* if not yet computed. Returns the build-id."
  (unless *build-id*
    (setf *build-id* (compute-build-id)))
  *build-id*)

;;; --- Swank lifecycle (only for long-running commands) ---

(defvar *swank-port* nil
  "Actual Swank port after startup, or NIL if not running.")

(defun start-swank (&key (port 14011))
  "Start Swank server for live debugging on PORT.
   Falls back to ephemeral port 0 if PORT is in use.
   Registers in XDG global dir for cross-session discovery.
   Returns actual port or NIL on failure."
  (let* ((configured-port port)
         (create-server (find-symbol "CREATE-SERVER" "SWANK")))
    (unless create-server
      (format *error-output* "~&;; Swank CREATE-SERVER not found, REPL disabled~%")
      (force-output *error-output*)
      (return-from start-swank nil))
    (flet ((try-start (port)
             (funcall create-server
                      :port port :interface "127.0.0.1"
                      :dont-close t :style :spawn)))
      (let ((port
              (handler-case (try-start configured-port)
                (#+sbcl sb-bsd-sockets:address-in-use-error ()
                  (format *error-output*
                          "~&;; Swank port ~A in use, trying ephemeral...~%"
                          configured-port)
                  (force-output *error-output*)
                  (handler-case (try-start 0)
                    (error (e)
                      (format *error-output*
                              "~&;; Swank ephemeral port failed: ~A~%" e)
                      (force-output *error-output*)
                      nil)))
                (error (e)
                  (format *error-output*
                          "~&;; Swank startup failed: ~A~%" e)
                  (force-output *error-output*)
                  nil))))
        (when port
          (setf *swank-port* port)
          (when *verbose*
            (format *error-output* "~&Swank started on 127.0.0.1:~A~%" port)
            (force-output *error-output*))
          (register-swank port))
        port))))

(defun register-swank (port)
  "Register Swank in XDG global dir for cross-session discovery."
  (ignore-errors
    (let* ((runtime-dir (or (uiop:getenv "XDG_RUNTIME_DIR")
                            (format nil "/run/user/~A"
                                    #+sbcl (sb-posix:getuid) #-sbcl 1000)))
           (shared-dir (merge-pathnames
                        "repl/swank/shared/"
                        (uiop:ensure-directory-pathname runtime-dir)))
           (file (merge-pathnames "kli.json" shared-dir))
           (pid #+sbcl (sb-posix:getpid) #-sbcl 0))
      (ensure-directories-exist shared-dir)
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out "{\"name\":\"kli\",\"protocol\":\"swank\",\"port\":~A,\"pid\":~A,\"kind\":\"daemon\",\"shared\":true,\"started\":~A}"
                port pid (get-universal-time))))))

(defun unregister-swank ()
  "Remove Swank registration file."
  (ignore-errors
    (let* ((runtime-dir (or (uiop:getenv "XDG_RUNTIME_DIR")
                            (format nil "/run/user/~A"
                                    #+sbcl (sb-posix:getuid) #-sbcl 1000)))
           (shared-dir (merge-pathnames
                        "repl/swank/shared/"
                        (uiop:ensure-directory-pathname runtime-dir)))
           (file (merge-pathnames "kli.json" shared-dir)))
      (delete-file file))))

(defun install-debugger-hook ()
  "Thread-aware debugger hook for long-running kli commands.
   Main thread errors are fatal (exit 1).  Worker thread errors
   are logged and aborted so the process continues."
  (let ((main-thread sb-thread:*current-thread*))
    ;; Prevent Swank from overriding our hook
    (let ((gd (find-symbol "*GLOBAL-DEBUGGER*" "SWANK")))
      (when gd (setf (symbol-value gd) nil)))
    (flet ((hook (condition previous-hook)
             (declare (ignore previous-hook))
             (let ((current sb-thread:*current-thread*))
               (cond
                 ((eq current main-thread)
                  (format *error-output* "~&[kli] FATAL: ~A~%" condition)
                  (sb-debug:print-backtrace :stream *error-output* :count 20)
                  (force-output *error-output*)
                  (unregister-swank)
                  (sb-ext:exit :code 1))
                 (t
                  (format *error-output* "~&[kli] WARNING on ~S: ~A~%"
                          (sb-thread:thread-name current) condition)
                  (force-output *error-output*)
                  (let ((abort (find-restart 'abort condition)))
                    (when abort (invoke-restart abort))))))))
      (setf sb-ext:*invoke-debugger-hook* #'hook)
      (setf *debugger-hook*
            (lambda (c old) (declare (ignore old)) (funcall #'hook c nil))))
    ;; Redirect *debug-io* so built-in debugger can't hang on terminal read
    (setf *debug-io* (make-two-way-stream
                       (make-string-input-stream "")
                       *error-output*))
    (when *verbose*
      (format *error-output* "~&[kli] Debugger hook installed~%")
      (force-output *error-output*))))

(defun prepare-daemon (&key (swank-port 14011))
  "Prepare for long-running execution: debugger hook + Swank in background.
   SWANK-PORT controls the Swank listener port for live debugging."
  (install-debugger-hook)
  (let ((port swank-port))
    (sb-thread:make-thread (lambda () (start-swank :port port))
                           :name "swank-startup")))

(defun discover-playbook-paths ()
  "Derive PLAYBOOK_PATHS from depot structure when env var is unset.
   Uses depot:list-depots-with-tasks to find depot roots, then probes
   for ace/playbook.md in each."
  (handler-case
      (let ((depot-pkg (find-package :depot)))
        (when depot-pkg
          (let ((siblings-fn (find-symbol "LIST-SIBLING-DEPOTS" depot-pkg)))
            (when siblings-fn
              (let* ((depots (funcall siblings-fn))
                     (root (funcall (find-symbol "FIND-DEPOT-ROOT" depot-pkg)))
                     (parent (when root
                               (namestring
                                (uiop:pathname-parent-directory-pathname
                                 (uiop:ensure-directory-pathname root)))))
                     (paths
                       (loop for depot in (or depots '())
                             for pb = (format nil "~Aace/playbook.md"
                                              (if parent
                                                  (format nil "~A~A/" parent depot)
                                                  (format nil "~A/" depot)))
                             when (probe-file pb) collect pb)))
                (when paths
                  (format nil "~{~A~^:~}" paths)))))))
    (error () nil)))

;;; --- Tool CLI: schema-driven arg parsing and help ---

(defun coerce-arg-value (value type)
  "Coerce string VALUE to JSON Schema TYPE."
  (cond
    ((string= type "integer") (parse-integer value))
    ((string= type "number") (read-from-string value))
    ((string= type "boolean")
     (cond ((member value '("true" "1" "yes") :test #'string-equal) t)
           ((member value '("false" "0" "no") :test #'string-equal) nil)
           (t (error "Invalid boolean: ~A" value))))
    (t value)))

(defun parse-tool-args (schema args)
  "Parse CLI args using JSON Schema from tool registry.
   Required params are filled positionally in declaration order.
   Optional params use --param_name value syntax.
   Type coercion from schema (integer, number, boolean, string)."
  (let* ((raw-props (cdr (assoc "properties" schema :test #'string=)))
         (props (if (hash-table-p raw-props) nil raw-props))
         (required (coerce (or (cdr (assoc "required" schema :test #'string=)) #())
                           'list))
         (result (make-hash-table :test 'equal))
         (positional-targets required)
         (remaining args))
    (loop while remaining do
      (let ((arg (pop remaining)))
        (cond
          ;; Named arg: --param_name value
          ((and (>= (length arg) 3) (string= "--" arg :end2 2))
           (let* ((param-name (subseq arg 2))
                  (prop (cdr (assoc param-name props :test #'string=))))
             (unless prop
               (error "Unknown parameter: --~A~%Run 'kli ~A --help' for usage."
                      param-name (or (first remaining) "TOOL")))
             (let ((value (pop remaining))
                   (type (cdr (assoc "type" prop :test #'string=))))
               (setf (gethash param-name result)
                     (coerce-arg-value value type)))))
          ;; Positional arg: fill next required param
          (t
           (if positional-targets
               (let* ((param-name (pop positional-targets))
                      (prop (cdr (assoc param-name props :test #'string=)))
                      (type (when prop
                              (cdr (assoc "type" prop :test #'string=)))))
                 (setf (gethash param-name result)
                       (coerce-arg-value arg (or type "string"))))
               (error "Too many positional arguments: ~A" arg))))))
    result))

(defun format-tool-help (tool-name)
  "Generate help text for a tool from *tool-registry*. Returns string or NIL."
  (let ((tool (mcp-framework:get-tool tool-name)))
    (unless tool (return-from format-tool-help nil))
    (let* ((info (funcall tool :inspect))
           (doc (getf info :documentation))
           (schema (funcall tool :schema))
           (raw-props (cdr (assoc "properties" schema :test #'string=)))
           (props (if (hash-table-p raw-props) nil raw-props))
           (required (coerce (or (cdr (assoc "required" schema :test #'string=)) #())
                             'list)))
      (with-output-to-string (s)
        (format s "Usage: kli ~A" tool-name)
        (dolist (r required)
          (format s " <~A>" r))
        (dolist (prop props)
          (unless (member (car prop) required :test #'string=)
            (format s " [--~A <~A>]" (car prop)
                    (cdr (assoc "type" (cdr prop) :test #'string=)))))
        (terpri s)
        (when doc (format s "~%~A~%" doc))
        (when props
          (format s "~%Parameters:~%")
          (dolist (prop props)
            (let* ((name (car prop))
                   (pschema (cdr prop))
                   (type (cdr (assoc "type" pschema :test #'string=)))
                   (desc (cdr (assoc "description" pschema :test #'string=)))
                   (req (member name required :test #'string=)))
              (format s "  ~24A ~8A ~A~A~%"
                      name type desc
                      (if req " (required)" "")))))))))

(defun format-all-tools ()
  "Generate listing of all registered MCP tools."
  (let ((tools (sort (mcp-framework:list-tools) #'string<)))
    (with-output-to-string (s)
      (format s "~%Tools (via MCP daemon):~%")
      (dolist (name tools)
        (let* ((tool (mcp-framework:get-tool name))
               (info (funcall tool :inspect))
               (doc (or (getf info :documentation) "")))
          (format s "  ~24A ~A~%"
                  name
                  (if (> (length doc) 52)
                      (concatenate 'string (subseq doc 0 49) "...")
                      doc))))
      (format s "~%Run 'kli <tool> --help' for details on a specific tool.~%"))))

;;; --- Tool CLI: HTTP session management ---

(defun cli-session-path ()
  "Path to persisted CLI session file."
  (let ((runtime-dir (or (uiop:getenv "XDG_RUNTIME_DIR")
                         (format nil "/run/user/~A"
                                 #+sbcl (sb-posix:getuid) #-sbcl 1000))))
    (merge-pathnames "kli/cli-session"
                     (uiop:ensure-directory-pathname runtime-dir))))

(defun save-session (session-id)
  "Persist MCP session ID for cross-invocation reuse."
  (let ((path (cli-session-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string session-id out))
    session-id))

(defun load-session ()
  "Load persisted MCP session ID, or NIL if none exists."
  (let ((path (cli-session-path)))
    (when (probe-file path)
      (ignore-errors
        (let ((sid (uiop:read-file-string path)))
          (when (plusp (length sid)) sid))))))

(defun mcp-init (&key (host "127.0.0.1") (port 8090))
  "Initialize MCP session with daemon. Returns session-id string."
  (let ((url (format nil "http://~A:~A/mcp" host port))
        (body (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-03-26\",\"capabilities\":{},\"clientInfo\":{\"name\":\"kli-cli\",\"version\":\"~A\"}}}" *version*)))
    (multiple-value-bind (body-bytes status headers)
        (dex:post url
                  :content body
                  :headers '(("Content-Type" . "application/json"))
                  :connect-timeout 3
                  :force-binary t)
      (declare (ignore body-bytes status))
      (let ((session-id (gethash "mcp-session-id" headers)))
        (unless session-id
          (error "No Mcp-Session-Id in response — is kli daemon running?"))
        session-id))))

(defun detect-cli-depot ()
  "Detect depot name from actual CWD for CLI HTTP header injection.
   Uses find-depot-root-from instead of find-depot-root to bypass
   DEPOT_ROOT env var, which may point to a different depot.
   Returns depot name string (e.g. \"kleisli\") or NIL if not in a depot."
  (let ((depot-root (depot:find-depot-root-from
                     (namestring (uiop:getcwd)))))
    (when depot-root
      (car (last (pathname-directory
                  (uiop:ensure-directory-pathname depot-root)))))))

(defun mcp-call (tool-name args-ht session-id &key (host "127.0.0.1") (port 8090))
  "Call MCP tool via daemon HTTP. Returns result text string."
  (let* ((url (format nil "http://~A:~A/mcp" host port))
         (params (make-hash-table :test 'equal))
         (request (make-hash-table :test 'equal)))
    (setf (gethash "name" params) tool-name
          (gethash "arguments" params) args-ht
          (gethash "jsonrpc" request) "2.0"
          (gethash "id" request) 2
          (gethash "method" request) "tools/call"
          (gethash "params" request) params)
    (let ((body (with-output-to-string (s) (yason:encode request s)))
          (depot (detect-cli-depot)))
      (multiple-value-bind (body-bytes status)
          (dex:post url
                    :content body
                    :headers `(("Content-Type" . "application/json")
                               ("Mcp-Session-Id" . ,session-id)
                               ,@(when depot
                                   `(("X-Depot" . ,depot))))
                    :connect-timeout 5
                    :read-timeout 30
                    :force-binary t)
        (declare (ignore status))
        (let* ((response-text (babel:octets-to-string body-bytes))
               (parsed (yason:parse response-text)))
          (if (gethash "error" parsed)
              (let* ((err (gethash "error" parsed))
                     (msg (gethash "message" err)))
                (error "~A" msg))
              (let ((content (gethash "content" (gethash "result" parsed))))
                (with-output-to-string (s)
                  (dolist (item content)
                    (format s "~A" (gethash "text" item)))))))))))

(defun ensure-session (&key (host "127.0.0.1") (port 8090))
  "Get a valid MCP session: load persisted or init new.
   Handles stale sessions (daemon restart) by re-initializing."
  (let ((sid (load-session)))
    (if sid
        ;; Validate by sending initialized notification
        (handler-case
            (let ((request (make-hash-table :test 'equal)))
              (setf (gethash "jsonrpc" request) "2.0"
                    (gethash "id" request) 0
                    (gethash "method" request) "notifications/initialized"
                    (gethash "params" request) (make-hash-table :test 'equal))
              (dex:post (format nil "http://~A:~A/mcp" host port)
                        :content (with-output-to-string (s)
                                   (yason:encode request s))
                        :headers `(("Content-Type" . "application/json")
                                   ("Mcp-Session-Id" . ,sid))
                        :connect-timeout 3
                        :force-binary t)
              sid)
          (error ()
            (let ((new-sid (mcp-init :host host :port port)))
              (save-session new-sid)
              new-sid)))
        ;; No persisted session
        (let ((new-sid (mcp-init :host host :port port)))
          (save-session new-sid)
          new-sid))))

;;; --- Tool CLI: dispatch ---

(defun extract-flag (flag args)
  "Extract --FLAG VALUE from ARGS. Returns (values value remaining-args).
   If flag not found, returns (values nil args)."
  (loop for tail on args
        when (string= (car tail) flag)
          return (values (cadr tail)
                         (append (ldiff args tail) (cddr tail)))
        finally (return (values nil args))))

(defun cli-dispatch (command args)
  "Dispatch a tool command. Returns (values tag result) or NIL if not a tool.
   TAG is :tool-help or :result. Business logic only — no process exit."
  (let ((tool (mcp-framework:get-tool command)))
    (cond
      ((null tool) nil)
      ;; --help: in-process schema introspection, no HTTP needed
      ((member "--help" args :test #'string=)
       (values :tool-help (format-tool-help command)))
      ;; Execute via HTTP to daemon
      (t
       (multiple-value-bind (task-id tool-args) (extract-flag "--task" args)
         (let* ((schema (funcall tool :schema))
                (raw-stdin (when (and (null tool-args)
                                      (not (interactive-stream-p *standard-input*)))
                             (uiop:slurp-stream-string *standard-input*)))
                (stdin-text (when raw-stdin
                              (let ((trimmed (string-right-trim
                                             '(#\Newline #\Return) raw-stdin)))
                                (when (plusp (length trimmed)) trimmed))))
                (effective-args (if stdin-text (list stdin-text) tool-args))
                (args-ht (parse-tool-args schema effective-args))
                (sid (ensure-session)))
           (when task-id
             (let ((ctx-ht (make-hash-table :test 'equal)))
               (setf (gethash "task_id" ctx-ht) task-id)
               (mcp-call "task_set_current" ctx-ht sid)))
           (values :result (mcp-call command args-ht sid))))))))

;;; --- CLI ---

(defun print-help ()
  (format t "kli ~A — event-sourced task graphs for Claude Code~%~%" *version*)
  (format t "Usage: kli <command> [options]~%~%")
  (format t "Commands:~%")
  (format t "  init                       Initialize kli in current project~%")
  (format t "  update [--yes] [--version V]  Update to latest or specific version~%")
  (format t "  serve [--task|--playbook]  Start MCP server (default: task)~%")
  (format t "  dashboard                  Start task graph dashboard~%")
  (format t "  hook <name>                Run Claude Code hook handler~%")
  (format t "  status [--task ID]         One-shot task overview~%")
  (format t "  version                    Show version~%")
  (format t "  help                       Show this help~%")
  (write-string (format-all-tools))
  (format t "~%Aliases:~%")
  (format t "  tq                         task_query~%")
  (format t "  pq                         pq_query~%")
  (format t "~%Environment:~%")
  (format t "  KLI_TASKS_DIR              Task storage (default: .kli/tasks/)~%")
  (format t "  PLAYBOOK_PATHS             Colon-separated playbook file paths~%")
  (format t "  OLLAMA_HOST                Ollama endpoint (default: http://localhost:11434)~%")
  (format t "  TASK_MCP_TRANSPORT         'http' for HTTP mode, else stdio~%")
  (format t "  TASK_MCP_SWANK_PORT        Swank port for task daemon (default: 14011)~%")
  (format t "  PLAYBOOK_MCP_SWANK_PORT    Swank port for playbook daemon (default: 14010)~%"))

(defun main ()
  "Top-level entry point for the kli binary.
   Dispatches to task-mcp or playbook-mcp based on command-line arguments."
  (let* ((args (uiop:command-line-arguments))
         (command (or (cdr (assoc (first args)
                                  '(("tq" . "task_query") ("pq" . "pq_query"))
                                  :test #'string=))
                      (first args))))
    ;; Initialize verbose logging early — before any command that calls
    ;; prepare-daemon, which reads *verbose* in install-debugger-hook.
    (when (getenv-nonempty "KLI_LOG")
      (setf *verbose* t))
    (handler-case
        (cond
          ((or (null command) (string= command "help") (string= command "--help"))
           (print-help)
           (uiop:quit 0))
          ((or (string= command "version")
               (string= command "--version")
               (string= command "-v"))
           (format t "kli ~A~%" *version*)
           (uiop:quit 0))
          ((string= command "init")
           (init)
           (uiop:quit 0))
          ((string= command "update")
           (update (rest args))
           (uiop:quit 0))
          ((string= command "serve")
           (let ((mode (second args)))
             ;; Propagate verbose flag to service-specific variables
             (when *verbose*
               (setf playbook-mcp:*log-verbose* t
                     task-mcp::*log-verbose* t))
             ;; Resolve Swank port from mode-specific env var
             (flet ((env-port (var default)
                      (or (ignore-errors
                            (parse-integer (uiop:getenv var) :junk-allowed t))
                          default)))
               (cond
                 ((or (null mode) (string= mode "--task"))
                  (prepare-daemon :swank-port (env-port "TASK_MCP_SWANK_PORT" 14011))
                  (task-mcp::initialize-ollama-config)
                  (playbook-mcp:initialize-ollama-config)
                  ;; Initialize playbook subsystem so co-registered playbook tools
                  ;; (pq_query, playbook_graph_health, playbook_status) have state.
                  ;; The kli binary bundles both task-mcp and playbook-mcp; define-tool
                  ;; macros register all tools globally, but only task-mcp:main runs.
                  ;; If PLAYBOOK_PATHS not set (daemon started before shell profile),
                  ;; derive from depot structure so patterns load regardless.
                  ;; Note: empty string from shell ${VAR:-} must be treated as unset.
                  (unless (getenv-nonempty "PLAYBOOK_PATHS")
                    (let ((paths (discover-playbook-paths)))
                      (when paths
                        (setf (uiop:getenv "PLAYBOOK_PATHS") paths))))
                  (playbook-mcp:initialize-server)
                  ;; Sync playbook HTTP mode with task daemon's mode.
                  ;; Playbook's select-transport never runs in unified mode,
                  ;; so *http-mode* stays unbound unless we set it here.
                  (setf playbook-mcp::*http-mode* t)
                  (task-mcp:main))
                 ((string= mode "--playbook")
                  ;; Standalone playbook server — for development/debugging only.
                  ;; Production uses unified --task mode which serves both.
                  (prepare-daemon :swank-port (env-port "PLAYBOOK_MCP_SWANK_PORT" 14010))
                  (playbook-mcp:initialize-ollama-config)
                  (unless (getenv-nonempty "PLAYBOOK_PATHS")
                    (let ((paths (discover-playbook-paths)))
                      (when paths
                        (setf (uiop:getenv "PLAYBOOK_PATHS") paths))))
                  (playbook-mcp:main))
                 (t
                  (format *error-output* "Unknown serve mode: ~A~%Try: kli help~%" mode)
                  (uiop:quit 1))))))
          ((string= command "dashboard")
           (prepare-daemon)
           (kli-dashboard:start)
           (format t "~&Press Ctrl-C to stop.~%")
           (handler-case (loop (sleep 60))
             (#+sbcl sb-sys:interactive-interrupt ()
               (kli-dashboard:stop)
               (unregister-swank))))
          ((string= command "hook")
           (let ((name (second args)))
             (if name
                 (progn (kli-hook:hook-main name) (uiop:quit 0))
                 (progn (format *error-output* "Usage: kli hook <name>~%") (uiop:quit 1)))))
          (t
           ;; Tool fallthrough: check *tool-registry* before erroring
           (multiple-value-bind (tag result) (cli-dispatch command (rest args))
             (case tag
               (:tool-help
                (write-string result)
                (uiop:quit 0))
               (:result
                (write-string result)
                (terpri)
                (uiop:quit 0))
               ((nil)
                (error "Unknown command: ~A~%Try: kli help" command))))))
      (error (e)
        (format *error-output* "Fatal error: ~A~%" e)
        (uiop:quit 1)))))
