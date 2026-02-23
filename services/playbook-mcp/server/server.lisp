;;; Playbook MCP Server - Server Entry Point
;;; Main function and server lifecycle

(in-package #:playbook-mcp)

(defvar *log-verbose*)  ; forward declaration — defined in lib/package.lisp

;;; Configuration

(defparameter *default-playbook-paths* nil
  "List of playbook.md paths to load on startup.
   Set via environment variable PLAYBOOK_PATHS (colon-separated).")


;;; Initialization

(defun getenv-nonempty (var)
  "Get environment variable VAR, returning NIL if unset or empty.
   Prevents empty strings from being treated as valid paths."
  (let ((val (uiop:getenv var)))
    (when (and val (plusp (length val))) val)))

(defun load-playbooks-from-env ()
  "Load playbook paths from PLAYBOOK_PATHS environment variable."
  (let ((paths-str (uiop:getenv "PLAYBOOK_PATHS")))
    (when (and paths-str (> (length paths-str) 0))
      (let ((paths (cl-ppcre:split ":" paths-str)))
        (dolist (path paths)
          (when (probe-file path)
            (let ((count (load-playbook-file path)))
              (when *log-verbose*
                (format *error-output* "Loaded ~D patterns from ~A~%" count path)))))))))

(defun initialize-edges-path ()
  "Set *manual-edges-path* to git-tracked manual edges file.
   Default is <meta-root>/playbook-manual-edges.json.
   Override via PLAYBOOK_EDGES_PATH environment variable."
  (let ((path (or (getenv-nonempty "PLAYBOOK_EDGES_PATH")
                  (let ((meta (detect-depot-meta-path)))
                    (when meta
                      (namestring (merge-pathnames "playbook-manual-edges.json" meta))))
                  "ace/playbook-manual-edges.json")))
    (setf *manual-edges-path* path)))

(defun initialize-temporal-path ()
  "Set *temporal-data-path* and load existing temporal data.
   Default is <meta-root>/playbook-temporal.json.
   Override via PLAYBOOK_TEMPORAL_PATH environment variable."
  (let ((path (or (getenv-nonempty "PLAYBOOK_TEMPORAL_PATH")
                  (let ((meta (detect-depot-meta-path)))
                    (when meta
                      (namestring (merge-pathnames "playbook-temporal.json" meta))))
                  "ace/playbook-temporal.json")))
    (setf *temporal-data-path* path)
    (when (probe-file path)
      (let ((count (load-temporal-data path)))
        (when (and count *log-verbose*)
          (format *error-output* "Loaded temporal data for ~D patterns from ~A~%"
                  count path))))))

(defun detect-depot-meta-path ()
  "Detect the depot's metadata directory (.kli/ or ace/) path.
   Tries depot:depot-meta-root first (available in kli binary),
   then falls back to probing .kli/ and ace/ under depot root.
   Returns path string or NIL if no depot found."
  (or (ignore-errors
        (let ((fn (find-symbol "DEPOT-META-ROOT" :depot)))
          (when fn (funcall fn nil))))
      (let ((depot-root (mcp-find-depot-root)))
        (when depot-root
          (let ((kli-dir (namestring (merge-pathnames ".kli/" depot-root))))
            (if (uiop:directory-exists-p kli-dir)
                kli-dir
                (let ((ace-dir (namestring (merge-pathnames "ace/" depot-root))))
                  (when (uiop:directory-exists-p ace-dir)
                    ace-dir))))))))

(defun detect-depot-ace-path ()
  "Deprecated: use detect-depot-meta-path instead.
   Kept for backward compatibility."
  (detect-depot-meta-path))

(defun initialize-server ()
  "Initialize the playbook server.
   Loads patterns from configured paths, cleans orphans, then loads edges and rebuilds graph."
  ;; Clear any existing state
  (clear-patterns)

  ;; Load from environment or defaults
  (if *default-playbook-paths*
      (dolist (path *default-playbook-paths*)
        (when (probe-file path)
          (let ((count (load-playbook-file path)))
            (when *log-verbose*
              (format *error-output* "Loaded ~D patterns from ~A~%" count path)))))
      (load-playbooks-from-env))

  ;; Clean up orphaned data (ledger pairs and embeddings for deleted patterns)
  ;; Then load co-app ledger and embedding cache before rebuilding graph
  (let ((meta-path (detect-depot-meta-path)))
    (when meta-path
      (post-load-cleanup meta-path)
      ;; Merge co-app ledger from all depots in PLAYBOOK_PATHS
      (let ((paths-str (uiop:getenv "PLAYBOOK_PATHS")))
        (when paths-str
          (dolist (playbook-path (cl-ppcre:split ":" paths-str))
            (let* ((ace-dir (uiop:pathname-directory-pathname (uiop:ensure-pathname playbook-path)))
                   (ledger (namestring (merge-pathnames "playbook-co-applications.json" ace-dir))))
              (when (probe-file ledger)
                (merge-co-app-ledger ledger)
                (when *log-verbose*
                  (format *error-output* "Merged co-app: ~D pairs from ~A~%"
                          (hash-table-count *co-app-ledger*) ledger)))))))
      ;; Merge relevance feedback sidecar from all depots in PLAYBOOK_PATHS
      (let ((paths-str (uiop:getenv "PLAYBOOK_PATHS")))
        (when paths-str
          (dolist (playbook-path (cl-ppcre:split ":" paths-str))
            (let* ((ace-dir (uiop:pathname-directory-pathname (uiop:ensure-pathname playbook-path)))
                   (relevance (namestring (merge-pathnames "playbook-relevance-feedback.json" ace-dir))))
              (when (probe-file relevance)
                (merge-relevance-feedback-file relevance)
                (when *log-verbose*
                  (format *error-output* "Merged relevance feedback: ~D patterns from ~A~%"
                          (hash-table-count *relevance-store*) relevance)))))))
      ;; Load embedding cache so patterns have embeddings without hitting ollama
      (let ((cache-path (namestring (merge-pathnames ".playbook-cache/embeddings.json" meta-path))))
        (when (probe-file cache-path)
          (load-embedding-cache cache-path)
          (when *log-verbose*
            (format *error-output* "Loaded embedding cache: ~D entries from ~A~%"
                    (embedding-cache-size) cache-path))))))

  ;; Initialize edges path and rebuild graph (reads edges from file directly)
  (initialize-edges-path)
  (rebuild-graph :manual-edges-path *manual-edges-path*)

  ;; Load temporal data for recency-based pattern ranking
  (initialize-temporal-path)
  (when (and *manual-edges-path* (probe-file *manual-edges-path*))
    ;; Load file edges into *edge-store* so new links append correctly
    (let ((edges (load-edges-file *manual-edges-path*)))
      (dolist (e edges) (add-edge e))
      (when *log-verbose*
        (format *error-output* "Loaded ~D edges from ~A~%" (length edges) *manual-edges-path*))))

  (when *log-verbose*
    (format *error-output* "Playbook MCP server initialized with ~D patterns, ~D graph edges~%"
            (pattern-count) (graph-edge-count))))

;;; =========================================================================
;;; TRANSPORT SELECTION
;;; =========================================================================
;;; Mirrors task-mcp/server.lisp pattern. Playbook MCP supports:
;;; - stdio (default): one subprocess per Claude Code session
;;; - HTTP: single daemon shared across all sessions (port 8091)

(defun get-playbook-http-port ()
  "Get HTTP port from PLAYBOOK_MCP_PORT env var, default 8091."
  (let ((port-str (uiop:getenv "PLAYBOOK_MCP_PORT")))
    (if (and port-str (> (length port-str) 0))
        (or (parse-integer port-str :junk-allowed t) 8091)
        8091)))

(defun select-transport ()
  "Select transport based on PLAYBOOK_MCP_TRANSPORT env var.
   Values: 'stdio' for STDIO transport, anything else for HTTP (default).
   Sets *http-mode* for session context management."
  (let ((transport-type (uiop:getenv "PLAYBOOK_MCP_TRANSPORT")))
    (if (and transport-type (string-equal transport-type "stdio"))
        (progn
          (setf *http-mode* nil)
          (when *log-verbose*
            (format *error-output* "  Transport: stdio~%")
            (format *error-output* "  Mode: single-session~%"))
          (mcp-framework:make-stdio-transport))
        (let ((port (get-playbook-http-port)))
          (setf *http-mode* t)
          (when *log-verbose*
            (format *error-output* "  Transport: HTTP on port ~D~%" port)
            (format *error-output* "  Mode: multi-session (contexts isolated per Mcp-Session-Id)~%"))
          (register-playbook-http-endpoints)
          (mcp-http:make-http-transport :port port :host "127.0.0.1")))))

;;; =========================================================================
;;; THREAD-SAFE SESSION ISOLATION (HTTP mode)
;;; =========================================================================
;;; Hunchentoot processes requests in a thread pool. Session state globals
;;; (*claimed-session*, *claimed-session-cwd*) are shared across all threads.
;;; This :around method creates per-thread dynamic bindings so concurrent
;;; requests from different sessions don't clobber each other's context.

(defmethod hunchentoot:acceptor-dispatch-request :around
    ((acceptor mcp-http::mcp-acceptor) request)
  "Wrap each HTTP request with thread-local playbook session state bindings."
  (declare (ignore request))
  (let ((*claimed-session* *claimed-session*)
        (*claimed-session-cwd* *claimed-session-cwd*)
        (*http-mode* *http-mode*))
    (ensure-playbook-session-context)
    (call-next-method)))

;;; =========================================================================
;;; HTTP ENDPOINTS (non-MCP, for hook integration)
;;; =========================================================================

(defun register-playbook-http-endpoints ()
  "Register HTTP endpoints for hook integration.
   These endpoints bypass MCP session validation since hooks run before
   Claude Code has established an MCP session."
  ;; Health check for daemon liveness detection
  (hunchentoot:define-easy-handler (playbook-health-handler :uri "/health") ()
    (setf (hunchentoot:content-type*) "application/json")
    (format nil "{\"status\":\"ok\",\"server\":\"playbook-mcp\",\"patterns\":~D,\"edges\":~D,\"sessions\":~D}"
            (pattern-count) (graph-edge-count)
            (hash-table-count *session-states*)))

  ;; Claude session ID registration (called by SessionStart hook)
  ;; Maps PID → Claude session ID so write-feedback-state-file writes
  ;; to the correct path (.claude/sessions/{CLAUDE-SID}/...)
  (hunchentoot:define-easy-handler (register-claude-session-handler
                                    :uri "/register-claude-session")
      (claude-session-id pid)
    "Register Claude Code's session ID for PID-based lookup.
     Called by SessionStart hook before MCP session exists.
     Query params: claude-session-id (string), pid (integer)."
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (let ((pid-int (when pid (parse-integer pid :junk-allowed nil))))
          (if (and claude-session-id (plusp (length claude-session-id))
                   pid-int (plusp pid-int))
              (let ((result (register-claude-session-for-pid pid-int claude-session-id)))
                (format nil "{\"success\":true,\"message\":~S}" result))
              (progn
                (setf (hunchentoot:return-code*) 400)
                "{\"success\":false,\"error\":\"Missing or invalid claude-session-id or pid\"}")))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"success\":false,\"error\":~S}" (princ-to-string e))))))

;;; Server creation

(defun start-playbook-server (&key background)
  "Create and start the playbook MCP server.
   Selects transport from PLAYBOOK_MCP_TRANSPORT env var.
   If BACKGROUND is true, runs in a separate thread."
  (initialize-server)
  ;; Install signal handlers for graceful shutdown (cleans up mcp.lock)
  (install-signal-handlers)
  (let* ((transport (select-transport))
         (server (mcp-framework:make-server
                  :name "playbook-mcp"
                  :version "1.0.0"
                  :transport transport)))
    (mcp-framework:run-server server :background background)))

;;; Main entry point (for buildLisp.program)

(defun main ()
  "Main entry point for standalone server."
  (handler-case
      (progn
        (when *log-verbose*
          (format *error-output* "Starting Playbook MCP Server...~%"))
        (start-playbook-server :background nil))
    (error (e)
      (format *error-output* "Fatal error: ~A~%" e)
      (uiop:quit 1))))
