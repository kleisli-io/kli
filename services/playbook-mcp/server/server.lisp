;;; Playbook MCP Server - Server Entry Point
;;; Main function and server lifecycle

(in-package #:playbook-mcp)

;;; Configuration

(defparameter *default-playbook-paths* nil
  "List of playbook.md paths to load on startup.
   Set via environment variable PLAYBOOK_PATHS (colon-separated).")


;;; Initialization

(defun load-playbooks-from-env ()
  "Load playbook paths from PLAYBOOK_PATHS environment variable."
  (let ((paths-str (uiop:getenv "PLAYBOOK_PATHS")))
    (when (and paths-str (> (length paths-str) 0))
      (let ((paths (cl-ppcre:split ":" paths-str)))
        (dolist (path paths)
          (when (probe-file path)
            (let ((count (load-playbook-file path)))
              (format *error-output* "Loaded ~D patterns from ~A~%" count path))))))))

(defun initialize-edges-path ()
  "Set *manual-edges-path* to git-tracked manual edges file.
   Default is ace/playbook-manual-edges.json (git-tracked).
   Override via PLAYBOOK_EDGES_PATH environment variable."
  (let ((path (or (uiop:getenv "PLAYBOOK_EDGES_PATH")
                  "ace/playbook-manual-edges.json")))
    (setf *manual-edges-path* path)))

(defun initialize-temporal-path ()
  "Set *temporal-data-path* and load existing temporal data.
   Default is ace/playbook-temporal.json.
   Override via PLAYBOOK_TEMPORAL_PATH environment variable."
  (let ((path (or (uiop:getenv "PLAYBOOK_TEMPORAL_PATH")
                  "ace/playbook-temporal.json")))
    (setf *temporal-data-path* path)
    (when (probe-file path)
      (let ((count (load-temporal-data path)))
        (when count
          (format *error-output* "Loaded temporal data for ~D patterns from ~A~%"
                  count path))))))

(defun detect-depot-ace-path ()
  "Detect the depot's ace/ directory path from depot root.
   Uses DEPOT_ROOT env, .depot marker, or git root fallback.
   Returns NIL if no depot found."
  (let ((depot-root (mcp-find-depot-root)))
    (when depot-root
      (namestring (merge-pathnames "ace/" depot-root)))))

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
            (format *error-output* "Loaded ~D patterns from ~A~%" count path))))
      (load-playbooks-from-env))

  ;; Clean up orphaned data (ledger pairs and embeddings for deleted patterns)
  ;; Then load co-app ledger and embedding cache before rebuilding graph
  (let ((ace-path (detect-depot-ace-path)))
    (when ace-path
      (post-load-cleanup ace-path)
      ;; Load co-app ledger so rebuild-graph produces co-app edges
      (let ((ledger-path (namestring (merge-pathnames "playbook-co-applications.json" ace-path))))
        (when (probe-file ledger-path)
          (load-co-app-ledger ledger-path)
          (format *error-output* "Loaded co-app ledger: ~D pairs from ~A~%"
                  (hash-table-count *co-app-ledger*) ledger-path)))
      ;; Load embedding cache so patterns have embeddings without hitting ollama
      (let ((cache-path (namestring (merge-pathnames ".playbook-cache/embeddings.json" ace-path))))
        (when (probe-file cache-path)
          (load-embedding-cache cache-path)
          (format *error-output* "Loaded embedding cache: ~D entries from ~A~%"
                  (embedding-cache-size) cache-path)))))

  ;; Initialize edges path and rebuild graph (reads edges from file directly)
  (initialize-edges-path)
  (rebuild-graph :manual-edges-path *manual-edges-path*)

  ;; Load temporal data for recency-based pattern ranking
  (initialize-temporal-path)
  (when (and *manual-edges-path* (probe-file *manual-edges-path*))
    ;; Load file edges into *edge-store* so new links append correctly
    (let ((edges (load-edges-file *manual-edges-path*)))
      (dolist (e edges) (add-edge e))
      (format *error-output* "Loaded ~D edges from ~A~%" (length edges) *manual-edges-path*)))

  ;; Initialize task library for cross-process event emission
  ;; Required for playbook_activate to emit :pattern.activate events to task event streams
  (task:detect-all-task-roots)

  (format *error-output* "Playbook MCP server initialized with ~D patterns, ~D graph edges~%"
          (pattern-count) (graph-edge-count)))

;;; Server creation

(defun start-playbook-server (&key background)
  "Create and start the playbook MCP server.
   If BACKGROUND is true, runs in a separate thread."
  (initialize-server)
  ;; Install signal handlers for graceful shutdown (cleans up mcp.lock)
  (install-signal-handlers)
  (let ((server (mcp-framework:make-server
                 :name "playbook-mcp"
                 :version "1.0.0")))
    (mcp-framework:run-server server :background background)))

;;; Main entry point (for buildLisp.program)

(defun main ()
  "Main entry point for standalone server."
  (handler-case
      (progn
        (format *error-output* "Starting Playbook MCP Server...~%")
        (start-playbook-server :background nil))
    (error (e)
      (format *error-output* "Fatal error: ~A~%" e)
      (uiop:quit 1))))
