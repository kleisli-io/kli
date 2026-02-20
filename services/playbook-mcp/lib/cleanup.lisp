;;; Playbook MCP Library - Orphan Cleanup
;;; Functions for cleaning up orphaned data after pattern deletion.
;;;
;;; Patterns are deleted by removing them from markdown files.
;;; Cleanup happens at server startup after loading patterns.

(in-package #:playbook-mcp)

(defvar *log-verbose*)  ; forward declaration — defined in package.lisp

(defun get-valid-pattern-ids ()
  "Get list of all currently loaded pattern IDs."
  (let (ids)
    (maphash (lambda (id pattern)
               (declare (ignore pattern))
               (push id ids))
             *pattern-store*)
    ids))

(defun cleanup-orphaned-data (depot-ace-path valid-pattern-ids)
  "Remove orphaned data from co-app ledger and embedding cache.

   DEPOT-ACE-PATH: Path to depot's ace/ directory (e.g., /path/to/depot/ace/)
   VALID-PATTERN-IDS: List of currently valid pattern IDs

   Returns alist with cleanup counts."
  (let ((ledger-path (merge-pathnames "playbook-co-applications.json" depot-ace-path))
        (cache-path (merge-pathnames ".playbook-cache/embeddings.json" depot-ace-path))
        (ledger-removed 0)
        (cache-removed 0))

    ;; Clean co-app ledger
    (when (probe-file ledger-path)
      (setf ledger-removed (prune-co-app-ledger (namestring ledger-path) valid-pattern-ids)))

    ;; Clean embedding cache
    (when (probe-file cache-path)
      (setf cache-removed (prune-embedding-cache-file (namestring cache-path) valid-pattern-ids)))

    (list :ledger-pruned ledger-removed
          :embeddings-pruned cache-removed)))

(defun post-load-cleanup (depot-ace-path)
  "Clean up orphaned data after loading patterns.
   Call this after load-playbook-file(s) and before rebuild-graph.

   DEPOT-ACE-PATH: Path to depot's ace/ directory

   Logs cleanup counts if any orphans were removed."
  (let ((valid-ids (get-valid-pattern-ids)))
    (when valid-ids
      (let ((result (cleanup-orphaned-data depot-ace-path valid-ids)))
        (when (and *log-verbose*
                   (or (plusp (getf result :ledger-pruned))
                       (plusp (getf result :embeddings-pruned))))
          (format *error-output* "Cleaned up orphaned data: ~D ledger pairs, ~D embeddings~%"
                  (getf result :ledger-pruned)
                  (getf result :embeddings-pruned)))
        result))))
