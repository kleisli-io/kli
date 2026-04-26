;;; Playbook MCP Library - Orphan Cleanup
;;; Functions for cleaning up orphaned data after pattern deletion.
;;;
;;; Patterns are deleted by removing them from markdown files.
;;; Cleanup happens at server startup after loading patterns.

(in-package #:playbook)

(defvar *log-verbose*)  ; forward declaration — defined in package.lisp

(defun get-valid-pattern-ids ()
  "Get list of all currently loaded pattern IDs."
  (let (ids)
    (maphash (lambda (id pattern)
               (declare (ignore pattern))
               (push id ids))
             *pattern-store*)
    ids))

(defun verify-playbook-consistency (meta-path)
  "Audit the playbook sidecars under META-PATH against the in-memory
   pattern store.  Read-only — never mutates files.

   Reports three classes of inconsistency, each one a key referencing
   a pattern-id that is no longer present in *PATTERN-STORE*:

     :evidence-orphans — entries in playbook-evidence.json whose key is
       not a loaded pattern.  These accumulate when a pattern is
       deleted from playbook.md but its feedback history is left behind.
     :feedback-orphans — entries in playbook-relevance-feedback.json
       whose key is not a loaded pattern.  Same accumulation pattern.
     :co-app-orphans — entries in playbook-co-applications.json whose
       \"id-a:id-b\" key references at least one missing pattern.

   Returns a plist with both the count and the offending IDs / keys
   for each class, plus :TOTAL — the sum of all three counts.  Callers
   that want repair (rather than inspection) should follow with
   CLEANUP-ORPHANED-DATA, which prunes all three sidecars."
  (let* ((valid-ids (get-valid-pattern-ids))
         (evidence-path
           (merge-pathnames "playbook-evidence.json" meta-path))
         (feedback-path
           (merge-pathnames "playbook-relevance-feedback.json" meta-path))
         (co-app-path
           (merge-pathnames "playbook-co-applications.json" meta-path))
         (evidence-orphans
           (when (probe-file evidence-path)
             (evidence-ledger-orphans (namestring evidence-path) valid-ids)))
         (feedback-orphans
           (when (probe-file feedback-path)
             (relevance-feedback-orphans (namestring feedback-path) valid-ids)))
         (co-app-orphans
           (when (probe-file co-app-path)
             (co-app-ledger-orphans (namestring co-app-path) valid-ids))))
    (list :evidence-orphans (length evidence-orphans)
          :evidence-orphan-ids evidence-orphans
          :feedback-orphans (length feedback-orphans)
          :feedback-orphan-ids feedback-orphans
          :co-app-orphans (length co-app-orphans)
          :co-app-orphan-keys co-app-orphans
          :total (+ (length evidence-orphans)
                    (length feedback-orphans)
                    (length co-app-orphans)))))

(defun cleanup-orphaned-data (meta-path valid-pattern-ids)
  "Remove orphaned data from every playbook sidecar that references
   patterns by ID.

   META-PATH: Path to .kli/ metadata directory
   VALID-PATTERN-IDS: List of currently valid pattern IDs

   Repairs in place (atomic write-then-rename) for each of:
     - playbook-co-applications.json (co-app pairs)
     - playbook-evidence.json (per-pattern feedback ledger)
     - playbook-relevance-feedback.json (query-scoped not-relevant)
     - .playbook-cache/embeddings.json

   Returns a plist of pruned counts per ledger."
  (let ((ledger-path (merge-pathnames "playbook-co-applications.json" meta-path))
        (evidence-path (merge-pathnames "playbook-evidence.json" meta-path))
        (feedback-path (merge-pathnames "playbook-relevance-feedback.json" meta-path))
        (cache-path (merge-pathnames ".playbook-cache/embeddings.json" meta-path))
        (ledger-removed 0)
        (evidence-removed 0)
        (feedback-removed 0)
        (cache-removed 0))

    (when (probe-file ledger-path)
      (setf ledger-removed
            (prune-co-app-ledger (namestring ledger-path) valid-pattern-ids)))

    (when (probe-file evidence-path)
      (setf evidence-removed
            (prune-evidence-ledger (namestring evidence-path) valid-pattern-ids)))

    (when (probe-file feedback-path)
      (setf feedback-removed
            (prune-relevance-feedback (namestring feedback-path) valid-pattern-ids)))

    (when (probe-file cache-path)
      (setf cache-removed
            (prune-embedding-cache-file (namestring cache-path) valid-pattern-ids)))

    (list :ledger-pruned ledger-removed
          :evidence-pruned evidence-removed
          :feedback-pruned feedback-removed
          :embeddings-pruned cache-removed)))

(defun log-playbook-inconsistencies (meta-path)
  "Run VERIFY-PLAYBOOK-CONSISTENCY against META-PATH and emit a
   single structured warning to *ERROR-OUTPUT* when any orphans were
   found.  Returns the verify result.  Called at daemon startup so an
   operator reading the log sees what was about to be pruned."
  (let ((report (verify-playbook-consistency meta-path)))
    (when (plusp (getf report :total))
      (format *error-output*
              "~&;; playbook-consistency: total=~D ~
               evidence=~D feedback=~D co-app=~D~%"
              (getf report :total)
              (getf report :evidence-orphans)
              (getf report :feedback-orphans)
              (getf report :co-app-orphans)))
    report))

(defun post-load-cleanup (meta-path)
  "Clean up orphaned data after loading patterns.
   Call this after load-playbook-file(s) and before rebuild-graph.

   META-PATH: Path to .kli/ metadata directory

   Audits the sidecars first via LOG-PLAYBOOK-INCONSISTENCIES (which
   warns once on non-zero totals) and then prunes via
   CLEANUP-ORPHANED-DATA.  Logs cleanup counts when *LOG-VERBOSE* is
   set and any prune was non-zero."
  (let ((valid-ids (get-valid-pattern-ids)))
    (when valid-ids
      (log-playbook-inconsistencies meta-path)
      (let ((result (cleanup-orphaned-data meta-path valid-ids)))
        (when (and *log-verbose*
                   (or (plusp (getf result :ledger-pruned))
                       (plusp (getf result :evidence-pruned))
                       (plusp (getf result :feedback-pruned))
                       (plusp (getf result :embeddings-pruned))))
          (format *error-output*
                  "Cleaned up orphaned data: ~D co-app pairs, ~D evidence, ~
                   ~D relevance, ~D embeddings~%"
                  (getf result :ledger-pruned)
                  (getf result :evidence-pruned)
                  (getf result :feedback-pruned)
                  (getf result :embeddings-pruned)))
        result))))
