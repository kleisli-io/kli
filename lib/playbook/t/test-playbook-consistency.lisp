;;;; playbook tests - cross-sidecar consistency
;;;;
;;;; Patterns are deleted by removing them from the markdown source,
;;;; so the four playbook sidecars under .kli/ — playbook-evidence.json,
;;;; playbook-relevance-feedback.json, playbook-co-applications.json,
;;;; and the embeddings cache — drift out of sync with the in-memory
;;;; pattern store.  VERIFY-PLAYBOOK-CONSISTENCY audits the three
;;;; pattern-keyed sidecars without mutating them; CLEANUP-ORPHANED-DATA
;;;; prunes them.  The tests below construct a known-bad fixture under
;;;; an ephemeral META-PATH and assert each defect surfaces in the
;;;; verify report.

(in-package :playbook.tests)
(in-suite :playbook-consistency)

;;; --- Fixture helpers ------------------------------------------------------

(defun %tmp-meta-root ()
  "Allocate an ephemeral .kli-shaped directory under /tmp.  Caller is
   responsible for deletion when the fixture has served its purpose."
  (let ((root (format nil "/tmp/test-playbook-consistency-~A/"
                      (random 1000000))))
    (ensure-directories-exist root)
    root))

(defun %write-evidence-fixture (meta-path entries)
  "ENTRIES is an alist of (PATTERN-ID . LIST-OF-ENTRY-PLISTS).  Writes
   the playbook-evidence.json shape (pattern-id → array of {type, text,
   ts}) used by APPEND-PATTERN-EVIDENCE."
  (let ((path (namestring (merge-pathnames "playbook-evidence.json" meta-path)))
        (ht (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((vec (map 'vector
                      (lambda (e)
                        (let ((eh (make-hash-table :test 'equal)))
                          (setf (gethash "type" eh) (getf e :type)
                                (gethash "text" eh) (getf e :text)
                                (gethash "ts"   eh) (getf e :ts))
                          eh))
                      (cdr entry))))
        (setf (gethash (car entry) ht) vec)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (yason:encode ht s))
    path))

(defun %write-feedback-fixture (meta-path entries)
  "ENTRIES is an alist of (PATTERN-ID . LIST-OF-RELEVANCE-PLISTS).
   Writes the playbook-relevance-feedback.json shape used by
   SAVE-RELEVANCE-FEEDBACK-FILE."
  (let ((path (namestring
               (merge-pathnames "playbook-relevance-feedback.json" meta-path)))
        (ht (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((vec (map 'vector
                      (lambda (e)
                        (let ((eh (make-hash-table :test 'equal)))
                          (setf (gethash "domains" eh)   (getf e :domains)
                                (gethash "timestamp" eh) (getf e :timestamp)
                                (gethash "signal" eh)    (getf e :signal))
                          eh))
                      (cdr entry))))
        (setf (gethash (car entry) ht) vec)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (yason:encode ht s))
    path))

(defun %write-co-app-fixture (meta-path counts)
  "COUNTS is an alist of (\"id-a:id-b\" . COUNT).  Writes the
   playbook-co-applications.json shape used by SAVE-CO-APP-LEDGER."
  (let ((path (namestring
               (merge-pathnames "playbook-co-applications.json" meta-path)))
        (ht (make-hash-table :test 'equal)))
    (dolist (entry counts)
      (setf (gethash (car entry) ht) (cdr entry)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (yason:encode ht s))
    path))

(defun %seed-valid-patterns (ids)
  "Replace *PATTERN-STORE* with a hash-table containing IDS as keys
   and minimal pattern structs as values.  Returns the hash-table for
   LET-binding."
  (let ((store (make-hash-table :test 'equal)))
    (dolist (id ids)
      (setf (gethash id store)
            (playbook::make-pattern :id id :domain "test")))
    store))

(defun %clean-up (path)
  "Best-effort recursive delete of PATH (a /tmp directory or file)."
  (ignore-errors
   (uiop:delete-directory-tree (uiop:ensure-directory-pathname path)
                               :validate t)))

;;; --- Inspection: verify-playbook-consistency -----------------------------

(test verify-detects-orphan-evidence-entries
  "Evidence entries keyed by patterns that no longer live in the
   in-memory store appear under :EVIDENCE-ORPHANS with their IDs in
   :EVIDENCE-ORPHAN-IDS."
  (let* ((meta (%tmp-meta-root))
         (playbook::*pattern-store* (%seed-valid-patterns '("nix-001"))))
    (unwind-protect
         (progn
           (%write-evidence-fixture
            meta
            '(("nix-001" . ((:type "helpful" :text "live" :ts 100)))
              ("nix-stale-A" . ((:type "harmful" :text "orphan" :ts 50)))
              ("nix-stale-B" . ((:type "helpful" :text "orphan" :ts 60)))))
           (let ((report (playbook:verify-playbook-consistency
                          (uiop:ensure-directory-pathname meta))))
             (is (= 2 (getf report :evidence-orphans)))
             (is (equal '("nix-stale-A" "nix-stale-B")
                        (sort (copy-list (getf report :evidence-orphan-ids))
                              #'string<)))
             (is (zerop (getf report :feedback-orphans)))
             (is (zerop (getf report :co-app-orphans)))
             (is (= 2 (getf report :total)))))
      (%clean-up meta))))

(test verify-detects-feedback-without-activation
  "Relevance-feedback entries for patterns that no longer live in the
   in-memory store surface under :FEEDBACK-ORPHANS — they were
   recorded against an activation that the pattern store no longer
   reflects."
  (let* ((meta (%tmp-meta-root))
         (playbook::*pattern-store*
           (%seed-valid-patterns '("lisp-100" "lisp-200"))))
    (unwind-protect
         (progn
           (%write-feedback-fixture
            meta
            '(("lisp-100"
               . ((:domains nil :timestamp 1 :signal "not-relevant")))
              ("lisp-deleted"
               . ((:domains nil :timestamp 2 :signal "not-relevant")))))
           (let ((report (playbook:verify-playbook-consistency
                          (uiop:ensure-directory-pathname meta))))
             (is (= 1 (getf report :feedback-orphans)))
             (is (equal '("lisp-deleted")
                        (getf report :feedback-orphan-ids)))
             (is (zerop (getf report :evidence-orphans)))
             (is (zerop (getf report :co-app-orphans)))
             (is (= 1 (getf report :total)))))
      (%clean-up meta))))

(test verify-detects-co-application-without-evidence
  "Co-application keys that reference at least one pattern absent
   from the in-memory store surface under :CO-APP-ORPHANS — the pair
   was activated together once but one half has since been deleted
   from the playbook source."
  (let* ((meta (%tmp-meta-root))
         (playbook::*pattern-store*
           (%seed-valid-patterns '("nix-001" "nix-002"))))
    (unwind-protect
         (progn
           (%write-co-app-fixture
            meta
            '(("nix-001:nix-002" . 5)              ; both live → keep
              ("nix-001:nix-deleted" . 2)          ; one half missing → orphan
              ("nix-gone-a:nix-gone-b" . 3)))      ; both missing → orphan
           (let ((report (playbook:verify-playbook-consistency
                          (uiop:ensure-directory-pathname meta))))
             (is (= 2 (getf report :co-app-orphans)))
             (let ((keys (sort (copy-list
                                (getf report :co-app-orphan-keys))
                               #'string<)))
               (is (equal '("nix-001:nix-deleted" "nix-gone-a:nix-gone-b")
                          keys)))
             (is (zerop (getf report :evidence-orphans)))
             (is (zerop (getf report :feedback-orphans)))
             (is (= 2 (getf report :total)))))
      (%clean-up meta))))

(test verify-emits-zero-on-clean-state
  "All three sidecars present and every key resolves to a live
   pattern → :TOTAL = 0 and every per-class count = 0.  This is the
   steady-state contract that LOG-PLAYBOOK-INCONSISTENCIES uses to
   decide whether to emit a warning at startup."
  (let* ((meta (%tmp-meta-root))
         (playbook::*pattern-store*
           (%seed-valid-patterns '("a-1" "a-2" "a-3"))))
    (unwind-protect
         (progn
           (%write-evidence-fixture
            meta
            '(("a-1" . ((:type "helpful" :text "ok" :ts 1)))
              ("a-2" . ((:type "harmful" :text "ok" :ts 2)))))
           (%write-feedback-fixture
            meta
            '(("a-3" . ((:domains nil :timestamp 3 :signal "not-relevant")))))
           (%write-co-app-fixture
            meta
            '(("a-1:a-2" . 4)
              ("a-2:a-3" . 1)))
           (let ((report (playbook:verify-playbook-consistency
                          (uiop:ensure-directory-pathname meta))))
             (is (zerop (getf report :evidence-orphans)))
             (is (zerop (getf report :feedback-orphans)))
             (is (zerop (getf report :co-app-orphans)))
             (is (zerop (getf report :total)))))
      (%clean-up meta))))

(test verify-handles-missing-sidecars-as-clean
  "The sidecars are written lazily — a fresh depot has none of them
   on disk yet.  Verify must treat absent files as zero orphans, not
   as errors, so a clean checkout passes the startup self-check."
  (let* ((meta (%tmp-meta-root))
         (playbook::*pattern-store*
           (%seed-valid-patterns '("only-id"))))
    (unwind-protect
         (let ((report (playbook:verify-playbook-consistency
                        (uiop:ensure-directory-pathname meta))))
           (is (zerop (getf report :evidence-orphans)))
           (is (zerop (getf report :feedback-orphans)))
           (is (zerop (getf report :co-app-orphans)))
           (is (zerop (getf report :total))))
      (%clean-up meta))))

;;; --- Repair: cleanup-orphaned-data prunes all four sidecars --------------

(test cleanup-prunes-evidence-and-feedback-and-co-app-atomically
  "CLEANUP-ORPHANED-DATA repairs in place: the three pattern-keyed
   sidecars all lose their orphan keys, and a follow-up
   VERIFY-PLAYBOOK-CONSISTENCY reports zero.  The pruned counts in
   the returned plist match the pre-cleanup orphan counts so an
   operator reading the log can reconcile what was removed."
  (let* ((meta (%tmp-meta-root))
         (playbook::*pattern-store* (%seed-valid-patterns '("live-1"))))
    (unwind-protect
         (progn
           (%write-evidence-fixture
            meta
            '(("live-1" . ((:type "helpful" :text "k" :ts 1)))
              ("dead-evidence" . ((:type "helpful" :text "x" :ts 2)))))
           (%write-feedback-fixture
            meta
            '(("dead-feedback"
               . ((:domains nil :timestamp 3 :signal "not-relevant")))))
           (%write-co-app-fixture
            meta '(("live-1:dead-co" . 2)))
           (let ((result (playbook:cleanup-orphaned-data
                          (uiop:ensure-directory-pathname meta)
                          (playbook::get-valid-pattern-ids))))
             (is (= 1 (getf result :evidence-pruned)))
             (is (= 1 (getf result :feedback-pruned)))
             (is (= 1 (getf result :ledger-pruned))))
           (let ((report (playbook:verify-playbook-consistency
                          (uiop:ensure-directory-pathname meta))))
             (is (zerop (getf report :total)))))
      (%clean-up meta))))
