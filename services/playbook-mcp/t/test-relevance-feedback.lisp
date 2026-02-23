;;;; playbook-mcp tests - Relevance Feedback
;;;; Tests for query-scoped :not-relevant feedback: storage, dispatch, penalty.

(in-package :playbook-mcp.tests)
(in-suite :relevance-feedback)

;;; Struct creation

(test relevance-entry-creation
  "relevance-entry struct has correct slots and defaults."
  (let ((entry (make-relevance-entry :pattern-id "nix-001"
                                     :domains '("nix" "lisp")
                                     :timestamp 12345
                                     :signal :not-relevant)))
    (is (string= "nix-001" (relevance-entry-pattern-id entry)))
    (is (equal '("nix" "lisp") (relevance-entry-domains entry)))
    (is (= 12345 (relevance-entry-timestamp entry)))
    (is (eq :not-relevant (relevance-entry-signal entry)))))

;;; Record + save + load roundtrip

(test relevance-record-and-roundtrip
  "Record entries, save to file, clear, load back — data preserved."
  (let ((playbook-mcp::*relevance-store* (make-hash-table :test 'equal))
        (playbook-mcp::*relevance-store-lock* (bt:make-lock "test-relevance"))
        (path (format nil "/tmp/test-relevance-rt-~a.json" (random 1000000))))
    (record-relevance-feedback "nix-001" '("nix" "lisp"))
    (record-relevance-feedback "nix-001" '("shell"))
    (record-relevance-feedback "lisp-042" '("nix"))
    ;; Save
    (save-relevance-feedback-file path)
    ;; Clear
    (bt:with-lock-held (playbook-mcp::*relevance-store-lock*)
      (clrhash playbook-mcp::*relevance-store*))
    (is (= 0 (hash-table-count playbook-mcp::*relevance-store*)))
    ;; Load
    (unwind-protect
         (let ((count (load-relevance-feedback-file path)))
           (is (= 2 count))
           (is (= 2 (length (gethash "nix-001" playbook-mcp::*relevance-store*))))
           (is (= 1 (length (gethash "lisp-042" playbook-mcp::*relevance-store*))))
           ;; Verify entry contents survived roundtrip
           (let ((entry (first (gethash "lisp-042" playbook-mcp::*relevance-store*))))
             (is (string= "lisp-042" (relevance-entry-pattern-id entry)))
             (is (equal '("nix") (relevance-entry-domains entry)))
             (is (eq :not-relevant (relevance-entry-signal entry)))))
      (ignore-errors (delete-file path)))))

;;; Merge dedup

(test relevance-merge-dedup
  "Merge appends new entries but deduplicates by timestamp."
  (let ((playbook-mcp::*relevance-store* (make-hash-table :test 'equal))
        (playbook-mcp::*relevance-store-lock* (bt:make-lock "test-merge"))
        (path (format nil "/tmp/test-relevance-merge-~a.json" (random 1000000))))
    ;; Existing: 1 entry for nix-001
    (record-relevance-feedback "nix-001" '("lisp"))
    (let ((existing-ts (relevance-entry-timestamp
                        (first (gethash "nix-001" playbook-mcp::*relevance-store*)))))
      ;; Write merge file with same-ts entry (should dedup) + different-ts entry (should merge)
      (let ((ht (make-hash-table :test 'equal))
            (dup-entry (make-hash-table :test 'equal))
            (new-entry (make-hash-table :test 'equal)))
        (setf (gethash "domains" dup-entry) #("lisp"))
        (setf (gethash "timestamp" dup-entry) existing-ts)
        (setf (gethash "signal" dup-entry) "not-relevant")
        (setf (gethash "domains" new-entry) #("shell"))
        (setf (gethash "timestamp" new-entry) 9999999999)
        (setf (gethash "signal" new-entry) "not-relevant")
        (setf (gethash "nix-001" ht) (vector dup-entry new-entry))
        (with-open-file (s path :direction :output :if-exists :supersede)
          (yason:encode ht s))))
    (unwind-protect
         (progn
           (merge-relevance-feedback-file path)
           ;; Should have 2 entries: original + new (not 3)
           (is (= 2 (length (gethash "nix-001" playbook-mcp::*relevance-store*)))))
      (ignore-errors (delete-file path)))))

;;; Prune

(test relevance-prune-removes-invalid
  "Prune removes entries for patterns not in valid set."
  (let ((playbook-mcp::*relevance-store* (make-hash-table :test 'equal))
        (playbook-mcp::*relevance-store-lock* (bt:make-lock "test-prune"))
        (path (format nil "/tmp/test-relevance-prune-~a.json" (random 1000000))))
    (record-relevance-feedback "valid-001" '("lisp"))
    (record-relevance-feedback "orphan-001" '("nix"))
    (save-relevance-feedback-file path)
    (unwind-protect
         (let ((pruned (prune-relevance-feedback path '("valid-001"))))
           (is (= 1 pruned))
           (is (= 1 (hash-table-count playbook-mcp::*relevance-store*)))
           (is-true (gethash "valid-001" playbook-mcp::*relevance-store*))
           (is-false (gethash "orphan-001" playbook-mcp::*relevance-store*)))
      (ignore-errors (delete-file path)))))

;;; Penalty: domain overlap

(test penalty-domain-overlap
  "Penalty is non-zero for overlapping domains, zero for non-overlapping."
  (let ((playbook-mcp::*relevance-store* (make-hash-table :test 'equal))
        (playbook-mcp::*relevance-store-lock* (bt:make-lock "test-penalty")))
    (record-relevance-feedback "nix-001" '("nix" "lisp"))
    (record-relevance-feedback "nix-001" '("nix"))
    ;; Overlapping: "nix" matches both signals
    (is (> (compute-relevance-penalty "nix-001" '("nix")) 0.0))
    ;; Partially overlapping: "shell" matches neither
    (is (= 0.0 (compute-relevance-penalty "nix-001" '("shell" "python"))))
    ;; No query domains
    (is (= 0.0 (compute-relevance-penalty "nix-001" nil)))
    ;; No entries for this pattern
    (is (= 0.0 (compute-relevance-penalty "nonexistent" '("nix"))))))

;;; Penalty: temporal decay

(test penalty-temporal-decay
  "Old signals contribute less penalty than fresh ones."
  (let ((playbook-mcp::*relevance-store* (make-hash-table :test 'equal))
        (playbook-mcp::*relevance-store-lock* (bt:make-lock "test-decay"))
        (now (get-universal-time)))
    ;; Fresh signal
    (bt:with-lock-held (playbook-mcp::*relevance-store-lock*)
      (push (make-relevance-entry :pattern-id "p1" :domains '("lisp")
                                  :timestamp now :signal :not-relevant)
            (gethash "p1" playbook-mcp::*relevance-store*)))
    (let ((fresh-penalty (compute-relevance-penalty "p1" '("lisp"))))
      ;; Replace with 14-day-old signal
      (bt:with-lock-held (playbook-mcp::*relevance-store-lock*)
        (setf (gethash "p1" playbook-mcp::*relevance-store*)
              (list (make-relevance-entry :pattern-id "p1" :domains '("lisp")
                                          :timestamp (- now (* 14 86400))
                                          :signal :not-relevant))))
      (let ((old-penalty (compute-relevance-penalty "p1" '("lisp"))))
        ;; Fresh > old (temporal decay)
        (is (> fresh-penalty old-penalty))
        ;; Old should be roughly 1/4 of fresh (2 half-lives)
        (is (< old-penalty (* 0.3 fresh-penalty)))))))

;;; Dispatch routing

(test dispatch-normalize-feedback-type
  "normalize-feedback-type handles all input variants correctly."
  (is (eq :helpful (playbook-mcp::normalize-feedback-type :helpful)))
  (is (eq :harmful (playbook-mcp::normalize-feedback-type :harmful)))
  (is (eq :not-relevant (playbook-mcp::normalize-feedback-type :not-relevant)))
  (is (eq :not-relevant (playbook-mcp::normalize-feedback-type :irrelevant)))
  (is (eq :helpful (playbook-mcp::normalize-feedback-type "helpful")))
  (is (eq :harmful (playbook-mcp::normalize-feedback-type "harmful")))
  (is (eq :not-relevant (playbook-mcp::normalize-feedback-type "not-relevant")))
  (is (eq :not-relevant (playbook-mcp::normalize-feedback-type "irrelevant")))
  (is (null (playbook-mcp::normalize-feedback-type :bogus))))

;;; Scoring integration

(test scoring-penalty-reduces-sim
  "Relevance penalty reduces adjusted similarity score."
  (let ((playbook-mcp::*relevance-store* (make-hash-table :test 'equal))
        (playbook-mcp::*relevance-store-lock* (bt:make-lock "test-scoring")))
    ;; 2 signals for pattern in "nix" domain
    (record-relevance-feedback "nix-001" '("nix"))
    (record-relevance-feedback "nix-001" '("nix"))
    (let* ((penalty (compute-relevance-penalty "nix-001" '("nix")))
           (boosted-sim 0.65)
           (adjusted (* boosted-sim (- 1.0 penalty))))
      ;; Penalty should be ~0.30 (2 signals × 0.15)
      (is (> penalty 0.2))
      ;; Adjusted should be less than original
      (is (< adjusted boosted-sim))
      ;; Adjusted should still be positive
      (is (> adjusted 0.0)))))
