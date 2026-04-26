(in-package #:task-tests)

(in-suite :task-tests)

;;; --- Tolerant parser ---

(test json-string-to-event-passes-through-existing-id
  "Lines with a top-level id parse straight through; synth-p is NIL."
  (let ((line "{\"id\":\"e-1\",\"timestamp\":1000,\"session\":\"s1\",\"clock\":{},\"type\":\"task.create\",\"data\":{}}"))
    (multiple-value-bind (ev synth-p) (json-string-to-event line)
      (is (string= "e-1" (event-id ev)))
      (is (null synth-p)))))

(test json-string-to-event-synthesizes-missing-id
  "Lines lacking id but carrying timestamp+type get a synthesized id."
  (let ((line "{\"timestamp\":1234,\"type\":\"observation\",\"session\":\"s\",\"data\":{\"text\":\"x\"}}"))
    (multiple-value-bind (ev synth-p) (json-string-to-event line)
      (is-true synth-p)
      (is (task::synthesized-id-p (event-id ev))))))

(test json-string-to-event-synthesis-is-deterministic
  "Same input produces same synthesized id (idempotent re-parse)."
  (let ((line "{\"timestamp\":42,\"type\":\"x\",\"session\":\"s\",\"data\":{\"k\":\"v\"}}"))
    (let ((id1 (event-id (json-string-to-event line)))
          (id2 (event-id (json-string-to-event line))))
      (is (string= id1 id2)))))

(test json-string-to-event-preserves-baked-in-synth-prefix
  "An already-synthesized id with the synth- prefix re-parses with synth-p=NIL."
  (let ((line "{\"id\":\"synth-1234-x-DEADBEEF\",\"timestamp\":1234,\"type\":\"x\",\"session\":\"s\",\"clock\":{},\"data\":{}}"))
    (multiple-value-bind (ev synth-p) (json-string-to-event line)
      (declare (ignore ev))
      (is (null synth-p)))))

(test json-string-to-event-rejects-missing-timestamp
  "Lines without an integer timestamp signal event-parse-error."
  (let ((line "{\"ts\":\"2026-02-14T15:35:35Z\",\"type\":\"x\",\"data\":{}}"))
    (signals task::event-parse-error (json-string-to-event line))))

(test json-string-to-event-rejects-missing-type
  "Lines without a string type signal event-parse-error."
  (let ((line "{\"timestamp\":100,\"data\":{}}"))
    (signals task::event-parse-error (json-string-to-event line))))

(test json-string-to-event-rejects-malformed-json
  "Garbage on the line signals event-parse-error (wrapped from yason)."
  (signals task::event-parse-error (json-string-to-event "this is not json")))

;;; --- File-level verify ---

(defun %make-tolerance-fixture (root)
  "Build a synthetic task tree under ROOT for verify/repair tests.
   taskA: 5 lines = 2 valid + 1 ts-old + 1 broken-json + 1 missing-id.
   taskB: 1 valid line.
   Returns (values taskA-events-path taskB-events-path)."
  (ensure-directories-exist (format nil "~AtaskA/" root))
  (ensure-directories-exist (format nil "~AtaskB/" root))
  (let ((a (format nil "~AtaskA/events.jsonl" root))
        (b (format nil "~AtaskB/events.jsonl" root)))
    (with-open-file (s a :direction :output :if-exists :supersede)
      (write-line "{\"id\":\"v1\",\"timestamp\":1000,\"session\":\"s\",\"clock\":{},\"type\":\"task.create\",\"data\":{}}" s)
      (write-line "{\"ts\":\"2026-02-14T15:35:35Z\",\"type\":\"x\",\"data\":{}}" s)
      (write-line "this-is-totally-broken-not-json" s)
      (write-line "{\"id\":\"v2\",\"timestamp\":1002,\"session\":\"s\",\"clock\":{},\"type\":\"observe\",\"data\":{}}" s)
      (write-line "{\"timestamp\":1003,\"type\":\"old.no.id\",\"session\":\"s\",\"data\":{\"k\":\"v\"}}" s))
    (with-open-file (s b :direction :output :if-exists :supersede)
      (write-line "{\"id\":\"only-good\",\"timestamp\":2000,\"session\":\"s\",\"clock\":{},\"type\":\"task.create\",\"data\":{}}" s))
    (values a b)))

(defun %fixture-root ()
  "Unique fixture root under /tmp/ to keep parallel test runs isolated."
  (format nil "/tmp/task-tolerance-test-~D-~D/"
          (get-universal-time)
          (sb-ext:atomic-incf task::*elog-save-counter*)))

(test verify-events-file-counts-good-synth-bad
  "Per-file verify reports good, synthesized-pending, quarantined."
  (let ((root (%fixture-root)))
    (multiple-value-bind (a b) (%make-tolerance-fixture root)
      (let ((va (verify-events-file a))
            (vb (verify-events-file b)))
        (is (= 5 (getf va :total)))
        (is (= 3 (getf va :good)))
        (is (= 1 (getf va :synthesized)))
        (is (= 2 (getf va :quarantined)))
        (is (= 1 (getf vb :total)))
        (is (= 1 (getf vb :good)))
        (is (zerop (getf vb :synthesized)))
        (is (zerop (getf vb :quarantined)))))
    ;; Cleanup
    (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))

(test verify-events-file-handles-missing-path
  "verify-events-file returns zero counts for a non-existent file."
  (let ((res (verify-events-file "/tmp/this-path-does-not-exist-task-test.jsonl")))
    (is (zerop (getf res :total)))
    (is (zerop (getf res :good)))
    (is (zerop (getf res :synthesized)))
    (is (zerop (getf res :quarantined)))
    (is (null (getf res :error)))))

;;; --- Tree-level verify ---

(test verify-events-tree-aggregates-totals
  "Tree verify rolls up per-file counts and reports each task."
  (let ((root (%fixture-root)))
    (%make-tolerance-fixture root)
    (let* ((data (verify-events-tree root))
           (totals (getf data :totals)))
      (is (= 2 (getf data :tasks)))
      (is (= 6 (getf totals :lines)))
      (is (= 4 (getf totals :good)))
      (is (= 1 (getf totals :synthesized)))
      (is (= 2 (getf totals :quarantined)))
      (is (zerop (getf totals :errors)))
      (is (= 2 (length (getf data :per-task))))
      ;; Sort puts taskA (bad>0) before taskB (clean).
      (is (string= "taskA" (getf (first (getf data :per-task)) :id)))
      (is (string= "taskB" (getf (second (getf data :per-task)) :id))))
    (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))

;;; --- Repair semantics ---

(test repair-events-file-bakes-in-synth-and-quarantines-bad
  "Repair atomically rewrites the file: synth ids baked in, bad lines
   moved to .quarantine, valid lines preserved in original order."
  (let ((root (%fixture-root)))
    (multiple-value-bind (a b) (%make-tolerance-fixture root)
      (declare (ignore b))
      (let ((res (repair-events-file a)))
        (is (= 5 (getf res :read)))
        (is (= 3 (getf res :written)))
        (is (= 1 (getf res :synthesized)))
        (is (= 2 (getf res :quarantined)))
        (is-true (getf res :rewrote-p)))
      ;; The rewritten file has 3 lines and they parse cleanly.
      (let ((reload (elog-load a)))
        (is (= 3 (length (event-log-events reload)))))
      ;; Quarantine sidecar exists and contains the 2 bad lines plus headers.
      (is-true (probe-file (concatenate 'string a ".quarantine"))))
    (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))

(test repair-events-file-is-idempotent
  "A second repair on a clean file is a no-op (rewrote-p=NIL)."
  (let ((root (%fixture-root)))
    (multiple-value-bind (a b) (%make-tolerance-fixture root)
      (declare (ignore b))
      (repair-events-file a)
      (let ((res2 (repair-events-file a)))
        (is-false (getf res2 :rewrote-p))
        (is (zerop (getf res2 :synthesized)))
        (is (zerop (getf res2 :quarantined)))))
    (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))

(test repair-events-file-skips-clean-files
  "A file with no synth-pending or bad lines is left untouched."
  (let ((root (%fixture-root)))
    (multiple-value-bind (a b) (%make-tolerance-fixture root)
      (declare (ignore a))
      (let ((res (repair-events-file b)))
        (is (= 1 (getf res :read)))
        (is-false (getf res :rewrote-p))))
    (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))

(test repair-events-tree-only-lists-affected-tasks
  "Tree repair returns per-task entries only for files that changed
   or had quarantined lines."
  (let ((root (%fixture-root)))
    (%make-tolerance-fixture root)
    (let ((data (repair-events-tree root)))
      (is (= 2 (getf data :tasks)))
      (is (= 1 (getf data :rewrote)))
      (is (= 1 (length (getf data :per-task))))
      (is (string= "taskA" (getf (first (getf data :per-task)) :id))))
    (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))

(test verify-after-repair-reports-clean
  "After a repair, verify shows synth=0 and bad=0 for all tasks."
  (let ((root (%fixture-root)))
    (%make-tolerance-fixture root)
    (repair-events-tree root)
    (let* ((data (verify-events-tree root))
           (totals (getf data :totals)))
      (is (zerop (getf totals :synthesized)))
      (is (zerop (getf totals :quarantined)))
      (is (= 4 (getf totals :good))))
    (uiop:delete-directory-tree (pathname root) :validate t :if-does-not-exist :ignore)))
