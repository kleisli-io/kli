;;;; playbook-hooks tests - Session I/O

(in-package :playbook-hooks.tests)
(in-suite :session-io)

(test read-activated-ids-basic
  "Reads and deduplicates pattern IDs from activated.jsonl."
  (let* ((sid "test-session-io")
         (dir (format nil "/tmp/test-read-activated-~a" (random 1000000)))
         (pb-dir (format nil "~a/.claude/sessions/~a/playbook" dir sid))
         (path (format nil "~a/activated.jsonl" pb-dir)))
    (ensure-directories-exist (format nil "~a/" pb-dir))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-line "[\"nix-000001\",\"nix-000002\"]" s)
      (write-line "[\"nix-000002\",\"ace-000001\"]" s))
    (let ((ids (read-activated-ids dir sid)))
      (is (= 3 (length ids)))
      (is (member "nix-000001" ids :test #'string=))
      (is (member "nix-000002" ids :test #'string=))
      (is (member "ace-000001" ids :test #'string=)))
    ;; cleanup
    (delete-file path)))

(test read-activated-ids-missing-file
  "Returns NIL for missing activated.jsonl."
  (is (null (read-activated-ids "/tmp/nonexistent" "no-session"))))

(test read-activated-ids-malformed-lines
  "Skips malformed JSON lines gracefully."
  (let* ((sid "test-malformed")
         (dir (format nil "/tmp/test-malformed-~a" (random 1000000)))
         (pb-dir (format nil "~a/.claude/sessions/~a/playbook" dir sid))
         (path (format nil "~a/activated.jsonl" pb-dir)))
    (ensure-directories-exist (format nil "~a/" pb-dir))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-line "not-json" s)
      (write-line "[\"valid-001\"]" s)
      (write-line "{\"wrong\":\"type\"}" s))
    (let ((ids (read-activated-ids dir sid)))
      (is (equal '("valid-001") ids)))
    (delete-file path)))
