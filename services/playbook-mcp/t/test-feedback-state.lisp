;;;; playbook-mcp tests - Feedback State File I/O
;;;; Tests for the feedback-state.json bridge between MCP server and Stop hook.
;;;; The Stop hook reads this file to decide whether to nudge for feedback.

(in-package :playbook-mcp.tests)
(in-suite :feedback-state)

;;; feedback-state-path

(test feedback-state-path-format
  "feedback-state-path produces correct path structure."
  (let ((path (namestring (feedback-state-path "/home/user/project" "ABC123"))))
    (is (search ".claude/sessions/ABC123/playbook/feedback-state.json" path))))

;;; write-feedback-state-file

(test write-feedback-state-file-basic
  "write-feedback-state-file creates correct JSON structure."
  (let* ((dir (format nil "/tmp/test-fb-state-~a" (random 1000000)))
         (sid "fb-state-test-001")
         (playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    ;; Set up session with activations and partial feedback
    (record-activation sid "pat-a")
    (record-activation sid "pat-b")
    (record-activation sid "pat-c")
    (record-feedback sid "pat-a" :helpful)
    (unwind-protect
         (let ((path (write-feedback-state-file dir sid)))
           (is (not (null path)))
           ;; Read back and verify
           (let ((state (read-feedback-state-file path)))
             (is (not (null state)))
             ;; Check activated count
             (is (= 3 (length (gethash "activated" state))))
             ;; Check feedback given
             (is (= 1 (length (gethash "feedback_given" state))))
             ;; Check pending (2 patterns without feedback)
             (is (= 2 (gethash "count" state)))
             (let ((pending (coerce (gethash "pending" state) 'list)))
               (is (= 2 (length pending)))
               (is (member "pat-b" pending :test #'string=))
               (is (member "pat-c" pending :test #'string=)))))
      ;; Cleanup
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

(test write-feedback-state-file-all-feedback-given
  "write-feedback-state-file shows zero pending when all feedback given."
  (let* ((dir (format nil "/tmp/test-fb-all-~a" (random 1000000)))
         (sid "fb-all-001")
         (playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-activation sid "pat-a")
    (record-activation sid "pat-b")
    (record-feedback sid "pat-a" :helpful)
    (record-feedback sid "pat-b" :harmful)
    (unwind-protect
         (let ((path (write-feedback-state-file dir sid)))
           (let ((state (read-feedback-state-file path)))
             (is (= 0 (gethash "count" state)))
             (is (= 0 (length (gethash "pending" state))))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

(test write-feedback-state-file-no-activations
  "write-feedback-state-file handles empty session (no activations)."
  (let* ((dir (format nil "/tmp/test-fb-empty-~a" (random 1000000)))
         (sid "fb-empty-001")
         (playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (get-or-create-session sid)
    (unwind-protect
         (let ((path (write-feedback-state-file dir sid)))
           (let ((state (read-feedback-state-file path)))
             (is (= 0 (gethash "count" state)))
             (is (= 0 (length (gethash "activated" state))))))
      (ignore-errors
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname dir) :validate t)))))

(test write-feedback-state-file-nil-session
  "write-feedback-state-file returns nil for nonexistent session."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (is (null (write-feedback-state-file "/tmp" "nonexistent")))))

(test write-feedback-state-file-nil-cwd
  "write-feedback-state-file returns nil when cwd is nil."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (is (null (write-feedback-state-file nil "some-session")))))

;;; read-feedback-state-file

(test read-feedback-state-file-missing
  "read-feedback-state-file returns nil for nonexistent file."
  (is (null (read-feedback-state-file "/tmp/nonexistent-feedback-state.json"))))

(test read-feedback-state-file-malformed
  "read-feedback-state-file returns nil for malformed JSON."
  (let ((path (format nil "/tmp/test-malformed-fb-~a.json" (random 1000000))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "not valid json {{{" s))
    (unwind-protect
         (is (null (read-feedback-state-file path)))
      (ignore-errors (delete-file path)))))

;;; Aggregate task state

(test aggregate-task-state-merges-sessions
  "aggregate-task-state correctly merges multiple session entries."
  (let ((entries (list
                  ;; Session 1: activated pat-a and pat-b, feedback on pat-a
                  (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "activated" ht) '("pat-a" "pat-b"))
                    (let ((fb (make-hash-table :test 'equal)))
                      (setf (gethash "id" fb) "pat-a")
                      (setf (gethash "type" fb) "helpful")
                      (setf (gethash "feedback" ht) (list fb)))
                    (setf (gethash "domains" ht) '("lisp"))
                    ht)
                  ;; Session 2: activated pat-b and pat-c, feedback on pat-c
                  (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "activated" ht) '("pat-b" "pat-c"))
                    (let ((fb (make-hash-table :test 'equal)))
                      (setf (gethash "id" fb) "pat-c")
                      (setf (gethash "type" fb) "harmful")
                      (setf (gethash "feedback" ht) (list fb)))
                    (setf (gethash "domains" ht) '("nix"))
                    ht))))
    (multiple-value-bind (total-act total-fb pending domains)
        (aggregate-task-state entries)
      ;; 3 unique patterns activated
      (is (= 3 total-act))
      ;; 2 unique feedback given
      (is (= 2 total-fb))
      ;; 1 pending (pat-b has no feedback)
      (is (= 1 (length pending)))
      (is (member "pat-b" pending :test #'string=))
      ;; 2 unique domains
      (is (= 2 (length domains))))))

(test aggregate-task-state-empty
  "aggregate-task-state returns zeros for empty input."
  (multiple-value-bind (total-act total-fb pending domains)
      (aggregate-task-state nil)
    (is (= 0 total-act))
    (is (= 0 total-fb))
    (is (null pending))
    (is (null domains))))
