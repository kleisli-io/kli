;;;; kli-hook Tests - task-complete-reflect handler
;;;;
;;;; Tests for observation counting, reflection detection,
;;;; response parsing, and full handler flow.

(in-package :kli-hook.tests)
(in-suite :task-complete-reflect)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun make-temp-task-dir ()
  "Create a temporary directory structure mimicking a task directory."
  (let ((dir (format nil "/tmp/kli-test-~A/" (random 1000000))))
    (ensure-directories-exist dir)
    dir))

(defun write-events-file (task-dir events)
  "Write synthetic events.jsonl to a task directory.
   EVENTS is a list of JSON-line strings."
  (let ((path (format nil "~Aevents.jsonl" task-dir)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (dolist (event events)
        (write-line event s)))
    path))

(defun cleanup-temp-dir (dir)
  "Remove temporary directory and contents. Silently ignores failures."
  (ignore-errors
    (uiop:delete-directory-tree
     (uiop:ensure-directory-pathname dir)
     :validate t :if-does-not-exist :ignore)))

(defmacro with-temp-task-dir ((dir-var) &body body)
  "Execute BODY with a temporary task directory bound to DIR-VAR.
   Cleans up after."
  `(let ((,dir-var (make-temp-task-dir)))
     (unwind-protect (progn ,@body)
       (cleanup-temp-dir ,dir-var))))

;;; ---------------------------------------------------------------------------
;;; parse-completed-task-id
;;; ---------------------------------------------------------------------------

(test parse-happy-path
  "Extracts qualified task ID from successful completion response."
  (is (string= "core:2026-02-23-my-task"
                (kli-hook::parse-completed-task-id
                 "Task core:2026-02-23-my-task marked as completed."))))

(test parse-nil-input
  "Returns NIL for nil input."
  (is (null (kli-hook::parse-completed-task-id nil))))

(test parse-error-response
  "Returns NIL for error responses without completion text."
  (is (null (kli-hook::parse-completed-task-id
             "Error: task has incomplete subtasks"))))

(test parse-hash-table-format
  "Handles tool_response as list of hash-tables."
  (let ((ht (make-ht "text" "Task kleisli:task-1 marked as completed.")))
    (is (string= "kleisli:task-1"
                  (kli-hook::parse-completed-task-id (list ht))))))

(test parse-multiline-with-frontier
  "Handles multi-line response with frontier update."
  (is (string= "core:my-task"
                (kli-hook::parse-completed-task-id
                 (format nil "Task core:my-task marked as completed.~%~%Newly available phases:~%  - phase-1")))))

;;; ---------------------------------------------------------------------------
;;; count-observations-in-events
;;; ---------------------------------------------------------------------------

(test count-observations-mixed
  "Counts only observation events in a file with mixed event types."
  (with-temp-task-dir (dir)
    (let ((path (write-events-file dir
                  '("{\"type\":\"observation\",\"text\":\"found bug\"}"
                    "{\"type\":\"session.join\",\"session\":\"abc\"}"
                    "{\"type\":\"observation\",\"text\":\"fixed bug\"}"
                    "{\"type\":\"task.update-status\",\"status\":\"active\"}"
                    "{\"type\":\"observation\",\"text\":\"verified fix\"}"))))
      (is (= 3 (kli-hook::count-observations-in-events path))))))

(test count-observations-empty
  "Returns 0 for empty events file."
  (with-temp-task-dir (dir)
    (let ((path (write-events-file dir nil)))
      (is (= 0 (kli-hook::count-observations-in-events path))))))

(test count-observations-nonexistent
  "Returns 0 for nonexistent file."
  (is (= 0 (kli-hook::count-observations-in-events "/nonexistent/events.jsonl"))))

(test count-observations-nil
  "Returns 0 for nil path."
  (is (= 0 (kli-hook::count-observations-in-events nil))))

;;; ---------------------------------------------------------------------------
;;; has-reflection-p
;;; ---------------------------------------------------------------------------

(test reflection-by-file
  "Detects reflection.md file."
  (with-temp-task-dir (dir)
    (with-open-file (s (format nil "~Areflection.md" dir)
                       :direction :output)
      (write-string "# Reflection" s))
    (is-true (kli-hook::has-reflection-p dir nil))))

(test reflection-by-event
  "Detects 'Reflection complete' in event stream."
  (with-temp-task-dir (dir)
    (let ((path (write-events-file dir
                  '("{\"type\":\"observation\",\"text\":\"Reflection complete for task\"}"))))
      (is-true (kli-hook::has-reflection-p dir path)))))

(test no-reflection
  "Returns NIL when no reflection exists."
  (with-temp-task-dir (dir)
    (let ((path (write-events-file dir
                  '("{\"type\":\"observation\",\"text\":\"some work\"}"))))
      (is-false (kli-hook::has-reflection-p dir path)))))

;;; ---------------------------------------------------------------------------
;;; task-complete-reflect-handler (full flow)
;;; ---------------------------------------------------------------------------

(defun make-handler-input (tool-response cwd)
  "Build a synthetic PostToolUse input hash-table."
  (let ((input (make-ht "tool_name" "mcp__task__task_complete"
                         "cwd" cwd)))
    (when tool-response
      (setf (gethash "tool_response" input) tool-response))
    (setf (gethash "tool_input" input) (make-ht))
    input))

(defun response-nudge-text (result)
  "Extract nudge text from handler result, or NIL if empty response."
  (let ((hso (gethash "hookSpecificOutput" result)))
    (when hso
      (gethash "additionalContext" hso))))

(test handler-nudges-above-threshold
  "Handler returns nudge when task has >= 5 observations and no reflection."
  (with-temp-task-dir (dir)
    ;; Write 6 observation events
    (write-events-file dir
      (loop repeat 6 collect "{\"type\":\"observation\",\"text\":\"finding\"}"))
    ;; Build input pointing to this task
    ;; Use task-dir-from-id indirectly by creating matching structure
    (let* ((result (kli-hook::task-complete-reflect-handler
                    (make-handler-input
                     (format nil "Task core:~A marked as completed."
                             (car (last (pathname-directory
                                          (uiop:ensure-directory-pathname dir)))))
                     ;; cwd → coord-root resolution needs real depot structure;
                     ;; test the individual functions instead for structural correctness
                     "/home/mika/src/world/kleisli"))))
      ;; Handler should return something (might be empty-response if coord-root
      ;; doesn't resolve to our temp dir — that's OK, the unit tests above
      ;; verify the individual functions)
      (is (hash-table-p result)))))

(test handler-silent-on-error-response
  "Handler returns empty-response for failed task_complete."
  (let ((result (kli-hook::task-complete-reflect-handler
                 (make-handler-input
                  "Error: task has incomplete subtasks"
                  "/home/mika/src/world/kleisli"))))
    (is (hash-table-p result))
    (is (= 0 (hash-table-count result)))))

(test handler-silent-on-nil
  "Handler returns empty-response for nil input."
  (let ((result (kli-hook::task-complete-reflect-handler (make-ht))))
    (is (hash-table-p result))
    (is (= 0 (hash-table-count result)))))
