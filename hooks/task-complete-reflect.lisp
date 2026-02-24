;;;; task-complete-reflect.lisp - PostToolUse hook for auto-reflection nudge
;;;;
;;;; Fires after mcp__task__task_complete. When the completed task has
;;;; sufficient observations and no prior reflection, injects a system
;;;; reminder nudging the agent to run /kli:reflect.
;;;;
;;;; This breaks the cold-start spiral where playbooks stay sparse because
;;;; reflection requires explicit human invocation.

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defvar *reflect-nudge-min-observations* 5
  "Minimum observation count before nudging for reflection.
   Tasks below this threshold are too thin for meaningful pattern extraction.")

;;; ---------------------------------------------------------------------------
;;; Response Parsing
;;; ---------------------------------------------------------------------------

(defun parse-completed-task-id (tool-response)
  "Extract task ID from task_complete response.
   Format: 'Task QUALIFIED-ID marked as completed.'
   Returns task ID string or NIL for failed/missing responses."
  (let ((text (extract-response-text tool-response)))
    (when (and text (stringp text))
      (let ((task-pos (search "Task " text))
            (marked-pos (search " marked as completed" text)))
        (when (and task-pos marked-pos (< task-pos marked-pos))
          (let ((id (subseq text (+ task-pos 5) marked-pos)))
            (when (> (length id) 0)
              id)))))))

;;; ---------------------------------------------------------------------------
;;; Task Directory Resolution
;;; ---------------------------------------------------------------------------

(defun task-dir-from-id (coord-root task-id)
  "Resolve task directory from coordination root and qualified task ID.
   Reuses the same depot/fallback logic as compute-events-path."
  (let* ((colon-pos (position #\: task-id))
         (depot (when colon-pos (subseq task-id 0 colon-pos)))
         (bare-id (if colon-pos
                      (subseq task-id (1+ colon-pos))
                      task-id))
         (tasks-root
           (or (ignore-errors
                 (let ((fn (find-symbol "DEPOT-TASKS-ROOT" :depot)))
                   (when fn (funcall fn depot))))
               (let ((base-dir (if depot
                                   (format nil "~A/~A/"
                                           (string-right-trim "/" (namestring
                                                                   (uiop:ensure-directory-pathname coord-root)))
                                           depot)
                                   (namestring (uiop:ensure-directory-pathname coord-root)))))
                 (let ((kli-candidate (format nil "~A.kli/tasks/" base-dir)))
                   (if (probe-file kli-candidate)
                       kli-candidate
                       (format nil "~Aace/tasks/" base-dir)))))))
    (format nil "~A~A/" tasks-root bare-id)))

;;; ---------------------------------------------------------------------------
;;; Observation Counting
;;; ---------------------------------------------------------------------------

(defun count-observations-in-events (events-path)
  "Count observation events in events.jsonl.
   Returns integer count, 0 for empty/missing files."
  (if (and events-path (probe-file events-path))
      (with-open-file (s events-path :direction :input :if-does-not-exist nil)
        (if s
            (loop for line = (read-line s nil nil)
                  while line
                  count (search "\"type\":\"observation\"" line))
            0))
      0))

;;; ---------------------------------------------------------------------------
;;; Reflection Detection
;;; ---------------------------------------------------------------------------

(defun has-reflection-p (task-dir events-path)
  "Check whether a task already has a reflection artifact.
   Looks for reflection.md in the task directory, or 'Reflection complete'
   in the event stream. Returns generalized boolean."
  (or (probe-file (format nil "~Areflection.md" task-dir))
      (and events-path
           (probe-file events-path)
           (with-open-file (s events-path :direction :input :if-does-not-exist nil)
             (when s
               (loop for line = (read-line s nil nil)
                     while line
                     thereis (search "Reflection complete" line)))))))

;;; ---------------------------------------------------------------------------
;;; Nudge Text
;;; ---------------------------------------------------------------------------

(defun format-reflect-nudge (task-id obs-count)
  "Format the reflection nudge system reminder."
  (format nil "Task ~A completed with ~D observation~:P. ~
               Consider running /kli:reflect to extract reusable patterns ~
               from this work before moving on."
          task-id obs-count))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun task-complete-reflect-handler (input)
  "PostToolUse handler for task_complete: nudge reflection when warranted.
   Returns post-tool-context with nudge, or empty-response when silent."
  (handler-case
      (let* ((tool-response (gethash "tool_response" input))
             (cwd (jref input "cwd"))
             (task-id (parse-completed-task-id tool-response)))
        (if (and task-id cwd)
            (let ((coord-root (depot:coordination-root-from cwd)))
              (if coord-root
                  (let* ((task-dir (task-dir-from-id coord-root task-id))
                         (events-path (format nil "~Aevents.jsonl" task-dir)))
                    (cond
                      ((has-reflection-p task-dir events-path)
                       (empty-response))
                      ((< (count-observations-in-events events-path)
                          *reflect-nudge-min-observations*)
                       (empty-response))
                      (t
                       (post-tool-context
                        (format-reflect-nudge
                         task-id
                         (count-observations-in-events events-path))))))
                  (empty-response)))
            (empty-response)))
    (error () (empty-response))))
