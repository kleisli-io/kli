;;;; session-task-write.lisp - PostToolUse hook handler for session file writes
;;;;
;;;; Writes claude-{PPID}.json after task_set_current or task_bootstrap.
;;;; Deletes it after task_release.

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Response Parsing
;;; ---------------------------------------------------------------------------

(defun extract-response-text (tool-response)
  "Extract plain text from tool_response regardless of format."
  (cond
    ((stringp tool-response) tool-response)
    ((and (listp tool-response) (hash-table-p (first tool-response)))
     (gethash "text" (first tool-response)))
    ((hash-table-p tool-response)
     (gethash "text" tool-response))
    (t nil)))

(defun parse-task-response (response-text)
  "Parse task_id and session_id from task_set_current/task_bootstrap response.
   Format: 'Current task set to QUALIFIED-ID (session HEX joined)...'
   Returns two values: task-id session-id, or NIL NIL."
  (when (and response-text (stringp response-text))
    (let* ((to-pos (search "to " response-text))
           (paren-pos (when to-pos
                        (search " (" response-text :start2 to-pos)))
           (task-id (when (and to-pos paren-pos)
                      (subseq response-text (+ to-pos 3) paren-pos)))
           (session-pos (search "session " response-text))
           (joined-pos (when session-pos
                         (search " joined" response-text :start2 session-pos)))
           (session-id (when (and session-pos joined-pos)
                         (subseq response-text (+ session-pos 8) joined-pos))))
      (when (and task-id session-id
                 (> (length task-id) 0)
                 (> (length session-id) 0))
        (values task-id session-id)))))

;;; ---------------------------------------------------------------------------
;;; Events Path Resolution
;;; ---------------------------------------------------------------------------

(defun compute-events-path (coord-root task-id)
  "Compute events.jsonl path from coordination root and qualified task ID.
   Uses depot:depot-tasks-root when available, falls back to .kli/tasks/ then ace/tasks/."
  (let* ((colon-pos (position #\: task-id))
         (depot (when colon-pos (subseq task-id 0 colon-pos)))
         (bare-id (if colon-pos
                      (subseq task-id (1+ colon-pos))
                      task-id))
         ;; Try depot:depot-tasks-root first (available in kli binary)
         (tasks-root
           (or (ignore-errors
                 (let ((fn (find-symbol "DEPOT-TASKS-ROOT" :depot)))
                   (when fn (funcall fn depot))))
               ;; Fallback: probe .kli/tasks/ then ace/tasks/ under depot dir
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
    (format nil "~A~A/events.jsonl" tasks-root bare-id)))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun session-task-write-handler (input)
  "PostToolUse handler: write/delete session file based on task context."
  (let ((tool-response (gethash "tool_response" input))
        (tool-name (jref input "tool_name"))
        (cwd (jref input "cwd")))
    (when (and tool-name cwd)
      (let ((coord-root (depot:coordination-root-from cwd)))
        (when coord-root
          (cond
            ;; task_release: delete session file
            ((search "task_release" tool-name)
             (delete-session-task-file coord-root))
            ;; task_set_current / task_bootstrap: write session file
            (tool-response
             (let ((response-text (extract-response-text tool-response)))
               (when response-text
                 (multiple-value-bind (task-id session-id)
                     (parse-task-response response-text)
                   (when (and task-id session-id)
                     (let ((events-path (compute-events-path coord-root task-id)))
                       (write-session-task-file
                        coord-root task-id session-id events-path))))))))))))
  (empty-response))
