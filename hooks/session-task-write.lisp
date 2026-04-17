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
  "Compute events.jsonl path from coordination root and task ID.
   Strips any legacy depot: prefix and resolves under .kli/tasks/."
  (let* ((colon-pos (position #\: task-id))
         (bare-id (if colon-pos
                      (subseq task-id (1+ colon-pos))
                      task-id))
         (base-dir (namestring (uiop:ensure-directory-pathname coord-root)))
         (tasks-root (format nil "~A.kli/tasks/" base-dir)))
    (format nil "~A~A/events.jsonl" tasks-root bare-id)))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun session-task-write-tool-p (tool-name)
  "Return T if TOOL-NAME identifies a tool whose response is expected
   to contain the 'Current task set to X (session Y joined)' parseable
   line — i.e. task_set_current or task_bootstrap.  Used to gate
   PARSE-TASK-RESPONSE so it does not false-positive on unrelated tool
   responses whose text happens to contain matching substrings."
  (and tool-name
       (or (search "task_set_current" tool-name)
           (search "task_bootstrap" tool-name))))

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
            ((and tool-response (session-task-write-tool-p tool-name))
             (let ((response-text (extract-response-text tool-response)))
               (when response-text
                 (multiple-value-bind (task-id session-id)
                     (parse-task-response response-text)
                   (when (and task-id session-id)
                     (let ((events-path (compute-events-path coord-root task-id)))
                       (write-session-task-file
                        coord-root task-id session-id events-path))))))))))))
  (empty-response))
