;;;; file-conflict.lisp - PostToolUse hook handler for swarm file conflict detection
;;;;
;;;; Warns Claude after editing a file recently touched by another session.
;;;; Delegates to the task-mcp daemon's /file-conflict endpoint which enforces
;;;; the Session Attribution Invariant (PID liveness + session-aware scanning).

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defparameter *conflict-window-minutes* 30)

;;; ---------------------------------------------------------------------------
;;; HTTP Client for task-mcp /file-conflict
;;; ---------------------------------------------------------------------------

(defun call-file-conflict-endpoint (session-id file-path max-age-minutes)
  "Call task-mcp's /file-conflict endpoint. Returns parsed JSON array or NIL."
  (handler-case
      (let ((url (format nil "http://localhost:~D/file-conflict?session-id=~A&file-path=~A&max-age-minutes=~D"
                         *task-mcp-port*
                         (quri:url-encode session-id)
                         (quri:url-encode file-path)
                         max-age-minutes)))
        (multiple-value-bind (body status)
            (dex:get url :connect-timeout 2 :read-timeout 3)
          (when (and status (< status 300) body (plusp (length body)))
            (let ((parsed (ignore-errors (yason:parse body))))
              (when (and parsed (listp parsed) (plusp (length parsed)))
                parsed)))))
    (error () nil)))

(defun json-conflicts-to-plists (json-array)
  "Convert JSON hash-table array from /file-conflict to plist format for formatting."
  (mapcar (lambda (ht)
            (let ((other-files-json (gethash "other_files" ht)))
              (list :session (gethash "session" ht)
                    :task (gethash "task" ht)
                    :age-minutes (gethash "age_minutes" ht)
                    :edit-count (gethash "edit_count" ht)
                    :task-description (gethash "task_description" ht)
                    :last-observation (gethash "last_observation" ht)
                    :other-files (when other-files-json
                                   (mapcar (lambda (f)
                                             (cons (gethash "path" f)
                                                   (gethash "count" f)))
                                           other-files-json)))))
          json-array))

;;; ---------------------------------------------------------------------------
;;; Warning Formatting
;;; ---------------------------------------------------------------------------

(defun format-other-files (other-files stream &optional (max-files 5))
  "Format the other-files alist as a compact summary line."
  (when other-files
    (let ((shown (subseq other-files 0 (min max-files (length other-files))))
          (remaining (max 0 (- (length other-files) max-files))))
      (format stream "  Also edited: ~{~A~^, ~}~@[~* (+~D more)~]~%"
              (mapcar (lambda (pair)
                        (format nil "~A (~D)"
                                (file-namestring (car pair))
                                (cdr pair)))
                      shown)
              (plusp remaining)
              remaining))))

(defun format-conflict-warning (conflicts file-path)
  "Format an enriched warning message about file conflicts."
  (with-output-to-string (s)
    (format s "~%SWARM AWARENESS: File recently modified by other session(s)~%~%")
    (format s "File: ~A~%~%" file-path)
    (dolist (conflict conflicts)
      (let ((session (getf conflict :session))
            (task (getf conflict :task))
            (age (getf conflict :age-minutes))
            (edit-count (getf conflict :edit-count))
            (task-desc (getf conflict :task-description))
            (last-obs (getf conflict :last-observation))
            (other-files (getf conflict :other-files)))
        (format s "Session ~A on task ~A (~Am ago)~%"
                (if (and session (> (length session) 8))
                    (subseq session 0 8)
                    session)
                task age)
        (when task-desc (format s "  Task: ~S~%" task-desc))
        (when edit-count (format s "  Edits to this file: ~D~%" edit-count))
        (format-other-files other-files s)
        (when last-obs (format s "  Last activity: ~S~%" last-obs))
        (format s "~%")))
    (format s "Investigate: mcp__task__timeline(task_id=~S)~%"
            (getf (first conflicts) :task))))

;;; ---------------------------------------------------------------------------
;;; Handler Entry Point
;;; ---------------------------------------------------------------------------

(defun file-conflict-handler (input)
  "PostToolUse handler: warn about recent file activity after Edit/Write.
   Delegates to the task-mcp daemon for PID-checked, session-aware conflict detection."
  (let ((file-path (normalize-file-path
                    (let ((ti (gethash "tool_input" input)))
                      (when ti (gethash "file_path" ti)))))
        (cwd (jref input "cwd")))
    (if (and file-path cwd)
        (let ((coord-root (depot:coordination-root-from cwd)))
          (if coord-root
              (let* ((session-info (read-session-file coord-root))
                     (session-id (when session-info
                                   (gethash "session_id" session-info)))
                     (json (when session-id
                             (call-file-conflict-endpoint
                              session-id file-path *conflict-window-minutes*))))
                (if json
                    (let ((conflicts (json-conflicts-to-plists json)))
                      (post-tool-context
                       (format-conflict-warning conflicts file-path)))
                    (empty-response)))
              (empty-response)))
        (empty-response))))
