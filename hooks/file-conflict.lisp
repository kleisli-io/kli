;;;; file-conflict.lisp - PostToolUse hook handler for swarm file conflict detection
;;;;
;;;; Warns Claude after editing a file recently touched by another session.
;;;; Enables stigmergic coordination.

(in-package #:kli-hook)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defparameter *conflict-window-minutes* 30)
(defparameter *observation-truncate-length* 120)

;;; ---------------------------------------------------------------------------
;;; Session & Event Scanning
;;; ---------------------------------------------------------------------------

(defun our-session-id (coord-root)
  "Return our session-id from our own session file."
  (let ((session-data (read-session-file coord-root)))
    (when session-data
      (gethash "session_id" session-data))))

(defun list-session-files (coord-root)
  "List all session JSON files in .claude/sessions/."
  (let ((dir (merge-pathnames ".claude/sessions/"
                              (uiop:ensure-directory-pathname coord-root))))
    (when (uiop:directory-exists-p dir)
      (uiop:directory-files dir "*.json"))))

(defun read-session-json (path)
  "Read and parse a session JSON file."
  (handler-case
      (with-open-file (s path)
        (yason:parse s))
    (error () nil)))

(defun session-active-p (session-data max-age-minutes)
  "Check if session was active within MAX-AGE-MINUTES."
  (let ((timestamp (gethash "timestamp" session-data)))
    (when timestamp
      (>= timestamp (- (get-universal-time) (* max-age-minutes 60))))))

(defun dedup-session-files (session-files)
  "Deduplicate session files by session_id, keeping most recent per session."
  (let ((by-session (make-hash-table :test #'equal)))
    (dolist (path session-files)
      (let ((data (read-session-json path)))
        (when data
          (let* ((sid (gethash "session_id" data))
                 (ts (or (gethash "timestamp" data) 0))
                 (existing (gethash sid by-session)))
            (when sid
              (if (or (null existing)
                      (> ts (or (gethash "timestamp" (cdr existing)) 0)))
                  (setf (gethash sid by-session) (cons path data))))))))
    (loop for v being the hash-values of by-session collect v)))

(defun scan-file-activity (events-path file-path max-age-minutes)
  "Scan events.jsonl for file modifications matching FILE-PATH.
   Both stored and query paths are normalized for consistent matching."
  (let ((norm-file-path (normalize-file-path file-path)))
    (when (and events-path norm-file-path (probe-file events-path))
      (let ((cutoff (- (get-universal-time) (* max-age-minutes 60)))
            (matches nil))
        (handler-case
            (with-open-file (s events-path)
              (loop for line = (read-line s nil nil)
                    while line
                    do (handler-case
                           (let* ((event (yason:parse line))
                                  (type (gethash "type" event))
                                  (ts (gethash "timestamp" event))
                                  (data (gethash "data" event)))
                             (when (and ts (>= ts cutoff) data)
                               (let ((path
                                       (normalize-file-path
                                        (cond
                                          ((and (string= type "tool.call")
                                                (member (gethash "tool" data)
                                                        '("Edit" "Write") :test #'string=))
                                           (let ((args (gethash "args" data)))
                                             (when args (gethash "file_path" args))))
                                          ((string= type "file.touch")
                                           (gethash "path" data))
                                          (t nil)))))
                                 (when (and path (string= path norm-file-path))
                                   (push event matches)))))
                         (error () nil))))
          (error () nil))
        (nreverse matches)))))

(defun scan-session-context (events-path)
  "Extract enrichment context from a task's events.jsonl."
  (when (and events-path (probe-file events-path))
    (let ((task-desc nil)
          (last-obs nil)
          (file-counts (make-hash-table :test #'equal)))
      (handler-case
          (with-open-file (s events-path)
            (loop for line = (read-line s nil nil)
                  while line
                  do (handler-case
                         (let* ((event (yason:parse line))
                                (type (gethash "type" event))
                                (data (gethash "data" event)))
                           (when data
                             (cond
                               ((and (string= type "task.create") (null task-desc))
                                (setf task-desc (gethash "description" data)))
                               ((string= type "observation")
                                (let ((text (gethash "text" data)))
                                  (when text (setf last-obs text))))
                               ((and (string= type "tool.call")
                                     (member (gethash "tool" data)
                                             '("Edit" "Write") :test #'string=))
                                (let* ((args (gethash "args" data))
                                       (fp (normalize-file-path
                                            (when args (gethash "file_path" args)))))
                                  (when fp (incf (gethash fp file-counts 0))))))))
                       (error () nil))))
        (error () nil))
      (when (and last-obs (> (length last-obs) *observation-truncate-length*))
        (setf last-obs (concatenate 'string
                                    (subseq last-obs 0 *observation-truncate-length*)
                                    "...")))
      (let ((file-alist nil))
        (maphash (lambda (k v) (push (cons k v) file-alist)) file-counts)
        (setf file-alist (sort file-alist #'> :key #'cdr))
        (list :task-description task-desc
              :last-observation last-obs
              :edited-files file-alist)))))

;;; ---------------------------------------------------------------------------
;;; Conflict Detection
;;; ---------------------------------------------------------------------------

(defun check-file-conflicts (file-path coord-root max-age-minutes)
  "Check all active sessions for recent edits to FILE-PATH.
   FILE-PATH is normalized before comparison."
  (let ((file-path (or (normalize-file-path file-path) (return-from check-file-conflicts nil)))
        (our-sid (our-session-id coord-root))
        (deduped (dedup-session-files (list-session-files coord-root)))
        (conflicts nil))
    (dolist (entry deduped)
      (let* ((session-data (cdr entry))
             (session-id (gethash "session_id" session-data)))
        (unless (and our-sid session-id (string= our-sid session-id))
          (when (session-active-p session-data max-age-minutes)
            (let* ((task-id (gethash "task_id" session-data))
                   (events-path (gethash "events_path" session-data))
                   (touches (scan-file-activity events-path file-path max-age-minutes)))
              (when touches
                (let* ((context (scan-session-context events-path))
                       (edited-files (getf context :edited-files))
                       (edit-count (or (cdr (assoc file-path edited-files
                                                   :test #'string=))
                                       (length touches)))
                       (other-files (remove file-path edited-files
                                            :key #'car :test #'string=))
                       (most-recent-ts (gethash "timestamp" (car (last touches))))
                       (age-minutes (when most-recent-ts
                                      (floor (- (get-universal-time) most-recent-ts) 60))))
                  (push (list :session session-id
                              :task task-id
                              :age-minutes age-minutes
                              :edit-count edit-count
                              :task-description (getf context :task-description)
                              :last-observation (getf context :last-observation)
                              :other-files other-files)
                        conflicts))))))))
    (nreverse conflicts)))

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
  "PostToolUse handler: warn about recent file activity after Edit/Write."
  (let ((file-path (normalize-file-path
                    (let ((ti (gethash "tool_input" input)))
                      (when ti (gethash "file_path" ti)))))
        (cwd (jref input "cwd")))
    (if (and file-path cwd)
        (let ((coord-root (depot:coordination-root-from cwd)))
          (if coord-root
              (let ((conflicts (check-file-conflicts file-path coord-root
                                                     *conflict-window-minutes*)))
                (if conflicts
                    (post-tool-context
                     (format-conflict-warning conflicts file-path))
                    (empty-response)))
              (empty-response)))
        (empty-response))))
