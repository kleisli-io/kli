;;; Playbook MCP Server - File Sync
;;; Read/write playbook.md files with atomic updates

(in-package #:playbook-mcp)

;;; File update operations

(defun update-pattern-in-file (path pattern-id update-fn)
  "Update a single pattern in a playbook.md file.
   UPDATE-FN receives the current line and returns the new line.
   Uses atomic write (write to temp, then rename)."
  (let* ((abs-path (namestring (merge-pathnames path)))
         (temp-path (format nil "~A.tmp" abs-path))
         (updated nil))
    (with-open-file (in abs-path :direction :input)
      (with-open-file (out temp-path :direction :output
                                     :if-exists :supersede)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((pattern (parse-pattern-line line)))
                   (if (and pattern (string= (pattern-id pattern) pattern-id))
                       (progn
                         (write-line (funcall update-fn line) out)
                         (setf updated t))
                       (write-line line out))))))
    ;; Atomic rename
    (when updated
      (rename-file temp-path abs-path))
    updated))

;;; Feedback persistence

(defun save-pattern-feedback (pattern-id feedback-type)
  "Save feedback (helpful/harmful) for a pattern.
   FEEDBACK-TYPE is :helpful or :harmful.
   Always updates in-memory; only writes to file if pattern has source-file."
  (let ((pattern (get-pattern pattern-id)))
    (when pattern
      ;; Always update in-memory
      (case feedback-type
        (:helpful (incf (pattern-helpful pattern)))
        (:harmful (incf (pattern-harmful pattern))))
      ;; Update in file only if pattern has a source file
      (when (pattern-source-file pattern)
        (update-pattern-in-file
         (pattern-source-file pattern)
         pattern-id
         (lambda (line)
           (declare (ignore line))
           (pattern-to-line pattern)))))))

;;; Evolution persistence

(defun save-pattern-evolution (pattern-id new-content reason)
  "Save pattern evolution to its source file.
   Records old content in evolution history."
  (let ((pattern (get-pattern pattern-id)))
    (when (and pattern (pattern-source-file pattern))
      (let ((old-content (pattern-content pattern))
            (timestamp (get-universal-time)))
        ;; Update in-memory
        (push (list timestamp reason old-content)
              (pattern-evolution-history pattern))
        (setf (pattern-content pattern) new-content)
        ;; Update in file
        (update-pattern-in-file
         (pattern-source-file pattern)
         pattern-id
         (lambda (line)
           (declare (ignore line))
           (pattern-to-line pattern)))))))

;;; Add new pattern to file

(defun append-pattern-to-file (path pattern)
  "Append a new pattern to a playbook.md file."
  (with-open-file (stream path :direction :output
                               :if-exists :append
                               :if-does-not-exist :create)
    (write-line "" stream)  ; Blank line before
    (write-line (pattern-to-line pattern) stream))
  (setf (pattern-source-file pattern) (namestring path))
  (store-pattern pattern))
