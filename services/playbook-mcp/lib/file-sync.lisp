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

;;; Evidence ledger

(defun evidence-ledger-path (source-file)
  "Return path to playbook-evidence.json alongside SOURCE-FILE."
  (when source-file
    (merge-pathnames "playbook-evidence.json"
                     (uiop:pathname-directory-pathname
                      (uiop:ensure-pathname source-file)))))

(defun append-pattern-evidence (pattern-id feedback-type evidence-text)
  "Append an evidence entry to playbook-evidence.json alongside the pattern's source file.
   Format: {pattern-id: [{type, text, ts}, ...]}. No-op if evidence is empty. Atomic write."
  (when (and evidence-text (string/= evidence-text ""))
    (let* ((pattern (get-pattern pattern-id))
           (ledger-path (when pattern
                          (evidence-ledger-path (pattern-source-file pattern)))))
      (when ledger-path
        (handler-case
            (let* ((abs-path (namestring ledger-path))
                   (temp-path (format nil "~A.tmp" abs-path))
                   (existing (if (probe-file abs-path)
                                 (yason:parse (alexandria:read-file-into-string abs-path))
                                 (make-hash-table :test 'equal)))
                   (entry (let ((ht (make-hash-table :test 'equal)))
                            (setf (gethash "type" ht)
                                  (string-downcase (symbol-name feedback-type)))
                            (setf (gethash "text" ht) evidence-text)
                            (setf (gethash "ts" ht) (get-universal-time))
                            ht))
                   (prev (gethash pattern-id existing))
                   (updated (concatenate 'vector (if prev prev #()) (vector entry))))
              (setf (gethash pattern-id existing) updated)
              (ensure-directories-exist abs-path)
              (with-open-file (s temp-path :direction :output :if-exists :supersede)
                (yason:encode existing s))
              (rename-file temp-path abs-path))
          (error (e)
            (format *error-output* "Warning: append-pattern-evidence ~A: ~A~%"
                    pattern-id e)))))))

;;; Feedback persistence

(defun save-pattern-feedback (pattern-id feedback-type &optional evidence)
  "Save feedback (helpful/harmful) for a pattern.
   FEEDBACK-TYPE is :helpful or :harmful. EVIDENCE is an optional explanatory string
   appended to playbook-evidence.json for audit and future training use.
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
           (pattern-to-line pattern))))
      ;; Append evidence to ledger if provided
      (append-pattern-evidence pattern-id feedback-type evidence))))

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
