;;; Playbook MCP Server - Playbook.md Parser
;;; Parse [domain-NNNNNN] helpful=N harmful=M :: content format

(in-package #:playbook-mcp)

;;; Pattern line regex
;;; Format: [domain-NNNNNN] helpful=N harmful=M :: content

(defparameter *pattern-line-regex*
  ;; Matches: [id] helpful=N harmful=M :: content
  ;; Group 1: full ID (e.g., "lisp-000042")
  ;; Group 2: domain (e.g., "lisp")
  ;; Group 3: helpful count
  ;; Group 4: harmful count
  ;; Group 5: content
  "^\\[([a-zA-Z]+-[0-9]+)\\]\\s+helpful=([0-9]+)\\s+harmful=([0-9]+)\\s+::\\s*(.*)$"
  "Regex for parsing playbook pattern lines.")

(defun parse-pattern-line (line &optional source-file line-number)
  "Parse a single pattern line into a pattern struct.
   Returns NIL if line doesn't match pattern format."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings *pattern-line-regex* line)
    (when match
      (let* ((id (aref groups 0))
             ;; Extract domain from ID (everything before the dash-number)
             (domain (cl-ppcre:regex-replace "-[0-9]+$" id "")))
        (make-pattern
         :id id
         :domain domain
         :helpful (parse-integer (aref groups 1))
         :harmful (parse-integer (aref groups 2))
         :content (aref groups 3)
         :source-file source-file
         :line-number line-number)))))

;;; File loading

(defun load-playbook-file (path)
  "Load all patterns from a playbook.md file.
   Returns the number of patterns loaded."
  (let ((count 0))
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (when stream
        (loop for line = (read-line stream nil nil)
              for line-number from 1
              while line
              for pattern = (parse-pattern-line line path line-number)
              when pattern
              do (store-pattern pattern)
                 (incf count))))
    count))

(defun load-playbook-files (paths)
  "Load patterns from multiple playbook.md files.
   Later files override earlier ones for same pattern ID.
   Returns total patterns loaded."
  (let ((total 0))
    (dolist (path paths)
      (incf total (load-playbook-file path)))
    total))

;;; Multi-line content support (future enhancement)
;;; For now, patterns are single-line only

(defun continuation-line-p (line)
  "Check if line is a continuation (starts with whitespace)."
  (and (> (length line) 0)
       (member (char line 0) '(#\Space #\Tab))))
