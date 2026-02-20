;;;; claude-hooks - JSON Utilities
;;;;
;;;; Wrappers around yason for Claude Code hook JSON I/O.

(in-package :claude-hooks)

(defun make-ht (&rest pairs)
  "Create a hash-table from alternating key-value PAIRS.
Odd argument count results in last key mapping to NIL (JSON null)."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun jref (obj &rest keys)
  "Nested hash-table access. Returns NIL if any key is missing.
   (jref ht \"a\" \"b\" \"c\") traverses ht[a][b][c]."
  (reduce (lambda (current key)
            (when (hash-table-p current)
              (gethash key current)))
          keys :initial-value obj))

(defun encode-json (value)
  "Encode VALUE to a JSON string."
  (with-output-to-string (s)
    (yason:encode value s)))

(defun parse-json (string)
  "Parse a JSON STRING to Lisp objects.
Hash-tables with string keys for objects, vectors for arrays."
  (yason:parse string))
