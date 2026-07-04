(in-package #:kli/tools/lisp)

(defparameter +cl-source-types+ '("lisp" "asd" "cl" "lsp")
  "Pathname types treated as Common Lisp source.")

(defun cl-source-path-p (path)
  (let ((type (pathname-type (pathname path))))
    (and (stringp type)
         (member type +cl-source-types+ :test #'string-equal))))


(defun overwrite-file (path content)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string content out)))

(defun string-lines (s)
  (loop with len = (length s)
        for start = 0 then (1+ nl)
        for nl = (position #\Newline s :start start)
        collect (subseq s start (or nl len))
        while nl))

(defun repair-note (path before after)
  (with-output-to-string (out)
    (format out "paren-repair: balanced delimiters in ~A.~%" path)
    (unified-diff out (string-lines before) (string-lines after)
                  :test-function #'equal
                  :from-file (namestring path) :to-file (namestring path))
    (format out "~%Repair is indentation-driven, so a sub-form can close at a ~
different place than simply adding a trailing paren would. Verify the structure ~
-- especially how many top-level forms it became -- matches what you intended.")))
