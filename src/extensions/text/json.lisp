(in-package #:kli/text)

;;; Canonical coercion to a jzon-safe tree. The event log and the JSON-RPC
;;; transport share this core and differ only in their hash-table policy.

(defun %json-proper-list-p (value)
  (and (listp value) (ignore-errors (list-length value)) t))

(defun %json-keyword-plist-p (value)
  "True for an even-length list led by a keyword, the shape that serializes to a
JSON object rather than an array."
  (and (consp value)
       (keywordp (car value))
       (let ((length (ignore-errors (list-length value))))
         (and length (evenp length)))))

(defun jsonify (value &key (hash-tables :normalize))
  "Coerce VALUE to a jzon-safe tree of strings, reals, booleans, vectors and
hash-tables. Keyword-keyed plists become objects with downcased keys and nil
values dropped, other proper lists become arrays, and anything else renders to
its printed string so serialization never faults on an exotic value.

HASH-TABLES selects the policy for hash-table values: :normalize rebuilds them
with downcased string keys and nil values dropped, the shape the event log
wants; :passthrough returns them unchanged so case-sensitive JSON-RPC argument
keys survive."
  (cond
    ((null value) nil)
    ((eq value t) t)
    ((keywordp value) (string-downcase (symbol-name value)))
    ((stringp value) value)
    ((realp value) value)
    ((%json-keyword-plist-p value) (%jsonify-plist value hash-tables))
    ((%json-proper-list-p value)
     (map 'vector (lambda (v) (jsonify v :hash-tables hash-tables)) value))
    ((hash-table-p value)
     (ecase hash-tables
       (:normalize (%jsonify-hash-table value hash-tables))
       (:passthrough value)))
    (t (princ-to-string value))))

(defun %jsonify-plist (plist hash-tables)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          when value
            do (setf (gethash (string-downcase (string key)) object)
                     (jsonify value :hash-tables hash-tables)))
    object))

(defun %jsonify-hash-table (table hash-tables)
  (let ((object (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (when value
                 (setf (gethash (string-downcase (princ-to-string key)) object)
                       (jsonify value :hash-tables hash-tables))))
             table)
    object))
