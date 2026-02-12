(in-package #:task)

(defstruct event
  (id "" :type string)
  (timestamp 0 :type integer)
  (session "" :type string)
  (clock (make-vector-clock) :type vector-clock)
  (type :unknown :type keyword)
  (data nil :type list))

;;; --- Serialization helpers ---

(defun plist-p (obj)
  "Check if OBJ is a plist (non-empty list starting with a keyword)."
  (and (consp obj)
       (keywordp (car obj))
       (evenp (length obj))))

(defun plist-to-ht (plist)
  "Convert plist to hash-table for yason serialization.
   Recursively converts nested plists."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht)
                   (if (plist-p v)
                       (plist-to-ht v)  ; Recursively convert nested plists
                       v)))
    ht))

(defun alist-p (obj)
  "Check if OBJ is an alist (list of cons cells with string keys)."
  (and (consp obj)
       (consp (car obj))
       (stringp (caar obj))))

(defun alist-to-plist (alist)
  "Convert alist with string keys to plist with keyword keys.
   Recursively converts nested alists."
  (loop for (k . v) in alist
        collect (intern (string-upcase k) :keyword)
        collect (if (alist-p v)
                    (alist-to-plist v)  ; Recursively convert nested alists
                    v)))

(defun vc-to-ht (vc)
  "Convert vector-clock entries to hash-table for JSON."
  (let ((ht (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k ht) v))
             (vc-entries vc))
    ht))

;;; --- JSON serialization ---

(defun event-to-json-string (ev)
  "Serialize event to JSON string."
  (with-output-to-string (s)
    (yason:encode-alist
     (list (cons "id" (event-id ev))
           (cons "timestamp" (event-timestamp ev))
           (cons "session" (event-session ev))
           (cons "clock" (vc-to-ht (event-clock ev)))
           (cons "type" (string-downcase (symbol-name (event-type ev))))
           (cons "data" (if (event-data ev)
                            (plist-to-ht (event-data ev))
                            (make-hash-table))))
     s)))

(defun json-string-to-event (str)
  "Parse JSON string to event struct."
  (let* ((alist (yason:parse str :object-as :alist))
         (clock-alist (cdr (assoc "clock" alist :test #'string=)))
         (vc (make-vector-clock)))
    (loop for (k . v) in clock-alist
          do (setf (gethash k (vc-entries vc)) v))
    (let ((data-alist (cdr (assoc "data" alist :test #'string=))))
      (make-event
       :id (cdr (assoc "id" alist :test #'string=))
       :timestamp (cdr (assoc "timestamp" alist :test #'string=))
       :session (cdr (assoc "session" alist :test #'string=))
       :clock vc
       :type (intern (string-upcase
                      (cdr (assoc "type" alist :test #'string=)))
                     :keyword)
       :data (when data-alist (alist-to-plist data-alist))))))
