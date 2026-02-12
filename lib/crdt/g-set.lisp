(in-package #:crdt)

(defstruct (g-set (:conc-name gs-))
  (elements (make-hash-table :test 'equal) :type hash-table))

(defun gs-add (gs element)
  "Add ELEMENT. Idempotent. Mutates and returns GS."
  (setf (gethash element (gs-elements gs)) t)
  gs)

(defun gs-contains-p (gs element)
  "Return T if ELEMENT is in the set."
  (gethash element (gs-elements gs)))

(defun gs-members (gs)
  "Return list of all elements."
  (alexandria:hash-table-keys (gs-elements gs)))

(defun gs-count (gs)
  "Return number of elements."
  (hash-table-count (gs-elements gs)))

(defun gs-merge (gs1 gs2)
  "Merge two G-Sets (union). Returns NEW g-set."
  (let ((result (make-g-set)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (setf (gethash k (gs-elements result)) t))
             (gs-elements gs1))
    (maphash (lambda (k v)
               (declare (ignore v))
               (setf (gethash k (gs-elements result)) t))
             (gs-elements gs2))
    result))
