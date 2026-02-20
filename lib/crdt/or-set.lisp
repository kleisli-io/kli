(in-package #:crdt)

(defstruct (or-set (:conc-name ors-))
  (elements (make-hash-table :test 'equal) :type hash-table)
  (tombstones (make-hash-table :test 'equal) :type hash-table))

(defun ors-add (ors element tag)
  "Add ELEMENT with unique TAG. Mutates and returns ORS."
  (pushnew tag (gethash element (ors-elements ors) nil) :test #'equal)
  ors)

(defun ors-remove (ors element)
  "Remove ELEMENT by tombstoning all current tags. Mutates and returns ORS."
  (dolist (tag (gethash element (ors-elements ors)))
    (setf (gethash tag (ors-tombstones ors)) t))
  ors)

(defun ors-contains-p (ors element)
  "Return T if ELEMENT has any non-tombstoned tags."
  (some (lambda (tag)
          (not (gethash tag (ors-tombstones ors))))
        (gethash element (ors-elements ors))))

(defun ors-members (ors)
  "Return list of elements currently in the set."
  (let (result)
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (ors-contains-p ors k)
                 (push k result)))
             (ors-elements ors))
    result))

(defun ors-merge (ors1 ors2)
  "Merge two OR-Sets. Union elements+tags, union tombstones. Returns NEW or-set."
  (let ((result (make-or-set)))
    (dolist (ors (list ors1 ors2))
      (maphash (lambda (elem tags)
                 (dolist (tag tags)
                   (pushnew tag
                            (gethash elem (ors-elements result) nil)
                            :test #'equal)))
               (ors-elements ors)))
    (dolist (ors (list ors1 ors2))
      (maphash (lambda (tag v)
                 (declare (ignore v))
                 (setf (gethash tag (ors-tombstones result)) t))
               (ors-tombstones ors)))
    result))
