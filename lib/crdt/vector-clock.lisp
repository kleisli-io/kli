(in-package #:crdt)

(defstruct (vector-clock (:conc-name vc-))
  (entries (make-hash-table :test 'equal) :type hash-table))

(defun vc-increment (vc session-id)
  "Increment clock for SESSION-ID. Mutates and returns VC."
  (incf (gethash session-id (vc-entries vc) 0))
  vc)

(defun vc-get (vc session-id)
  "Get clock value for SESSION-ID (0 if absent)."
  (gethash session-id (vc-entries vc) 0))

(defun vc-<= (vc1 vc2)
  "Return T if VC1 is pointwise <= VC2."
  (block check
    (maphash (lambda (k v)
               (when (> v (gethash k (vc-entries vc2) 0))
                 (return-from check nil)))
             (vc-entries vc1))
    t))

(defun vc-merge (vc1 vc2)
  "Merge two vector clocks (pointwise max). Returns NEW vector-clock."
  (let ((result (make-vector-clock)))
    (maphash (lambda (k v)
               (setf (gethash k (vc-entries result))
                     (max v (gethash k (vc-entries vc2) 0))))
             (vc-entries vc1))
    (maphash (lambda (k v)
               (unless (gethash k (vc-entries result))
                 (setf (gethash k (vc-entries result)) v)))
             (vc-entries vc2))
    result))

(defun vc-happened-before-p (vc1 vc2)
  "Return T if VC1 happened strictly before VC2."
  (and (vc-<= vc1 vc2)
       (not (vc-<= vc2 vc1))))

(defun vc-concurrent-p (vc1 vc2)
  "Return T if VC1 and VC2 are concurrent (neither dominates)."
  (and (not (vc-<= vc1 vc2))
       (not (vc-<= vc2 vc1))))

(defun vc-equal-p (vc1 vc2)
  "Return T if two vector clocks are equal."
  (and (vc-<= vc1 vc2) (vc-<= vc2 vc1)))
