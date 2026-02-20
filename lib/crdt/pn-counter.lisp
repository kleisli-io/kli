(in-package #:crdt)

(defstruct (pn-counter (:conc-name pnc-))
  (positive (make-hash-table :test 'equal) :type hash-table)
  (negative (make-hash-table :test 'equal) :type hash-table))

(defun pnc-increment (pnc session-id &optional (amount 1))
  "Increment counter for SESSION-ID. Mutates and returns PNC."
  (incf (gethash session-id (pnc-positive pnc) 0) amount)
  pnc)

(defun pnc-decrement (pnc session-id &optional (amount 1))
  "Decrement counter for SESSION-ID. Mutates and returns PNC."
  (incf (gethash session-id (pnc-negative pnc) 0) amount)
  pnc)

(defun pnc-value (pnc)
  "Net value: sum(positive) - sum(negative)."
  (let ((pos 0) (neg 0))
    (maphash (lambda (k v) (declare (ignore k)) (incf pos v))
             (pnc-positive pnc))
    (maphash (lambda (k v) (declare (ignore k)) (incf neg v))
             (pnc-negative pnc))
    (- pos neg)))

(defun pnc-merge (pnc1 pnc2)
  "Merge two PN-Counters (max per session). Returns NEW pn-counter."
  (let ((result (make-pn-counter)))
    (dolist (pnc (list pnc1 pnc2))
      (maphash (lambda (k v)
                 (setf (gethash k (pnc-positive result))
                       (max v (gethash k (pnc-positive result) 0))))
               (pnc-positive pnc)))
    (dolist (pnc (list pnc1 pnc2))
      (maphash (lambda (k v)
                 (setf (gethash k (pnc-negative result))
                       (max v (gethash k (pnc-negative result) 0))))
               (pnc-negative pnc)))
    result))
