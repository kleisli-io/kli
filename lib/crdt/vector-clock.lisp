(in-package #:crdt)

(defstruct (vector-clock (:conc-name vc-))
  (entries (make-hash-table :test 'equal) :type hash-table))

(defun vc-increment (vc session-id)
  "Increment clock for SESSION-ID. Mutates and returns VC."
  (incf (gethash session-id (vc-entries vc) 0))
  vc)

(defun vc-copy (vc)
  "Return a fresh VECTOR-CLOCK whose entries hash table is a shallow
   copy of VC's entries.  Use to give each thread an independent
   working clock when multiple requests share a session VC in the
   registry — SBCL hash tables are not safe for concurrent mutation,
   even on different keys."
  (let* ((src (vc-entries vc))
         (dst (make-hash-table :test (hash-table-test src)
                               :size (max 4 (hash-table-count src)))))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    (make-vector-clock :entries dst)))

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

(defun vc-equal-p (vc1 vc2)
  "Return T if two vector clocks are equal."
  (and (vc-<= vc1 vc2) (vc-<= vc2 vc1)))
