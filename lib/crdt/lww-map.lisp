(in-package #:crdt)

(defstruct (lww-map (:conc-name lwwm-))
  (entries (make-hash-table :test 'equal) :type hash-table))

(defun lwwm-set (map key value timestamp session)
  "Set KEY to VALUE with LWW semantics. Mutates and returns MAP."
  (let ((reg (or (gethash key (lwwm-entries map))
                 (setf (gethash key (lwwm-entries map))
                       (make-lww-register)))))
    (lww-set reg value timestamp session))
  map)

(defun lwwm-get (map key)
  "Get current value for KEY, or NIL."
  (let ((reg (gethash key (lwwm-entries map))))
    (when reg (lww-value reg))))

(defun lwwm-keys (map)
  "Return list of all keys."
  (alexandria:hash-table-keys (lwwm-entries map)))

(defun lwwm-merge (map1 map2)
  "Merge two LWW-Maps. Returns NEW lww-map."
  (let ((result (make-lww-map)))
    (dolist (map (list map1 map2))
      (maphash (lambda (k reg)
                 (let ((existing (gethash k (lwwm-entries result))))
                   (setf (gethash k (lwwm-entries result))
                         (if existing
                             (lww-merge existing reg)
                             reg))))
               (lwwm-entries map)))
    result))
