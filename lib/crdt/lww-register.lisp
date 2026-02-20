(in-package #:crdt)

(defstruct (lww-register (:conc-name lww-))
  (value nil :type t)
  (timestamp 0 :type integer)
  (session "" :type string))

(defun lww-set (reg value timestamp session)
  "Set value if TIMESTAMP is strictly newer. Mutates and returns REG."
  (when (> timestamp (lww-timestamp reg))
    (setf (lww-value reg) value
          (lww-timestamp reg) timestamp
          (lww-session reg) session))
  reg)

(defun lww-merge (reg1 reg2)
  "Merge two LWW-Registers (higher timestamp wins). Returns NEW register."
  (if (>= (lww-timestamp reg1) (lww-timestamp reg2))
      (make-lww-register :value (lww-value reg1)
                         :timestamp (lww-timestamp reg1)
                         :session (lww-session reg1))
      (make-lww-register :value (lww-value reg2)
                         :timestamp (lww-timestamp reg2)
                         :session (lww-session reg2))))
