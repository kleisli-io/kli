(in-package #:kli/runtime/snapshot)

;;;; Snapshot contract, declared ahead of every implementer. Default methods and
;;;; serialize machinery live in extension.lisp (loads last).

(defgeneric snapshot-representation (object)
  (:documentation
   "OBJECT's durable state as a plist of representation keys. The default
captures serializable bound slots; a stateful object overrides to declare
exactly what it carries and what it deliberately omits."))

(defgeneric restore-representation (object datum context)
  (:documentation
   "Reconstruct OBJECT's live state from DATUM, the record that
`snapshot-representation` produced. The default writes captured slots back."))
