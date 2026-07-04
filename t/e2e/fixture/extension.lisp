(in-package #:e2e-dir)

(defextension e2e-dir
  (:provides
   (tool ping
     :description "Returns a value folded across ordered component files."
     :parameters '(:object)
     :runner (lambda (tool parameters context &key call-id on-update)
               (declare (ignore tool parameters context call-id on-update))
               (princ-to-string *derived*)))))
