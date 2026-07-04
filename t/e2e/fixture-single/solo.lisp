(defextension e2e-solo
  (:provides
   (tool ping
     :description "Single-file extension probe."
     :parameters '(:object)
     :runner (lambda (tool parameters context &key call-id on-update)
               (declare (ignore tool parameters context call-id on-update))
               "solo-ok"))))
