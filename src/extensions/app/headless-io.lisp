(in-package #:kli/app)

(defmacro with-headless-io ((&key (result '*standard-output*)
                                  (diagnostics '*error-output*))
                            &body body)
  "Run BODY with the headless result channel bound to RESULT and the diagnostic
channel to DIAGNOSTICS, so a headless surface writes its machine-consumable
output to one stream and every diagnostic to another. The defaults preserve the
ambient stdout/stderr split; a caller passes string streams to capture each
channel independently."
  `(let ((*standard-output* ,result)
         (*error-output* ,diagnostics))
     ,@body))
