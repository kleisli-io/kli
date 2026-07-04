(in-package #:kli/model/runtime)

(defun make-model-runtime-contract ()
  (make-provider-contract
   :id :model/runtime/v1
   :capability :model/runtime
   :required-entries
   '(:make-model-request
     :complete-text
     :register-model-stream-adapter
     :unregister-model-stream-adapter
     :find-model-stream-adapter
     :stream-model-response
     :handle-model-delta
     :abort-model-request
     :inspect-model-request
     :inspect-model-response
     :inspect-model-stream)))

(defun make-model-runtime-provider ()
  (make-provider
   :id :model-runtime-provider
   :capability :model/runtime
   :contracts '(:model/runtime/v1)
   :entries
   (list :make-model-request #'make-model-request
         :complete-text #'complete-text
         :register-model-stream-adapter #'register-model-stream-adapter
         :unregister-model-stream-adapter #'unregister-model-stream-adapter
         :find-model-stream-adapter #'find-model-stream-adapter
         :stream-model-response #'stream-model-response
         :handle-model-delta #'handle-model-delta
         :abort-model-request #'abort-model-request
         :inspect-model-request #'inspect-model-request
         :inspect-model-response #'inspect-model-response
         :inspect-model-stream #'inspect-model-stream)))

(defextension model-runtime
  (:requires
   (capability auth :contract auth/v1)
   (capability context/lens :contract context/lens/v1)
   (capability model/registry :contract model/registry/v1)
   (capability session/log :contract session/log/v1)
   (capability session/entries :contract session/entries/v1))
  (:provides
   (contract model/runtime/v1
     (make-model-runtime-contract))
   (capability model/runtime (make-model-runtime-provider))
   (live-object model-runtime
     (make-model-runtime))))
