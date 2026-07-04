(defpackage #:kli/runtime/history
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:live-object
                #:object-id
                #:object-protocol)
  (:import-from #:kli/ext
                #:defextension
                #:define-capability-binding
                #:make-provider
                #:make-provider-contract)
  (:export
   #:history-service
   #:make-history-service
   #:record-history-entry
   #:history-entries
   #:history-switch-protocol
   #:history-recover-protocol
   #:*history-extension-manifest*))
