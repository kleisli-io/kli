(defpackage #:kli/object
  (:use #:cl)
  (:import-from #:kli
                #:live-object)
  (:import-from #:kli/ext
                #:defextension
                #:make-provider
                #:make-provider-contract)
  (:export
   #:standard-live-object
   #:standard-live-object-p
   #:make-standard-live-object
   #:object-kind
   #:object-owner
   #:object-source
   #:object-version
   #:object-state
   #:object-capabilities
   #:object-metadata
   #:make-standard-object-provider
   #:make-standard-object-contract
   #:*standard-object-extension-manifest*))
