(defpackage #:kli/runtime/introspection
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object
                #:map-live-objects
                #:object-id)
  (:import-from #:kli/ext
                #:defextension
                #:make-provider
                #:make-provider-contract)
  (:import-from #:kli/object
                #:standard-live-object-p
                #:object-kind
                #:object-source
                #:object-version)
  (:export
   #:registry-object-ids
   #:describe-live-object
   #:object-id-string
   #:describe-by-id
   #:context-summary
   #:*introspection-extension-manifest*))
