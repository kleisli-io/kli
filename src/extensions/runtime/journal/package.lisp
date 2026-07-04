(defpackage #:kli/runtime/journal
  (:use #:cl)
  (:import-from #:kli
                #:live-object)
  (:import-from #:kli/ext
                #:defextension
                #:make-provider
                #:make-provider-contract)
  (:export
   #:journal-service
   #:make-journal-service
   #:make-journal-entry
   #:record-journal-entry
   #:journal-entries
   #:*journal-extension-manifest*))
