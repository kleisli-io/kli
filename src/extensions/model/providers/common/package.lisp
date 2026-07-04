(defpackage #:kli/model/providers/common
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object)
  (:import-from #:kli/ext
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/model/registry
                #:make-model-provider
                #:make-model-definition
                 #:make-model-option-schema
                 #:model-option-schema-option-id)
  (:import-from #:kli/model/transports
                #:ensure-stream-adapter
                 #:transport-supports-option-p)
  (:export
   #:install-provider-catalogue
   #:retract-provider-catalogue))

(in-package #:kli/model/providers/common)
