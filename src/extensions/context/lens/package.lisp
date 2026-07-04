(defpackage #:kli/context/lens
  (:use #:cl)
  (:import-from #:kli
                #:next-keyword-id
                #:make-id-counter
                #:active-protocol
                #:context-registry
                #:live-object
                #:object-id
                #:register-live-object)
  (:import-from #:kli/ext
                #:defextension
                #:make-provider
                #:make-provider-contract
                #:require-capability-provider
                #:provider-call)
  (:import-from #:kli/event
                #:emit-event
                #:make-event)
  (:import-from #:let-over-lambda
                #:get-pandoric
                #:pandoriclet)
  (:import-from #:kli/session/log
                #:custom-entry-p
                #:entry-custom-type
                #:entry-data
                #:session-context-entries
                #:session-context-messages
                #:session-leaf-id
                #:serialize-record
                #:serialize-value
                #:deserialize-value
                #:define-record-deserializer
                #:make-record
                #:record-field
                #:advance-keyword-counter)
  (:export
   #:agent-context
   #:context-projection
   #:context-patch
   #:context-patch-set
   #:sealed-context
   #:context-capability

   #:make-context-patch
   #:make-append-message-patch
   #:make-remove-message-patch
   #:make-replace-message-patch
   #:context-patch-kind
   #:context-patch-payload
   #:context-patch-actor
   #:context-patch-timestamp
   #:context-patch-metadata
   #:context-patch-set-patches

   #:agent-context-session
   #:agent-context-store
   #:agent-context-leaf-id
   #:agent-context-projection
   #:agent-context-capsule

   #:projection-messages
   #:projection-entries
   #:projection-leaf-id
   #:projection-epoch
   #:sealed-context-messages
   #:sealed-context-epoch
   #:sealed-context-source-context-id
   #:sealed-context-leaf-id
   #:sealed-context-timestamp

   #:inspect-context
   #:context-capsule-value
   #:recode-context-capsule
   #:context-projected-messages
   #:context-staged-patches
   #:context-committed-patches

   #:make-agent-context
   #:rebuild-context-projection
   #:inspect-agent-context
   #:stage-context-patch
   #:commit-context-patches
   #:abort-context-patches
   #:seal-context-projection
   #:seal-range-projection
   #:context-model-messages
   #:context-epoch
   #:*context-lens-extension-manifest*))

(in-package #:kli/context/lens)
