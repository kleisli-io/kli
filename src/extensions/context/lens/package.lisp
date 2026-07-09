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
                #:entry-agent-message
                #:transcript-repair-entry
                #:entry-repair-kind
                #:entry-repair-policy
                #:make-transcript-repair-entry
                #:session-context-entries
                #:session-context-messages
                #:session-leaf-id
                #:message-role
                #:message-metadata
                #:tool-call-id
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
   #:context-view
   #:context-view-item
   #:context-view-payload
   #:sealed-context-view
   #:context-view-validation-error
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
   #:context-view-kind
   #:context-view-source-id
   #:context-view-source-epoch
   #:context-view-items
   #:context-view-provenance
   #:context-view-policy
   #:context-view-item-id
   #:context-view-item-payload
   #:context-view-item-payload-kind
   #:context-view-item-payload-value
   #:context-view-item-ordinal
   #:context-view-item-group-id
   #:context-view-payload-kind
   #:context-view-payload-value
   #:context-view-payload-metadata
   #:context-view-validation-diagnostic
   #:context-view-provenance-for-item
   #:sealed-context-view
   #:sealed-context-view-sealed-id
   #:sealed-context-view-base-view-kind
   #:sealed-context-view-leaf-id
   #:sealed-context-view-sealed-at
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
   #:editable-context-view
   #:transcript-context-view
   #:summarizer-context-view
   #:provider-replay-context-view
   #:resolve-transcript-repair-policy
   #:transcript-display-items
   #:summarizer-input-items
   #:provider-replay-items
   #:provider-replay-messages
   #:context-epoch
   #:*context-lens-extension-manifest*))

(in-package #:kli/context/lens)
