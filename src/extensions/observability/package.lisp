(defpackage #:kli/observability
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object
                #:live-object)
  (:import-from #:kli/ext
                #:defextension
                #:provider-call
                #:require-capability-provider
                #:contribution-extension
                #:contribution-state
                #:extension-protocol)
  (:import-from #:kli/event
                #:event
                #:event-type
                #:event-payload
                #:event-source
                #:event-timestamp
                #:dispatch-event)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content)
  (:import-from #:kli/config
                #:find-config-service
                #:config-service-settings
                #:settings-value
                #:expand-config-path)
  (:export
   #:register-observability
   #:unregister-observability
   #:*observability-extension-manifest*
   #:sink-path
   #:sink-write-count))

(in-package #:kli/observability)
