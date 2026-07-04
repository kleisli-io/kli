(defpackage #:kli/event
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:protocol)
  (:import-from #:kli/ext
                #:contribution
                #:contribution-name
                #:defcontribution-kind
                #:defextension
                #:deregister-fault-note-emitter
                #:ensure-protocol-storage
                #:extension-protocol
                #:install-contribution
                #:make-provider
                #:make-provider-contract
                #:normalize-extension-id
                #:protocol-installed-contributions
                #:protocol-storage
                #:register-author-clause-requirements
                #:register-fault-note-emitter
                #:retract-contribution
                #:safely-invoke)
  (:export
   #:event
   #:make-event
   #:event-type
   #:event-payload
   #:event-source
   #:event-timestamp
   #:dispatch-event
   #:emit-event
   #:make-fault-emitter
   #:protocol-fault-emitter
   #:make-event-provider
   #:make-event-contract
   #:event-type-contribution
   #:make-event-type-contribution
   #:event-handler-contribution
   #:make-event-handler-contribution
   #:on-contribution-name
   #:contribution-event-type
   #:contribution-handler
   #:*events-extension-manifest*
   #:protocol-event-types))
