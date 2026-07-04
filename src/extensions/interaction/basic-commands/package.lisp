(defpackage #:kli/interaction/basic-commands
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object)
  (:import-from #:kli/ext
                #:contribution-extension
                #:contribution-state
                #:defextension
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/agent/session
                #:reset-agent-session)
  (:import-from #:kli/interaction/commands
                #:command-description
                #:command-label
                #:command-name
                #:command-usage-text
                #:make-command
                #:make-command-result
                #:make-command-text-content)
  (:export
   #:register-basic-commands
   #:unregister-basic-commands
   #:*basic-commands-extension-manifest*))
