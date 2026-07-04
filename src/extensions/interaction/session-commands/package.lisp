(defpackage #:kli/interaction/session-commands
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object)
  (:import-from #:kli/ext
                #:contribution-extension
                #:contribution-state
                #:defextension
                #:find-capability-provider
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content)
  (:import-from #:kli/agent/session
                #:session-mode-info
                #:session-mode-bindings
                #:rename-agent-session
                #:resume-agent-session
                #:rewind-agent-session
                #:list-rewind-targets
                #:compact-agent-session
                #:agent-session-busy-p
                #:session-restore-unfocused-mode
                #:usage-total-tokens)
  (:import-from #:kli/session/log
                #:list-stored-sessions
                #:session-forest
                #:session-row-branch-label
                #:blank-session-row-p
                #:delete-stored-session)
  (:export
   #:register-session-commands
   #:unregister-session-commands
   #:*session-commands-extension-manifest*))

(in-package #:kli/interaction/session-commands)
