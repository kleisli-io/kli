(defpackage #:kli/context/commands
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object
                #:object-id)
  (:import-from #:kli/ext
                #:contribution-extension
                #:contribution-state
                #:defextension
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/text
                #:trim-whitespace
                #:blank-string-p
                #:whitespace-char-p
                #:split-on-whitespace)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content
                #:argument-words)
  (:import-from #:kli/context/lens
                #:context-staged-patches
                #:context-projected-messages
                #:context-patch-kind
                #:context-patch-payload
                #:context-epoch
                #:stage-context-patch
                #:commit-context-patches
                #:abort-context-patches
                #:make-append-message-patch
                #:make-remove-message-patch
                #:make-replace-message-patch)
  (:import-from #:kli/session/log
                #:make-user-message
                #:message-content)
  (:import-from #:kli/agent/session
                #:agent-session-context)
  (:export
   #:register-context-commands
   #:unregister-context-commands
   #:*context-commands-extension-manifest*))

(in-package #:kli/context/commands)
