(defpackage #:kli/tools/eval
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:live-object
                #:object-id
                #:context-registry
                #:register-live-object
                #:find-live-object
                #:remove-live-object)
  (:import-from #:kli/agent/session
                #:add-system-prompt-layer
                #:remove-system-prompt-layer)
  (:import-from #:paren-repair
                #:repair-if-needed)
  (:import-from #:kli/ext
                #:*call-subject*
                #:contribution-extension
                #:contribution-name
                #:contribution-state
                #:contribution-tool
                #:defextension
                #:install-contribution
                #:invoke-tool
                #:list-tool-contributions
                #:make-effect-contribution
                #:make-tool-result
                #:make-tool-text-content
                #:provider-call
                #:require-capability-provider
                #:retract-contribution
                #:tool-abort-requested-p
                #:tool-parameter
                #:tool-result-content
                #:tool-result-details
                #:tool-result-error-p)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:with-operator-capability)
  (:import-from #:kli/output-spill
                #:open-spill-tee
                #:finalize-spill-tee
                #:discard-spill-tee
                #:spill-tee-stream
                #:tee-window
                #:tee-truncated-p
                #:spill-entry-token
                #:spill-entry-bytes
                #:format-spill-marker)
  (:import-from #:let-over-lambda
                #:defmacro!)
  (:export
   #:run-eval-tool
   #:run-eval-continue-tool
   #:run-eval-abort-tool
   #:run-recompile-rerun-tool
   #:recompile-and-rerun
   #:eval-park-thread
   #:*eval-output-character-limit*
   #:*eval-value-character-limit*
   #:run-eval-command
   #:register-eval-command
   #:unregister-eval-command
   #:*eval-tool-extension-manifest*
   #:*eval-continue-tool-extension-manifest*
   #:*eval-abort-tool-extension-manifest*
   #:*recompile-rerun-tool-extension-manifest*
   #:*eval-command-extension-manifest*))
