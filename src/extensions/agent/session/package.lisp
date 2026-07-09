(defpackage #:kli/agent/session
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object
                #:live-object
                #:object-id)
  (:import-from #:kli/ext
                #:defextension
                #:find-capability-provider
                #:make-provider
                #:make-provider-contract
                #:provider-call
                #:require-capability
                #:require-capability-provider
                #:capabilities-subject
                #:make-unrestricted-subject
                #:list-tools
                #:tool-name
                #:tool-description)
  (:import-from #:let-over-lambda
                #:pandoriclet)
  (:import-from #:kli/context/lens
                #:seal-context-projection
                #:provider-replay-items
                #:context-view-item-id
                #:context-view-item-group-id
                #:context-view-item-payload-kind
                #:context-view-item-payload-value
                #:context-view-provenance-for-item)
  (:import-from #:kli/model/runtime
                #:materialize-provider-replay-items)
  (:import-from #:kli/model/registry
                #:selection-context-window
                #:model-selection-provider-id
                #:model-selection-model-id
                #:model-provider-api)
  (:import-from #:kli/model/transports
                #:terminal-openai-usage-limit-error-p
                #:convert-responses-input
                #:convert-completions-messages
                #:convert-anthropic-messages)
  (:export
   #:agent-session
   #:agent-session-binding
   #:agent-context-binding
   #:mode-binding
   #:session-event-listener
   #:context-usage

   #:session-mode-bindings
   #:session-active-mode-id
   #:session-restore-unfocused-mode
   #:session-source->mode
   #:session-event-listeners
   #:session-event-persistence-policy
   #:session-prompt-expansion-policy
   #:session-context-transform-policy
   #:session-retry-policy
   #:session-compaction-policy
   #:mode-binding-mode-id
   #:mode-binding-session-binding
   #:mode-binding-context-binding
   #:mode-binding-agent-id
   #:mode-binding-compaction-interrupt
   #:session-binding-session-id
   #:session-binding-leaf-id
   #:context-binding-context-id
   #:context-binding-rebuilt-at
   #:context-binding-usage
   #:context-binding-live-usage
   #:listener-id
   #:listener-handler
   #:listener-filter
   #:listener-registered-by
   #:usage-input-tokens
   #:usage-output-tokens
   #:usage-cache-read-tokens
   #:usage-cache-write-tokens
   #:usage-total-tokens
   #:usage-over-threshold-p
   #:provider-accounting-result
   #:make-provider-accounting-result
   #:provider-accounting-result-provider-id
   #:provider-accounting-result-model-id
   #:provider-accounting-result-api
   #:provider-accounting-result-units
   #:provider-accounting-result-exact-p
   #:provider-accounting-result-upper-bound-p
   #:provider-accounting-result-total
   #:provider-accounting-result-attributions
   #:provider-accounting-result-group-costs
   #:provider-accounting-result-entry-costs
   #:provider-accounting-result-wire-input
   #:account-provider-replay

   #:submit-agent-session-prompt
   #:steer-agent-session
   #:queue-agent-session-steer
   #:drain-agent-session-work
   #:follow-up-agent-session
   #:switch-agent-session
   #:focus-agent-session-mode
   #:branch-agent-session
   #:rewind-agent-session
   #:rewind-agent-session-available-p
   #:list-rewind-targets
   #:reset-agent-session
   #:configured-agent-subject
   #:root-agent-subject
   #:clear-agent-session
   #:abort-agent-session
   #:agent-session-busy-p
   #:agent-session-context
   #:current-agent-session-leaf-id
   #:retract-agent-session-pending-prompt
   #:register-session-event-listener
   #:unregister-session-event-listener
   #:persist-agent-event
   #:persist-context-patch
   #:execute-session-compaction
   #:set-agent-session-model
   #:set-agent-session-option
   #:inspect-agent-session
   #:session-mode-info
   #:mode-current-selection
   #:rename-agent-session
   #:resume-agent-session
   #:compact-agent-session

   #:make-agent-session-service
   #:make-mode-binding
   #:make-session-event-listener
   #:make-context-usage

   #:make-event-persistence-policy
   #:make-prompt-expansion-policy
   #:make-context-transform-policy
   #:make-retry-policy
   #:transient-model-error-p
   #:make-compaction-policy
   #:recode-event-persistence-policy
   #:recode-prompt-expansion-policy
   #:recode-context-transform-policy
   #:add-system-prompt-layer
   #:remove-system-prompt-layer
   #:recode-retry-policy
   #:recode-compaction-policy

   #:default-agent-system-prompt
   #:serialize-conversation

   #:agent-session-error
   #:agent-session-error-reason
   #:agent-session-error-provider-id

   #:make-agent-session-contract
   #:make-agent-session-provider
   #:*agent-session-extension-manifest*))

(in-package #:kli/agent/session)
