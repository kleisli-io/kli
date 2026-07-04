(defpackage #:kli/model/commands
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
  (:import-from #:kli/text
                #:trim-whitespace
                #:blank-string-p
                #:split-on-whitespace)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content
                #:argument-words)
  (:import-from #:kli/model/registry
                #:registry-providers
                #:registry-models
                #:make-model-selection
                #:model-definition-provider-id
                #:model-definition-model-id
                #:model-definition-name
                #:model-definition-option-schemas
                #:model-option-definition-enum-values
                #:model-option-schema-for
                #:model-option-schema-option-id
                #:model-option-schema-values
                #:find-model-option-definition
                #:parse-model-option-value
                #:model-supports-option-p
                #:preserve-valid-model-options
                #:model-provider-provider-id
                #:model-provider-auth-required-p
                #:model-provider-credential-provider-id
                #:model-selection-provider-id
                #:model-selection-model-id
                #:model-selection-options
                 #:model-selection-option-value)
  (:import-from #:kli/agent/session
                #:set-agent-session-model
                #:set-agent-session-option
                #:mode-current-selection)
  (:import-from #:kli/auth/oauth
                #:pkce-verifier
                #:pkce-challenge
                #:oauth-state
                #:build-authorize-url
                #:parse-authorization-input
                #:token-exchange
                #:jwt-account-id
                #:token-endpoint-error
                #:token-network-error
                #:pending-login
                #:pending-logins
                #:clear-pending-login)
  (:export
   #:make-model-command
   #:make-models-command
   #:make-providers-command
   #:make-thinking-command
   #:make-auth-command
   #:model-completion
   #:thinking-completion
   #:reasoning-effort-for-word
   #:auth-completion
   #:register-model-commands
   #:unregister-model-commands
   #:*model-commands-extension-manifest*))

(in-package #:kli/model/commands)
