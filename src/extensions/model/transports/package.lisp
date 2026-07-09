(defpackage #:kli/model/transports
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object
                #:object-id)
  (:import-from #:kli/ext
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/text
                #:blank-string-p
                #:jsonify)
  (:import-from #:kli/model/runtime
                #:make-block-start-delta
                #:make-block-end-delta
                #:make-thinking-delta
                #:make-assistant-delta
                #:make-tool-call-delta
                #:make-usage-delta
                #:make-stop-reason-delta
                #:assistant-delta
                #:assistant-delta-text
                #:tool-call-delta
                #:tool-call-delta-call-id
                #:tool-call-delta-name
                #:tool-call-delta-arguments
                #:model-delta-content-index
                #:model-request-model-id
                #:model-request-provider-id
                #:model-request-model-messages
                #:model-request-instructions
                #:model-request-session-id
                #:model-request-tool-schemas
                #:model-request-selection
                #:model-request-stream
                #:note-model-stream-timing
                #:record-model-request-wire-input
                #:model-request-stream-closer)
  (:import-from #:kli/model/registry
                #:model-provider-credential-provider-id
                #:model-provider-config
                #:model-provider-metadata
                #:model-definition-metadata
                #:provider-config-base-url
                #:provider-config-headers
                #:model-selection-option-value)
  (:import-from #:kli/auth/core
                #:oauth-credential-reference
                #:oauth-credential-reference-account-id
                #:resolved-credential-value)
  (:export
   #:openai-responses-adapter
   #:openai-api-error
   #:openai-api-error-body
   #:terminal-openai-usage-limit-error-p
   #:*responses-http*
   #:*codex-websocket-stream*
   #:*transport-connect*
   #:open-cancellable-stream
   #:stream-sse-events
   #:model-network-error
   #:model-network-error-cause
   #:model-stream-format-error
   #:model-stream-format-error-payload
   #:model-stream-format-error-cause
   #:model-stream-overflow-error
   #:*sse-max-line-length*
   #:*sse-max-event-size*
   #:*error-body-cap*
   #:*provider-error-display-cap*
   #:provider-error-display
   #:parse-sse-payload
   #:call-classifying-network-errors
   #:map-responses-event
   #:convert-responses-input
   #:tool-parameters->json-schema
   #:transport-supports-option-p
   #:build-responses-body
   #:responses-url
   #:build-responses-headers
   #:build-codex-websocket-headers
   #:%responses-endpoint
   #:openai-completions-adapter
   #:*completions-http*
   #:map-completions-chunk
   #:finish-completions
   #:make-completions-state
   #:convert-completions-messages
   #:build-completions-body
   #:build-completions-headers
   #:completions-url
   #:anthropic-messages-adapter
   #:anthropic-api-error
   #:*anthropic-http*
   #:map-anthropic-event
   #:make-anthropic-state
   #:convert-anthropic-messages
   #:build-anthropic-body
   #:build-anthropic-headers
   #:anthropic-url
   #:ensure-stream-adapter
   #:stream-socket-fd))

(in-package #:kli/model/transports)
