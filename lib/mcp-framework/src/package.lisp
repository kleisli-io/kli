;;; MCP Framework - Package definitions
;;; A comprehensive framework for building MCP servers in Common Lisp

(defpackage #:mcp-framework
  (:use #:cl #:alexandria)
  (:import-from #:let-over-lambda
                #:dlambda
                #:pandoriclet
                #:get-pandoric
                #:with-pandoric)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-condition-variable
                #:condition-wait
                #:condition-notify
                #:make-thread
                #:join-thread
                #:thread-alive-p)
  ;; Conditions
  (:export #:mcp-error
           #:mcp-error-code
           #:mcp-error-message
           #:mcp-error-data
           #:jsonrpc-parse-error
           #:invalid-request
           #:method-not-found
           #:invalid-params
           #:internal-error
           #:tool-not-found
           #:tool-execution-error
           #:resource-not-found
           #:prompt-not-found)
  ;; Content types
  (:export #:make-text-content
           #:make-image-content
           #:make-resource-content
           #:text-content
           #:content-type
           #:content-text
           #:content-data
           #:content-mime-type
           #:content-uri
           #:content-to-json)
  ;; Schema generation
  (:export #:generate-schema
           #:param-to-json-type
           #:*type-mappings*)
  ;; Protocol
  (:export #:parse-jsonrpc-message
           #:encode-jsonrpc-response
           #:encode-jsonrpc-error
           #:handle-mcp-message
           #:register-mcp-handler
           #:*mcp-handlers*)
  ;; Tools
  (:export #:make-pandoric-tool
           #:register-tool
           #:unregister-tool
           #:get-tool
           #:list-tools
           #:clear-tools
           #:call-tool
           #:inspect-tool
           #:evolve-tool
           #:devolve-tool
           #:export-tool-state
           #:import-tool-state
           #:*tool-registry*)
  ;; Tool macros
  (:export #:define-tool
           #:lisp-name-to-mcp-name
           #:mcp-name-to-lisp-name)
  ;; Transport
  (:export #:transport
           #:transport-running-p
           #:start-transport
           #:stop-transport
           #:receive-message
           #:send-message
           #:transport-serve
           #:stdio-transport
           #:make-stdio-transport)
  ;; Server
  (:export #:mcp-server
           #:make-server
           #:server-name
           #:server-version
           #:server-transport
           #:server-running-p
           #:start-server
           #:stop-server
           #:run-server)
  ;; Hooks
  (:export #:*before-tool-call-hooks*
           #:*after-tool-call-hooks*
           #:add-before-tool-hook
           #:add-after-tool-hook
           #:remove-before-tool-hook
           #:remove-after-tool-hook
           #:clear-tool-hooks
           #:call-with-hooks)
  ;; Resources
  (:export #:mcp-resource
           #:make-mcp-resource
           #:resource-uri
           #:resource-name
           #:resource-description
           #:resource-mime-type
           #:resource-handler
           #:register-resource
           #:unregister-resource
           #:get-resource
           #:list-resources
           #:clear-resources
           #:read-resource
           #:define-resource
           #:*resource-registry*)
  ;; Prompts
  (:export #:mcp-prompt
           #:make-mcp-prompt
           #:prompt-name
           #:prompt-description
           #:prompt-arguments
           #:prompt-handler
           #:prompt-argument
           #:make-prompt-argument
           #:prompt-arg-name
           #:prompt-arg-description
           #:prompt-arg-required
           #:register-prompt
           #:unregister-prompt
           #:get-prompt
           #:list-prompts
           #:clear-prompts
           #:make-user-message
           #:make-assistant-message
           #:define-prompt
           #:arguments  ; Anaphor for define-prompt body
           #:*prompt-registry*)
  ;; Tool Evolution (opt-in)
  (:export #:enable-tool-evolution-handlers
           #:disable-tool-evolution-handlers
           #:*evolution-handlers-enabled*
           #:*before-evolution-hooks*
           #:add-before-evolution-hook
           #:remove-before-evolution-hook
           #:clear-evolution-hooks))
