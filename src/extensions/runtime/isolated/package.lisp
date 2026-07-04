(defpackage #:kli/runtime/isolated
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:with-extension-fault-barrier
                #:tool-abort-requested-p
                #:make-tool
                #:tool-name
                #:tool-metadata
                #:make-tool-result
                #:make-tool-text-content
                #:make-extension
                #:normalize-extension-id
                #:make-effect-contribution
                #:make-tool-contribution
                #:make-resource
                #:make-resource-contribution
                #:install-contribution
                #:contribution-extension
                #:contribution-state)
  (:import-from #:kli/event
                #:make-event
                #:emit-event)
  (:import-from #:kli/text
                #:render-bounded-lines)
  (:import-from #:kli/output-spill
                #:write-string-spill
                #:spill-entry-token
                #:spill-entry-bytes
                #:format-spill-marker)
  (:export
   ;; persistent transport
   #:isolated-process
   #:isolated-process-p
   #:spawn-isolated-process
   #:call-isolated
   #:notify-isolated
   #:teardown-isolated
   #:isolated-process-alive-p
   ;; stream-level framing
   #:write-jsonrpc-request
   #:write-jsonrpc-notification
   #:write-jsonrpc-response
   #:write-jsonrpc-error
   #:read-jsonrpc-message
   #:classify-message
   #:message-id
   #:message-method
   #:message-params
   #:message-result
   #:message-error
   ;; conditions
   #:isolated-error
   #:isolated-protocol-error
   #:isolated-timeout
   #:isolated-aborted
   #:isolated-jsonrpc-error
   #:isolated-transport-closed
   #:isolated-jsonrpc-error-payload
   #:isolated-error-method
   #:isolated-error-timeout
   #:isolated-error-detail
   ;; tunables
   #:*default-isolated-timeout*
   #:*reader-poll-interval*
   #:*max-message-length*
   #:*mcp-tool-result-character-limit*
   ;; membrane: MCP wire shapes mapped to kli analogues
   #:inputschema->parameters
   #:content->tool-result
   #:mcp-resource->plist
   #:read-contents->plists
   #:notification->event
   ;; MCP client over the transport
   #:+mcp-protocol-version+
   #:*default-client-name*
   #:*default-client-version*
   #:mcp-client
   #:mcp-client-p
   #:mcp-client-process
   #:mcp-client-server-info
   #:mcp-client-server-capabilities
   #:mcp-client-protocol-version
   #:mcp-client-context
   #:mcp-connect
   #:mcp-list-tools
   #:mcp-call-tool
   #:mcp-list-resources
   #:mcp-read-resource
   #:mcp-disconnect
   ;; inbound lift: an MCP server as a first-class kli manifest
   #:lifted-server-id
   #:lifted-server-capability
   #:lift-mcp-server))
