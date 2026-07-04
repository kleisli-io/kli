(defpackage #:kli/runtime/mcp-server
  (:use #:cl)
  (:local-nicknames (#:ext #:kli/ext)
                    (#:tx  #:kli/model/transports)
                    (#:rpc #:kli/runtime/isolated))
  (:export #:*protocol-version*
           #:*supported-protocol-versions*
           #:*server-name*
           #:*server-version*
           #:extension-surface
           #:surface-subject
           #:surface-tools
           #:surface-prompts
           #:surface-resources
           #:tools-list-result
           #:tools-call-result
           #:dispatch
           #:serve-stream))
