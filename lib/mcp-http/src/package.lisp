;;; MCP HTTP Transport - Package definition

(defpackage #:mcp-http
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  ;; Transport
  (:export #:http-transport
           #:make-http-transport
           #:transport-port
           #:transport-host
           #:transport-endpoint
           #:transport-session-store)
  ;; Session management
  (:export #:session-store
           #:make-session-store
           #:generate-session-id
           #:create-session
           #:valid-session-p
           #:terminate-session
           #:valid-origin-p)
  ;; SSE
  (:export #:format-sse-event))
