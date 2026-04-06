(defsystem "kli-mcp-http"
  :description "HTTP+SSE transport for MCP — multi-session, CORS, session lifecycle"
  :version (:read-file-form "../../version.sexp")
  :license "MIT"
  :depends-on ("kli-mcp-framework" "hunchentoot" "bordeaux-threads" "yason")
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "session")
               (:file "sse")
               (:file "transport")
               (:file "handler")))
