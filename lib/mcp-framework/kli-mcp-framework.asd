(defsystem "kli-mcp-framework"
  :description "MCP server framework — tool/resource/prompt registration, schema, hooks, evolution"
  :version "0.2.0"
  :license "MIT"
  :depends-on ("alexandria" "bordeaux-threads" "let-over-lambda" "yason" "cl-ppcre")
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "conditions")
               (:file "content")
               (:file "schema")
               (:file "protocol")
               (:module "tools"
                :components ((:file "core")
                             (:file "macros")))
               (:file "hooks")
               (:file "resources")
               (:file "prompts")
               (:file "evolution")
               (:module "transport"
                :components ((:file "protocol")
                             (:file "stdio")))
               (:file "server")))
