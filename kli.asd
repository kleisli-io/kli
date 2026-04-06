(defsystem "kli"
  :description "Event-sourced task graphs, pattern learning, and swarm coordination for Claude Code"
  :version (:read-file-form "version.sexp")
  :author "Kleisli.IO"
  :license "MIT"
  :depends-on ("kli-crdt"
               "kli-task"
               "kli-playbook"
               "kli-mcp-framework"
               "kli-mcp-http"
               "lol-reactive"
               ;; External deps (bundled into binary via Quicklisp)
               "alexandria"
               "bordeaux-threads"
               "cl-ppcre"
               "dexador"
               "hunchentoot"
               "let-over-lambda"
               "usocket"
               "yason")
  :serial t
  :components (;; Playbook lib (pattern store, graph, activation)
               (:module "playbook-lib"
                :pathname "lib/playbook"
                :serial t
                :components ((:file "package")
                             (:file "locking")
                             (:file "pattern")
                             (:file "store")
                             (:file "edges")
                             (:file "graph")
                             (:file "activation")
                             (:file "session")
                             (:file "parser")
                             (:file "search")
                             (:file "file-sync")
                             (:file "embeddings")
                             (:file "cleanup")
                             ;; MCP tools/resources (previously in services/playbook-mcp/server/)
                             (:file "tools")
                             (:file "pq-tool")
                             (:file "resources")))
               ;; Services (load before entry point so packages exist)
               (:module "services"
                :serial t
                :components ((:module "task-mcp"
                              :components ((:file "package")
                                           (:file "macros")
                                           (:file "format")
                                           (:file "embeddings")
                                           (:file "obs-index")
                                           (:file "retrieval")
                                           (:file "session")
                                           (:file "graph")
                                           (:file "query")
                                           (:file "tools")
                                           (:file "server")))
                             (:module "dashboard"
                              :components ((:file "package")
                                           (:file "config")
                                           (:file "css/base")
                                           (:file "css/frontier")
                                           (:file "css/health")
                                           (:file "css/forest")
                                           (:file "css/composition")
                                           (:file "css/task-detail")
                                           (:file "css/clusters")
                                           (:file "css/plan")
                                           (:file "css/activity")
                                           (:file "css/stats")
                                           (:file "components/layout")
                                           (:file "components/task-card")
                                           (:file "components/board")
                                           (:file "data/enrichment")
                                           (:file "scripts/composition")
                                           (:file "scripts/composition-webgl")
                                           (:file "scripts/plan")
                                           (:file "scripts/activity")
                                           (:file "scripts/frontier")
                                           (:file "pages/home")
                                           (:file "pages/frontier")
                                           (:file "pages/health")
                                           (:file "pages/forest")
                                           (:file "pages/composition")
                                           (:file "pages/composition-webgl")
                                           (:file "pages/clusters")
                                           (:file "pages/task-detail")
                                           (:file "pages/plan")
                                           (:file "pages/activity")
                                           (:file "pages/stats")
                                           (:file "routes")
                                           (:file "server")))))
               ;; CLI entry point (loaded last — references task-mcp and playbook packages)
               (:module "src"
                :components ((:file "package")
                             (:file "main")))))

(defsystem "kli/tests"
  :depends-on ("kli" "fiveam" "bordeaux-threads")
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :crdt-tests)
             (symbol-call :fiveam :run! :task-tests)
             (symbol-call :fiveam :run! :pq-tests)))
