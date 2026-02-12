(defpackage #:task-mcp
  (:use #:cl #:crdt #:task)
  (:import-from #:mcp-framework
                #:make-server
                #:run-server
                #:define-tool
                #:make-text-content)
  (:export #:main #:start-task-server))
