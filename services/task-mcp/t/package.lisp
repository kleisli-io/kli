(defpackage #:task-mcp-tests
  (:use #:cl #:fiveam)
  (:import-from #:task-mcp
                #:*event-required-fields*
                #:check-required-fields
                #:resolve-task-id
                #:validate-required-string
                #:tq-require-non-nil
                #:tq-mutation-handler
                #:emit-event
                #:*current-task-id*
                #:*session-id*
                #:*session-vc*
                #:*http-mode*)
  (:import-from #:mcp-framework
                #:call-tool
                #:content-to-json)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-thread
                #:join-thread
                #:thread-alive-p))

(in-package #:task-mcp-tests)

(def-suite :task-mcp-tests
  :description
  "Regression tests for the null-boundary rejection guard across the
   task-mcp entry surfaces (direct tools, TQ mutations, emit-event).")

(in-suite :task-mcp-tests)
