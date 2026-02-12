(defpackage #:task-tests
  (:use #:cl #:task #:crdt #:fiveam)
  (:import-from #:tq
                #:*graph*
                #:*mutation-handler*
                #:*current-task-id*
                #:all-nodes
                #:node
                #:active
                #:dormant
                #:follow
                #:back
                #:where-step
                #:select-fields
                #:sort-by-field
                #:take-n
                #:ids-step
                #:count-step
                #:enrich-step
                #:group-by-step
                #:group-by-result-p
                #:group-by-result-groups
                #:node-union
                #:node-intersection
                #:node-difference
                #:safe-read-query
                #:interpret-query
                #:format-query-result
                #:mutation-log-p
                #:scaffold-result-p
                #:tq-error
                #:tq-error-message
                #:tq-parse-error
                #:mutation-without-handler
                #:query
                #:define-query)
  (:import-from #:task-validation
                #:validate-task-name
                #:validation-result-valid-p
                #:validation-result-reason
                #:validation-result-suggestion
                #:suggest-name-from-description
                #:slugify))

(in-package #:task-tests)

(def-suite :task-tests
  :description "Tests for task event-sourced library")

(in-suite :task-tests)
