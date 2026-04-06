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
                #:define-query
                #:exact-node)
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

;;; --- Shared Test Helpers ---
;;; Defined here (loaded first) so test-markov.lisp and test-query.lisp
;;; can use WITH-TASK-ROOTS as a macro rather than an undefined function.

(defun task-roots-available-p ()
  "Try to set up *tasks-root* from real data. Returns T if available."
  (handler-case
      (progn
        (detect-tasks-root)
        (and *tasks-root* (uiop:directory-exists-p *tasks-root*)))
    (error () nil)))

(defmacro with-task-roots (&body body)
  "Execute BODY only if tasks root is available, otherwise skip."
  `(if (not (task-roots-available-p))
       (skip "No tasks root available (sandbox or missing .kli/tasks/)")
       (progn ,@body)))
