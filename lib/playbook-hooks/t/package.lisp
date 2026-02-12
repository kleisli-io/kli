;;;; playbook-hooks tests - Package Definition

(defpackage :playbook-hooks.tests
  (:use :cl :playbook-hooks)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:is-true
                #:is-false
                #:signals
                #:run!)
  (:export #:run-all-tests))
