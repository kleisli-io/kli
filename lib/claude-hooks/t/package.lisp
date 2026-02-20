;;;; claude-hooks Test Suite - Package Definition

(defpackage :claude-hooks.tests
  (:use :cl :claude-hooks)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:is-true
                #:is-false
                #:signals
                #:run!)
  (:export
   #:run-all-tests))
