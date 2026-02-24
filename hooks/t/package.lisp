;;;; kli-hook Test Suite - Package Definition

(defpackage :kli-hook.tests
  (:use :cl :kli-hook :claude-hooks)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:is-true
                #:is-false
                #:run!)
  (:export
   #:run-all-tests))
