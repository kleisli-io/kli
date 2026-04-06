;;; Playbook tests package (pattern store, graph, activation, etc.)
;;; PQ tests are in pq-package.lisp (standalone build uses that separately)

(defpackage :playbook.tests
  (:use :cl :playbook)
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
