;;;; kli-hook Test Suite - Suite Definitions

(in-package :kli-hook.tests)

(def-suite :kli-hook.tests
  :description "Root test suite for kli hook handlers")

(def-suite :task-complete-reflect :in :kli-hook.tests
  :description "Tests for task-complete-reflect hook handler")

(defun run-all-tests ()
  "Run all kli-hook tests."
  (run! :kli-hook.tests))
