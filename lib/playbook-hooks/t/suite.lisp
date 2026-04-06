;;;; playbook-hooks tests - Suite Definitions

(in-package :playbook-hooks.tests)

(def-suite :playbook-hooks.tests
  :description "All playbook-hooks tests")

(def-suite :domains :in :playbook-hooks.tests
  :description "Tests for domain detection")

(def-suite :co-app :in :playbook-hooks.tests
  :description "Tests for co-application utilities")

(def-suite :session-io :in :playbook-hooks.tests
  :description "Tests for session I/O")

(defun run-all-tests ()
  "Run all playbook-hooks tests."
  (run! :playbook-hooks.tests))
