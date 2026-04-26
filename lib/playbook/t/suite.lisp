;;;; playbook tests - Suite Definitions

(in-package :playbook.tests)

(def-suite :playbook.tests
  :description "All playbook server tests")

(def-suite :session-state :in :playbook.tests
  :description "Tests for session state management")

(def-suite :session-discovery :in :playbook.tests
  :description "Tests for session discovery (world root, depot root)")

(def-suite :http-transport :in :playbook.tests
  :description "Tests for HTTP transport feature parity with stdio")

(def-suite :feedback-state :in :playbook.tests
  :description "Tests for feedback state file I/O")

(def-suite :session-contract :in :playbook.tests
  :description "Tests for the session contract: get-or-error split + write-merge")

(def-suite :relevance-feedback :in :playbook.tests
  :description "Tests for query-scoped :not-relevant feedback")

(def-suite :playbook-consistency :in :playbook.tests
  :description "Tests for cross-sidecar consistency: evidence, ~
                relevance-feedback, co-applications all reference ~
                live patterns")

(defun run-all-tests ()
  "Run all playbook tests."
  (run! :playbook.tests))
