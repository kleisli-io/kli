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

(def-suite :relevance-feedback :in :playbook.tests
  :description "Tests for query-scoped :not-relevant feedback")

(defun run-all-tests ()
  "Run all playbook tests."
  (run! :playbook.tests))
