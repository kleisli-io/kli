;;;; playbook-mcp tests - Suite Definitions

(in-package :playbook-mcp.tests)

(def-suite :playbook-mcp.tests
  :description "All playbook-mcp server tests")

(def-suite :session-state :in :playbook-mcp.tests
  :description "Tests for session state management")

(def-suite :session-discovery :in :playbook-mcp.tests
  :description "Tests for session discovery (world root, depot root)")

(def-suite :http-transport :in :playbook-mcp.tests
  :description "Tests for HTTP transport feature parity with stdio")

(def-suite :feedback-state :in :playbook-mcp.tests
  :description "Tests for feedback state file I/O")

(defun run-all-tests ()
  "Run all playbook-mcp tests."
  (run! :playbook-mcp.tests))
