;;;; claude-hooks Test Suite - Suite Definitions

(in-package :claude-hooks.tests)

(def-suite :claude-hooks.tests
  :description "Root test suite for claude-hooks library")

(def-suite :json :in :claude-hooks.tests
  :description "Tests for JSON utilities")

(def-suite :responses :in :claude-hooks.tests
  :description "Tests for response builders")

(def-suite :paths :in :claude-hooks.tests
  :description "Tests for session path helpers")

(def-suite :file-io :in :claude-hooks.tests
  :description "Tests for file I/O utilities")

(def-suite :prompts :in :claude-hooks.tests
  :description "Tests for prompt skip helpers")

(def-suite :hook :in :claude-hooks.tests
  :description "Tests for hook lifecycle and defhook")

(defun run-all-tests ()
  "Run all claude-hooks tests."
  (run! :claude-hooks.tests))
