(in-package #:kli/tests)

(in-suite all)

(test pending-login-is-per-protocol
  "A second protocol's login must not destroy the first's verifier."
  (multiple-value-bind (ctx-a proto-a) (model-command-test-context)
    (declare (ignore ctx-a))
    (multiple-value-bind (ctx-b proto-b) (model-command-test-context)
      (declare (ignore ctx-b))
      (is (not (eq proto-a proto-b)))
      (setf (oauth:pending-login proto-a "openai") (list :verifier "VA" :state "SA"))
      (setf (oauth:pending-login proto-b "openai") (list :verifier "VB" :state "SB"))
      (is (string= "VA" (getf (oauth:pending-login proto-a "openai") :verifier))
          "protocol-a's verifier survives a login on protocol-b")
      (is (string= "VB" (getf (oauth:pending-login proto-b "openai") :verifier))))))

(test pending-login-is-per-provider-within-a-protocol
  "Two providers can be mid-login in one protocol without clobbering."
  (multiple-value-bind (ctx proto) (model-command-test-context)
    (declare (ignore ctx))
    (setf (oauth:pending-login proto "openai") (list :verifier "VX" :state "SX"))
    (setf (oauth:pending-login proto "anthropic") (list :verifier "VY" :state "SY"))
    (is (string= "VX" (getf (oauth:pending-login proto "openai") :verifier)))
    (is (string= "VY" (getf (oauth:pending-login proto "anthropic") :verifier)))
    (oauth:clear-pending-login proto "openai")
    (is (null (oauth:pending-login proto "openai")))
    (is (string= "VY" (getf (oauth:pending-login proto "anthropic") :verifier))
        "clearing one provider leaves the other")))

(test tool-output-expanded-is-per-protocol
  "The toggle is per-protocol, so flipping one must not flip the other."
  (multiple-value-bind (ctx-a proto-a) (model-command-test-context)
    (declare (ignore ctx-a))
    (multiple-value-bind (ctx-b proto-b) (model-command-test-context)
      (declare (ignore ctx-b))
      (is (null (tui-transcript:tool-output-expanded-p proto-a)))
      (is (eq t (tui-transcript:toggle-tool-output-expansion proto-a)))
      (is (eq t (tui-transcript:tool-output-expanded-p proto-a)))
      (is (null (tui-transcript:tool-output-expanded-p proto-b))
          "flipping protocol-a's toggle must not affect protocol-b"))))
