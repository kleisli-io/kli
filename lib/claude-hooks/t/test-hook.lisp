;;;; claude-hooks Tests - Hook Lifecycle & Defhook

(in-package :claude-hooks.tests)
(in-suite :hook)

;;; test-run-hook exit paths

(test hook-success-with-response
  "Handler returning response -> exit 0, JSON stdout."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook (lambda (input)
                       (declare (ignore input))
                       (pre-tool-deny :reason "no"))
                     (make-ht))
    (is (= 0 code))
    (is (search "block" stdout))
    (is (string= "" stderr))))

(test hook-success-nil-return
  "Handler returning nil -> exit 0, empty stdout."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook (lambda (input)
                       (declare (ignore input))
                       nil)
                     (make-ht))
    (is (= 0 code))
    (is (string= "" stdout))
    (is (string= "" stderr))))

(test hook-block-exit
  "block! -> exit 2 with message on stderr."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook (lambda (input)
                       (declare (ignore input))
                       (block! "Access denied"))
                     (make-ht))
    (is (= 2 code))
    (is (null stdout))
    (is (string= "Access denied" stderr))))

(test hook-unexpected-error
  "Unexpected error -> exit 1 with error on stderr."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook (lambda (input)
                       (declare (ignore input))
                       (error "oops"))
                     (make-ht))
    (is (= 1 code))
    (is (null stdout))
    (is (search "oops" stderr))))

(test hook-malformed-json
  "Malformed JSON string -> exit 1."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook (lambda (input)
                       (declare (ignore input))
                       (empty-response))
                     "not valid json{{{")
    (is (= 1 code))
    (is (null stdout))
    (is (search "parse" stderr))))

(test hook-empty-json
  "Empty JSON object string -> exit 0."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook (lambda (input)
                       (declare (ignore input))
                       (empty-response))
                     "{}")
    (is (= 0 code))
    (is (string= "{}" stdout))
    (is (string= "" stderr))))

;;; defhook

(defhook test-env-guard ((path "tool_input" "file_path"))
  "Block .env file access."
  (if (and path (search ".env" path))
      (pre-tool-deny :reason "Protected file")
      (empty-response)))

(test defhook-with-bindings-match
  "defhook handler blocks .env access."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook #'test-env-guard-handler
                     (make-ht "tool_input" (make-ht "file_path" "/app/.env")))
    (is (= 0 code))
    (is (search "block" stdout))
    (is (search "Protected" stdout))
    (is (string= "" stderr))))

(test defhook-with-bindings-no-match
  "defhook handler allows non-.env access."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook #'test-env-guard-handler
                     (make-ht "tool_input" (make-ht "file_path" "/app/readme.md")))
    (is (= 0 code))
    (is (string= "{}" stdout))
    (is (string= "" stderr))))

(test defhook-with-bindings-missing-key
  "defhook handler handles missing keys (nil binding)."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook #'test-env-guard-handler (make-ht))
    (is (= 0 code))
    (is (string= "{}" stdout))
    (is (string= "" stderr))))

(defhook test-always-allow ()
  "Always allow."
  (empty-response))

(test defhook-no-bindings
  "defhook with no bindings works."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook #'test-always-allow-handler (make-ht "anything" "ignored"))
    (is (= 0 code))
    (is (string= "{}" stdout))
    (is (string= "" stderr))))

(defhook test-blocker ()
  "Always block."
  (block! "Nope"))

(test defhook-block
  "defhook handler can signal block!."
  (multiple-value-bind (stdout code stderr)
      (test-run-hook #'test-blocker-handler (make-ht))
    (is (= 2 code))
    (is (null stdout))
    (is (string= "Nope" stderr))))
