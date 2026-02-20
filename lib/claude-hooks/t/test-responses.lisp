;;;; claude-hooks Tests - Response Builders

(in-package :claude-hooks.tests)
(in-suite :responses)

;;; Helper to check JSON output
(defun json-has (ht key expected)
  "Check that HT has KEY with value equal to EXPECTED."
  (equal expected (gethash key ht)))

;;; PreToolUse

(test pre-tool-allow-minimal
  "pre-tool-allow with no args produces decision=approve."
  (let ((r (pre-tool-allow)))
    (is (string= "approve" (gethash "decision" r)))
    (is (= 1 (hash-table-count r)))))

(test pre-tool-allow-full
  "pre-tool-allow with all options."
  (let ((r (pre-tool-allow :reason "ok" :context "info"
                           :updated-input (make-ht "x" 1))))
    (is (string= "approve" (gethash "decision" r)))
    (is (string= "ok" (gethash "reason" r)))
    (is (string= "info" (gethash "additionalContext" r)))
    (is (= 1 (jref r "updatedInput" "x")))))

(test pre-tool-deny-minimal
  "pre-tool-deny with no args."
  (let ((r (pre-tool-deny)))
    (is (string= "block" (gethash "decision" r)))
    (is (= 1 (hash-table-count r)))))

(test pre-tool-deny-with-reason
  "pre-tool-deny with reason and context."
  (let ((r (pre-tool-deny :reason "blocked" :context "details")))
    (is (string= "block" (gethash "decision" r)))
    (is (string= "blocked" (gethash "reason" r)))
    (is (string= "details" (gethash "additionalContext" r)))))

(test pre-tool-ask-minimal
  "pre-tool-ask with no args."
  (let ((r (pre-tool-ask)))
    (is (string= "ask" (gethash "decision" r)))))

(test pre-tool-ask-with-reason
  "pre-tool-ask with reason."
  (let ((r (pre-tool-ask :reason "check this")))
    (is (string= "check this" (gethash "reason" r)))))

;;; PermissionRequest

(test permission-allow-minimal
  "permission-allow with no args."
  (let ((r (permission-allow)))
    (is (string= "allow" (gethash "behavior" r)))
    (is (= 1 (hash-table-count r)))))

(test permission-allow-with-input
  "permission-allow with updated-input."
  (let ((r (permission-allow :updated-input (make-ht "k" "v"))))
    (is (string= "allow" (gethash "behavior" r)))
    (is (string= "v" (jref r "updatedInput" "k")))))

(test permission-deny-minimal
  "permission-deny with no args."
  (let ((r (permission-deny)))
    (is (string= "deny" (gethash "behavior" r)))
    (is (= 1 (hash-table-count r)))))

(test permission-deny-interrupt-false
  "permission-deny with interrupt=false produces JSON false, not null."
  (let* ((r (permission-deny :message "no" :interrupt nil))
         (json (encode-json r)))
    (is (string= "deny" (gethash "behavior" r)))
    (is (string= "no" (gethash "message" r)))
    ;; Must be JSON false, not null
    (is (search "false" json))))

(test permission-deny-interrupt-true
  "permission-deny with interrupt=true."
  (let* ((r (permission-deny :message "stop" :interrupt t))
         (json (encode-json r)))
    (is (search "true" json))))

(test permission-deny-no-interrupt
  "permission-deny without interrupt kwarg omits the key."
  (let ((r (permission-deny :message "denied")))
    (is (null (nth-value 1 (gethash "interrupt" r))))))

;;; Stop/SubagentStop

(test stop-continue-with-reason
  "stop-continue produces continue=true with reason."
  (let ((r (stop-continue :reason "patterns need feedback")))
    (is (eq t (gethash "continue" r)))
    (is (string= "patterns need feedback" (gethash "reason" r)))
    (is (= 2 (hash-table-count r)))))

(test stop-continue-minimal
  "stop-continue with no reason produces only continue=true."
  (let ((r (stop-continue)))
    (is (eq t (gethash "continue" r)))
    (is (= 1 (hash-table-count r)))))

(test stop-allow-basic
  "stop-allow returns NIL (empty stdout = allow stop)."
  (is (null (stop-allow))))

(test stop-continue-json-roundtrip
  "stop-continue encodes to correct JSON with boolean true."
  (let ((json (encode-json (stop-continue :reason "keep going"))))
    (is (search "true" json))
    (is (search "keep going" json))))

;;; Generic

(test context-response-basic
  "context-response wraps text."
  (let ((r (context-response "extra info")))
    (is (string= "extra info" (gethash "additionalContext" r)))))

(test block-response-basic
  "block-response with reason."
  (let ((r (block-response :reason "nope")))
    (is (string= "block" (gethash "decision" r)))
    (is (string= "nope" (gethash "reason" r)))))

(test post-tool-block-basic
  "post-tool-block combines decision and hookSpecificOutput."
  (let ((r (post-tool-block :reason "bad" :context "details")))
    (is (string= "block" (gethash "decision" r)))
    (is (string= "bad" (gethash "reason" r)))
    (is (string= "details" (jref r "hookSpecificOutput" "additionalContext")))))

(test empty-response-basic
  "empty-response produces empty hash-table."
  (let ((r (empty-response)))
    (is (= 0 (hash-table-count r)))
    (is (string= "{}" (encode-json r)))))

(test suppress-output-basic
  "suppress-output adds suppressOutput=true."
  (let ((r (suppress-output (pre-tool-allow :reason "ok"))))
    (is (eq t (gethash "suppressOutput" r)))
    (is (string= "approve" (gethash "decision" r)))))

(test suppress-output-composable
  "suppress-output works with any response type."
  (let ((r (suppress-output (context-response "ctx"))))
    (is (eq t (gethash "suppressOutput" r)))
    (is (string= "ctx" (gethash "additionalContext" r)))))

;;; Event Context Wrappers

(test hook-context-structure
  "hook-context produces nested hookSpecificOutput structure."
  (let ((r (hook-context "TestEvent" "test message")))
    (is (hash-table-p (gethash "hookSpecificOutput" r)))
    (is (string= "TestEvent" (jref r "hookSpecificOutput" "hookEventName")))
    (is (string= "test message" (jref r "hookSpecificOutput" "additionalContext")))))

(test hook-context-json-roundtrip
  "hook-context encodes to correct JSON schema."
  (let ((json (encode-json (hook-context "UserPromptSubmit" "ctx"))))
    (is (search "hookSpecificOutput" json))
    (is (search "hookEventName" json))
    (is (search "UserPromptSubmit" json))
    (is (search "additionalContext" json))
    (is (search "ctx" json))))

(test prompt-context-basic
  "prompt-context wraps for UserPromptSubmit."
  (let ((r (prompt-context "domains detected")))
    (is (string= "UserPromptSubmit" (jref r "hookSpecificOutput" "hookEventName")))
    (is (string= "domains detected" (jref r "hookSpecificOutput" "additionalContext")))))

(test session-start-context-basic
  "session-start-context wraps for SessionStart."
  (let ((r (session-start-context "42 patterns loaded")))
    (is (string= "SessionStart" (jref r "hookSpecificOutput" "hookEventName")))
    (is (string= "42 patterns loaded" (jref r "hookSpecificOutput" "additionalContext")))))
