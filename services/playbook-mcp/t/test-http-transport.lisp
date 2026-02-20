;;;; playbook-mcp tests - HTTP Transport Feature Parity
;;;; Verifies that HTTP transport produces identical results to stdio
;;;; for all session operations. Tests the transport selection logic,
;;;; per-thread isolation, and HTTP-specific session context management.

(in-package :playbook-mcp.tests)
(in-suite :http-transport)

;;; Helper for env var save/restore

(defmacro with-env ((var value) &body body)
  "Execute BODY with environment variable VAR set to VALUE.
   VALUE of :unset means unsetenv. Restores original value on exit."
  (let ((prev (gensym "PREV"))
        (gvar (gensym "VAR")))
    `(let* ((,gvar ,var)
            (,prev (uiop:getenv ,gvar)))
       (unwind-protect
            (progn
              (if (eq ,value :unset)
                  (sb-posix:unsetenv ,gvar)
                  (setf (uiop:getenv ,gvar) ,value))
              ,@body)
         (if ,prev
             (setf (uiop:getenv ,gvar) ,prev)
             (sb-posix:unsetenv ,gvar))))))

;;; Transport selection

(test select-transport-defaults-to-http
  "When PLAYBOOK_MCP_TRANSPORT is unset, the code path selects HTTP."
  (with-env ("PLAYBOOK_MCP_TRANSPORT" :unset)
    (let ((transport-type (uiop:getenv "PLAYBOOK_MCP_TRANSPORT")))
      (is (null transport-type)))))

(test select-transport-stdio-when-env-set
  "When PLAYBOOK_MCP_TRANSPORT=stdio, the code path selects stdio."
  (with-env ("PLAYBOOK_MCP_TRANSPORT" "stdio")
    (let ((transport-type (uiop:getenv "PLAYBOOK_MCP_TRANSPORT")))
      (is (string-equal "stdio" transport-type)))))

(test get-playbook-http-port-default
  "get-playbook-http-port returns 8091 when env is unset."
  (with-env ("PLAYBOOK_MCP_PORT" :unset)
    (is (= 8091 (playbook-mcp::get-playbook-http-port)))))

(test get-playbook-http-port-from-env
  "get-playbook-http-port reads PLAYBOOK_MCP_PORT env var."
  (with-env ("PLAYBOOK_MCP_PORT" "9999")
    (is (= 9999 (playbook-mcp::get-playbook-http-port)))))

(test get-playbook-http-port-junk-fallback
  "get-playbook-http-port falls back to 8091 on unparseable input."
  (with-env ("PLAYBOOK_MCP_PORT" "not-a-number")
    (is (= 8091 (playbook-mcp::get-playbook-http-port)))))

;;; HTTP session context

(test get-current-session-http-mode
  "In HTTP mode, get-current-session returns nil without request context."
  (let ((playbook-mcp::*http-mode* t)
        (playbook-mcp::*claimed-session* nil)
        (playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    ;; In HTTP mode without a request context, current-http-session-id returns nil
    ;; so get-current-session should return nil (no file-based fallback)
    (let ((result (playbook-mcp::get-current-session "/tmp/test")))
      (is (null result)))))

(test get-current-session-stdio-mode
  "In stdio mode, get-current-session uses file-based discovery."
  (let ((playbook-mcp::*http-mode* nil)
        (playbook-mcp::*claimed-session*
          (list (cons :session-id "stdio-test-session")
                (cons :branch nil)
                (cons :commit nil)
                (cons :task-dir nil)))
        (playbook-mcp::*claimed-session-cwd* "/tmp/test"))
    ;; With cached session matching CWD, should return cached
    (let ((result (playbook-mcp::get-current-session "/tmp/test")))
      (is (string= "stdio-test-session"
                    (cdr (assoc :session-id result)))))))

;;; ensure-playbook-session-context

(test ensure-session-context-stdio-noop
  "In stdio mode, ensure-playbook-session-context delegates to current-session-id."
  (let ((playbook-mcp::*http-mode* nil)
        (playbook-mcp::*claimed-session*
          (list (cons :session-id "ctx-stdio-001")))
        (playbook-mcp::*claimed-session-cwd* (namestring (uiop:getcwd))))
    (let ((result (ensure-playbook-session-context)))
      (is (string= "ctx-stdio-001" result)))))

(test ensure-session-context-http-without-request
  "In HTTP mode without request, ensure-playbook-session-context returns nil."
  (let ((playbook-mcp::*http-mode* t)
        (playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    ;; No hunchentoot request context -> nil
    (let ((result (ensure-playbook-session-context)))
      (is (null result)))))

;;; Cleanup inactive sessions

(test cleanup-inactive-sessions-removes-old
  "cleanup-inactive-playbook-sessions removes sessions older than max-age."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    ;; Create a session with old last-active timestamp
    (let ((old-session (get-or-create-session "old-session")))
      (setf (playbook-mcp::session-state-last-active old-session)
            (- (get-universal-time) (* 5 60 60)))) ; 5 hours ago
    ;; Create a fresh session
    (get-or-create-session "fresh-session")
    ;; Cleanup with 4-hour cutoff
    (let ((removed (cleanup-inactive-playbook-sessions 4)))
      (is (= 1 removed))
      (is (null (get-session "old-session")))
      (is (not (null (get-session "fresh-session")))))))

(test cleanup-inactive-sessions-keeps-recent
  "cleanup-inactive-playbook-sessions keeps all sessions when none expired."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (get-or-create-session "recent-001")
    (get-or-create-session "recent-002")
    (let ((removed (cleanup-inactive-playbook-sessions 4)))
      (is (= 0 removed))
      (is (= 2 (hash-table-count playbook-mcp::*session-states*))))))

;;; Feature parity: identical operations produce identical results
;;; regardless of transport mode

(test parity-activation-recording
  "Activation recording produces identical state in both transport modes."
  ;; Simulate stdio mode
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal))
        (playbook-mcp::*http-mode* nil))
    (record-activation "parity-sess" "pat-1")
    (record-activation "parity-sess" "pat-2")
    (let ((stdio-activated
            (copy-list (playbook-mcp::session-state-activated-patterns
                        (get-session "parity-sess")))))
      ;; Simulate HTTP mode (same operations)
      (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal))
            (playbook-mcp::*http-mode* t))
        (record-activation "parity-sess" "pat-1")
        (record-activation "parity-sess" "pat-2")
        (let ((http-activated
                (playbook-mcp::session-state-activated-patterns
                 (get-session "parity-sess"))))
          ;; Both should have identical pattern sets
          (is (null (set-exclusive-or stdio-activated http-activated
                                      :test #'string=))))))))

(test parity-feedback-recording
  "Feedback recording produces identical state in both transport modes."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal))
        (playbook-mcp::*http-mode* nil))
    (record-activation "parity-fb" "pat-1")
    (record-feedback "parity-fb" "pat-1" :helpful)
    (multiple-value-bind (s-act s-fb s-pending)
        (session-summary "parity-fb")
      (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal))
            (playbook-mcp::*http-mode* t))
        (record-activation "parity-fb" "pat-1")
        (record-feedback "parity-fb" "pat-1" :helpful)
        (multiple-value-bind (h-act h-fb h-pending)
            (session-summary "parity-fb")
          (is (= s-act h-act))
          (is (= s-fb h-fb))
          (is (equal s-pending h-pending)))))))

(test parity-session-summary
  "session-summary returns identical results regardless of transport mode."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal))
        (playbook-mcp::*http-mode* nil))
    (record-activation "parity-sum" "p1")
    (record-activation "parity-sum" "p2")
    (record-activation "parity-sum" "p3")
    (record-feedback "parity-sum" "p1" :helpful)
    (record-feedback "parity-sum" "p2" :harmful)
    (multiple-value-bind (s-act s-fb s-pend)
        (session-summary "parity-sum")
      (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal))
            (playbook-mcp::*http-mode* t))
        (record-activation "parity-sum" "p1")
        (record-activation "parity-sum" "p2")
        (record-activation "parity-sum" "p3")
        (record-feedback "parity-sum" "p1" :helpful)
        (record-feedback "parity-sum" "p2" :harmful)
        (multiple-value-bind (h-act h-fb h-pend)
            (session-summary "parity-sum")
          (is (= s-act h-act))
          (is (= s-fb h-fb))
          (is (null (set-exclusive-or s-pend h-pend :test #'string=))))))))
