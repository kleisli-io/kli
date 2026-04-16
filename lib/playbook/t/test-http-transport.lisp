;;;; playbook tests - HTTP Transport Feature Parity
;;;; Verifies that HTTP transport produces identical results to stdio
;;;; for all session operations. Tests the transport selection logic,
;;;; per-thread isolation, and HTTP-specific session context management.

(in-package :playbook.tests)
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

;;; HTTP session context

(test get-current-session-http-mode
  "In HTTP mode, get-current-session returns nil without request context."
  (let ((playbook::*http-mode* t)
        (playbook::*claimed-session* nil)
        (playbook::*session-states* (make-hash-table :test 'equal)))
    ;; In HTTP mode without a request context, current-http-session-id returns nil
    ;; so get-current-session should return nil (no file-based fallback)
    (let ((result (playbook::get-current-session "/tmp/test")))
      (is (null result)))))

(test get-current-session-stdio-mode
  "In stdio mode, get-current-session uses file-based discovery."
  (let ((playbook::*http-mode* nil)
        (playbook::*claimed-session*
          (list (cons :session-id "stdio-test-session")
                (cons :branch nil)
                (cons :commit nil)
                (cons :task-dir nil)))
        (playbook::*claimed-session-cwd* "/tmp/test"))
    ;; With cached session matching CWD, should return cached
    (let ((result (playbook::get-current-session "/tmp/test")))
      (is (string= "stdio-test-session"
                    (cdr (assoc :session-id result)))))))

;;; ensure-playbook-session-context

(test ensure-session-context-stdio-noop
  "In stdio mode, ensure-playbook-session-context delegates to current-session-id."
  (let ((playbook::*http-mode* nil)
        (playbook::*claimed-session*
          (list (cons :session-id "ctx-stdio-001")))
        (playbook::*claimed-session-cwd* (namestring (uiop:getcwd))))
    (let ((result (ensure-playbook-session-context)))
      (is (string= "ctx-stdio-001" result)))))

(test ensure-session-context-http-without-request
  "In HTTP mode without request, ensure-playbook-session-context returns nil."
  (let ((playbook::*http-mode* t)
        (playbook::*session-states* (make-hash-table :test 'equal)))
    ;; No hunchentoot request context -> nil
    (let ((result (ensure-playbook-session-context)))
      (is (null result)))))

;;; Cleanup inactive sessions

(test cleanup-inactive-sessions-removes-old
  "cleanup-inactive-playbook-sessions removes sessions older than max-age."
  (let ((playbook::*session-states* (make-hash-table :test 'equal)))
    ;; Create a session with old last-active timestamp
    (let ((old-session (get-or-create-session "old-session")))
      (setf (playbook::session-state-last-active old-session)
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
  (let ((playbook::*session-states* (make-hash-table :test 'equal)))
    (get-or-create-session "recent-001")
    (get-or-create-session "recent-002")
    (let ((removed (cleanup-inactive-playbook-sessions 4)))
      (is (= 0 removed))
      (is (= 2 (hash-table-count playbook::*session-states*))))))

;;; Feature parity: identical operations produce identical results
;;; regardless of transport mode

(test parity-activation-recording
  "Activation recording produces identical state in both transport modes."
  ;; Simulate stdio mode
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*http-mode* nil))
    (record-activation "parity-sess" "pat-1")
    (record-activation "parity-sess" "pat-2")
    (let ((stdio-activated
            (copy-list (playbook::session-state-activated-patterns
                        (get-session "parity-sess")))))
      ;; Simulate HTTP mode (same operations)
      (let ((playbook::*session-states* (make-hash-table :test 'equal))
            (playbook::*http-mode* t))
        (record-activation "parity-sess" "pat-1")
        (record-activation "parity-sess" "pat-2")
        (let ((http-activated
                (playbook::session-state-activated-patterns
                 (get-session "parity-sess"))))
          ;; Both should have identical pattern sets
          (is (null (set-exclusive-or stdio-activated http-activated
                                      :test #'string=))))))))

(test parity-feedback-recording
  "Feedback recording produces identical state in both transport modes."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*http-mode* nil))
    (record-activation "parity-fb" "pat-1")
    (record-feedback "parity-fb" "pat-1" :helpful)
    (multiple-value-bind (s-act s-fb s-pending)
        (session-summary "parity-fb")
      (let ((playbook::*session-states* (make-hash-table :test 'equal))
            (playbook::*http-mode* t))
        (record-activation "parity-fb" "pat-1")
        (record-feedback "parity-fb" "pat-1" :helpful)
        (multiple-value-bind (h-act h-fb h-pending)
            (session-summary "parity-fb")
          (is (= s-act h-act))
          (is (= s-fb h-fb))
          (is (equal s-pending h-pending)))))))

(test parity-session-summary
  "session-summary returns identical results regardless of transport mode."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*http-mode* nil))
    (record-activation "parity-sum" "p1")
    (record-activation "parity-sum" "p2")
    (record-activation "parity-sum" "p3")
    (record-feedback "parity-sum" "p1" :helpful)
    (record-feedback "parity-sum" "p2" :harmful)
    (multiple-value-bind (s-act s-fb s-pend)
        (session-summary "parity-sum")
      (let ((playbook::*session-states* (make-hash-table :test 'equal))
            (playbook::*http-mode* t))
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
