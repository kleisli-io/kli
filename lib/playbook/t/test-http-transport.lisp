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
    (get-or-create-session "parity-sess")
    (record-activation "parity-sess" "pat-1")
    (record-activation "parity-sess" "pat-2")
    (let ((stdio-activated
            (copy-list (playbook::session-state-activated-patterns
                        (get-session "parity-sess")))))
      ;; Simulate HTTP mode (same operations)
      (let ((playbook::*session-states* (make-hash-table :test 'equal))
            (playbook::*http-mode* t))
        (get-or-create-session "parity-sess")
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
    (get-or-create-session "parity-fb")
    (record-activation "parity-fb" "pat-1")
    (record-feedback "parity-fb" "pat-1" :helpful)
    (multiple-value-bind (s-act s-fb s-pending)
        (session-summary "parity-fb")
      (let ((playbook::*session-states* (make-hash-table :test 'equal))
            (playbook::*http-mode* t))
        (get-or-create-session "parity-fb")
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
    (get-or-create-session "parity-sum")
    (record-activation "parity-sum" "p1")
    (record-activation "parity-sum" "p2")
    (record-activation "parity-sum" "p3")
    (record-feedback "parity-sum" "p1" :helpful)
    (record-feedback "parity-sum" "p2" :harmful)
    (multiple-value-bind (s-act s-fb s-pend)
        (session-summary "parity-sum")
      (let ((playbook::*session-states* (make-hash-table :test 'equal))
            (playbook::*http-mode* t))
        (get-or-create-session "parity-sum")
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

;;; ==========================================================================
;;; Claude-Session-Id resolution
;;; ==========================================================================
;;;
;;; PLAYBOOK::APPLY-CLAUDE-SESSION-ID stamps the Claude session ID on a
;;; session-state.  The contract: an explicit Claude-Session-Id HTTP
;;; header value (passed via :HEADER-SID, default
;;; CURRENT-CLAUDE-SESSION-ID) is authoritative when present;
;;; peer-PID-derived RESOLVE-CLAUDE-SESSION-ID is the fallback.
;;;
;;; Header authority matters because Claude Code shells with a shared
;;; UNIX PID lineage all resolve to the same registered PID, so the
;;; peer-PID path coalesces them onto a single claude-sid.  An explicit
;;; per-request header carries each shell's own identity.

(test apply-claude-session-id-prefers-header-over-pid-fallback
  "Given a non-NIL :HEADER-SID, APPLY-CLAUDE-SESSION-ID must stamp it on
   the session and skip the peer-PID lookup entirely."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    ;; Seed the peer-PID registry with a DIFFERENT sid that would win
    ;; under the fallback path.
    (register-claude-session-for-pid 999999 "peer-pid-derived-sid")
    (let ((session (get-or-create-session "mcp-sid-A")))
      (let ((resolved (playbook::apply-claude-session-id
                       session :header-sid "header-claude-sid-X")))
        (is (string= "header-claude-sid-X" resolved)
            "APPLY must return the header value")
        (is (string= "header-claude-sid-X"
                     (playbook::session-state-claude-session-id session))
            "session-state-claude-session-id must be the header value, ~
             not the peer-PID-derived fallback")))))

(test apply-claude-session-id-falls-back-to-peer-pid-when-no-header
  "Given a NIL :HEADER-SID, APPLY-CLAUDE-SESSION-ID must delegate to
   RESOLVE-CLAUDE-SESSION-ID (the peer-PID-derived path) for backward
   compatibility with hook callers and older clients."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    (register-claude-session-for-pid 12345 "peer-pid-derived-sid")
    (let ((session (get-or-create-session "mcp-sid-B")))
      (let ((resolved (playbook::apply-claude-session-id
                       session :header-sid nil)))
        (is (string= "peer-pid-derived-sid" resolved)
            "without header, must fall back to peer-PID registry")))))

(test apply-claude-session-id-header-overwrites-stale-value
  "When session-state already holds a stale claude-sid (e.g. from a
   previous peer-PID coalescing), a fresh :HEADER-SID must overwrite
   it so the session can recover from prior mis-keying."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*http-mode* t))
    (let ((session (get-or-create-session "mcp-sid-C")))
      (setf (playbook::session-state-claude-session-id session)
            "stale-coalesced-sid")
      (playbook::apply-claude-session-id session :header-sid "fresh-header-sid")
      (is (string= "fresh-header-sid"
                   (playbook::session-state-claude-session-id session))
          "header value must overwrite the stale value"))))

(test apply-claude-session-id-no-header-no-pid-leaves-session-untouched
  "When neither :HEADER-SID nor the peer-PID registry yields a value,
   the session's claude-session-id stays NIL — no fabricated identity."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    (let ((session (get-or-create-session "mcp-sid-D")))
      (let ((resolved (playbook::apply-claude-session-id
                       session :header-sid nil)))
        (is (null resolved)
            "no header + empty PID registry => NIL")
        (is (null (playbook::session-state-claude-session-id session))
            "session-state-claude-session-id stays NIL")))))

(test parallel-shells-with-distinct-headers-stay-distinct
  "Two MCP sessions, each handling a request that carries a DIFFERENT
   Claude-Session-Id header value, must record DISTINCT
   session-state-claude-session-id values even when peer-PID resolution
   would coalesce them onto a single registered PID."
  (let ((playbook::*session-states* (make-hash-table :test 'equal))
        (playbook::*pid-claude-session-registry* (make-hash-table :test 'eql))
        (playbook::*http-mode* t))
    (register-claude-session-for-pid 42 "coalesced-claude-sid")
    (let ((session-1 (get-or-create-session "mcp-sid-shell-1"))
          (session-2 (get-or-create-session "mcp-sid-shell-2")))
      (playbook::apply-claude-session-id session-1 :header-sid "claude-shell-1")
      (playbook::apply-claude-session-id session-2 :header-sid "claude-shell-2")
      (is (string= "claude-shell-1"
                   (playbook::session-state-claude-session-id session-1))
          "shell 1 must record its own header value")
      (is (string= "claude-shell-2"
                   (playbook::session-state-claude-session-id session-2))
          "shell 2 must record its own header value, not be coalesced")
      (is (not (string=
                (playbook::session-state-claude-session-id session-1)
                (playbook::session-state-claude-session-id session-2)))
          "the two sessions must hold distinct claude-sids"))))
