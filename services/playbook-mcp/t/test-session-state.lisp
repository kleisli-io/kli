;;;; playbook-mcp tests - Session State Management
;;;; Tests for session state CRUD, isolation, and serialization.
;;;; These operations are transport-agnostic — both stdio and HTTP
;;;; must produce identical results.

(in-package :playbook-mcp.tests)
(in-suite :session-state)

;;; Session creation and retrieval

(test get-or-create-session-creates-new
  "get-or-create-session creates a new session when none exists."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (let ((session (get-or-create-session "test-new-001")))
      (is (not (null session)))
      (is (string= "test-new-001" (playbook-mcp::session-state-id session)))
      (is (null (playbook-mcp::session-state-activated-patterns session)))
      (is (null (playbook-mcp::session-state-feedback-given session)))
      (is (null (playbook-mcp::session-state-active-domains session))))))

(test get-or-create-session-returns-existing
  "get-or-create-session returns the same object on second call."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (let ((first (get-or-create-session "test-existing-001"))
          (second (get-or-create-session "test-existing-001")))
      (is (eq first second)))))

(test get-session-returns-nil-for-missing
  "get-session returns NIL when session does not exist."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (is (null (get-session "nonexistent")))))

;;; Activation recording

(test record-activation-basic
  "record-activation adds pattern ID to session."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-activation "sess-act-001" "lisp-000001")
    (record-activation "sess-act-001" "lisp-000002")
    (let ((session (get-session "sess-act-001")))
      (is (= 2 (length (playbook-mcp::session-state-activated-patterns session))))
      (is (member "lisp-000001" (playbook-mcp::session-state-activated-patterns session)
                  :test #'string=)))))

(test record-activation-deduplicates
  "record-activation does not duplicate pattern IDs."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-activation "sess-dedup-001" "lisp-000001")
    (record-activation "sess-dedup-001" "lisp-000001")
    (record-activation "sess-dedup-001" "lisp-000001")
    (let ((session (get-session "sess-dedup-001")))
      (is (= 1 (length (playbook-mcp::session-state-activated-patterns session)))))))

;;; Feedback recording

(test record-feedback-basic
  "record-feedback stores pattern feedback in session."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-feedback "sess-fb-001" "lisp-000001" :helpful)
    (let ((session (get-session "sess-fb-001")))
      (is (= 1 (length (playbook-mcp::session-state-feedback-given session))))
      (is (eq :helpful (cdr (assoc "lisp-000001"
                                   (playbook-mcp::session-state-feedback-given session)
                                   :test #'string=)))))))

(test record-feedback-overwrites
  "record-feedback overwrites previous feedback for same pattern."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-feedback "sess-overwrite-001" "lisp-000001" :helpful)
    (record-feedback "sess-overwrite-001" "lisp-000001" :harmful)
    (let ((session (get-session "sess-overwrite-001")))
      (is (= 1 (length (playbook-mcp::session-state-feedback-given session))))
      (is (eq :harmful (cdr (assoc "lisp-000001"
                                    (playbook-mcp::session-state-feedback-given session)
                                    :test #'string=)))))))

;;; Domain recording

(test record-domain-basic
  "record-domain adds domain to session."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-domain "sess-dom-001" "lisp")
    (record-domain "sess-dom-001" "nix")
    (let ((session (get-session "sess-dom-001")))
      (is (= 2 (length (playbook-mcp::session-state-active-domains session)))))))

(test record-domain-deduplicates
  "record-domain does not duplicate domains."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-domain "sess-dom-dedup" "lisp")
    (record-domain "sess-dom-dedup" "lisp")
    (let ((session (get-session "sess-dom-dedup")))
      (is (= 1 (length (playbook-mcp::session-state-active-domains session)))))))

;;; Session summary

(test session-summary-computes-pending
  "session-summary returns correct counts and pending IDs."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-activation "sess-sum-001" "pat-a")
    (record-activation "sess-sum-001" "pat-b")
    (record-activation "sess-sum-001" "pat-c")
    (record-feedback "sess-sum-001" "pat-a" :helpful)
    (multiple-value-bind (activated feedback pending)
        (session-summary "sess-sum-001")
      (is (= 3 activated))
      (is (= 1 feedback))
      (is (= 2 (length pending)))
      (is (member "pat-b" pending :test #'string=))
      (is (member "pat-c" pending :test #'string=)))))

(test session-summary-empty-session
  "session-summary returns zeros for nonexistent session."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (multiple-value-bind (activated feedback pending)
        (session-summary "nonexistent")
      (is (= 0 activated))
      (is (= 0 feedback))
      (is (null pending)))))

;;; Session cleanup

(test cleanup-session-removes-state
  "cleanup-session removes session state entirely."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (get-or-create-session "sess-cleanup-001")
    (is (not (null (get-session "sess-cleanup-001"))))
    (cleanup-session "sess-cleanup-001")
    (is (null (get-session "sess-cleanup-001")))))

;;; Session JSON serialization

(test session-to-json-alist-structure
  "session-to-json-alist produces expected keys."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-activation "sess-json-001" "pat-a")
    (record-feedback "sess-json-001" "pat-a" :helpful)
    (record-domain "sess-json-001" "lisp")
    (let* ((session (get-session "sess-json-001"))
           (alist (session-to-json-alist session)))
      (is (string= "sess-json-001" (cdr (assoc "session_id" alist :test #'string=))))
      (is (= 1 (length (cdr (assoc "activated" alist :test #'string=)))))
      (is (= 1 (length (cdr (assoc "feedback" alist :test #'string=)))))
      (is (= 1 (length (cdr (assoc "domains" alist :test #'string=))))))))

;;; Multi-session isolation (transport-agnostic)

(test multiple-sessions-isolated
  "Sessions do not share state: activations in one are invisible to another."
  (let ((playbook-mcp::*session-states* (make-hash-table :test 'equal)))
    (record-activation "session-A" "pat-1")
    (record-activation "session-B" "pat-2")
    (let ((a (get-session "session-A"))
          (b (get-session "session-B")))
      (is (equal '("pat-1") (playbook-mcp::session-state-activated-patterns a)))
      (is (equal '("pat-2") (playbook-mcp::session-state-activated-patterns b))))))
