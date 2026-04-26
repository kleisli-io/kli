(in-package #:task-mcp-tests)

;;; Regression tests for the get-OR-create antipattern fix.
;;;
;;; Background: GET-SESSION-CONTEXT used to be get-OR-create — any
;;; unknown Mcp-Session-Id silently inserted a virgin context with
;;; task-id=NIL into *SESSION-CONTEXTS*.  Combined with the fact
;;; that the framework's mcp-http session-store has no TTL but
;;; task-mcp's CLEANUP-INACTIVE-SESSIONS evicts after 4h, this caused
;;; silent session resurrection: a CLI request that survived a daemon
;;; restart or 4h idle would land on a fresh virgin context, with no
;;; way for the client to know its state had been lost.
;;;
;;; The fix splits creation from lookup:
;;;
;;;   GET-SESSION-CONTEXT     — pure read, NIL on miss.
;;;   CREATE-SESSION-CONTEXT  — explicit, idempotent insertion.
;;;   LOAD-SESSION-CONTEXT    — signals UNKNOWN-SESSION on miss.
;;;
;;; The :AROUND method translates UNKNOWN-SESSION to HTTP 404 with
;;; code=session-not-found so the CLI can detect the case and
;;; re-initialize.  The CLI's MCP-CALL retries the request once after
;;; re-initializing.
;;;
;;; REGISTER-CLAUDE-PID is a legitimate creation path (SessionStart
;;; hook may run before the first tool call) and uses
;;; CREATE-SESSION-CONTEXT.

(in-suite :task-mcp-tests)

;;; --- Helpers --------------------------------------------------------------

(defmacro with-empty-session-contexts (() &body body)
  "Rebind *SESSION-CONTEXTS* to a fresh empty hash so test inserts do
   not bleed into the daemon's running registry."
  `(let ((task-mcp::*session-contexts* (make-hash-table :test 'equal))
         (task-mcp::*session-contexts-lock*
           (bt:make-lock "test-session-contexts")))
     ,@body))

;;; --- GET-SESSION-CONTEXT contract ----------------------------------------

(test get-session-context-returns-nil-on-miss
  "GET-SESSION-CONTEXT must NOT auto-create. Looking up an unknown sid
   returns NIL and leaves the registry empty — silent resurrection of an
   evicted sid into a virgin context is the defect this guards against."
  (with-empty-session-contexts ()
    (is (null (task-mcp::get-session-context "STALE-SID-NOT-IN-REGISTRY")))
    (is (zerop (hash-table-count task-mcp::*session-contexts*))
        "stale lookup must not have inserted a virgin context")))

(test get-session-context-returns-existing-ctx
  "On a hit, GET-SESSION-CONTEXT returns the existing context unchanged."
  (with-empty-session-contexts ()
    (let ((ctx (task-mcp::create-session-context "live-sid")))
      (is (eq ctx (task-mcp::get-session-context "live-sid"))))))

;;; --- CREATE-SESSION-CONTEXT contract -------------------------------------

(test create-session-context-inserts-fresh
  "CREATE-SESSION-CONTEXT inserts a context for the given sid and
   returns it; the registry now contains exactly one entry."
  (with-empty-session-contexts ()
    (let ((ctx (task-mcp::create-session-context "fresh-sid")))
      (is (not (null ctx)))
      (is (equal "fresh-sid" (task-mcp::ctx-id ctx)))
      (is (null (task-mcp::ctx-task-id ctx)))
      (is (= 1 (hash-table-count task-mcp::*session-contexts*)))
      (is (eq ctx (gethash "fresh-sid" task-mcp::*session-contexts*))))))

(test create-session-context-is-idempotent
  "Creating an already-existing context returns the SAME instance.
   Required so legitimate creation paths (post-/initialize hook,
   register-claude-pid) cannot accidentally replace existing state."
  (with-empty-session-contexts ()
    (let ((first (task-mcp::create-session-context "sid-X")))
      (setf (task-mcp::ctx-task-id first) "marker-task-id")
      (let ((second (task-mcp::create-session-context "sid-X")))
        (is (eq first second))
        (is (equal "marker-task-id" (task-mcp::ctx-task-id second))
            "idempotent re-create must NOT clear existing task-id")))))

;;; --- LOAD-SESSION-CONTEXT signals UNKNOWN-SESSION on miss ----------------

(test load-session-context-signals-unknown-session-on-stale-sid
  "LOAD-SESSION-CONTEXT signals UNKNOWN-SESSION when the sid is not
   in the registry.  The :AROUND method catches this and translates
   to HTTP 404 + code=session-not-found."
  (with-empty-session-contexts ()
    (signals task-mcp::unknown-session
      (task-mcp::load-session-context "evicted-sid"))))

(test unknown-session-condition-carries-session-id
  "The condition's UNKNOWN-SESSION-SESSION-ID accessor returns the
   sid that triggered the error so the 404 body can echo it back."
  (with-empty-session-contexts ()
    (handler-case
        (task-mcp::load-session-context "echo-test-sid")
      (task-mcp::unknown-session (c)
        (is (equal "echo-test-sid"
                   (task-mcp::unknown-session-session-id c)))
        (is (search "echo-test-sid" (princ-to-string c)))))))

(test load-session-context-succeeds-on-existing-ctx
  "LOAD-SESSION-CONTEXT is a no-op (no error) when the ctx exists.
   It binds *session-id* and *current-task-id* from the loaded ctx."
  (with-empty-session-contexts ()
    (let ((ctx (task-mcp::create-session-context "good-sid")))
      (setf (task-mcp::ctx-task-id ctx) "task-A")
      (let ((task-mcp::*session-id* nil)
            (task-mcp::*current-task-id* nil)
            (task-mcp::*session-vc* nil)
            (task-mcp::*event-counter* 0)
            (task-mcp::*claude-pid* nil))
        (task-mcp::load-session-context "good-sid")
        (is (equal "good-sid" task-mcp::*session-id*))
        (is (equal "task-A" task-mcp::*current-task-id*))))))

;;; --- REGISTER-CLAUDE-PID is a legitimate creation path -------------------

(test register-claude-pid-creates-ctx-when-absent
  "The SessionStart hook may run before any /mcp tool call has loaded
   the MCP session, so REGISTER-CLAUDE-PID must legitimately create
   the context.  This is the ONE other creation path besides the
   :AROUND method's post-/initialize hook."
  (with-empty-session-contexts ()
    (let ((result (task-mcp::register-claude-pid "session-from-hook" 12345)))
      (is (search "Registered PID 12345" result))
      (let ((ctx (gethash "session-from-hook"
                          task-mcp::*session-contexts*)))
        (is (not (null ctx)))
        (is (eql 12345 (task-mcp::ctx-claude-pid ctx)))))))

(test register-claude-pid-updates-existing-ctx
  "When the ctx already exists, REGISTER-CLAUDE-PID updates the
   PID without replacing the ctx.  Required so SessionStart-hook
   firing after the first tool call doesn't clobber accumulated
   session state."
  (with-empty-session-contexts ()
    (let ((existing (task-mcp::create-session-context "live-session")))
      (setf (task-mcp::ctx-task-id existing) "task-in-progress")
      (task-mcp::register-claude-pid "live-session" 99999
                                     :team-name "kli"
                                     :agent-name "researcher"
                                     :agent-type "research")
      (let ((after (gethash "live-session" task-mcp::*session-contexts*)))
        (is (eq existing after)
            "register-claude-pid must reuse the existing ctx")
        (is (eql 99999 (task-mcp::ctx-claude-pid after)))
        (is (equal "task-in-progress" (task-mcp::ctx-task-id after))
            "existing task-id must survive PID registration")
        (is (equal "kli" (task-mcp::ctx-team-name after)))))))
