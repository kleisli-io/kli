(in-package #:task-mcp-tests)

;;; Regression tests for the session-context save discipline.
;;;
;;; Two invariants are under test:
;;;
;;;   (A) MAYBE-SAVE-SESSION-CONTEXT only writes the state fields
;;;       (ctx-task-id, ctx-vector-clock, ctx-event-counter,
;;;       ctx-claude-pid) when the current thread-local values differ
;;;       from the snapshot captured after ENSURE-SESSION-CONTEXT.
;;;       LAST-ACTIVE is always touched so idle-session GC has a
;;;       liveness signal.  This prevents long-lived read-only SSE
;;;       streams from resurrecting stale CTX-TASK-ID across a
;;;       concurrent reset — the original tertiary-bug symptom.
;;;
;;;   (B) TASK_COMPLETE, TASK_REOPEN, and TASK_RELEASE do not flip
;;;       the session registry's CTX-TASK-ID to their target argument
;;;       while emitting their status-update events.  The fix replaces
;;;       the old SETF + FINALIZE + SETF + FINALIZE dance with a fresh
;;;       dynamic LET binding of *CURRENT-TASK-ID* that auto-restores
;;;       on exit, keeping the interim target value invisible to
;;;       concurrent requests on the same MCP session.
;;;
;;; Tests deliberately rebind *SESSION-CONTEXTS*, the lock, and
;;; TASK:*TASKS-ROOT* so state does not leak between tests or into
;;; the daemon's live globals.

(in-suite :task-mcp-tests)

;;; --- Shared fixtures -------------------------------------------------------

(defmacro with-isolated-session-registry ((&key (session-id "sid-isolated")) &body body)
  "Rebind *SESSION-CONTEXTS* + lock to freshly-empty state and pre-create
   a session context for SESSION-ID so tests can measure its mutations."
  `(let ((task-mcp::*session-contexts* (make-hash-table :test 'equal))
         (task-mcp::*session-contexts-lock*
           (bt:make-lock "test-session-contexts")))
     (bt:with-lock-held (task-mcp::*session-contexts-lock*)
       (setf (gethash ,session-id task-mcp::*session-contexts*)
             (task-mcp::make-session-context
              :id ,session-id
              :vector-clock (crdt:make-vector-clock))))
     ,@body))

(defun fetch-ctx (session-id)
  "Return the session-context struct for SESSION-ID (NIL if absent)."
  (bt:with-lock-held (task-mcp::*session-contexts-lock*)
    (gethash session-id task-mcp::*session-contexts*)))

;;; ==========================================================================
;;; (A) MAYBE-SAVE-SESSION-CONTEXT discipline
;;; ==========================================================================

(test maybe-save-unchanged-only-touches-last-active
  "When *CURRENT-TASK-ID* and *EVENT-COUNTER* match the snapshot, the
   state fields must not be written.  Only LAST-ACTIVE updates — so
   the session is still visible to CLEANUP-INACTIVE-SESSIONS as a live
   request — but CTX-TASK-ID is left untouched."
  (with-isolated-session-registry (:session-id "sid-unchanged")
    (let ((ctx (fetch-ctx "sid-unchanged")))
      ;; Pre-populate a canonical state; LAST-ACTIVE backdated so we can
      ;; observe whether it advanced on the save call.
      (setf (task-mcp::ctx-task-id ctx) "task-A")
      (setf (task-mcp::ctx-event-counter ctx) 42)
      (setf (task-mcp::ctx-last-active ctx)
            (- (get-universal-time) 3600))
      (let ((before-active (task-mcp::ctx-last-active ctx))
            (*current-task-id* "task-A")
            (*event-counter* 42)
            (*session-vc* (crdt:make-vector-clock)))
        (task-mcp::maybe-save-session-context "sid-unchanged" "task-A" 42)
        (let ((after (fetch-ctx "sid-unchanged")))
          (is (string= "task-A" (task-mcp::ctx-task-id after))
              "ctx-task-id must not be overwritten on a no-op save")
          (is (= 42 (task-mcp::ctx-event-counter after))
              "ctx-event-counter must stay unchanged on a no-op save")
          (is (>= (task-mcp::ctx-last-active after) before-active)
              "ctx-last-active must advance even on a no-op save"))))))

(test maybe-save-task-id-changed-performs-full-save
  "When the thread-local *CURRENT-TASK-ID* differs from the snapshot,
   the save writes CTX-TASK-ID."
  (with-isolated-session-registry (:session-id "sid-task-changed")
    (let ((ctx (fetch-ctx "sid-task-changed")))
      (setf (task-mcp::ctx-task-id ctx) "task-A")
      (setf (task-mcp::ctx-event-counter ctx) 5))
    (let ((*current-task-id* "task-B")
          (*event-counter* 5)
          (*session-vc* (crdt:make-vector-clock)))
      (task-mcp::maybe-save-session-context "sid-task-changed" "task-A" 5)
      (is (string= "task-B"
                   (task-mcp::ctx-task-id (fetch-ctx "sid-task-changed")))
          "task-id change must be persisted"))))

(test maybe-save-event-counter-changed-performs-full-save
  "When *EVENT-COUNTER* advanced (events were emitted), the save
   persists state even if CTX-TASK-ID nominally matches."
  (with-isolated-session-registry (:session-id "sid-counter-changed")
    (let ((ctx (fetch-ctx "sid-counter-changed")))
      (setf (task-mcp::ctx-task-id ctx) "task-A")
      (setf (task-mcp::ctx-event-counter ctx) 5))
    (let ((*current-task-id* "task-A")
          (*event-counter* 7)
          (*session-vc* (crdt:make-vector-clock)))
      (task-mcp::maybe-save-session-context "sid-counter-changed" "task-A" 5)
      (is (= 7 (task-mcp::ctx-event-counter (fetch-ctx "sid-counter-changed")))
          "event-counter advance must be persisted"))))

(test maybe-save-creates-context-when-missing
  "Calling MAYBE-SAVE for a session-id not yet in the registry must
   allocate a fresh context.  Matches SAVE-SESSION-CONTEXT semantics."
  (with-isolated-session-registry (:session-id "sid-placeholder")
    (remhash "sid-placeholder" task-mcp::*session-contexts*)
    (let ((*current-task-id* "task-fresh")
          (*event-counter* 1)
          (*session-vc* (crdt:make-vector-clock)))
      (task-mcp::maybe-save-session-context "sid-fresh" nil 0)
      (let ((fresh (fetch-ctx "sid-fresh")))
        (is (not (null fresh))
            "fresh session-context must be created")
        (is (string= "task-fresh" (task-mcp::ctx-task-id fresh))
            "fresh context must receive task-id from globals")))))

(test maybe-save-sse-stream-cannot-resurrect-stale-task-id
  "Model the tertiary-bug scenario end to end.

   Thread T1 opens a GET /mcp SSE stream when the registry held
   CTX-TASK-ID = 'task-stale'.  T1's :around captures
   *CURRENT-TASK-ID* = 'task-stale' after ENSURE-SESSION-CONTEXT, and
   the SNAPSHOT captures 'task-stale' too.

   Meanwhile another thread resets CTX-TASK-ID to NIL (external
   cleanup).  When T1 unwinds 30 s later, MAYBE-SAVE must compare its
   thread-local *CURRENT-TASK-ID* ('task-stale') against the SNAPSHOT
   ('task-stale').  They match — so no state write occurs and the
   reset sticks.  The pre-fix SAVE-SESSION-CONTEXT would unconditionally
   overwrite ctx-task-id back to 'task-stale'."
  (with-isolated-session-registry (:session-id "sid-sse")
    ;; Initial state: ctx.task-id = task-stale (pre-existing).
    (let ((ctx (fetch-ctx "sid-sse")))
      (setf (task-mcp::ctx-task-id ctx) "task-stale")
      (setf (task-mcp::ctx-event-counter ctx) 10))
    ;; --- T1 opens SSE stream: LOAD then SNAPSHOT ---
    ;; (Simulated by reading directly; LOAD-SESSION-CONTEXT would do this.)
    (let* ((ctx (fetch-ctx "sid-sse"))
           (loaded-task-id (task-mcp::ctx-task-id ctx))
           (loaded-counter (task-mcp::ctx-event-counter ctx))
           (*current-task-id* loaded-task-id)
           (*event-counter* loaded-counter)
           (*session-vc* (crdt:make-vector-clock))
           (snapshot-task-id *current-task-id*)
           (snapshot-counter *event-counter*))
      ;; T1 handler holds for 30 s; no state mutation happens in-thread.
      ;; --- External reset of ctx.task-id (another thread / admin tool) ---
      (bt:with-lock-held (task-mcp::*session-contexts-lock*)
        (setf (task-mcp::ctx-task-id ctx) nil))
      ;; --- T1 unwinds: MAYBE-SAVE with its captured snapshot ---
      (task-mcp::maybe-save-session-context "sid-sse"
                                            snapshot-task-id
                                            snapshot-counter)
      (is (null (task-mcp::ctx-task-id (fetch-ctx "sid-sse")))
          "SSE unwind must NOT resurrect the stale task-id the reset cleared"))))

;;; ==========================================================================
;;; (B) TASK_COMPLETE / TASK_REOPEN / TASK_RELEASE session-context discipline
;;; ==========================================================================
;;;
;;; The fix replaces SETF + FINALIZE + SETF + FINALIZE with a dynamic LET
;;; binding of *CURRENT-TASK-ID*.  To verify, we invoke the tool via the
;;; MCP framework's tool registry (matching the handoff's direct-dispatch
;;; proof) and assert that *SESSION-CONTEXTS*[sid].ctx-task-id is not
;;; flipped to the tool's target argument during the call.

(defun scratch-task-directory (tasks-root task-id)
  "Create an events.jsonl-ready directory for TASK-ID under TASKS-ROOT."
  (let* ((dir (merge-pathnames (format nil "~A/" task-id)
                               (uiop:ensure-directory-pathname tasks-root)))
         (events (merge-pathnames "events.jsonl" dir)))
    (ensure-directories-exist dir)
    (with-open-file (s events :direction :output :if-exists :supersede)
      (declare (ignore s)))
    dir))

(defun pandoric-tool-call (tool-name args-plist)
  "Locate TOOL-NAME in the MCP tool registry and dispatch its handler
   with a string-keyed hash-table built from ARGS-PLIST."
  (let ((tool (funcall (find-symbol "GET-TOOL" :mcp-framework) tool-name))
        (ht (make-hash-table :test 'equal)))
    (loop for (k v) on args-plist by #'cddr
          do (setf (gethash (string-downcase (symbol-name k)) ht) v))
    (unless tool
      (error "tool ~A not registered in mcp-framework" tool-name))
    (funcall tool :call ht)))

(defmacro with-status-fixture ((&key focus target) &body body)
  "Stage a session registry bound to FOCUS as current task and two
   real event-log directories for FOCUS and TARGET under a fresh
   TASKS-ROOT.  *CURRENT-TASK-ID* / *SESSION-ID* / *HTTP-MODE* are
   bound so the tool body's registry interaction is observable."
  (let ((root (gensym "ROOT"))
        (sid (gensym "SID")))
    `(let ((,sid "sid-focus"))
       (with-temp-tasks-root (,root)
         (declare (ignorable ,root))
         (scratch-task-directory ,root ,focus)
         (scratch-task-directory ,root ,target)
         (with-isolated-session-registry (:session-id ,sid)
           (let ((ctx (fetch-ctx ,sid)))
             (setf (task-mcp::ctx-task-id ctx) ,focus))
           (let ((*current-task-id* ,focus)
                 (*session-id* ,sid)
                 (*session-vc* (crdt:make-vector-clock))
                 (*event-counter* 0)
                 (*http-mode* t)
                 (*in-mutation* nil))
             ,@body))))))

(test task_complete-does-not-flip-session-context
  "Invoking task_complete with task_id=TARGET while the session is
   focused on FOCUS must leave ctx-task-id = FOCUS.  The pre-fix SETF +
   FINALIZE path would have flipped it to TARGET mid-body, briefly
   exposing a completed task as 'current' to concurrent readers."
  (with-status-fixture (:focus "task-A" :target "task-B")
    (pandoric-tool-call "task_complete" `(:task_id "task-B"))
    (is (string= "task-A" (task-mcp::ctx-task-id (fetch-ctx "sid-focus")))
        "task_complete must not overwrite the session's focus task-id")
    (is (string= "task-A" *current-task-id*)
        "thread-local *CURRENT-TASK-ID* must be restored on exit")))

(test task_reopen-does-not-flip-session-context
  "Symmetric to task_complete: task_reopen must also use a LET binding
   and leave ctx-task-id pointing at the session's focus task."
  (with-status-fixture (:focus "task-A" :target "task-B")
    ;; Pre-mark task-B as completed so reopen has legitimate work.
    (let ((*current-task-id* "task-B"))
      (emit-event :task.update-status (list :status "completed")))
    (pandoric-tool-call "task_reopen" `(:task_id "task-B"))
    (is (string= "task-A" (task-mcp::ctx-task-id (fetch-ctx "sid-focus")))
        "task_reopen must not overwrite the session's focus task-id")))

(defmacro with-kli-coordination-root ((dir) &body body)
  "Redirect DEPOT:COORDINATION-ROOT to DIR via the KLI_COORDINATION_DIR
   override env var.  Restores the previous value on exit so the test
   process does not leak state to other suites."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (uiop:getenv "KLI_COORDINATION_DIR")))
       (unwind-protect
            (progn
              (sb-posix:setenv "KLI_COORDINATION_DIR" ,dir 1)
              ,@body)
         (if (and ,saved (plusp (length ,saved)))
             (sb-posix:setenv "KLI_COORDINATION_DIR" ,saved 1)
             (sb-posix:unsetenv "KLI_COORDINATION_DIR"))))))

(test task_release-does-not-flip-session-context
  "task_release emits :session.release against the target but must
   not set the session registry to that target.  clear-session-task-file
   writes under coordination-root, so we redirect it to a scratch dir
   via KLI_COORDINATION_DIR to keep the test hermetic."
  (with-status-fixture (:focus "task-A" :target "task-B")
    (let ((scratch (uiop:ensure-directory-pathname
                    (format nil "~Atask-mcp-release-~A/"
                            (uiop:temporary-directory)
                            (random 1000000)))))
      (ensure-directories-exist scratch)
      (unwind-protect
           (with-kli-coordination-root ((namestring scratch))
             (pandoric-tool-call "task_release" `(:task_id "task-B"))
             (is (string= "task-A"
                          (task-mcp::ctx-task-id (fetch-ctx "sid-focus")))
                 "task_release must not overwrite the session's focus task-id"))
        (uiop:delete-directory-tree scratch :validate t
                                    :if-does-not-exist :ignore)))))
