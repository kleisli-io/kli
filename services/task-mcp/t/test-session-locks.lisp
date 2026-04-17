(in-package #:task-mcp-tests)

;;; Regression tests for the session-context lock-discipline fixes.
;;;
;;; Two invariants are under test:
;;;
;;;   (A) WITH-TIMED-LOCK records acquisition and hold durations into
;;;       the ring buffer so /admin/diag can surface contention.
;;;
;;;   (B) RESOLVE-PEER-PID/LINUX no longer holds
;;;       *SESSION-CONTEXTS-LOCK* across /proc I/O: the lock is held
;;;       only for SNAPSHOT-REGISTERED-PIDS (a bounded MAPHASH), and
;;;       PID-OWNS-SOCKET-P short-circuits dead PIDs via PROBE-FILE.
;;;
;;; Tests in this file are deliberately isolated: they rebind
;;; *SESSION-CONTEXTS* and the trace buffer to fresh values so they
;;; don't leak into or out of the daemon's globals.

(in-suite :task-mcp-tests)

;;; --- Helpers ---------------------------------------------------------------

(defmacro with-fresh-lock-trace (() &body body)
  "Rebind the lock-trace ring buffer and cursor to freshly-zeroed
   state so assertions in BODY see only events generated here."
  `(let ((task-mcp::*lock-trace*
           (make-array task-mcp::+lock-trace-size+ :initial-element nil))
         (task-mcp::*lock-trace-pos* (cons 0 nil)))
     ,@body))

(defmacro with-fresh-session-contexts (() &body body)
  "Rebind *SESSION-CONTEXTS* and its lock so test population doesn't
   bleed into the daemon's registry."
  `(let ((task-mcp::*session-contexts* (make-hash-table :test 'equal))
         (task-mcp::*session-contexts-lock*
           (bt:make-lock "test-session-contexts")))
     ,@body))

(defun register-fake-session (session-id pid)
  "Insert a minimal session-context for SESSION-ID with CTX-CLAUDE-PID
   set to PID.  Uses the test-bound *SESSION-CONTEXTS* and lock."
  (bt:with-lock-held (task-mcp::*session-contexts-lock*)
    (let ((ctx (task-mcp::make-session-context
                :id session-id
                :vector-clock (crdt:make-vector-clock))))
      (setf (task-mcp::ctx-claude-pid ctx) pid)
      (setf (gethash session-id task-mcp::*session-contexts*) ctx)
      ctx)))

(defun assuredly-dead-pid (seed)
  "Return a PID that is guaranteed to have no /proc/<pid> entry.
   Uses values well above the kernel's pid_max ceiling so PROBE-FILE
   reliably returns NIL."
  (+ 2000000 seed))

;;; --- (A) WITH-TIMED-LOCK bookkeeping --------------------------------------

(test with-timed-lock-records-wait-and-hold
  "WITH-TIMED-LOCK must push one event per acquisition into
   *LOCK-TRACE*, with both wait-ms and hold-ms as non-negative
   integers and :NAME / :THREAD populated."
  (with-fresh-lock-trace ()
    (let ((test-lock (bt:make-lock "test-wtl")))
      (task-mcp::with-timed-lock (test-lock :probe-event)
        (sleep 0.01))
      (let ((events (task-mcp::recent-lock-events)))
        (is (= 1 (length events))
            "expected exactly one event, got ~D" (length events))
        (let ((e (first events)))
          (is (eq :probe-event (getf e :name)))
          (is (integerp (getf e :wait-ms)))
          (is (integerp (getf e :hold-ms)))
          (is (>= (getf e :wait-ms) 0))
          (is (>= (getf e :hold-ms) 0))
          (is (stringp (getf e :thread))))))))

(test with-timed-lock-unwinds-on-error
  "An error inside WITH-TIMED-LOCK must still record the event and
   release the lock.  We verify the event is recorded and that a
   second acquisition succeeds immediately."
  (with-fresh-lock-trace ()
    (let ((test-lock (bt:make-lock "test-wtl-unwind")))
      (ignore-errors
       (task-mcp::with-timed-lock (test-lock :unwind-event)
         (error "intentional")))
      ;; Lock must be released — a second acquisition should not block.
      (task-mcp::with-timed-lock (test-lock :post-unwind)
        nil)
      (let ((events (task-mcp::recent-lock-events)))
        (is (= 2 (length events)))
        (is (find :unwind-event events :key (lambda (e) (getf e :name))))
        (is (find :post-unwind events :key (lambda (e) (getf e :name))))))))

(test recent-lock-events-respects-size-cap
  "After recording more events than +LOCK-TRACE-SIZE+, the ring buffer
   keeps only the most recent slice.  The cursor advances monotonically
   and the returned list length is at most the cap."
  (with-fresh-lock-trace ()
    (let ((test-lock (bt:make-lock "test-ring"))
          (iterations (+ task-mcp::+lock-trace-size+ 5)))
      (dotimes (i iterations)
        (declare (ignore i))
        (task-mcp::with-timed-lock (test-lock :fill)
          nil))
      (let ((events (task-mcp::recent-lock-events)))
        (is (<= (length events) task-mcp::+lock-trace-size+))))))

;;; --- (B) RESOLVE-PEER-PID / SNAPSHOT discipline ---------------------------

(test snapshot-registered-pids-returns-unique
  "SNAPSHOT-REGISTERED-PIDS must return the set of distinct claude
   PIDs, excluding NIL entries.  Duplicate registrations collapse."
  (with-fresh-session-contexts ()
    (register-fake-session "s1" 1001)
    (register-fake-session "s2" 1002)
    (register-fake-session "s3" 1001)  ; duplicate claude-pid
    (register-fake-session "s4" nil)   ; no pid — must be skipped
    (let ((pids (task-mcp::snapshot-registered-pids)))
      (is (= 2 (length pids)))
      (is (member 1001 pids))
      (is (member 1002 pids))
      (is (not (member nil pids))))))

(test snapshot-registered-pids-lock-hold-is-bounded
  "Populating *SESSION-CONTEXTS* with many entries, then calling
   SNAPSHOT-REGISTERED-PIDS, must hold the lock for only a few
   milliseconds — the function's sole work under the lock is a
   MAPHASH into a local list."
  (with-fresh-lock-trace ()
    (with-fresh-session-contexts ()
      (dotimes (i 500)
        (register-fake-session (format nil "s-~D" i)
                               (assuredly-dead-pid i)))
      (task-mcp::snapshot-registered-pids)
      (let ((events (remove-if-not
                     (lambda (e) (eq (getf e :name) :snapshot-pids))
                     (task-mcp::recent-lock-events))))
        (is (plusp (length events))
            "expected a :snapshot-pids event in the trace")
        (dolist (e events)
          (is (< (getf e :hold-ms) 100)
              ":snapshot-pids held lock ~Dms, budget was <100ms"
              (getf e :hold-ms)))))))

(test pid-owns-socket-p-short-circuits-dead-pid
  "PID-OWNS-SOCKET-P must return NIL immediately for a PID with no
   /proc/<pid> entry, via the PROBE-FILE pre-check.  A 200-entry
   dead-PID sweep must complete well under one second — if the
   short-circuit regresses, each call would make 256 READLINK
   syscalls for nothing."
  (let ((start (get-internal-real-time)))
    (dotimes (i 200)
      (is (null (task-mcp::pid-owns-socket-p (assuredly-dead-pid i) 1))))
    (let ((elapsed-ms (floor (* 1000 (- (get-internal-real-time) start))
                             internal-time-units-per-second)))
      (is (< elapsed-ms 500)
          "200 dead-PID probes took ~Dms, budget was <500ms" elapsed-ms))))

(test snapshot-pids-does-not-block-concurrent-save
  "While one thread iterates the snapshot outside the lock, another
   thread must be able to take the lock and add a session-context.
   Concretely, we take the snapshot first (releases the lock), then
   we save a context from the same thread — both operations succeed
   and both events appear in the trace."
  (with-fresh-lock-trace ()
    (with-fresh-session-contexts ()
      (dotimes (i 10)
        (register-fake-session (format nil "pre-~D" i)
                               (assuredly-dead-pid i)))
      (let ((snapshot (task-mcp::snapshot-registered-pids)))
        (is (= 10 (length snapshot))))
      ;; After snapshot, lock must be free — a fresh registration must not block.
      (let ((done nil))
        (let ((t1 (bt:make-thread
                   (lambda ()
                     (register-fake-session "post" 99999)
                     (setf done t))
                   :name "post-snapshot-register")))
          (bt:join-thread t1)
          (is (eq done t) "concurrent registration did not complete"))))))

;;; --- Session GC thread -----------------------------------------------------

(test cleanup-inactive-sessions-removes-old-keeps-fresh
  "CLEANUP-INACTIVE-SESSIONS must remove contexts whose LAST-ACTIVE is
   older than MAX-AGE-HOURS and leave fresh ones alone.  Uses a test
   rebinding so the daemon's registry is not touched."
  (with-fresh-session-contexts ()
    (let ((old-ctx (register-fake-session "old" 1111))
          (fresh-ctx (register-fake-session "fresh" 2222)))
      ;; old: 25 hours ago; fresh: now.
      (setf (task-mcp::ctx-last-active old-ctx)
            (- (get-universal-time) (* 25 3600)))
      (setf (task-mcp::ctx-last-active fresh-ctx)
            (get-universal-time))
      (let ((removed (task-mcp::cleanup-inactive-sessions 4)))
        (is (= 1 removed))
        (is (null (gethash "old" task-mcp::*session-contexts*)))
        (is (gethash "fresh" task-mcp::*session-contexts*))))))

(test session-gc-thread-lifecycle
  "START-SESSION-GC-THREAD spawns a live thread.  STOP-SESSION-GC-THREAD
   sets the stop flag, and within a bounded window the thread exits.
   This test manipulates the daemon's own GC globals because the GC
   thread reads globals directly; teardown restores them."
  (let ((old-interval task-mcp::*session-gc-interval-seconds*)
        (old-max-age task-mcp::*session-gc-max-age-hours*))
    (unwind-protect
         (progn
           (setf task-mcp::*session-gc-interval-seconds* 0
                 task-mcp::*session-gc-max-age-hours* 9999
                 task-mcp::*session-gc-stop* nil
                 task-mcp::*session-gc-thread* nil)
           (let ((thr (task-mcp::start-session-gc-thread)))
             (is (bt:thread-alive-p thr)
                 "GC thread must be alive after START"))
           (task-mcp::stop-session-gc-thread)
           (let ((thr task-mcp::*session-gc-thread*)
                 (deadline (+ (get-internal-real-time)
                              (* 3 internal-time-units-per-second))))
             (loop while (and thr (bt:thread-alive-p thr)
                              (< (get-internal-real-time) deadline))
                   do (sleep 0.05))
             (is (not (and thr (bt:thread-alive-p thr)))
                 "GC thread did not exit after stop flag was set")))
      ;; Teardown — ensure no stray thread outlives the test.
      (setf task-mcp::*session-gc-stop* t)
      (let ((thr task-mcp::*session-gc-thread*))
        (when (and thr (bt:thread-alive-p thr))
          (ignore-errors (bt:join-thread thr))))
      (setf task-mcp::*session-gc-interval-seconds* old-interval
            task-mcp::*session-gc-max-age-hours* old-max-age
            task-mcp::*session-gc-thread* nil
            task-mcp::*session-gc-stop* nil))))

;;; --- /admin/unwedge ---------------------------------------------------------

(test perform-unwedge-evicts-old-keeps-fresh
  "PERFORM-UNWEDGE must call CLEANUP-INACTIVE-SESSIONS and report the
   delta in its plist.  We populate the registry with one stale and one
   fresh context; PERFORM-UNWEDGE with max-age 1h evicts the stale one
   (backdated 25h), leaves the fresh one, and reports correct
   before/after counts.  A wide window keeps the test resilient to
   build-host scheduling jitter."
  (with-fresh-session-contexts ()
    (let ((stale (register-fake-session "stale" 9001))
          (fresh (register-fake-session "fresh-pid" 9002)))
      (setf (task-mcp::ctx-last-active stale)
            (- (get-universal-time) (* 25 3600)))    ; 25h ago
      (setf (task-mcp::ctx-last-active fresh)
            (get-universal-time))                    ; now
      (let ((result (task-mcp::perform-unwedge 1)))
        (is (= 1 (getf result :sessions-removed)))
        (is (= 1 (getf result :max-age-hours)))
        (is (equal '("graph" "infos" "task-state")
                   (getf result :caches-cleared)))
        (is (= 2 (getf (getf result :before) :sessions)))
        (is (= 1 (getf (getf result :after) :sessions)))
        (is (null (gethash "stale" task-mcp::*session-contexts*)))
        (is (gethash "fresh-pid" task-mcp::*session-contexts*))))))

(test parse-max-age-param-defaults-on-blank-or-junk
  "PARSE-MAX-AGE-PARAM returns the default for nil, empty string, and
   non-numeric input; otherwise the parsed positive real."
  (let ((default task-mcp::*unwedge-default-max-age-hours*))
    (is (eql default (task-mcp::parse-max-age-param nil)))
    (is (eql default (task-mcp::parse-max-age-param "")))
    (is (eql default (task-mcp::parse-max-age-param "  ")))
    (is (eql default (task-mcp::parse-max-age-param "garbage")))
    (is (= 2 (task-mcp::parse-max-age-param "2")))
    (is (= 0.5d0 (task-mcp::parse-max-age-param "0.5")))
    ;; Reject zero and negative as invalid → fall back to default.
    (is (eql default (task-mcp::parse-max-age-param "0")))
    (is (eql default (task-mcp::parse-max-age-param "-3")))))

(test unwedge-json-stream-shape
  "UNWEDGE-JSON-STREAM emits the contract /admin/unwedge consumers
   depend on: top-level keys before/after/sessions_removed/max_age_hours/
   caches_cleared, and each before/after has sessions/threads/close_wait."
  (let* ((plist (list :before (list :sessions 5 :threads 30 :close-wait 2)
                      :after  (list :sessions 1 :threads 28 :close-wait 0)
                      :sessions-removed 4
                      :max-age-hours 0.25
                      :caches-cleared (list "graph" "infos" "task-state")))
         (json (with-output-to-string (s)
                 (task-mcp::unwedge-json-stream s plist)))
         (parsed (with-input-from-string (s json) (yason:parse s))))
    (is (= 4 (gethash "sessions_removed" parsed)))
    (is (= 0.25 (gethash "max_age_hours" parsed)))
    (is (equal '("graph" "infos" "task-state")
               (gethash "caches_cleared" parsed)))
    (let ((before (gethash "before" parsed))
          (after (gethash "after" parsed)))
      (is (= 5 (gethash "sessions" before)))
      (is (= 30 (gethash "threads" before)))
      (is (= 2 (gethash "close_wait" before)))
      (is (= 1 (gethash "sessions" after)))
      (is (= 28 (gethash "threads" after)))
      (is (= 0 (gethash "close_wait" after))))))

;;; --- /register-pid race vs concurrent SAVE / MAYBE-SAVE --------------------

(test save-session-context-preserves-late-registered-pid
  "SAVE-SESSION-CONTEXT must NOT overwrite CTX-CLAUDE-PID when the
   thread-local *CLAUDE-PID* is NIL.  Models the race where a request
   loads ctx (claude-pid=NIL) before /register-pid arrives, and then
   unwinds after /register-pid set ctx-claude-pid=12345.  Without the
   guard the unwind save would clobber the registration with NIL."
  (let ((task-mcp::*session-contexts* (make-hash-table :test 'equal))
        (task-mcp::*session-contexts-lock*
          (bt:make-lock "test-save-pid-race")))
    (bt:with-lock-held (task-mcp::*session-contexts-lock*)
      (setf (gethash "race-sid" task-mcp::*session-contexts*)
            (task-mcp::make-session-context
             :id "race-sid"
             :vector-clock (crdt:make-vector-clock))))
    ;; /register-pid lands first — write 12345 directly under the lock.
    (bt:with-lock-held (task-mcp::*session-contexts-lock*)
      (setf (task-mcp::ctx-claude-pid
             (gethash "race-sid" task-mcp::*session-contexts*))
            12345))
    ;; Concurrent in-flight request unwinds with *claude-pid* = NIL.
    (let ((*current-task-id* nil)
          (*claude-pid* nil)
          (*event-counter* 0)
          (*session-vc* (crdt:make-vector-clock)))
      (task-mcp::save-session-context "race-sid"))
    (is (eql 12345
             (task-mcp::ctx-claude-pid
              (bt:with-lock-held (task-mcp::*session-contexts-lock*)
                (gethash "race-sid" task-mcp::*session-contexts*))))
        "save-session-context must not overwrite a registered PID with NIL")))

(test maybe-save-session-context-preserves-late-registered-pid
  "Symmetric guard for MAYBE-SAVE-SESSION-CONTEXT: when *EVENT-COUNTER*
   advanced (full save path) but *CLAUDE-PID* is NIL, do not clobber
   a concurrently registered PID."
  (let ((task-mcp::*session-contexts* (make-hash-table :test 'equal))
        (task-mcp::*session-contexts-lock*
          (bt:make-lock "test-maybe-save-pid-race")))
    (bt:with-lock-held (task-mcp::*session-contexts-lock*)
      (setf (gethash "race-sid-2" task-mcp::*session-contexts*)
            (task-mcp::make-session-context
             :id "race-sid-2"
             :vector-clock (crdt:make-vector-clock))))
    (bt:with-lock-held (task-mcp::*session-contexts-lock*)
      (setf (task-mcp::ctx-claude-pid
             (gethash "race-sid-2" task-mcp::*session-contexts*))
            54321))
    (let ((*current-task-id* nil)
          (*claude-pid* nil)
          ;; event-counter advanced from 0 → 1, so the maybe-save full
          ;; path runs (not the no-op branch).
          (*event-counter* 1)
          (*session-vc* (crdt:make-vector-clock)))
      (task-mcp::maybe-save-session-context "race-sid-2" nil 0))
    (is (eql 54321
             (task-mcp::ctx-claude-pid
              (bt:with-lock-held (task-mcp::*session-contexts-lock*)
                (gethash "race-sid-2" task-mcp::*session-contexts*))))
        "maybe-save full-save path must not overwrite a registered PID with NIL")))

;;; --- VC copy-on-load + merge-on-save ---------------------------------------

(test vc-copy-returns-independent-struct
  "VC-COPY must return a fresh VECTOR-CLOCK whose entries hash table
   is NOT EQ to the source's.  Mutating the copy must not change the
   original."
  (let ((src (crdt:make-vector-clock)))
    (crdt:vc-increment src "sid-A")
    (crdt:vc-increment src "sid-A")
    (let ((dst (crdt:vc-copy src)))
      (is (not (eq (crdt:vc-entries src) (crdt:vc-entries dst)))
          "vc-copy must allocate a fresh entries hash table")
      (is (= 2 (crdt:vc-get dst "sid-A"))
          "vc-copy must preserve existing entries")
      (crdt:vc-increment dst "sid-A")
      (is (= 2 (crdt:vc-get src "sid-A"))
          "mutating the copy must not affect the source")
      (is (= 3 (crdt:vc-get dst "sid-A")))
      (crdt:vc-increment src "sid-B")
      (is (= 0 (crdt:vc-get dst "sid-B"))
          "mutating the source must not affect the copy"))))

(test load-session-context-isolates-vc-from-registry
  "LOAD-SESSION-CONTEXT must give the request a VC whose entries hash
   table is independent of the registry's stored VC.  Otherwise two
   concurrent requests on the same session would VC-INCREMENT the same
   hash table — SBCL hash tables are not safe for concurrent writes."
  (with-fresh-session-contexts ()
    (let* ((registry-vc (crdt:make-vector-clock))
           (ctx (task-mcp::make-session-context
                 :id "sid-iso"
                 :vector-clock registry-vc)))
      (crdt:vc-increment registry-vc "sid-iso")
      (bt:with-lock-held (task-mcp::*session-contexts-lock*)
        (setf (gethash "sid-iso" task-mcp::*session-contexts*) ctx))
      (let ((*session-id* nil)
            (*current-task-id* nil)
            (*session-vc* nil)
            (*claude-pid* nil)
            (*event-counter* 0))
        (task-mcp::load-session-context "sid-iso")
        (is (not (eq registry-vc *session-vc*))
            "*session-vc* must be a fresh struct, not the registry's struct")
        (is (not (eq (crdt:vc-entries registry-vc)
                     (crdt:vc-entries *session-vc*)))
            "entries hash table must be independent")
        (is (= 1 (crdt:vc-get *session-vc* "sid-iso"))
            "loaded VC must preserve the registry's existing entry")
        (crdt:vc-increment *session-vc* "sid-iso")
        (is (= 1 (crdt:vc-get registry-vc "sid-iso"))
            "incrementing the loaded copy must not affect the registry")))))

(test save-session-context-merges-vc-monotonically
  "SAVE-SESSION-CONTEXT must vc-merge (pointwise max) the thread-local
   VC into the registry's stored VC.  If a concurrent request advanced
   any entry beyond what we hold locally, the save must NOT roll it back."
  (with-fresh-session-contexts ()
    (let* ((registry-vc (crdt:make-vector-clock))
           (ctx (task-mcp::make-session-context
                 :id "sid-merge"
                 :vector-clock registry-vc)))
      (bt:with-lock-held (task-mcp::*session-contexts-lock*)
        (setf (gethash "sid-merge" task-mcp::*session-contexts*) ctx))
      ;; Registry already advanced "other-sid" to 5 (from another request).
      (setf (gethash "other-sid" (crdt:vc-entries registry-vc)) 5)
      ;; Our request only knows about its own entry advance.
      (let ((local-vc (crdt:make-vector-clock)))
        (setf (gethash "sid-merge" (crdt:vc-entries local-vc)) 3)
        (let ((*current-task-id* nil)
              (*claude-pid* nil)
              (*event-counter* 7)
              (*session-vc* local-vc))
          (task-mcp::save-session-context "sid-merge")))
      (let ((merged (task-mcp::ctx-vector-clock
                     (bt:with-lock-held (task-mcp::*session-contexts-lock*)
                       (gethash "sid-merge" task-mcp::*session-contexts*)))))
        (is (= 5 (crdt:vc-get merged "other-sid"))
            "concurrent advance to other-sid must not be rolled back")
        (is (= 3 (crdt:vc-get merged "sid-merge"))
            "our local advance must persist")))))
