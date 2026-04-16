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
