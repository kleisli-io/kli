(in-package #:task-mcp)

;;; Forward references to specials defined later in the load order
;;; (session.lisp, server.lisp).  Declaiming them special here keeps
;;; the compiler from warning and documents the dependency.
(declaim (special *session-contexts*
                  *session-contexts-lock*
                  *log-verbose*))

;;; ==========================================================================
;;; Lock tracing, session GC thread, request deadline, admin endpoints
;;; ==========================================================================
;;;
;;; The diagnostic primitives in this file exist to bound two pathologies
;;; that can wedge the task-MCP daemon:
;;;
;;;  1. A lock held across slow I/O (e.g. /proc walks in resolve-peer-pid)
;;;     stalls every concurrent request whose unwind path needs the same
;;;     lock.  The with-timed-lock macro records wait-to-acquire and hold
;;;     durations into a bounded ring buffer so /admin/diag can surface
;;;     long holds before they become deadlocks.
;;;
;;;  2. *session-contexts* grows without bound because
;;;     cleanup-inactive-sessions only runs from task_create /
;;;     task_bootstrap.  A long-running daemon with no bootstrap traffic
;;;     accumulates dead-Claude contexts.  The session GC thread calls
;;;     cleanup-inactive-sessions on a fixed interval.
;;;
;;; Together with a request deadline in the acceptor's :around method,
;;; these give us three orthogonal guards: observability, proactive
;;; cleanup, and a hard time budget.

;;; --- Lock trace ring buffer ------------------------------------------------

(defconstant +lock-trace-size+ 512
  "Capacity of the lock-event ring buffer.  Fixed so writers never
   allocate after startup and readers see a bounded snapshot.")

(defvar *lock-trace*
  (make-array +lock-trace-size+ :initial-element nil)
  "Ring buffer of recent lock events.  Each entry is a plist with
   keys :NAME :WAIT-MS :HOLD-MS :THREAD :AT.  NIL slots indicate
   unwritten positions (seen only before the buffer has wrapped once).")

(defvar *lock-trace-pos* (cons 0 nil)
  "Monotonic write cursor into *LOCK-TRACE*; taken modulo
   +LOCK-TRACE-SIZE+ at store time.  The cursor lives in CAR so
   SB-EXT:ATOMIC-INCF can bump it without a mutex (SBCL's
   atomic-incf supports CAR/CDR places but not plain symbol-value
   on every version we ship).")

(defvar *lock-trace-warn-ms* 500
  "Wait or hold duration above which a lock event is also logged to
   *ERROR-OUTPUT* at record time.  Tune by rebinding at the REPL.")

(declaim (inline elapsed-ms))
(defun elapsed-ms (since-internal)
  "Milliseconds elapsed from SINCE-INTERNAL (a GET-INTERNAL-REAL-TIME
   value) to now, as a non-negative integer."
  (floor (* 1000 (- (get-internal-real-time) since-internal))
         internal-time-units-per-second))

(defun record-lock-event (name wait-ms hold-ms thread)
  "Append a lock event to *LOCK-TRACE* and emit a warning on
   *ERROR-OUTPUT* when wait or hold exceeds *LOCK-TRACE-WARN-MS*.
   Lock-free: uses SB-EXT:ATOMIC-INCF on the cursor and a single
   SVREF store."
  (let ((entry (list :name name
                     :wait-ms wait-ms
                     :hold-ms hold-ms
                     :thread thread
                     :at (get-universal-time))))
    (let ((idx (mod (sb-ext:atomic-incf (car *lock-trace-pos*))
                    +lock-trace-size+)))
      (setf (svref *lock-trace* idx) entry))
    (when (or (> wait-ms *lock-trace-warn-ms*)
              (> hold-ms *lock-trace-warn-ms*))
      (format *error-output*
              "~&;; WARNING: lock ~S wait=~Dms hold=~Dms thread=~A~%"
              name wait-ms hold-ms thread)))
  nil)

(defmacro with-timed-lock ((lock name) &body body)
  "Acquire LOCK, run BODY, release LOCK; record wait-to-acquire and
   hold duration via RECORD-LOCK-EVENT.  NAME is a keyword identifying
   the call site in traces.  Drop-in replacement for
   BT:WITH-LOCK-HELD on locks whose contention we care about."
  (let ((lk (gensym "LOCK-"))
        (nm (gensym "NAME-"))
        (ws (gensym "WAIT-START-"))
        (acq (gensym "ACQUIRED-"))
        (wait-ms (gensym "WAIT-MS-")))
    `(let* ((,lk ,lock)
            (,nm ,name)
            (,ws (get-internal-real-time)))
       (bt:with-lock-held (,lk)
         (let* ((,acq (get-internal-real-time))
                (,wait-ms (floor (* 1000 (- ,acq ,ws))
                                 internal-time-units-per-second)))
           (unwind-protect
                (progn ,@body)
             (record-lock-event
              ,nm ,wait-ms (elapsed-ms ,acq)
              (bt:thread-name (bt:current-thread)))))))))

(defun recent-lock-events (&optional (n +lock-trace-size+))
  "Return up to N most-recent lock events in chronological order
   (oldest first).  Reads are snapshot-consistent up to concurrent
   writes that overlap the read window.  Takes no locks."
  (let* ((pos (car *lock-trace-pos*))
         (size (min n +lock-trace-size+))
         (result nil))
    (loop for i from 0 below size
          for idx = (mod (- pos 1 i) +lock-trace-size+)
          for entry = (svref *lock-trace* idx)
          when entry do (push entry result))
    result))

;;; --- CLOSE_WAIT counter ----------------------------------------------------

(defun count-close-wait-on (port)
  "Count TCP connections on PORT in CLOSE_WAIT state by reading
   /proc/net/tcp.  Linux state codes are hex; 08 is TCP_CLOSE_WAIT.
   Returns the count as an integer, or NIL when /proc is unavailable."
  (handler-case
      (with-open-file (s "/proc/net/tcp" :if-does-not-exist nil)
        (when s
          (read-line s nil nil) ; header
          (loop with count = 0
                for line = (read-line s nil nil)
                while line
                do (let* ((trimmed (string-trim '(#\Space #\Tab) line))
                          (parts (remove "" (uiop:split-string trimmed)
                                         :test #'string=)))
                     (when (>= (length parts) 4)
                       (let* ((local-str (nth 1 parts))
                              (state (nth 3 parts))
                              (colon (position #\: local-str :from-end t)))
                         (when (and colon (string= state "08"))
                           (let ((local-port
                                   (ignore-errors
                                    (parse-integer local-str
                                                   :start (1+ colon)
                                                   :radix 16
                                                   :junk-allowed t))))
                             (when (eql local-port port)
                               (incf count)))))))
                finally (return count))))
    (error () nil)))

;;; --- Periodic session GC thread -------------------------------------------

(defvar *session-gc-thread* nil
  "Handle of the background thread that runs CLEANUP-INACTIVE-SESSIONS
   on a fixed interval.  NIL when the thread is not running.")

(defvar *session-gc-stop* nil
  "When set to T, the session GC thread exits its loop on the next
   wakeup.  Primarily a test affordance; production tears down via
   process exit.")

(defvar *session-gc-interval-seconds* 300
  "Seconds between successive CLEANUP-INACTIVE-SESSIONS runs in the
   GC thread.  Bind to a small value in tests.")

(defvar *session-gc-max-age-hours* 4
  "Age threshold passed to CLEANUP-INACTIVE-SESSIONS by the GC thread.")

(defun session-gc-loop ()
  "Body of the GC thread.  Sleeps, then calls
   CLEANUP-INACTIVE-SESSIONS inside a HANDLER-CASE so a transient
   error doesn't take the thread out.  Exits when *SESSION-GC-STOP*
   becomes true."
  (loop
    (when *session-gc-stop* (return))
    (sleep *session-gc-interval-seconds*)
    (when *session-gc-stop* (return))
    (handler-case
        (let ((removed (cleanup-inactive-sessions *session-gc-max-age-hours*)))
          (when (and *log-verbose* (plusp removed))
            (format *error-output*
                    "~&;; session-gc: removed ~D inactive context(s)~%"
                    removed)))
      (error (e)
        (format *error-output*
                "~&;; WARNING: session-gc error: ~A~%" e)))))

(defun start-session-gc-thread ()
  "Spawn the session GC thread if it is not already running.
   Returns the thread object."
  (unless (and *session-gc-thread*
               (bt:thread-alive-p *session-gc-thread*))
    (setf *session-gc-stop* nil)
    (setf *session-gc-thread*
          (bt:make-thread #'session-gc-loop
                          :name "task-mcp-session-gc")))
  *session-gc-thread*)

(defun stop-session-gc-thread ()
  "Request the session GC thread to exit on its next wakeup.  Does
   not block; use in tests to tear down the thread cleanly."
  (setf *session-gc-stop* t))

;;; --- Request deadline ------------------------------------------------------

(defvar *request-deadline-seconds* 30
  "Default maximum wall-clock seconds a single request handler may run
   before its deadline fires and the handler returns an HTTP 503.
   Applies to all routes that do not have a longer-lived contract
   (see *SSE-DEADLINE-SECONDS* for SSE long-polls).  Bind to NIL to
   disable the deadline entirely (not recommended outside of tests).")

(defvar *sse-deadline-seconds* 86400
  "Deadline (in seconds) for SSE long-poll routes — currently GET /mcp.
   The MCP HTTP transport's GET /mcp is a Server-Sent-Events stream
   that deliberately holds the connection open across multiple tool
   calls, so the default 30s deadline used for tool POSTs would 503
   the stream every 30 seconds.  Defaults to 24 hours as a sanity
   ceiling.  Bind to NIL to disable the deadline for SSE routes.")

(defvar *route-deadline-overrides*
  '(((:get  . "/mcp")           . *sse-deadline-seconds*)
    ((:post . "/mcp")           . *request-deadline-seconds*)
    ((:post . "/admin/unwedge") . 60)
    ((:get  . "/health")        . 5))
  "Alist of per-route deadline overrides consulted by
   EFFECTIVE-DEADLINE-SECONDS.  Each entry has the shape
   ((METHOD . PATH-PREFIX) . VALUE) where:

     METHOD is a keyword (:GET, :POST, ...) or NIL to match any method.
     PATH-PREFIX is a string; it matches a request PATH iff PATH equals
       PATH-PREFIX, or PATH starts with PATH-PREFIX immediately followed
       by a `/' (so `/mcp' matches `/mcp' and `/mcp/' but not
       `/mcp-other').
     VALUE is one of: a non-negative number of seconds; NIL (run with no
       deadline); or a SYMBOL naming a defvar whose current value is the
       seconds.  Symbol resolution happens at lookup time, so callers
       and tests that LET-bind *REQUEST-DEADLINE-SECONDS* or
       *SSE-DEADLINE-SECONDS* continue to influence the table.

   Order encodes priority — the first matching entry wins.  Routes with
   no entry fall back to *REQUEST-DEADLINE-SECONDS*.  Ship the table as
   the single source of truth for per-route deadlines so future
   long-running endpoints can declare themselves without touching the
   dispatch logic.")

(defun route-prefix-matches-p (path prefix)
  "Return T iff PATH matches PREFIX as a route prefix.  PATH equals
   PREFIX exactly, or PATH starts with PREFIX immediately followed by
   a `/'.  Both arguments must be strings."
  (let ((plen (length prefix))
        (slen (length path)))
    (and (>= slen plen)
         (string= prefix path :end2 plen)
         (or (= slen plen)
             (eql #\/ (char path plen))))))

(defun resolve-deadline-value (value)
  "Resolve a *ROUTE-DEADLINE-OVERRIDES* entry VALUE to a concrete
   seconds-or-NIL.  Numbers and NIL pass through; symbols are resolved
   via SYMBOL-VALUE so that the table can name defvars whose bindings
   are intended to remain mutable (e.g. for tests)."
  (cond ((null value) nil)
        ((numberp value) value)
        ((symbolp value) (symbol-value value))
        (t (error "Invalid deadline override value: ~S" value))))

(defun effective-deadline-seconds (method path)
  "Return the deadline (in seconds) appropriate for an HTTP request
   with METHOD (a keyword like :GET, :POST) and PATH (a string).
   Returns NIL when the request should run with no deadline.

   Walks *ROUTE-DEADLINE-OVERRIDES* in order; the first entry whose
   METHOD matches (or is NIL) and whose PATH-PREFIX matches PATH wins,
   and its VALUE is resolved via RESOLVE-DEADLINE-VALUE.  When no entry
   matches the request falls back to *REQUEST-DEADLINE-SECONDS*."
  (let ((entry
          (find-if (lambda (e)
                     (destructuring-bind ((entry-method . prefix) . _) e
                       (declare (ignore _))
                       (and (or (null entry-method) (eq method entry-method))
                            (route-prefix-matches-p path prefix))))
                   *route-deadline-overrides*)))
    (if entry
        (resolve-deadline-value (cdr entry))
        *request-deadline-seconds*)))

(define-condition request-deadline-exceeded (error)
  ((path :initarg :path :reader request-deadline-path)
   (session-id :initarg :session-id :reader request-deadline-session-id)
   (deadline-seconds :initarg :deadline-seconds
                     :reader request-deadline-deadline-seconds
                     :initform nil))
  (:report
   (lambda (c stream)
     (format stream "request exceeded ~Ds deadline (path=~A session=~A)"
             (or (request-deadline-deadline-seconds c)
                 *request-deadline-seconds*)
             (request-deadline-path c)
             (request-deadline-session-id c)))))

(defmacro with-request-deadline ((&key method path session-id) &body body)
  "Wrap BODY in SB-SYS:WITH-DEADLINE with a timeout chosen by
   EFFECTIVE-DEADLINE-SECONDS for METHOD and PATH.  On timeout,
   signal REQUEST-DEADLINE-EXCEEDED carrying PATH, SESSION-ID and
   the actual deadline seconds for diagnostic attribution; callers
   translate that to HTTP 503.  When the effective deadline is NIL,
   BODY runs with no deadline."
  (let ((secs (gensym "SECS-")))
    `(let ((,secs (effective-deadline-seconds ,method ,path)))
       (if ,secs
           (handler-case
               (sb-sys:with-deadline (:seconds ,secs)
                 ,@body)
             (sb-sys:deadline-timeout ()
               (error 'request-deadline-exceeded
                      :path ,path :session-id ,session-id
                      :deadline-seconds ,secs)))
           (progn ,@body)))))

;;; --- /admin/diag + /admin/unwedge endpoints -------------------------------

(defun route-deadlines-snapshot ()
  "Snapshot of *ROUTE-DEADLINE-OVERRIDES* with values resolved to the
   numbers (or NIL) currently in effect.  Each entry is a plist
   (:METHOD :PATH :SECONDS) suitable for direct JSON serialisation; the
   shape mirrors the alist input so an operator reading /admin/diag
   sees the same routes in the same priority order as the source."
  (mapcar (lambda (entry)
            (destructuring-bind ((method . prefix) . value) entry
              (list :method method
                    :path prefix
                    :seconds (resolve-deadline-value value))))
          *route-deadline-overrides*))

(defun diag-snapshot-plist ()
  "Collect a snapshot of daemon health for /admin/diag as a plist."
  (let* ((sessions-n
           ;; Briefly take the lock to count contexts — an O(1)
           ;; operation that itself goes into the trace.
           (with-timed-lock (*session-contexts-lock* :diag-count)
             (hash-table-count *session-contexts*)))
         (threads (length (bt:all-threads)))
         (close-wait (count-close-wait-on (get-http-port)))
         (events (recent-lock-events)))
    (list :sessions sessions-n
          :threads threads
          :close-wait close-wait
          :deadline-seconds *request-deadline-seconds*
          :route-deadlines (route-deadlines-snapshot)
          :gc-thread-alive
          (and *session-gc-thread*
               (bt:thread-alive-p *session-gc-thread*)
               t)
          :gc-interval-seconds *session-gc-interval-seconds*
          :gc-max-age-hours *session-gc-max-age-hours*
          :lock-events events)))

(defun diag-json-stream (stream plist)
  "Encode the /admin/diag PLIST to STREAM as JSON."
  (let ((ht (make-hash-table :test 'equal))
        (events (getf plist :lock-events))
        (routes (getf plist :route-deadlines)))
    (setf (gethash "sessions" ht) (getf plist :sessions)
          (gethash "threads" ht) (getf plist :threads)
          (gethash "close_wait" ht) (getf plist :close-wait)
          (gethash "deadline_seconds" ht) (getf plist :deadline-seconds)
          (gethash "gc_thread_alive" ht) (getf plist :gc-thread-alive)
          (gethash "gc_interval_seconds" ht) (getf plist :gc-interval-seconds)
          (gethash "gc_max_age_hours" ht) (getf plist :gc-max-age-hours))
    (setf (gethash "route_deadlines" ht)
          (mapcar
           (lambda (r)
             (let ((rh (make-hash-table :test 'equal))
                   (method (getf r :method)))
               (setf (gethash "method" rh)
                     (if method
                         (string-downcase (symbol-name method))
                         ""))
               (setf (gethash "path" rh) (getf r :path)
                     (gethash "seconds" rh) (getf r :seconds))
               rh))
           routes))
    (setf (gethash "lock_events" ht)
          (mapcar
           (lambda (e)
             (let ((eh (make-hash-table :test 'equal)))
               (setf (gethash "name" eh)
                     (string-downcase (symbol-name (getf e :name))))
               (setf (gethash "wait_ms" eh) (getf e :wait-ms)
                     (gethash "hold_ms" eh) (getf e :hold-ms)
                     (gethash "thread" eh) (or (getf e :thread) "")
                     (gethash "at" eh) (getf e :at))
               eh))
           events))
    (yason:encode ht stream)))

(defun unwedge-snapshot-plist ()
  "Counts used by /admin/unwedge for before/after reporting.
   Same primitives as DIAG-SNAPSHOT-PLIST minus the lock-trace tail —
   /admin/unwedge cares about *quantitative* deltas, not the most
   recent contention events."
  (list :sessions
        (with-timed-lock (*session-contexts-lock* :unwedge-count)
          (hash-table-count *session-contexts*))
        :threads (length (bt:all-threads))
        :close-wait (count-close-wait-on (get-http-port))))

(defparameter *unwedge-default-max-age-hours* 0.25
  "Default MAX-AGE-HOURS handed to CLEANUP-INACTIVE-SESSIONS by
   /admin/unwedge.  Aggressive (15 minutes) so a wedged daemon sheds
   dead-Claude contexts that the periodic GC hasn't reached yet.
   Override per request via ?max_age_hours=N.")

(defun perform-unwedge (max-age-hours)
  "Run the unwedge sequence and return a plist describing the work.

   Steps, in order:
     1. CLEANUP-INACTIVE-SESSIONS (registry contexts older than
        MAX-AGE-HOURS).  Holds *SESSION-CONTEXTS-LOCK* briefly.
     2. Drop the task graph caches (TASK:CLEAR-GRAPH-CACHE,
        TASK:CLEAR-INFOS-CACHE, TASK:CLEAR-TASK-STATE-CACHE) so the
        next read recomputes from disk.

   Returns a plist with the BEFORE snapshot, the AFTER snapshot, the
   number of sessions removed, the MAX-AGE-HOURS that was applied, and
   a list of the cache names that were cleared.  No request handler
   threads are killed — that requires the request deadline to fire
   independently."
  (let* ((before (unwedge-snapshot-plist))
         (removed (cleanup-inactive-sessions max-age-hours))
         (caches (list "graph" "infos" "task-state")))
    (task:clear-graph-cache)
    (task:clear-infos-cache)
    (task:clear-task-state-cache)
    (let ((after (unwedge-snapshot-plist)))
      (list :before before
            :after after
            :sessions-removed removed
            :max-age-hours max-age-hours
            :caches-cleared caches))))

(defun unwedge-json-stream (stream result-plist)
  "Encode the /admin/unwedge RESULT-PLIST to STREAM as JSON."
  (let ((ht (make-hash-table :test 'equal))
        (before-ht (make-hash-table :test 'equal))
        (after-ht (make-hash-table :test 'equal))
        (before (getf result-plist :before))
        (after (getf result-plist :after)))
    (setf (gethash "sessions" before-ht) (getf before :sessions)
          (gethash "threads" before-ht) (getf before :threads)
          (gethash "close_wait" before-ht) (getf before :close-wait))
    (setf (gethash "sessions" after-ht) (getf after :sessions)
          (gethash "threads" after-ht) (getf after :threads)
          (gethash "close_wait" after-ht) (getf after :close-wait))
    (setf (gethash "before" ht) before-ht
          (gethash "after" ht) after-ht
          (gethash "sessions_removed" ht) (getf result-plist :sessions-removed)
          (gethash "max_age_hours" ht) (getf result-plist :max-age-hours)
          (gethash "caches_cleared" ht) (getf result-plist :caches-cleared))
    (yason:encode ht stream)))

(defun parse-max-age-param (raw)
  "Decode an optional max_age_hours query parameter.
   Accepts integer or decimal strings; returns
   *UNWEDGE-DEFAULT-MAX-AGE-HOURS* when absent or unparseable."
  (or (when (and raw (plusp (length raw)))
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) raw))
               (with-read (handler-case
                              (let ((*read-default-float-format* 'double-float))
                                (with-input-from-string (s trimmed)
                                  (let ((v (read s)))
                                    (when (realp v) v))))
                            (error () nil))))
          (when (and with-read (plusp with-read))
            with-read)))
      *unwedge-default-max-age-hours*))

(defun register-admin-endpoints ()
  "Register /admin/diag (GET), /admin/unwedge (any method), and
   /admin/verify-events (GET) for daemon introspection and recovery.
   Called from REGISTER-HTTP-ENDPOINTS alongside /health and friends."
  (hunchentoot:define-easy-handler (admin-diag :uri "/admin/diag") ()
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (with-output-to-string (s)
          (diag-json-stream s (diag-snapshot-plist)))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"error\": ~S}" (princ-to-string e)))))
  (hunchentoot:define-easy-handler (admin-unwedge :uri "/admin/unwedge")
      (max_age_hours)
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (let* ((max-age (parse-max-age-param max_age_hours))
               (result (perform-unwedge max-age)))
          (with-output-to-string (s)
            (unwedge-json-stream s result)))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"error\": ~S}" (princ-to-string e)))))
  (hunchentoot:define-easy-handler (admin-verify-events :uri "/admin/verify-events") ()
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (with-output-to-string (s)
          (task:verify-events-tree-to-json s task:*tasks-root*))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (format nil "{\"error\": ~S}" (princ-to-string e))))))
