(in-package #:kli/runtime/isolated)

;;; A long-lived child process held in a persistent struct: a blocking reader
;;; thread frames responses and notifications off stdout, callers correlate
;;; replies by id through per-request semaphore boxes, and teardown reaps the
;;; whole process group so an abandoned request cannot leak the child.

(defparameter *default-isolated-timeout* 30
  "Default seconds a synchronous call waits for its response.")

(defparameter *reader-poll-interval* 0.05
  "Seconds a waiting caller blocks per poll, bounding abort and deadline latency.")

(defparameter *teardown-join-timeout* 5
  "Seconds teardown waits for the reader thread to finish before interrupting it.")

(defstruct (pending-call (:constructor make-pending-call))
  (semaphore (sb-thread:make-semaphore))
  (message nil)
  (error nil))

(defstruct isolated-process
  (process nil)
  (group-leader-p nil)
  (input-stream nil)
  (output-stream nil)
  (reader-thread nil)
  (pending (make-hash-table :test #'eql))
  (next-id 0)
  (lock (sb-thread:make-mutex :name "kli-isolated"))
  (notification-handler nil)
  (closing-p nil))

(defun isolated-process-alive-p (proc)
  "True when the child is still running."
  (let ((process (isolated-process-process proc)))
    (and process (sb-ext:process-alive-p process) t)))

;;; Pending-call registry. The lock guards the table and the id counter.

(defun next-request-id (proc)
  (sb-thread:with-mutex ((isolated-process-lock proc))
    (incf (isolated-process-next-id proc))))

(defun register-pending (proc id box)
  (sb-thread:with-mutex ((isolated-process-lock proc))
    (setf (gethash id (isolated-process-pending proc)) box)))

(defun find-pending (proc id)
  (sb-thread:with-mutex ((isolated-process-lock proc))
    (gethash id (isolated-process-pending proc))))

(defun take-pending (proc id)
  (sb-thread:with-mutex ((isolated-process-lock proc))
    (remhash id (isolated-process-pending proc))))

(defun fail-all-pending (proc condition)
  (sb-thread:with-mutex ((isolated-process-lock proc))
    (maphash (lambda (id box)
               (declare (ignore id))
               (setf (pending-call-error box) condition)
               (sb-thread:signal-semaphore (pending-call-semaphore box)))
             (isolated-process-pending proc))
    (clrhash (isolated-process-pending proc))))

;;; Reader thread: blocks on stdout, routes responses to their pending box and
;;; id-less messages to the notification handler. Removal stays with the caller
;;; (or fail-all-pending) so a response races with exactly one owner.

(defun invoke-notification (proc msg)
  (let ((handler (isolated-process-notification-handler proc)))
    (when handler
      (with-extension-fault-barrier (:seam :extension/isolated :policy :continue
                                     :on-fault nil)
        (funcall handler msg)))))

(defun route-message (proc msg)
  (multiple-value-bind (id id-p) (gethash "id" msg)
    (if id-p
        (let ((box (find-pending proc id)))
          (when box
            (setf (pending-call-message box) msg)
            (sb-thread:signal-semaphore (pending-call-semaphore box))))
        (invoke-notification proc msg))))

(defun isolated-reader-loop (proc)
  (unwind-protect
       (loop
         (when (isolated-process-closing-p proc) (return))
         (let ((msg (with-extension-fault-barrier
                        (:seam :extension/isolated :policy :continue
                         :on-fault :transport-fault)
                      (read-jsonrpc-message (isolated-process-output-stream proc)))))
           (cond
             ((eq msg :eof) (return))
             ((eq msg :transport-fault)
              (when (or (isolated-process-closing-p proc)
                        (not (isolated-process-alive-p proc)))
                (return)))
             (t (route-message proc msg)))))
    (progn
      (setf (isolated-process-closing-p proc) t)
      (fail-all-pending proc (make-condition 'isolated-transport-closed)))))

;;; Spawn. run-program cannot set the child's process group, so route through
;;; setsid(1) when present, mirroring the shell tool: the child then leads its
;;; own group and teardown can killpg the whole subtree.

(defvar *setsid-program* :unprobed
  "Absolute path of setsid(1), NIL when absent, :unprobed before first spawn.")

(defun setsid-program ()
  (when (eq *setsid-program* :unprobed)
    (setf *setsid-program*
          (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                              :separator ":")
                for path = (and (plusp (length dir))
                                (probe-file
                                 (merge-pathnames
                                  "setsid"
                                  (uiop:ensure-directory-pathname dir))))
                when path return (namestring path))))
  *setsid-program*)

(defun spawn-isolated-process (command &key arguments directory environment
                                            notification-handler)
  "Spawn COMMAND with ARGUMENTS as a long-lived child wired for line-framed
JSON-RPC over its stdin/stdout, start its reader thread, and return the struct.
NOTIFICATION-HANDLER, when given, receives each id-less message. Returns NIL if
the spawn faults."
  (with-extension-fault-barrier (:seam :extension/isolated :policy :continue
                                 :on-fault nil)
    (let ((setsid (setsid-program)))
      (multiple-value-bind (program args)
          (if setsid
              (values setsid (cons command arguments))
              (values command arguments))
        (let* ((process (apply #'sb-ext:run-program program args
                               :search (not setsid)
                               :wait nil
                               :input :stream
                               :output :stream
                               :external-format :utf-8
                               (append (when directory (list :directory directory))
                                       (when environment
                                         (list :environment environment)))))
               (proc (make-isolated-process
                      :process process
                      :group-leader-p (and setsid t)
                      :input-stream (sb-ext:process-input process)
                      :output-stream (sb-ext:process-output process)
                      :notification-handler notification-handler)))
          (setf (isolated-process-reader-thread proc)
                (sb-thread:make-thread (lambda () (isolated-reader-loop proc))
                                       :name "kli-isolated-reader"))
          proc)))))

;;; Synchronous call: write the request, then wait on its box while polling the
;;; abort predicate and the deadline. On abort or timeout the pending entry is
;;; reclaimed so a later response is dropped rather than leaked.

(defun resolve-response (box)
  (when (pending-call-error box)
    (error (pending-call-error box)))
  (let ((msg (pending-call-message box)))
    (multiple-value-bind (err err-p) (gethash "error" msg)
      (if err-p
          (error 'isolated-jsonrpc-error :payload err)
          (gethash "result" msg)))))

(defun await-response (proc id box method timeout)
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (tool-abort-requested-p)
        (take-pending proc id)
        (error 'isolated-aborted :method method))
      (when (sb-thread:wait-on-semaphore (pending-call-semaphore box)
                                         :timeout *reader-poll-interval*)
        (take-pending proc id)
        (return (resolve-response box)))
      (when (>= (get-internal-real-time) deadline)
        (take-pending proc id)
        (if (sb-thread:try-semaphore (pending-call-semaphore box))
            (return (resolve-response box))
            (error 'isolated-timeout :method method :timeout timeout))))))

(defun call-isolated (proc method params &key (timeout *default-isolated-timeout*))
  "Send METHOD with PARAMS and block for the correlated response, returning its
result. Signals isolated-jsonrpc-error on an error reply, isolated-timeout on
deadline, isolated-aborted on a true abort predicate, isolated-transport-closed
if the child goes away."
  (let* ((id (next-request-id proc))
         (box (make-pending-call)))
    (register-pending proc id box)
    (let ((written (with-extension-fault-barrier
                       (:seam :extension/isolated :policy :continue
                        :on-fault :write-failed)
                     (write-jsonrpc-request (isolated-process-input-stream proc)
                                            id method params)
                     t)))
      (when (eq written :write-failed)
        (take-pending proc id)
        (error 'isolated-transport-closed)))
    (await-response proc id box method timeout)))

(defun notify-isolated (proc method params)
  "Send an id-less notification. No reply is awaited."
  (with-extension-fault-barrier (:seam :extension/isolated :policy :continue
                                 :on-fault nil)
    (write-jsonrpc-notification (isolated-process-input-stream proc) method params)
    t))

;;; Teardown: close stdin so a well-behaved child exits, SIGTERM then SIGKILL
;;; the group (the child's natural stdout EOF then unblocks the reader), join,
;;; and fail any residual pending so no caller waits forever.

(defun terminate-isolated-process (proc)
  (let ((process (isolated-process-process proc)))
    (when process
      (let ((whom (if (isolated-process-group-leader-p proc) :process-group :pid)))
        (when (sb-ext:process-alive-p process)
          (ignore-errors (sb-ext:process-kill process 15 whom))
          (sleep 0.1)
          (when (sb-ext:process-alive-p process)
            (ignore-errors (sb-ext:process-kill process 9 whom))
            (loop repeat 100
                  while (sb-ext:process-alive-p process)
                  do (sleep 0.05))))))))

(defun teardown-isolated (proc)
  "Stop the child and its reader, reclaim every pending call. Idempotent."
  (with-extension-fault-barrier (:seam :extension/isolated :policy :continue
                                 :on-fault nil)
    (setf (isolated-process-closing-p proc) t)
    (ignore-errors (close (isolated-process-input-stream proc)))
    (terminate-isolated-process proc)
    (let ((thread (isolated-process-reader-thread proc)))
      (when (and thread (sb-thread:thread-alive-p thread))
        (when (eq :timed-out
                  (sb-thread:join-thread thread
                                         :timeout *teardown-join-timeout*
                                         :default :timed-out))
          (ignore-errors (sb-thread:terminate-thread thread))
          (ignore-errors (sb-thread:join-thread thread :timeout 1 :default nil)))))
    (ignore-errors (close (isolated-process-output-stream proc)))
    (fail-all-pending proc (make-condition 'isolated-transport-closed))
    (let ((process (isolated-process-process proc)))
      (when process (ignore-errors (sb-ext:process-wait process))))
    t))
