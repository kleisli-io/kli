(in-package #:kli/tools/eval)

;;; Interactive (parking) eval. On :on-error "interactive" a handler-bind over the
;;; live condition parks the eval thread at the error site -- stack intact,
;;; restarts live -- instead of unwinding; eval-continue invokes a named restart,
;;; eval-abort unwinds. The park is a per-protocol live object stamped with the
;;; eval extension, so an unresolved park's thread is reaped when that extension
;;; tears down. Two semaphores sequence it: one announces a park or completion
;;; (with a state slot read after), the other releases the thread. Exactly one
;;; runner waits per event, so a re-park reuses both. A live thread cannot be
;;; serialized, so a park is never snapshotted.

(defclass eval-park (live-object)
  ((thread
    :initform nil
    :accessor eval-park-thread)
   (package
    :initarg :package
    :reader eval-park-package)
   (stream
    :initarg :stream
    :reader eval-park-stream)
   (tee
    :initarg :tee
    :reader eval-park-tee)
   (event
    :initarg :event
    :reader eval-park-event)
   (resume
    :initarg :resume
    :reader eval-park-resume)
   (state
    :initform nil
    :accessor eval-park-state)
   (condition-state
    :initform nil
    :accessor eval-park-condition-state)
   (restarts
    :initform nil
    :accessor eval-park-restarts)
   (restart-names
    :initform nil
    :accessor eval-park-restart-names)
   (chosen
    :initform nil
    :accessor eval-park-chosen)
   (chosen-arg
    :initform nil
    :accessor eval-park-chosen-arg)
   (outcome
    :initform nil
    :accessor eval-park-outcome)
   (final-package
    :initform nil
    :accessor eval-park-final-package)
   (warnings
    :initform nil
    :accessor eval-park-warnings)
   (timeout
    :initarg :timeout
    :reader eval-park-timeout)
   (last-error
    :initform nil
    :accessor eval-park-last-error)
   (stdout
    :initform ""
    :accessor eval-park-stdout)
   (stdout-truncated-p
    :initform nil
    :accessor eval-park-stdout-truncated-p)
   (surfaced-p
    :initform nil
    :accessor eval-park-surfaced-p)
   (warnings-surfaced
    :initform 0
    :accessor eval-park-warnings-surfaced)
   (contribution
    :initform nil
    :accessor eval-park-contribution))
  (:documentation "A parked interactive eval and the state that sequences it."))

(defvar *eval-park-counter* 0
  "Monotonic source of park ids; a park id round-trips as a plain string.")

(defvar *eval-park-counter-lock*
  (sb-thread:make-mutex :name "kli-eval-park-counter")
  "Guards *eval-park-counter* so concurrent interactive evals cannot mint the
same park id.")

(defun next-eval-park-id ()
  (format nil "eval-park-~D"
          (sb-thread:with-mutex (*eval-park-counter-lock*)
            (incf *eval-park-counter*))))

(defun make-eval-park (package timeout-seconds stream tee)
  "A fresh park bound to STREAM (the output stream the worker writes) and TEE (its
spill backing, or NIL when spilling is unavailable). The tee is finalized only when
the park resolves terminally -- a re-parked thread stays alive and writes more."
  (make-instance 'eval-park
                 :id (next-eval-park-id)
                 :package package
                 :timeout timeout-seconds
                 :stream stream
                 :tee tee
                 :event (sb-thread:make-semaphore :name "kli-eval-park-event")
                 :resume (sb-thread:make-semaphore :name "kli-eval-park-resume")))

(defun signal-park-event (park state)
  "Publish STATE then wake the runner; the setf precedes the signal so the woken
runner reads the state just written."
  (setf (eval-park-state park) state)
  (sb-thread:signal-semaphore (eval-park-event park)))

(defun await-park-event (park timeout-seconds)
  "Wait for PARK's next event, cut short by the deadline or an abort request.
Returns (values state timed-out-p aborted-p); STATE is :parked, :done, or :dead."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop
      (when (sb-thread:wait-on-semaphore (eval-park-event park) :timeout 0.05)
        (return (values (eval-park-state park) nil nil)))
      (when (tool-abort-requested-p)
        (return (values nil nil t)))
      (when (>= (get-internal-real-time) deadline)
        (return (values nil t nil)))
      (let ((thread (eval-park-thread park)))
        (when (and thread (not (sb-thread:thread-alive-p thread)))
          (return (values :dead nil nil)))))))

(defun find-restart-by-name (name condition)
  "CONDITION's active restart whose name string-equals NAME, or nil."
  (find name (compute-restarts condition)
        :key (lambda (restart)
               (let ((restart-name (restart-name restart)))
                 (and restart-name (string restart-name))))
        :test (lambda (a b) (and b (string-equal a b)))))

(defun read-restart-arg (arg)
  "Read exactly one form from ARG. Returns (values form nil) on success, or
(values nil message) when ARG cannot be read as a single form -- it holds no
complete form, or it carries tokens after the first -- so the caller re-parks
with the message rather than escaping the live handler or silently dropping
tokens."
  (handler-case
      (multiple-value-bind (form position) (read-from-string arg)
        (let ((eof '#:eof))
          (if (eq eof (read-from-string arg nil eof :start position))
              (values form nil)
              (values nil ":arg must be a single form; got trailing tokens."))))
    (error (condition)
      (values nil (format nil ":arg could not be read as a single form: ~A"
                          condition)))))

(defun park-and-await (park condition form)
  "In the signaling handler, on the eval thread: capture CONDITION onto PARK,
announce the park, block for a resume decision, then invoke the chosen restart --
which transfers control, so a successful resume does not return here; an unknown
name re-parks with a note instead of letting the error escape. An :arg is read as
a single form in the eval package the handler runs under; trailing tokens re-park
with a note rather than being dropped. Output captured so far is snapshotted now,
while this thread holds the stream, so the park can surface it safely."
  (multiple-value-bind (stdout truncated-p)
      (captured-eval-output (eval-park-stream park) t)
    (setf (eval-park-stdout park) stdout
          (eval-park-stdout-truncated-p park) truncated-p))
  (setf (eval-park-condition-state park) (capture-eval-condition condition form)
        (eval-park-restarts park) (eval-condition-restarts condition)
        (eval-park-restart-names park) (eval-condition-restart-names condition))
  (loop
    (signal-park-event park :parked)
    (sb-thread:wait-on-semaphore (eval-park-resume park))
    (let* ((chosen (eval-park-chosen park))
           (restart (and chosen (find-restart-by-name chosen condition))))
      (cond
        (restart
         (let ((arg (eval-park-chosen-arg park)))
           (if arg
               (multiple-value-bind (form trailing-error) (read-restart-arg arg)
                 (if trailing-error
                     (setf (eval-park-last-error park) trailing-error)
                     (return (invoke-restart restart form))))
               (return (invoke-restart restart)))))
        (chosen
         (setf (eval-park-last-error park)
               (format nil "No active restart named ~A; choose from: ~{~A~^, ~}."
                       chosen (eval-park-restart-names park))))))))

(defun run-park-body (source park)
  "Evaluate SOURCE on the park thread; an unhandled error parks rather than
unwinds. Records the terminal outcome -- (:values list count) or (:aborted) --
and announces it. abort-eval is the outer restart eval-abort unwinds through."
  (let ((current-form nil))
    (let ((outcome
            (restart-case
                (handler-bind
                    ((error (lambda (condition)
                              (park-and-await park condition current-form))))
                  (let ((values nil)
                        (count 0))
                    (with-source-forms (form source :current current-form)
                      (incf count)
                      (let ((warning (directive-warning form)))
                        (when warning (push warning (eval-park-warnings park))))
                      (setf values (multiple-value-list (eval form))))
                    (when (zerop count)
                      (error "Eval tool received no forms."))
                    (list :values values count)))
              (abort-eval ()
                :report "Abort the interactive eval and unwind."
                (list :aborted)))))
      ;; One capture site: *package* is live for every on-thread exit -- success
      ;; and the graceful abort-eval unwind alike. A killed thread never reaches
      ;; here, so its package is never recorded.
      (setf (eval-park-final-package park) (package-name *package*))
      (setf (eval-park-outcome park) outcome)
      (signal-park-event park :done)
      outcome)))

(defun eval-tool-extension (protocol tool)
  "The id of the extension that contributed TOOL, so a park it installs drains
under the same owner."
  (let ((contribution (find tool (list-tool-contributions protocol)
                            :key #'contribution-tool)))
    (and contribution (contribution-extension contribution))))

(defun make-eval-park-contribution (park extension)
  "An effect contribution that registers PARK on install and reaps its thread plus
deregisters it on retract, stamped with EXTENSION so an unresolved park drains on
teardown."
  (let ((contribution
          (make-effect-contribution
           :name (object-id park)
           :source extension
           :installer (lambda (protocol contribution context)
                        (declare (ignore protocol contribution))
                        (register-live-object (context-registry context) park)
                        nil)
           :retractor (lambda (protocol contribution context)
                        (declare (ignore protocol contribution))
                        (terminate-eval-thread (eval-park-thread park))
                        (remove-live-object (context-registry context)
                                            (object-id park))))))
    (when extension
      (setf (contribution-extension contribution) extension))
    (setf (eval-park-contribution park) contribution)
    contribution))

(defun retract-eval-park (protocol park context)
  "Retract PARK's contribution -- reap its thread, deregister it."
  (retract-contribution protocol (eval-park-contribution park) context))

(defun lookup-eval-park (context park-id)
  (let ((object (and park-id (find-live-object (context-registry context) park-id))))
    (unless (typep object 'eval-park)
      (error "No parked eval ~A." park-id))
    object))

(defun parked-eval-result (park timeout-seconds)
  "Tool-result for a parked eval: restart names + park id + captured condition
state ride :details. Not an error -- a pause awaiting the model's decision.
Marks the park surfaced, so a later resume keeps the :park/:resumed-p metadata.
Only warnings not surfaced at an earlier park are emitted, so a re-park does not
replay cautions the model already saw."
  (setf (eval-park-surfaced-p park) t)
  (let* ((state (eval-park-condition-state park))
         (all-warnings (reverse (eval-park-warnings park)))
         (warnings (nthcdr (eval-park-warnings-surfaced park) all-warnings))
         (restart-error (shiftf (eval-park-last-error park) nil))
         (stdout (eval-park-stdout park))
         (truncated-p (eval-park-stdout-truncated-p park)))
    (setf (eval-park-warnings-surfaced park) (length all-warnings))
    (make-tool-result
     :content (list (make-tool-text-content
                     (format nil "~@[~A~%~]~{~A~%~}~@[~A~%~]Eval parked on ~A.~
~@[~%Restarts: ~{~A~^, ~}~]~%~
Resume with eval-continue (a restart by name, optional :arg) or eval-abort, ~
park ~A.~@[~%~A~]"
                             restart-error
                             warnings
                             (and (plusp (length stdout)) stdout)
                             (getf state :message)
                             (eval-park-restart-names park)
                             (object-id park)
                             (eval-output-spill-notice truncated-p nil nil))))
     :details (append (list :park (object-id park)
                            :parked-p t
                            :restart-names (eval-park-restart-names park)
                            :package (package-name (eval-park-package park))
                            :timeout-seconds timeout-seconds
                            :stdout stdout
                            :stdout-truncated-p truncated-p)
                      (when restart-error (list :restart-error restart-error))
                      (when warnings (list :warnings warnings))
                      state))))

(defun interactive-done-result (outcome park package timeout-seconds stdout truncated-p
                                &key handle bytes protocol)
  "Tool-result for a resolved interactive eval: a value or a clean abort. The
:park/:resumed-p metadata rides only when the park was actually surfaced to the
model, so an interactive eval that completes without ever parking is byte-
identical to a plain success. HANDLE/BYTES point at the retained output backing;
PROTOCOL retains an overflowing value."
  (ecase (first outcome)
    (:values
     (eval-values-result (second outcome) (third outcome) package timeout-seconds
                         stdout truncated-p
                         :current-package (eval-park-final-package park)
                         :warnings (reverse (eval-park-warnings park))
                         :handle handle :bytes bytes :protocol protocol
                         :extra-details (when (eval-park-surfaced-p park)
                                          (list :park (object-id park) :resumed-p t))))
    (:aborted
     (make-tool-result
      :content (list (make-tool-text-content
                      (format nil "~@[~A~%~]Eval aborted; the parked computation ~
unwound cleanly.~@[~%~A~]"
                              (and (plusp (length stdout)) stdout)
                              (eval-output-spill-notice truncated-p handle bytes))))
      :details (append (list :park (object-id park)
                             :aborted-p t
                             :package (package-name package)
                             :current-package (or (eval-park-final-package park)
                                                  (package-name package))
                             :timeout-seconds timeout-seconds
                             :stdout stdout
                             :stdout-truncated-p truncated-p)
                       (when handle (list :stdout-handle handle))
                       (when (eval-park-warnings park)
                         (list :warnings (reverse (eval-park-warnings park)))))))))

(defun resolve-park-event (park protocol context package timeout-seconds)
  "Await PARK's next event and render it: a fresh park returns the restart list; a
completion returns the value/aborted result and drains the park; a timeout/abort
reaps the thread and returns the interrupted result; a worker that died without
announcing completion drains the park and returns the interrupted result too."
  (multiple-value-bind (state timed-out-p aborted-p)
      (await-park-event park timeout-seconds)
    (cond
      ((or timed-out-p aborted-p)
       (let ((stopped-p (terminate-eval-thread (eval-park-thread park))))
         (multiple-value-bind (stdout truncated-p)
             (captured-eval-output (eval-park-stream park) stopped-p)
           (multiple-value-bind (handle bytes)
               (and stopped-p (finalize-eval-tee (eval-park-tee park) truncated-p))
             (retract-eval-park protocol park context)
             (interrupted-eval-result (if timed-out-p :timeout :abort)
                                      package timeout-seconds
                                      stdout truncated-p stopped-p
                                      :handle handle :bytes bytes)))))
      ((eq state :dead)
       (let ((stopped-p (terminate-eval-thread (eval-park-thread park))))
         (multiple-value-bind (stdout truncated-p)
             (captured-eval-output (eval-park-stream park) stopped-p)
           (multiple-value-bind (handle bytes)
               (and stopped-p (finalize-eval-tee (eval-park-tee park) truncated-p))
             (retract-eval-park protocol park context)
             (interrupted-eval-result :killed package timeout-seconds
                                      stdout truncated-p stopped-p
                                      :handle handle :bytes bytes)))))
      ((eq state :parked)
       (parked-eval-result park timeout-seconds))
      (t
       (sb-thread:join-thread (eval-park-thread park) :timeout 5 :default nil)
       (multiple-value-bind (stdout truncated-p)
           (captured-eval-output (eval-park-stream park) t)
         (multiple-value-bind (handle bytes)
             (finalize-eval-tee (eval-park-tee park) truncated-p)
           (let ((outcome (eval-park-outcome park)))
             (persist-eval-package context (eval-park-final-package park))
             (retract-eval-park protocol park context)
             (interactive-done-result outcome park package timeout-seconds
                                      stdout truncated-p
                                      :handle handle :bytes bytes
                                      :protocol protocol))))))))

(defun run-interactive-eval (tool source package context timeout-seconds)
  "Start an interactive eval on a parked-capable thread under a park stamped with
TOOL's extension, returning the first of: parked, value, aborted, or interrupted."
  (let* ((protocol (active-protocol context))
         (extension (eval-tool-extension protocol tool)))
    (multiple-value-bind (stream tee) (make-eval-output context)
      (let* ((park (make-eval-park package timeout-seconds stream tee))
             (contribution (make-eval-park-contribution park extension)))
        (install-contribution protocol contribution context)
        (setf (eval-park-thread park)
              (spawn-eval-worker package (eval-park-stream park)
                                 (lambda () (run-park-body source park))))
        (resolve-park-event park protocol context package timeout-seconds)))))

(defun resume-park (park protocol context restart-name arg &optional timeout-seconds)
  "Write the resume decision, release the parked thread, render the next event.
TIMEOUT-SECONDS defaults to the park's original budget, so a resume inherits the
deadline the eval that opened it set rather than the bare default."
  (setf (eval-park-chosen park) restart-name
        (eval-park-chosen-arg park) arg)
  (sb-thread:signal-semaphore (eval-park-resume park))
  (resolve-park-event park protocol context
                      (eval-park-package park)
                      (or timeout-seconds (eval-park-timeout park))))

(defun resume-timeout (parameters)
  "An explicit resume :timeout in seconds, or nil to inherit the park's budget."
  (when (tool-parameter parameters :timeout)
    (eval-timeout-seconds parameters)))

(defun run-eval-continue-tool (tool parameters context &key call-id on-update)
  "Invoke the named :restart (optional :arg) on the parked thread :park names, then
return the resumed outcome: a value, another park, or an aborted result."
  (declare (ignore tool call-id on-update))
  (let* ((park-id (or (tool-parameter parameters :park)
                      (error "eval-continue requires :park.")))
         (restart-name (or (tool-parameter parameters :restart)
                           (error "eval-continue requires :restart.")))
         (park (lookup-eval-park context park-id)))
    (resume-park park (active-protocol context) context restart-name
                 (tool-parameter parameters :arg)
                 (resume-timeout parameters))))

(defun run-eval-abort-tool (tool parameters context &key call-id on-update)
  "Unwind the parked thread :park names through its abort restart."
  (declare (ignore tool call-id on-update))
  (let* ((park-id (or (tool-parameter parameters :park)
                      (error "eval-abort requires :park.")))
         (park (lookup-eval-park context park-id)))
    (resume-park park (active-protocol context) context "ABORT-EVAL" nil
                 (resume-timeout parameters))))
