(in-package #:kli/ext)

(defgeneric condition-category (condition)
  (:documentation "User-facing category of CONDITION: :network for transport
failures, :provider for model-provider API errors, :internal otherwise.
Producers specialize this so consumers (rendering, retry policies) classify
without depending on the producer's package.")
  (:method ((condition t)) :internal))

(defgeneric condition-http-status (condition)
  (:documentation "HTTP status carried by CONDITION, or NIL when it has none.")
  (:method ((condition t)) nil))

(defvar *extension-fault-policy* nil
  "When non-NIL, overrides every fault barrier's install-time policy.
One of :continue, :reify or :escalate. The test suite binds :escalate so a
contained seam fault re-signals and fails loud. Production leaves this NIL so
each barrier's per-seam policy applies.")

(defvar *fault-reify-hook* nil
  "Function of (CONDITION SEAM ID) that surfaces a contained fault to the
user, e.g. as a transcript event. NIL until a UI layer installs one, and the
:reify policy degrades to :continue while unset. A fault inside the hook is
noted to the seam's file sink and dropped -- reification must never break
containment.")

(defvar *fault-note-hook* nil
  "Function of (SEAM ID CONDITION CONTEXT) invoked for every contained fault,
the universal observation seam under every policy. Only ever let-bound -- a
thread's owner binds it to route that thread's faults to its own context.
While NIL, note-fault falls back to the registered per-context emitters.
Called inside note-fault's ignore-errors, so a hook fault is dropped and can
never break containment.")

(defvar *fault-note-emitters* '()
  "Registered (PROTOCOL CONTEXT EMITTER) entries, one per protocol that wires
a fault emitter. The fallback observation route for threads that never bind
*FAULT-NOTE-HOOK*: an unattributable fault reaches each registered context's
stream once, so no fault is lost and no install/retract order can strand a
coexisting protocol. Mutated only by register/deregister under the lock --
note-fault reads the current list as a snapshot.")

(defvar *fault-note-emitters-lock*
  (sb-thread:make-mutex :name "fault-note-emitters"))

(defun register-fault-note-emitter (protocol context emitter)
  "Route unattributed faults to CONTEXT's stream via EMITTER while PROTOCOL
stays installed. Replaces any prior entry for PROTOCOL."
  (sb-thread:with-mutex (*fault-note-emitters-lock*)
    (setf *fault-note-emitters*
          (cons (list protocol context emitter)
                (remove protocol *fault-note-emitters* :key #'first))))
  emitter)

(defun deregister-fault-note-emitter (protocol)
  "Drop PROTOCOL's fault-note emitter entry, leaving every other protocol's
routing intact regardless of install/retract order."
  (sb-thread:with-mutex (*fault-note-emitters-lock*)
    (setf *fault-note-emitters*
          (remove protocol *fault-note-emitters* :key #'first)))
  nil)

(defvar *in-fault-barrier* nil
  "True while a fault barrier is handling a fault. A barrier entered
re-entrantly -- a fault inside the handling machinery itself -- declines to
handle, so the condition escalates toward the process boundary instead of
looping through the very machinery that is failing.")

(defvar *dispatch-chain* nil
  "Innermost-first (PROVIDER . NAME) conses for capability dispatches in
flight, bound by WITH-SUPERVISED-DISPATCH. NOTE-FAULT records it by default,
so a fault contained at an owner barrier carries the dispatch breadcrumb
while the dispatch points themselves stay silent.")

(defparameter *fault-log-cap-bytes* (* 1 1024 1024)
  "Soft cap per seam fault log. The file is truncated on overflow.")

(defun fault-log-path (seam)
  (merge-pathnames (format nil "kli/~(~A~)-faults.log" seam)
                   (uiop:xdg-cache-home)))

(defun dispatch-chain-label (&optional (chain *dispatch-chain*))
  "Readable (LABEL . NAME) form of the in-flight dispatch chain. Provider
objects shrink to their ids. Computed at fault time only."
  (mapcar (lambda (entry)
            (let ((provider (car entry)))
              (cons (typecase provider
                      (hash-table :table)
                      (cons :plist)
                      (standard-object (object-id provider))
                      (t (type-of provider)))
                    (cdr entry))))
          chain))

(defun note-fault (seam id condition &key (context (dispatch-chain-label)))
  "Append one line for CONDITION to SEAM's fault log. Best-effort -- a sink
failure loses the line and nothing else, since the diagnostic path must never
re-poison the caller. File sink, not *ERROR-OUTPUT*, so writes must not corrupt
a running TUI. Unless overridden, CONTEXT defaults to the in-flight dispatch
chain."
  (ignore-errors
   (let ((path (fault-log-path seam)))
     (ensure-directories-exist path)
     (when (and (probe-file path)
                (> (with-open-file (in path :direction :input)
                     (file-length in))
                   *fault-log-cap-bytes*))
       (with-open-file (out path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
         (format out "~&[log truncated at ~A]~%" (get-universal-time))))
     (with-open-file (out path :direction :output
                               :if-exists :append
                               :if-does-not-exist :create)
       (format out "~&~A id=~S~@[ ctx=~A~] condition=~A~%"
               (get-universal-time) id context condition)))
   (if *fault-note-hook*
       (ignore-errors (funcall *fault-note-hook* seam id condition context))
       (let ((noted '()))
         (dolist (entry *fault-note-emitters*)
           (destructuring-bind (protocol emitter-context emitter) entry
             (declare (ignore protocol))
             (unless (member emitter-context noted)
               (push emitter-context noted)
               (ignore-errors (funcall emitter seam id condition context))))))))
  nil)

(defun reify-fault (seam id condition)
  "Surface a contained fault via *FAULT-REIFY-HOOK*. The hook's own faults
are noted and dropped so reification can never break containment."
  (when *fault-reify-hook*
    (handler-case (funcall *fault-reify-hook* condition seam id)
      (error (hook-condition)
        (note-fault seam id hook-condition :context "reify-hook")))))

(defmacro! with-extension-fault-barrier ((&key (seam (error "with-extension-fault-barrier requires :seam"))
                                               id
                                               (policy :continue)
                                               on-fault)
                                         &body body)
  "Run BODY guarded against faults in extension or hot-patched code.

Catches ERROR, never SERIOUS-CONDITION. On a fault, note it to SEAM's file
sink, then apply *EXTENSION-FAULT-POLICY* (when non-NIL) or POLICY, which is
re-evaluated at each fault.

  :continue  unwind BODY and return ON-FAULT's value (evaluated only on fault)
  :reify     additionally surface the fault via *FAULT-REIFY-HOOK*
  :escalate  decline -- the condition keeps propagating

Containment unwinds through a barrier-owned CATCH tag, never through the
restarts, so an outer barrier never resumes at an inner barrier's restart.
SKIP-UNIT and USE-VALUE remain as the supervisor API. SEAM names the seam
class and its log sink. ID identifies the faulted unit."
  `(block ,g!barrier
     (let* ((,g!seam ,seam) (,g!id ,id) (,g!tag (list ,g!seam)))
       (catch ,g!tag
         (return-from ,g!barrier
           (restart-case
               (handler-bind
                   ((error
                      (lambda (condition)
                        (unless *in-fault-barrier*
                          (let ((*in-fault-barrier* t))
                            (note-fault ,g!seam ,g!id condition)
                            (ecase (or *extension-fault-policy* ,policy)
                              (:reify (reify-fault ,g!seam ,g!id condition)
                                      (throw ,g!tag nil))
                              (:continue (throw ,g!tag nil))
                              (:escalate nil)))))))
                 ,@body)
             (skip-unit ()
               :report "Skip the faulted unit and return the barrier's fallback value."
               (throw ,g!tag nil))
             (use-value (value)
               :report "Return VALUE from the fault barrier."
               value))))
       ,on-fault)))

(defun safely-invoke (fn seam id &rest args)
  "FUNCALL FN on ARGS inside a fault barrier (policy :continue, fallback NIL).
The function form for raw funcall/apply seams."
  (with-extension-fault-barrier (:seam seam :id id)
    (apply fn args)))

(defun boot-timing-enabled-p ()
  "True when KLI_BOOT_TIMING names a non-empty value, gating the boot timer."
  (let ((value (uiop:getenv "KLI_BOOT_TIMING")))
    (and value (plusp (length value)))))
(defun boot-marker (label)
  "Print a one-shot boot marker when KLI_BOOT_TIMING is enabled. Unlike
WITH-BOOT-STAGE, this logs immediately, so it can mark entry into long-lived
loops that do not return during normal interactive use."
  (when (boot-timing-enabled-p)
    (format *error-output* "~&[boot] ~A~%" label)
    (finish-output *error-output*))
  label)

(defmacro! with-boot-stage ((label) &body body)
  "Evaluate BODY; when KLI_BOOT_TIMING is set, print its wall-clock cost to
*error-output* as a [boot] line. A transparent pass-through otherwise."
  `(if (boot-timing-enabled-p)
       (let ((,g!start (get-internal-real-time)))
         (multiple-value-prog1 (progn ,@body)
           (format *error-output* "~&[boot] ~A ~,1Fms~%"
                   ,label
                   (* 1000d0 (/ (- (get-internal-real-time) ,g!start)
                                internal-time-units-per-second)))
           (finish-output *error-output*)))
       (progn ,@body)))

(defmacro with-supervised-dispatch ((&key provider name) &body body)
  "Run BODY -- the final apply of a capability dispatch -- transparently to
the condition system: nothing is caught, nothing is logged. Signal-vs-fault
is the caller's distinction, so a dispatch point must not contain. It records
the dispatch on *DISPATCH-CHAIN* for owner barriers' fault lines and offers
USE-VALUE so a supervisor can substitute the dispatch result without
unwinding."
  `(let ((*dispatch-chain* (cons (cons ,provider ,name) *dispatch-chain*)))
     (restart-case (progn ,@body)
       (use-value (value)
         :report "Return VALUE as the dispatch result."
         value))))
