(in-package #:kli/runtime/control)

(defclass control-plane (live-object) ())

(defun make-control-plane (&key (id :control-plane))
  (make-instance 'control-plane :id id))

(defun kernel-boot-protocol (context)
  (let ((boot (find-live-object (context-registry context) :boot-protocol)))
    (unless (typep boot 'protocol)
      (error "Boot protocol is not available."))
    boot))

(defun kernel-control-plane (context)
  (let ((control (find-live-object (context-registry context) :control-plane)))
    (unless (typep control 'control-plane)
      (error "Control plane is not available."))
    control))

(defun control-install-protocol (control definition context)
  (declare (ignore control))
  (kli/ext:require-capability :protocol/create)
  (install-protocol (kernel-boot-protocol context) definition context))

(defun control-switch-protocol (control protocol-id context)
  (declare (ignore control))
  (kli/ext:require-capability :protocol/switch)
  (switch-protocol (kernel-boot-protocol context) protocol-id context))

(defun control-rollback-protocol (control context)
  (declare (ignore control))
  (kli/ext:require-capability :protocol/rollback)
  (let* ((boot (kernel-boot-protocol context))
         (transaction (boot-last-protocol-transaction boot)))
    (unless transaction
      (error "No protocol transaction is available for rollback."))
    (setf (boot-last-protocol-transaction boot) nil)
    (rollback-protocol boot transaction context)
    (active-protocol context)))

(defun control-recover-protocol (control context &optional (protocol-id :boot-protocol))
  (declare (ignore control))
  (kli/ext:require-capability :protocol/recover)
  (recover-protocol (kernel-boot-protocol context) protocol-id context))

(defmacro! with-kernel-recovery ((o!context) &body body)
  ;; Recovery to boot is a kernel act, not the caller's authority: bind a
  ;; privileged subject inside the restart clause so a fault barrier recovers
  ;; whatever subject the failing turn ran under. A restart clause runs in the
  ;; restart-case's dynamic environment, so binding here -- not at the
  ;; invoke-restart site -- is what reaches control-recover-protocol's gate.
  `(restart-case
       (progn ,@body)
     (recover-to-boot ()
       :report "Recover the kernel host to the boot protocol."
       (let ((kli/ext:*call-subject*
               (kli/ext:make-subject :capabilities '(:protocol/recover))))
         (control-recover-protocol (kernel-control-plane ,g!context)
                                   ,g!context)))))

(defmethod run-agent-loop :around ((agent agent) context)
  ;; A protocol whose dispatch throws makes run-one-agent-turn's own error-event
  ;; emission re-throw and escape the loop, unwinding the agent thread. Catch
  ;; that escape and recover to boot so the next turn runs under a working
  ;; protocol. The handler-bind wraps outside with-kernel-recovery so its
  ;; recover-to-boot restart is in scope; absent a control plane it declines and
  ;; the error propagates as before.
  (handler-bind ((error (lambda (condition)
                          (declare (ignore condition))
                          (when (find-live-object (context-registry context)
                                                  :control-plane)
                            (let ((restart (find-restart 'recover-to-boot)))
                              (when restart (invoke-restart restart)))))))
    (with-kernel-recovery (context)
      (call-next-method))))

(defun make-control-contract ()
  (make-provider-contract
   :id :runtime/control/v1
   :capability :runtime/control
   :required-entries
   '(:control-install-protocol
     :control-switch-protocol
     :control-rollback-protocol
     :control-recover-protocol)))

(defun make-control-provider ()
  (make-provider
   :id :runtime-control-provider
   :capability :runtime/control
   :contracts '(:runtime/control/v1)
   :entries
   (list :control-install-protocol #'control-install-protocol
         :control-switch-protocol #'control-switch-protocol
         :control-rollback-protocol #'control-rollback-protocol
         :control-recover-protocol #'control-recover-protocol)))

(defextension control
  (:provides
   (contract runtime/control/v1
     (make-control-contract))
   (capability runtime/control (make-control-provider))
   (live-object control-plane
     (make-control-plane))))
