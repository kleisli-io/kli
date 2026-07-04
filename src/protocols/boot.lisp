(in-package #:kli)

(defclass boot-protocol (protocol)
  ((last-protocol-transaction
    :initform nil
    :accessor boot-last-protocol-transaction)))

(defclass protocol-transaction ()
  ((previous-protocol
    :initarg :previous-protocol
    :reader transaction-previous-protocol)
   (next-protocol
    :initarg :next-protocol
    :reader transaction-next-protocol)))

(defun make-kernel-host ()
  (let* ((context (make-instance 'kernel-context))
         (boot (make-instance 'boot-protocol
                              :id :boot-protocol)))
    (register-live-object (context-registry context) boot)
    (setf (active-protocol context) boot)
    context))

(defmethod install-protocol ((protocol boot-protocol) (installed protocol) context)
  (declare (ignore protocol))
  (validate-protocol installed context)
  (register-live-object (context-registry context) installed)
  installed)

(defmethod install-protocol ((protocol boot-protocol) definition context)
  (declare (ignore protocol context))
  (error "Boot can only install protocol objects: ~S" definition))

(defun resolve-protocol (context protocol-id)
  (etypecase protocol-id
    (protocol protocol-id)
    (symbol
     (or (find-live-object (context-registry context) protocol-id)
         (error "Unknown protocol: ~S" protocol-id)))
    (string
     (or (find-live-object (context-registry context) protocol-id)
         (error "Unknown protocol: ~S" protocol-id)))))

(defmethod switch-protocol ((protocol boot-protocol) protocol-id context)
  (let* ((previous (active-protocol context))
         (candidate (resolve-protocol context protocol-id))
         (transaction (make-instance 'protocol-transaction
                                     :previous-protocol previous
                                     :next-protocol candidate)))
    (handler-case
        (progn
          (validate-protocol candidate context)
          (smoke-test-protocol candidate context)
          (setf (active-protocol context) candidate)
          (setf (boot-last-protocol-transaction protocol) transaction)
          candidate)
      (error (condition)
        (rollback-protocol protocol transaction context)
        (error condition)))))

(defmethod rollback-protocol ((protocol boot-protocol) transaction context)
  (declare (ignore protocol))
  (setf (active-protocol context)
        (transaction-previous-protocol transaction)))

(defgeneric recover-protocol (protocol protocol-id context)
  (:documentation "Recover CONTEXT's active protocol to PROTOCOL-ID's protocol
through boot's own validate and smoke-test, recording a reversible
protocol-transaction so the recovery itself can be rolled back."))

(defmethod recover-protocol ((protocol boot-protocol) protocol-id context)
  (let* ((previous (active-protocol context))
         (candidate (resolve-protocol context protocol-id))
         (transaction (make-instance 'protocol-transaction
                                     :previous-protocol previous
                                     :next-protocol candidate)))
    ;; Recovery is the last resort and its target is normally boot, which
    ;; always validates -- so unlike switch-protocol there is no rollback to a
    ;; broken protocol on a failed recovery; let a failed recovery propagate.
    (validate-protocol candidate context)
    (smoke-test-protocol candidate context)
    (setf (active-protocol context) candidate)
    (setf (boot-last-protocol-transaction protocol) transaction)
    candidate))
