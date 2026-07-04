(in-package #:kli/event)

(defparameter +events-types-key+ :kli/event/types)

(defparameter +events-handlers-key+ :kli/event/handlers)

(defparameter +events-fault-emitter-key+ :kli/event/fault-emitter)

(defun protocol-fault-emitter (protocol)
  "The fault emitter the events extension wired for PROTOCOL's context, or
NIL when events is not installed. Context owners bind *fault-note-hook* to
this to attribute a thread's contained faults to their own stream."
  (protocol-storage protocol +events-fault-emitter-key+))

(defun protocol-event-types (protocol)
  (ensure-protocol-storage protocol +events-types-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun protocol-event-handlers (protocol)
  (ensure-protocol-storage protocol +events-handlers-key+
                           (lambda () (make-hash-table :test #'equal))))

(defclass event-type-contribution (contribution) ())

(defun make-event-type-contribution (&key name source)
  (make-instance 'event-type-contribution
                 :kind :event-type
                 :name name
                 :source source))

(defclass event-handler-contribution (contribution)
  ((event-type
    :initarg :event-type
    :reader contribution-event-type)
   (handler
    :initarg :handler
    :reader contribution-handler)))

(defun make-event-handler-contribution (&key name event-type handler source)
  (make-instance 'event-handler-contribution
                 :kind :event-handler
                 :name name
                 :event-type event-type
                 :handler handler
                 :source source))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution event-type-contribution)
                                 context)
  (declare (ignore context))
  (setf (gethash (contribution-name contribution)
                 (protocol-event-types protocol))
        contribution)
  (push contribution (protocol-installed-contributions protocol))
  contribution)

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution event-handler-contribution)
                                 context)
  (declare (ignore context))
  ;; Appended in install order so the per-event dispatch iterates directly.
  (setf (gethash (contribution-event-type contribution)
                 (protocol-event-handlers protocol))
        (append (gethash (contribution-event-type contribution)
                         (protocol-event-handlers protocol))
                (list contribution)))
  (push contribution (protocol-installed-contributions protocol))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution event-type-contribution)
                                 context)
  (declare (ignore context))
  (remhash (contribution-name contribution)
           (protocol-event-types protocol))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution event-handler-contribution)
                                 context)
  (declare (ignore context))
  (let ((handlers (remove contribution
                          (gethash (contribution-event-type contribution)
                                   (protocol-event-handlers protocol)))))
    (if handlers
        (setf (gethash (contribution-event-type contribution)
                       (protocol-event-handlers protocol))
              handlers)
        (remhash (contribution-event-type contribution)
                 (protocol-event-handlers protocol))))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defmethod dispatch-event ((protocol extension-protocol) event context)
  (dolist (contribution (gethash (event-type event)
                                 (protocol-event-handlers protocol)))
    (safely-invoke (contribution-handler contribution)
                   :event-callback (contribution-name contribution)
                   event context))
  event)

(defcontribution-kind :event-type (extension-id form)
  (destructuring-bind (_ name) form
    (declare (ignore _))
    `(make-event-type-contribution
      :name ',(normalize-extension-id name)
      :source ',extension-id)))

(defcontribution-kind :event-handler (extension-id form)
  (destructuring-bind (_ name &key event-type handler) form
    (declare (ignore _))
    `(make-event-handler-contribution
      :name ',(normalize-extension-id name)
      :event-type ',(normalize-extension-id event-type)
      :handler ,handler
      :source ',extension-id)))
