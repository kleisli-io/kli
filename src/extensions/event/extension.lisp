(in-package #:kli/event)

(defun make-event-contract ()
  (make-provider-contract
   :id :events/v1
   :capability :events
   :required-entries
   '(:make-event
     :emit-event
     :event-type
     :event-payload
     :event-source
     :make-event-type-contribution
     :make-event-handler-contribution
     :dispatch-event)))

(defun make-event-provider ()
  (make-provider
   :id :event-provider
   :capability :events
   :contracts '(:events/v1)
   :entries
   (list :make-event #'make-event
         :emit-event #'emit-event
         :event-type #'event-type
         :event-payload #'event-payload
         :event-source #'event-source
         :make-event-type-contribution #'make-event-type-contribution
         :make-event-handler-contribution #'make-event-handler-contribution
         :dispatch-event 'dispatch-event)))

(defun events-allocate-protocol-storage (protocol)
  (protocol-event-types protocol)
  (protocol-event-handlers protocol)
  nil)

(defun events-clear-protocol-storage (protocol)
  (let ((table (kli/ext:protocol-storage-table protocol)))
    (remhash +events-types-key+ table)
    (remhash +events-handlers-key+ table))
  nil)

(defun install-fault-note-hook (protocol contribution context)
  "Wire CONTEXT's fault emitter: store it in PROTOCOL's storage as the
canonical per-context binding source and register it as the fallback route
for threads that never bind *fault-note-hook*, so contained faults reach the
stream in headless profiles too. The seam variable itself is never setf'd --
UI loops keep shadowing it with their own per-thread bindings."
  (declare (ignore contribution))
  (let ((emitter (make-fault-emitter context)))
    (setf (protocol-storage protocol +events-fault-emitter-key+) emitter)
    (register-fault-note-emitter protocol context emitter))
  nil)

(defun revert-fault-note-hook (protocol contribution context)
  (declare (ignore contribution context))
  (deregister-fault-note-emitter protocol)
  (remhash +events-fault-emitter-key+
           (kli/ext:protocol-storage-table protocol))
  nil)

(defextension events
  (:provides
   (effect events-protocol-storage
           (lambda (protocol contribution context)
             (declare (ignore contribution context))
             (events-allocate-protocol-storage protocol))
           (lambda (protocol contribution context)
             (declare (ignore contribution context))
             (events-clear-protocol-storage protocol)))
   (effect fault-note-hook
           #'install-fault-note-hook
           #'revert-fault-note-hook)
   (contract events/v1
     (make-event-contract))
   (capability events (make-event-provider))
   (event-type :fault)))
