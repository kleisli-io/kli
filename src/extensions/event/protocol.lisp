(in-package #:kli/event)

(defclass event ()
  ((type
    :initarg :type
    :reader event-type)
   (payload
    :initarg :payload
    :initform nil
    :reader event-payload)
   (source
    :initarg :source
    :initform nil
    :reader event-source)
   (timestamp
    :initarg :timestamp
    :initform nil
    :reader event-timestamp)))

(defun make-event (type &key payload source)
  "Construct an event, stamping the wall-clock time it occurred. The timestamp
is canonical and identical for every observer, so the envelope stays uniform.
Use a monotonic clock for intervals, never this stamp."
  (make-instance 'event :type type :payload payload :source source
                        :timestamp (get-universal-time)))

(defgeneric dispatch-event (protocol event context))

(defmethod dispatch-event ((protocol protocol) event context)
  (declare (ignore protocol event context))
  nil)

(defun emit-event (context event)
  (when (active-protocol context)
    (dispatch-event (active-protocol context) event context))
  event)

(defun make-fault-emitter (context)
  "A note-hook closure of (SEAM ID CONDITION DISPATCH) that emits a :fault
event onto CONTEXT's stream. A context owner binds it dynamically so contained
faults reach the same stream as any other emission, headless included."
  (lambda (seam id condition dispatch)
    (emit-event context
                (make-event :fault
                            :payload (list :seam seam
                                           :unit id
                                           :condition (princ-to-string condition)
                                           :dispatch dispatch)
                            :source seam))))
