(in-package #:kli)

(defclass live-object ()
  ((id
    :initarg :id
    :reader object-id)
   (protocol
    :initarg :protocol
    :initform nil
    :reader object-protocol)))

(defstruct (id-counter (:constructor make-id-counter))
  ;; A word slot so SB-EXT:ATOMIC-INCF mints ids lock-free under concurrent
  ;; workers; a bare special-variable INCF loses ids when two threads race.
  (value 0 :type sb-ext:word))

(defun next-counter-value (counter-symbol)
  "Mint the next monotone value from the ID-COUNTER named by COUNTER-SYMBOL.
ATOMIC-INCF returns the prior value, so 1+ yields the new one."
  (1+ (sb-ext:atomic-incf (id-counter-value (symbol-value counter-symbol)))))

(defun advance-counter (counter-symbol minimum)
  "Raise the ID-COUNTER named by COUNTER-SYMBOL to at least MINIMUM, so a freshly
minted value never collides with one reloaded from disk."
  (sb-ext:atomic-update (id-counter-value (symbol-value counter-symbol))
                        #'max minimum))

(defun counter-value (counter-symbol)
  "Current value of the ID-COUNTER named by COUNTER-SYMBOL."
  (id-counter-value (symbol-value counter-symbol)))

(defun next-keyword-id (prefix counter-symbol)
  "Fresh keyword id PREFIX-N from the ID-COUNTER named by COUNTER-SYMBOL."
  (intern (format nil "~A-~D" prefix (next-counter-value counter-symbol))
          :keyword))

(defclass live-registry ()
  ;; Agent worker threads register objects per turn and per request while
  ;; the TUI loop thread reads on every trampoline tick, so the table must
  ;; be synchronized.
  ((objects
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor registry-objects)))

(defun make-registry ()
  (make-instance 'live-registry))

(defun register-live-object (registry object)
  (let* ((id (object-id object))
         (existing (gethash id (registry-objects registry))))
    (when existing
      (error "Live object already registered: ~S" id))
    (setf (gethash id (registry-objects registry)) object)
    object))

(defun find-live-object (registry id)
  (gethash id (registry-objects registry)))

(defun remove-live-object (registry id)
  (remhash id (registry-objects registry)))

(defun map-live-objects (function registry)
  (maphash function (registry-objects registry))
  nil)

(defclass kernel-context ()
  ((registry
    :initarg :registry
    :initform (make-registry)
    :accessor context-registry)
   (active-protocol
    :initarg :active-protocol
    :initform nil
    :accessor active-protocol)))

(defclass protocol (live-object) ())

(defmethod object-protocol ((self protocol))
  self)

(defgeneric install-protocol (protocol definition context))
(defgeneric switch-protocol (protocol protocol-id context))
(defgeneric rollback-protocol (protocol transaction context))
(defgeneric validate-protocol (protocol context))
(defgeneric smoke-test-protocol (protocol context))

(defmethod validate-protocol ((protocol protocol) context)
  (declare (ignore context))
  protocol)

(defmethod smoke-test-protocol ((protocol protocol) context)
  (declare (ignore context))
  t)
