(in-package #:kli/tui/core)

(defclass behavior-cell (live-object)
  ((function
    :initarg :function
    :accessor behavior-function)
   (pandoric-p
    :initarg :pandoric-p
    :initform t
    :accessor behavior-pandoric-p)
   (state
    :initarg :state
    :initform nil
    :accessor behavior-state)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor behavior-metadata)
   (capabilities
    :initarg :capabilities
    :initform '()
    :accessor behavior-capabilities)
   (version
    :initarg :version
    :initform 0
    :accessor behavior-version)
   (fault-policy
    :initarg :fault-policy
    :initform :continue
    :reader behavior-fault-policy
    :documentation "call-behavior's disposition when the cell's function faults, one of :continue, :reify, or :escalate. An install-time contract that recoding metadata cannot touch.")
   (fault-fallback
    :initarg :fault-fallback
    :initform nil
    :reader behavior-fault-fallback
    :documentation "call-behavior's return value for a contained fault.")))

(defun make-pandoric-behavior (function &key state)
  (pandoriclet ((state state))
    function))

(defun make-behavior-cell (&key id function (version 0) state metadata
                             capabilities (pandoric t)
                             (fault-policy :continue) fault-fallback)
  (let ((function (or function (constantly nil))))
    (make-instance 'behavior-cell
                   :id id
                   :function (if pandoric
                                 (make-pandoric-behavior function
                                                         :state state)
                                 function)
                   :pandoric-p pandoric
                   :state state
                   :metadata metadata
                   :capabilities capabilities
                   :version version
                   :fault-policy fault-policy
                   :fault-fallback fault-fallback)))
