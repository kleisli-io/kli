;;; MCP Framework - Transport Protocol
;;; Generic transport interface for MCP message I/O

(in-package #:mcp-framework)

;;; Transport base class

(defclass transport ()
  ((running :initform nil :accessor transport-running-p)
   (lock :initform (make-lock "transport") :reader transport-lock))
  (:documentation "Base class for MCP transports."))

;;; Generic functions

(defgeneric start-transport (transport)
  (:documentation "Start the transport, preparing it for message I/O."))

(defgeneric stop-transport (transport)
  (:documentation "Stop the transport and clean up resources."))

(defgeneric receive-message (transport)
  (:documentation "Receive a message from the transport. Returns string or nil on EOF."))

(defgeneric send-message (transport message)
  (:documentation "Send a message string through the transport."))

(defgeneric transport-serve (transport server)
  (:documentation "Transport-specific main serving loop.
   Called by run-server after start-server. Each transport defines how
   messages are received and dispatched. For stdio, this is a blocking
   read loop. For HTTP, this blocks while Hunchentoot handles requests."))

;;; Default implementations

(defmethod start-transport :around ((transport transport))
  "Wrap start with lock and state management."
  (with-lock-held ((transport-lock transport))
    (unless (transport-running-p transport)
      (call-next-method)
      (setf (transport-running-p transport) t))))

(defmethod stop-transport :around ((transport transport))
  "Wrap stop with lock and state management."
  (with-lock-held ((transport-lock transport))
    (when (transport-running-p transport)
      (call-next-method)
      (setf (transport-running-p transport) nil))))
