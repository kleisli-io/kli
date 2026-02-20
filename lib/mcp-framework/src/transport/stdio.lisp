;;; MCP Framework - STDIO Transport
;;; Standard input/output transport for MCP servers
;;;
;;; Uses newline-delimited JSON per MCP specification:
;;; "Messages are delimited by newlines, and MUST NOT contain embedded newlines."
;;; See: https://modelcontextprotocol.io/specification/2025-06-18/basic/transports

(in-package #:mcp-framework)

;;; STDIO Transport class

(defclass stdio-transport (transport)
  ((input-stream :initarg :input :initform *standard-input* :reader transport-input)
   (output-stream :initarg :output :initform *standard-output* :reader transport-output)
   (output-lock :initform (make-lock "stdio-output") :reader transport-output-lock))
  (:documentation "STDIO transport for MCP communication.
   Uses newline-delimited JSON (no Content-Length headers)."))

(defun make-stdio-transport (&key (input *standard-input*) (output *standard-output*))
  "Create a new STDIO transport."
  (make-instance 'stdio-transport :input input :output output))

;;; Transport implementation

(defmethod start-transport ((transport stdio-transport))
  "Start STDIO transport (no-op, streams are already open)."
  t)

(defmethod stop-transport ((transport stdio-transport))
  "Stop STDIO transport (no-op for standard streams)."
  t)

(defmethod receive-message ((transport stdio-transport))
  "Receive a JSON-RPC message from stdin.
   Reads one newline-delimited JSON message."
  (let ((input (transport-input transport)))
    (handler-case
        (let ((line (read-line input nil nil)))
          (when (null line)
            (return-from receive-message nil))
          ;; Return the raw JSON line
          line)
      (end-of-file ()
        nil))))

(defmethod send-message ((transport stdio-transport) message)
  "Send a JSON-RPC message to stdout as newline-delimited JSON."
  (with-lock-held ((transport-output-lock transport))
    (let ((output (transport-output transport)))
      (write-line message output)
      (force-output output))))

(defmethod transport-serve ((transport stdio-transport) server)
  "Blocking read loop for stdio transport."
  (loop while (server-running-p server)
        for message = (receive-message transport)
        while message
        do (let ((response (handle-mcp-message message)))
             (when response
               (send-message transport response)))))
