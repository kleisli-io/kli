;;; MCP HTTP Transport - Server-Sent Events
;;;
;;; SSE event formatting and connection management per the SSE standard.
;;; Used for server-initiated messages (GET /mcp) and optionally for
;;; streaming POST responses (MCP Streamable HTTP spec 2025-03-26).

(in-package #:mcp-http)

;;; SSE Event Formatting

(defun format-sse-event (data &key id event)
  "Format a Server-Sent Event string.
   DATA is the event data (typically a JSON-RPC message string).
   ID is an optional event ID for resumability.
   EVENT is an optional event type name.
   Returns a string ready to write to an SSE stream."
  (with-output-to-string (s)
    (when id (format s "id: ~A~%" id))
    (when event (format s "event: ~A~%" event))
    ;; Each line of data must be prefixed with "data: "
    (dolist (line (uiop:split-string data :separator '(#\Newline)))
      (format s "data: ~A~%" line))
    ;; Blank line terminates the event
    (terpri s)))

;;; SSE Connection

(defclass sse-connection ()
  ((stream :initarg :stream :reader connection-stream
           :documentation "Text stream wrapping the Hunchentoot output.")
   (alive :initform t :accessor connection-alive-p))
  (:documentation "An open SSE connection from a GET request."))

(defun register-sse-connection (transport connection)
  "Register an SSE connection with the transport. Thread-safe."
  (with-lock-held ((transport-sse-lock transport))
    (push connection (transport-sse-connections transport)))
  connection)

(defun unregister-sse-connection (transport connection)
  "Remove an SSE connection from the transport. Thread-safe."
  (with-lock-held ((transport-sse-lock transport))
    (setf (transport-sse-connections transport)
          (delete connection (transport-sse-connections transport)))))

;;; SSE Writing

(defun write-sse-event (connection data &key id event)
  "Write an SSE event to a connection. Returns T on success, NIL on failure.
   On failure, marks the connection as dead."
  (handler-case
      (let ((stream (connection-stream connection)))
        (write-string (format-sse-event data :id id :event event) stream)
        (force-output stream)
        t)
    (error ()
      (setf (connection-alive-p connection) nil)
      nil)))

(defun write-sse-comment (connection text)
  "Write an SSE comment (used for heartbeats). Returns T on success, NIL on failure."
  (handler-case
      (let ((stream (connection-stream connection)))
        (format stream ": ~A~%~%" text)
        (force-output stream)
        t)
    (error ()
      (setf (connection-alive-p connection) nil)
      nil)))
