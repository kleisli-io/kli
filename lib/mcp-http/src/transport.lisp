;;; MCP HTTP Transport - Transport class and lifecycle
;;;
;;; Implements mcp-framework's transport protocol for Streamable HTTP.
;;; Hunchentoot handles requests in its worker thread pool; the POST
;;; handler calls handle-mcp-message directly (no queue needed since
;;; handle-mcp-message is pure: JSON string in, JSON string out).
;;;
;;; Server-initiated messages are pushed to open SSE connections via
;;; send-message, which writes SSE events to all registered connections.

(in-package #:mcp-http)

;;; MCP Acceptor

(defclass mcp-acceptor (hunchentoot:easy-acceptor)
  ((endpoint :initarg :endpoint :initform "/mcp" :reader acceptor-endpoint)
   (http-transport :initarg :transport :reader acceptor-transport))
  (:documentation "Hunchentoot acceptor with per-instance MCP endpoint dispatch.
   Holds a reference to the http-transport for SSE connection management."))

;;; HTTP Transport

(defclass http-transport (mcp-framework:transport)
  ((port :initarg :port :initform 8080 :reader transport-port)
   (host :initarg :host :initform "127.0.0.1" :reader transport-host)
   (endpoint :initarg :endpoint :initform "/mcp" :reader transport-endpoint)
   (acceptor :initform nil :accessor transport-acceptor)
   (session-store :initform (make-session-store) :reader transport-session-store)
   (sse-connections :initform nil :accessor transport-sse-connections)
   (sse-lock :initform (make-lock "sse-connections") :reader transport-sse-lock))
  (:documentation "Streamable HTTP transport for MCP servers.
   Uses Hunchentoot to serve JSON-RPC over HTTP POST.
   Supports SSE for server-initiated messages via GET.
   Sessions tracked via Mcp-Session-Id header (MCP spec 2025-03-26)."))

(defun make-http-transport (&key (port 8080) (host "127.0.0.1") (endpoint "/mcp"))
  "Create an HTTP transport for MCP Streamable HTTP."
  (make-instance 'http-transport :port port :host host :endpoint endpoint))

;;; Transport protocol implementation

(defmethod mcp-framework:start-transport ((transport http-transport))
  "Start Hunchentoot acceptor for MCP HTTP endpoint.
   Signals a clear error if the port is already in use."
  (let ((acceptor (make-instance 'mcp-acceptor
                    :port (transport-port transport)
                    :address (transport-host transport)
                    :endpoint (transport-endpoint transport)
                    :transport transport)))
    (setf (transport-acceptor transport) acceptor)
    (handler-case (hunchentoot:start acceptor)
      (usocket:address-in-use-error ()
        (setf (transport-acceptor transport) nil)
        (error "Port ~A is already in use. Stop the existing process or ~
                configure a different port." (transport-port transport))))))

(defmethod mcp-framework:stop-transport ((transport http-transport))
  "Stop Hunchentoot acceptor and close all SSE connections."
  ;; Mark all SSE connections as dead so their handler loops exit
  (with-lock-held ((transport-sse-lock transport))
    (dolist (conn (transport-sse-connections transport))
      (setf (connection-alive-p conn) nil))
    (setf (transport-sse-connections transport) nil))
  ;; Stop Hunchentoot
  (when (transport-acceptor transport)
    (hunchentoot:stop (transport-acceptor transport))
    (setf (transport-acceptor transport) nil)))

(defmethod mcp-framework:transport-serve ((transport http-transport) server)
  "Block while Hunchentoot handles HTTP requests in its worker threads.
   Hunchentoot has its own thread pool — this method just keeps the
   main server thread alive until shutdown."
  (loop while (mcp-framework:server-running-p server)
        do (sleep 0.5)))

(defmethod mcp-framework:send-message ((transport http-transport) message)
  "Send a server-initiated message to all open SSE connections as SSE events."
  (with-lock-held ((transport-sse-lock transport))
    (dolist (conn (transport-sse-connections transport))
      (when (connection-alive-p conn)
        (write-sse-event conn message)))
    ;; Prune dead connections
    (setf (transport-sse-connections transport)
          (remove-if-not #'connection-alive-p
                         (transport-sse-connections transport)))))
