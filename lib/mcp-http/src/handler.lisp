;;; MCP HTTP Transport - Request handler
;;;
;;; Implements MCP Streamable HTTP (spec 2025-03-26):
;;; - POST /mcp: JSON-RPC request → JSON response (or 202 for notifications)
;;; - GET /mcp: SSE stream for server-initiated messages
;;; - DELETE /mcp: Session termination
;;;
;;; Security:
;;; - Origin header validation on all requests (DNS rebinding prevention)
;;; - Mcp-Session-Id lifecycle (generate on initialize, validate on subsequent)

(in-package #:mcp-http)

;;; Request dispatch

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor mcp-acceptor) request)
  "Route MCP endpoint requests to the handler; delegate everything else."
  (if (string= (hunchentoot:script-name request) (acceptor-endpoint acceptor))
      (handle-mcp-request acceptor request)
      (call-next-method)))

;;; Origin validation gate

(defun check-origin (transport)
  "Validate Origin header. Returns T if valid, sets 403 and returns NIL otherwise."
  (let ((origin (hunchentoot:header-in* "Origin")))
    (if (valid-origin-p origin
                        (transport-host transport)
                        (transport-port transport))
        t
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
          nil))))

;;; Session validation gate

(defun check-session (transport)
  "Validate Mcp-Session-Id header on non-initialize requests.
   Returns T if valid (or no session exists yet), sets 404 and returns NIL
   if session ID is required but missing/invalid."
  (let* ((store (transport-session-store transport))
         (client-id (hunchentoot:header-in* "Mcp-Session-Id"))
         (has-sessions (plusp (hash-table-count (store-sessions store)))))
    (cond
      ;; No sessions created yet — server hasn't seen initialize
      ((not has-sessions) t)
      ;; Client provides valid session ID
      ((and client-id (valid-session-p store client-id)) t)
      ;; Session required but missing or invalid → 404 (client must reinitialize)
      (t (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
         nil))))

;;; Initialize detection

(defun initialize-request-p (body)
  "Check if BODY is a JSON-RPC initialize request.
   Returns T if the method field is \"initialize\"."
  (handler-case
      (let ((parsed (yason:parse body)))
        (and (hash-table-p parsed)
             (equal (gethash "method" parsed) "initialize")))
    (error () nil)))

;;; MCP request handling

(defun handle-mcp-request (acceptor request)
  "Handle an HTTP request to the MCP endpoint.
   POST: JSON-RPC message → JSON response (or 202 for notifications).
   GET: Open SSE stream for server-initiated messages.
   DELETE: Terminate session.
   All methods: Origin validation, session validation."
  (let ((transport (acceptor-transport acceptor)))
    ;; Gate 1: Origin validation (all methods)
    (unless (check-origin transport)
      (return-from handle-mcp-request ""))
    (case (hunchentoot:request-method request)
      (:post (handle-post transport))
      (:get
       ;; Gate 2: Session validation (GET requires valid session if sessions active)
       (unless (check-session transport)
         (return-from handle-mcp-request ""))
       (handle-sse-stream acceptor))
      (:delete
       (handle-delete transport))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
       ""))))

;;; POST handler

(defun handle-post (transport)
  "Handle POST request: JSON-RPC message processing with session lifecycle."
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (is-init (initialize-request-p body)))
    ;; Session validation: skip for initialize (it creates the session)
    (unless is-init
      (unless (check-session transport)
        (return-from handle-post "")))
    ;; Process the JSON-RPC message
    (let ((response (mcp-framework:handle-mcp-message body)))
      (setf (hunchentoot:content-type*) "application/json")
      (cond
        ;; Initialize response: create session, set header
        ((and is-init response)
         (let ((session-id (create-session (transport-session-store transport))))
           (setf (hunchentoot:header-out "Mcp-Session-Id") session-id))
         response)
        ;; Normal response
        (response response)
        ;; Notification (no response body)
        (t (setf (hunchentoot:return-code*) hunchentoot:+http-accepted+)
           "")))))

;;; DELETE handler

(defun handle-delete (transport)
  "Handle DELETE request: session termination."
  (let* ((store (transport-session-store transport))
         (session-id (hunchentoot:header-in* "Mcp-Session-Id")))
    (cond
      ;; Valid session → terminate it
      ((and session-id (valid-session-p store session-id))
       (terminate-session store session-id)
       (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
       "")
      ;; No/invalid session → 404
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       ""))))

;;; SSE stream handler

(defun handle-sse-stream (acceptor)
  "Open an SSE stream for server-initiated messages.
   Blocks the Hunchentoot worker thread for the duration of the connection.
   The server pushes events via send-message on the http-transport."
  (setf (hunchentoot:content-type*) "text/event-stream")
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (setf (hunchentoot:header-out "X-Accel-Buffering") "no")
  ;; Send headers and get the raw binary stream
  (let* ((binary-stream (hunchentoot:send-headers))
         (text-stream (flexi-streams:make-flexi-stream
                       binary-stream :external-format :utf-8))
         (transport (acceptor-transport acceptor))
         (conn (make-instance 'sse-connection :stream text-stream)))
    (register-sse-connection transport conn)
    (unwind-protect
        ;; Block: heartbeat loop keeps the connection alive and detects disconnects
        (loop while (and (connection-alive-p conn)
                         (mcp-framework:transport-running-p transport))
              do (unless (write-sse-comment conn "heartbeat")
                   (return))
                 (sleep 15))
      ;; Cleanup on exit (disconnect, server stop, or heartbeat failure)
      (setf (connection-alive-p conn) nil)
      (unregister-sse-connection transport conn)))
  ;; Return nil — response already sent via send-headers
  nil)
