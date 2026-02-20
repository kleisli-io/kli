;;; MCP Framework - Server
;;; Server lifecycle and main loop

(in-package #:mcp-framework)

;;; Server class

(defclass mcp-server ()
  ((name :initarg :name :initform "mcp-server" :reader server-name)
   (version :initarg :version :initform "1.0.0" :reader server-version)
   (transport :initarg :transport :initform nil :accessor server-transport)
   (running :initform nil :accessor server-running-p)
   (thread :initform nil :accessor server-thread)
   (lock :initform (make-lock "server") :reader server-lock))
  (:documentation "MCP server instance."))

(defun make-server (&key (name "mcp-server") (version "1.0.0") transport tools)
  "Create a new MCP server.
   TOOLS is a list of tool symbols to register (tools must be defined with define-tool)."
  (declare (ignore tools))  ; Tools are registered globally via define-tool
  (let ((server (make-instance 'mcp-server
                               :name name
                               :version version
                               :transport (or transport (make-stdio-transport)))))
    server))

;;; Server lifecycle

(defmethod start-server ((server mcp-server))
  "Start the server, initializing transport."
  (with-lock-held ((server-lock server))
    (when (server-running-p server)
      (return-from start-server server))
    ;; Set server info for initialize response
    (setf *server-info* `(("name" . ,(server-name server))
                          ("version" . ,(server-version server))))
    ;; Start transport
    (start-transport (server-transport server))
    (setf (server-running-p server) t))
  server)

(defmethod stop-server ((server mcp-server))
  "Stop the server and clean up."
  (with-lock-held ((server-lock server))
    (when (server-running-p server)
      (setf (server-running-p server) nil)
      (stop-transport (server-transport server))))
  server)

;;; Main server loop

(defun server-loop (server)
  "Main message processing loop."
  (let ((transport (server-transport server)))
    (loop while (server-running-p server)
          for message = (receive-message transport)
          while message
          do (let ((response (handle-mcp-message message)))
               (when response
                 (send-message transport response))))))

(defun run-server (server &key background)
  "Run the MCP server.
   If BACKGROUND is true, run in a separate thread and return immediately.
   Otherwise, block until server stops."
  (start-server server)
  (let ((transport (server-transport server)))
    (if background
        (progn
          (setf (server-thread server)
                (make-thread (lambda () (transport-serve transport server))
                             :name (format nil "MCP-~A" (server-name server))))
          server)
        (unwind-protect
             (transport-serve transport server)
          (stop-server server)))))
