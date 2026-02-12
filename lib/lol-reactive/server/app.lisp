;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Lack application builder and Clack integration
;;;;
;;;; Provides a composable application builder using Lack middleware,
;;;; abstracting away direct Hunchentoot usage.

(in-package :lol-reactive)

;; Required for function-lambda-list introspection
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

;;; ============================================================================
;;; ROUTE REGISTRY
;;; ============================================================================

(defvar *routes* (make-hash-table :test 'equal)
  "Route registry mapping (method . path) to handler functions.
   Methods are keywords (:GET, :POST, etc.), paths are strings.")

(defun clear-routes ()
  "Clear all registered routes."
  (clrhash *routes*))

(defun list-routes ()
  "List all registered routes as list of (method path) pairs."
  (loop for key being the hash-keys of *routes*
        collect (list (car key) (cdr key))))

;;; ============================================================================
;;; PATH PARAMETER SUPPORT
;;; ============================================================================

(defvar *path-params* nil
  "Alist of path parameters extracted from route matching.
   Bound during request handling for routes with :param segments.")

(defun path-param (name)
  "Get a path parameter value by name (string).
   Returns nil if parameter not found."
  (cdr (assoc name *path-params* :test #'string=)))

(defun path-pattern-p (path)
  "Check if path contains parameter segments (e.g., :slug)."
  (and (stringp path)
       (find #\: path)))

(defun match-path-pattern (pattern request-path)
  "Match a path pattern against a request path.
   Pattern segments starting with : are parameters.
   Returns alist of (name . value) if match, nil otherwise.
   Example: (match-path-pattern \"/users/:id\" \"/users/123\") => ((\"id\" . \"123\"))"
  (let ((pattern-segments (remove "" (uiop:split-string pattern :separator "/") :test #'string=))
        (path-segments (remove "" (uiop:split-string request-path :separator "/") :test #'string=)))
    (when (= (length pattern-segments) (length path-segments))
      (loop with params = nil
            for pat in pattern-segments
            for seg in path-segments
            do (cond
                 ;; Parameter segment
                 ((and (> (length pat) 0) (char= (char pat 0) #\:))
                  (push (cons (subseq pat 1) seg) params))
                 ;; Literal segment must match
                 ((string= pat seg) nil)
                 ;; No match
                 (t (return nil)))
            finally (return (nreverse params))))))

(defun find-matching-route (method request-path)
  "Find a route handler that matches the request.
   First tries exact match, then pattern matching.
   Returns (handler . path-params) or nil."
  ;; Try exact match first
  (let ((handler (gethash (cons method request-path) *routes*)))
    (when handler
      (return-from find-matching-route (cons handler nil))))
  ;; Try pattern matching
  (loop for key being the hash-keys of *routes* using (hash-value handler)
        for (route-method . route-path) = key
        when (and (eq route-method method)
                  (path-pattern-p route-path))
        do (let ((params (match-path-pattern route-path request-path)))
             (when params
               (return (cons handler params))))))

;;; ============================================================================
;;; ROUTE DISPATCHER
;;; ============================================================================

(defun route-handler (env)
  "Main route dispatcher for Clack.
   Looks up handler by (method . path) in *routes* registry.
   Supports path parameters (e.g., /users/:id).
   Supports both regular handlers (no args, use *env*) and
   streaming handlers like WebSocket/SSE (take env as arg)."
  (let* ((*env* env)
         (path (request-path))
         (method (request-method))
         (match (find-matching-route method path)))
    (if match
        (let ((*path-params* (cdr match))
              (handler (car match)))
          ;; Check if handler takes an argument (streaming handlers need env)
          (let ((lambda-list (ignore-errors
                              (sb-introspect:function-lambda-list handler))))
            (if (and lambda-list (not (null lambda-list)))
                ;; Handler takes env - streaming handler (WebSocket/SSE)
                (funcall handler env)
                ;; Regular handler - no args, uses *env*
                (with-response-headers ()
                  (handler-case
                      (funcall handler)
                    (error (e)
                      (error-response 500
                                      :message (if *debug-mode*
                                                  (format nil "Error: ~A" e)
                                                  "Internal Server Error"))))))))
        (error-response 404 :message "Not Found"))))

;;; ============================================================================
;;; APPLICATION BUILDER
;;; ============================================================================

(defun make-app (&key (static-path "/static/")
                      (static-root #P"static/")
                      (use-session t)
                      (use-csrf t)
                      (use-accesslog t)
                      (use-static t))
  "Create a Lack application with configurable middleware stack.

   Middleware (applied bottom-up):
   - Session: Memory-backed session management (optional)
   - CSRF: Cross-site request forgery protection (optional, requires session)
   - Accesslog: Request logging (optional)
   - Static: Static file serving (optional)

   Returns a function suitable for Clack:clackup."
  ;; Build middleware wrapper functions dynamically
  ;; Each middleware fn takes an app and returns wrapped app
  (let ((middleware-fns '()))
    ;; Static file serving (outermost - checked first)
    (when use-static
      (let ((static-mw (lack/util:find-middleware :static)))
        (push (lambda (app)
                (funcall static-mw app :path static-path :root static-root))
              middleware-fns)))
    ;; Access logging
    (when use-accesslog
      (push (lack/util:find-middleware :accesslog) middleware-fns))
    ;; CSRF protection (requires session)
    ;; Configure to use "csrf-token" (matching security.lisp) instead of Lack's default "_csrf_token"
    (when use-csrf
      (let ((csrf-mw (lack/util:find-middleware :csrf)))
        (push (lambda (app)
                (funcall csrf-mw app
                         :session-key "csrf-token"
                         :form-token "csrf-token"))
              middleware-fns)))
    ;; Session management (innermost middleware)
    (when use-session
      (push (lack/util:find-middleware :session) middleware-fns))
    ;; Compose middlewares: reduce from right wraps innermost first
    (reduce #'funcall
            (remove-if #'null middleware-fns)
            :initial-value (lack/component:to-app #'route-handler)
            :from-end t)))

;;; ============================================================================
;;; SERVER LIFECYCLE
;;; ============================================================================
;;; Note: defroute and defapi are defined in routes.lisp with enhanced
;;; security, error handling, and content-type support.

(defvar *server* nil
  "Current Hunchentoot acceptor.")

(defvar *lack-app* nil
  "Current Lack application function.")

(defvar *client-socket* nil
  "Current client socket for streaming responses.")

;;; ----------------------------------------------------------------------------
;;; Clack-compatible client for streaming/WebSocket support
;;; ----------------------------------------------------------------------------

(defclass streaming-client ()
  ((stream :initarg :stream :reader client-stream)
   (socket :initarg :socket :reader client-socket)
   (read-callback :initform nil :accessor client-read-callback)
   (write-lock :initform (bordeaux-threads:make-lock "client-write")
               :reader client-write-lock)
   (write-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8)
                                            :adjustable t :fill-pointer 0)
                 :accessor client-write-buffer))
  (:documentation "Client wrapper for streaming responses and WebSocket."))

(defmethod clack.socket:read-callback ((client streaming-client))
  (client-read-callback client))

(defmethod (setf clack.socket:read-callback) (callback (client streaming-client))
  (setf (client-read-callback client) callback))

(defmethod clack.socket:write-sequence-to-socket ((client streaming-client) data &key callback)
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (let ((stream (client-stream client)))
      (write-sequence data stream)
      (force-output stream)))
  (when callback
    (funcall callback)))

(defmethod clack.socket:write-sequence-to-socket-buffer ((client streaming-client) data)
  "Buffer data for later flushing (used by WebSocket handshake)."
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (let ((buffer (client-write-buffer client)))
      (loop for byte across data
            do (vector-push-extend byte buffer)))))

(defmethod clack.socket:write-byte-to-socket-buffer ((client streaming-client) byte)
  "Buffer a single byte for later flushing."
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (vector-push-extend byte (client-write-buffer client))))

(defmethod clack.socket:close-socket ((client streaming-client))
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (finish-output (client-stream client))))

(defmethod clack.socket:flush-socket-buffer ((client streaming-client) &key callback)
  "Flush buffered data to the socket stream."
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (let ((buffer (client-write-buffer client))
          (stream (client-stream client)))
      (when (> (length buffer) 0)
        (write-sequence buffer stream)
        (setf (fill-pointer buffer) 0))
      (force-output stream)))
  (when callback
    (funcall callback)))

(defmethod clack.socket:socket-async-p ((client streaming-client))
  nil)

(defmethod clack.socket:socket-stream ((client streaming-client))
  (client-stream client))

;;; ----------------------------------------------------------------------------
;;; Lack Acceptor with streaming support
;;; ----------------------------------------------------------------------------

(defclass lack-acceptor (hunchentoot:easy-acceptor)
  ((app :initarg :app :accessor lack-app)
   (debug :initarg :debug :initform nil :accessor acceptor-debug))
  (:documentation "Hunchentoot acceptor that dispatches to a Lack app with streaming support."))

(defmethod hunchentoot:acceptor-log-message ((acceptor lack-acceptor) log-level format-string &rest format-arguments)
  "Filter out noisy connection errors from health check clients."
  (let ((message (apply #'format nil format-string format-arguments)))
    ;; Suppress connection-aborted and connection-reset errors (common with health checks)
    (unless (or (search "CONNECTION-ABORTED" message)
                (search "Connection reset by peer" message))
      (call-next-method))))

(defmethod hunchentoot:process-connection :around ((acceptor lack-acceptor) socket)
  "Capture client socket for streaming responses.
   Silently handles connection-aborted errors (common with health check clients)."
  (let ((*client-socket* socket))
    (handler-case (call-next-method)
      (usocket:connection-aborted-error ()
        ;; Client disconnected early - common with health checks, not an error
        nil))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor lack-acceptor) request)
  "Dispatch request through Lack app with streaming/delayed response support."
  (let ((app (lack-app acceptor)))
    (if app
        (let* ((env (build-clack-env request))
               (response (if (acceptor-debug acceptor)
                             (funcall app env)
                             (handler-case (funcall app env)
                               (error (e)
                                 (format *error-output* "~&Error: ~A~%" e)
                                 '(500 () ("Internal Server Error")))))))
          (handle-lack-response response))
        (call-next-method))))

(defun handle-lack-response (response)
  "Handle Lack response - either normal (status headers body) or delayed (function)."
  (etypecase response
    (list (handle-normal-response response))
    (function (funcall response #'handle-normal-response))))

(defun handle-normal-response (response)
  "Handle a normal (status headers body) response."
  (destructuring-bind (status headers &optional body) response
    (setf (hunchentoot:return-code*) status)
    ;; Set headers
    (loop for (key val) on headers by #'cddr
          if (eq key :content-type)
            do (setf (hunchentoot:content-type*) val)
          else if (eq key :content-length)
            do (setf (hunchentoot:content-length*) val)
          else if (eq key :set-cookie)
            do (rplacd (last (hunchentoot:headers-out*))
                       (list (cons key val)))
          else
            do (setf (hunchentoot:header-out key) val))
    ;; Handle body
    (unless body
      ;; No body provided - return streaming writer for delayed response
      (return-from handle-normal-response
        (let ((out (hunchentoot:send-headers)))
          (lambda (data &key (start 0) (end (length data)) close)
            (handler-case
                (etypecase data
                  (null nil)
                  (string
                   (write-sequence
                    (flexi-streams:string-to-octets
                     data :start start :end end
                     :external-format hunchentoot:*hunchentoot-default-external-format*)
                    out))
                  ((vector (unsigned-byte 8))
                   (write-sequence data out :start start :end end)))
              (error (e)
                (format *error-output* "~&Error writing to socket: ~A~%" e)))
            (if close
                (finish-output out)
                (force-output out))))))
    ;; Normal body handling
    (handler-case
        (etypecase body
          (null nil)
          (pathname
           (hunchentoot:handle-static-file body (getf headers :content-type)))
          (list
           (let ((out (hunchentoot:send-headers)))
             (dolist (chunk body)
               (write-sequence
                (flexi-streams:string-to-octets
                 chunk :external-format hunchentoot:*hunchentoot-default-external-format*)
                out))
             (finish-output out)))
          ((vector (unsigned-byte 8))
           (let ((out (hunchentoot:send-headers)))
             (write-sequence body out)
             (finish-output out))))
      (error (e)
        (format *error-output* "~&Error writing response: ~A~%" e)))))

(defun build-clack-env (request)
  "Build Clack environment plist from Hunchentoot request.
   Includes :clack.streaming and :clack.io for WebSocket/SSE support."
  ;; Convert headers alist to hash-table as Lack middleware expects
  (let ((headers-ht (make-hash-table :test 'equal)))
    (dolist (header (hunchentoot:headers-in request))
      (setf (gethash (string-downcase (string (car header))) headers-ht)
            (cdr header)))
    (list :request-method (hunchentoot:request-method request)
          :script-name ""
          :path-info (hunchentoot:script-name request)
          :query-string (or (hunchentoot:query-string request) "")
          :server-name (hunchentoot:host request)
          :server-port (hunchentoot:acceptor-port (hunchentoot:request-acceptor request))
          :server-protocol (hunchentoot:server-protocol request)
          :request-uri (hunchentoot:request-uri request)
          :url-scheme (if (hunchentoot:ssl-p) "https" "http")
          :remote-addr (hunchentoot:remote-addr request)
          :remote-port (hunchentoot:remote-port request)
          :content-type (hunchentoot:header-in :content-type request)
          :content-length (alexandria:when-let (cl (hunchentoot:header-in :content-length request))
                            (parse-integer cl :junk-allowed t))
          :headers headers-ht
          :raw-body (hunchentoot:raw-post-data :request request :want-stream t)
          ;; Streaming support for WebSocket/SSE
          ;; Use Hunchentoot's content-stream for bidirectional communication
          ;; (same as Clack's hunchentoot handler does)
          :clack.streaming t
          :clack.io (when *client-socket*
                      (make-instance 'streaming-client
                                     :socket *client-socket*
                                     :stream (hunchentoot::content-stream request))))))

(defun start-server (&key (port 8080) debug
                          (static-path "/static/") (static-root #P"static/")
                          (use-session t) (use-csrf t) (use-accesslog t) (use-static t))
  "Start the web server.

   PORT: Listen port (default 8080)
   DEBUG: Enable debug mode for verbose errors
   STATIC-PATH: URL path for static files (default /static/)
   STATIC-ROOT: Filesystem path for static files
   USE-SESSION: Enable session middleware (default t)
   USE-CSRF: Enable CSRF protection (default t)
   USE-ACCESSLOG: Enable access logging (default t)
   USE-STATIC: Enable static file serving (default t)

   Returns server handle for stop-server, or NIL if port is already in use."
  (when *server*
    (error "Server already running. Call stop-server first."))
  (when debug
    (enable-debug-mode))
  (setf *lack-app* (make-app :static-path static-path
                             :static-root static-root
                             :use-session use-session
                             :use-csrf use-csrf
                             :use-accesslog use-accesslog
                             :use-static use-static))
  (setf *server* (make-instance 'lack-acceptor :port port :app *lack-app*))
  (handler-case
      (progn
        (hunchentoot:start *server*)
        (format t "~&Server started on port ~A~%" port)
        *server*)
    (usocket:address-in-use-error (c)
      (format *error-output* "~&[lol-reactive] Port ~A already in use: ~A~%" port c)
      (setf *server* nil)
      (setf *lack-app* nil)
      nil)))

(defun stop-server (&optional (server *server*))
  "Stop the web server.

   SERVER: Server handle from start-server (default *server*)"
  (when server
    (hunchentoot:stop server)
    (setf *server* nil)
    (setf *lack-app* nil)
    (format t "~&Server stopped~%")
    t))

;;; ============================================================================
;;; HTMX INTEGRATION HELPERS
;;; ============================================================================

(defun htmx-request-p ()
  "Check if current request is from HTMX client."
  (string= "true" (request-header "HX-Request")))

(defun htmx-boosted-p ()
  "Check if request is from hx-boost link."
  (string= "true" (request-header "HX-Boosted")))

(defun htmx-history-restore-request-p ()
  "Check if request is for history restoration."
  (string= "true" (request-header "HX-History-Restore-Request")))

(defun htmx-target ()
  "Get target element ID from HTMX request."
  (request-header "HX-Target"))

(defun htmx-trigger ()
  "Get triggering element ID from HTMX request."
  (request-header "HX-Trigger"))

(defun htmx-trigger-name ()
  "Get triggering element's name attribute."
  (request-header "HX-Trigger-Name"))

(defun htmx-current-url ()
  "Get current URL from HTMX request."
  (request-header "HX-Current-URL"))

(defun htmx-prompt ()
  "Get user response from hx-prompt."
  (request-header "HX-Prompt"))

(defmacro with-htmx-response ((&key trigger retarget reswap reselect
                                    push-url replace-url refresh)
                              &body body)
  "Execute body with HTMX response headers.

   TRIGGER: Event to trigger on client (string or JSON)
   RETARGET: CSS selector to retarget the swap
   RESWAP: How to swap (innerHTML, outerHTML, etc.)
   RESELECT: CSS selector to select from response
   PUSH-URL: URL to push to browser history
   REPLACE-URL: URL to replace in browser history
   REFRESH: If true, trigger full page refresh"
  `(progn
     ,@(when trigger `((add-response-header "HX-Trigger" ,trigger)))
     ,@(when retarget `((add-response-header "HX-Retarget" ,retarget)))
     ,@(when reswap `((add-response-header "HX-Reswap" ,reswap)))
     ,@(when reselect `((add-response-header "HX-Reselect" ,reselect)))
     ,@(when push-url `((add-response-header "HX-Push-Url" ,push-url)))
     ,@(when replace-url `((add-response-header "HX-Replace-Url" ,replace-url)))
     ,@(when refresh `((add-response-header "HX-Refresh" "true")))
     (let ((body-result (progn ,@body)))
       (if (stringp body-result)
           (html-response body-result :headers (get-response-headers))
           body-result))))
