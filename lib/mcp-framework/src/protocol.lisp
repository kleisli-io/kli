;;; MCP Framework - Protocol Layer
;;; JSON-RPC 2.0 message handling and MCP method dispatch

(in-package #:mcp-framework)

;;; Alist to hash-table conversion for yason encoding

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash-table recursively for yason encoding.
   Handles nested alists and vectors of alists."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht)
            (convert-for-json (cdr pair))))
    ht))

(defun convert-for-json (value)
  "Convert VALUE to a form suitable for yason encoding."
  (cond
    ;; Strings pass through unchanged
    ((stringp value) value)
    ;; Alist (list of (string . value) pairs)
    ((and (consp value)
          (consp (car value))
          (stringp (caar value)))
     (alist-to-hash-table value))
    ;; Vector (not string) - convert elements
    ((vectorp value)
     (map 'vector #'convert-for-json value))
    ;; List that's not an alist - convert elements
    ((and (consp value) (not (consp (car value))))
     (mapcar #'convert-for-json value))
    ;; Everything else (numbers, nil, :false, etc.)
    (t value)))

;;; JSON-RPC message parsing

(defun parse-jsonrpc-message (json-string)
  "Parse a JSON-RPC 2.0 message from string.
   Returns hash-table or signals JSONRPC-PARSE-ERROR."
  (handler-case
      (yason:parse json-string)
    (error (e)
      (error 'jsonrpc-parse-error
             :data (format nil "~A" e)))))

(defun validate-jsonrpc-request (message)
  "Validate that MESSAGE is a valid JSON-RPC 2.0 request.
   Returns (values method params id) or signals condition."
  (unless (hash-table-p message)
    (error 'invalid-request :data "Message must be an object"))
  (let ((jsonrpc (gethash "jsonrpc" message))
        (method (gethash "method" message))
        (params (gethash "params" message (make-hash-table :test 'equal)))
        (id (gethash "id" message)))
    (unless (equal jsonrpc "2.0")
      (error 'invalid-request :data "jsonrpc must be \"2.0\""))
    (unless (stringp method)
      (error 'invalid-request :data "method must be a string"))
    (values method params id)))

;;; JSON-RPC response encoding

(defun encode-jsonrpc-response (id result)
  "Encode a successful JSON-RPC 2.0 response."
  (with-output-to-string (s)
    (yason:encode
     (alist-to-hash-table
      `(("jsonrpc" . "2.0")
        ("id" . ,id)
        ("result" . ,result)))
     s)))

(defun encode-jsonrpc-error (id code message &optional data)
  "Encode a JSON-RPC 2.0 error response."
  (let ((error-obj `(("code" . ,code)
                     ("message" . ,message))))
    (when data
      (push (cons "data" data) error-obj))
    (with-output-to-string (s)
      (yason:encode
       (alist-to-hash-table
        `(("jsonrpc" . "2.0")
          ("id" . ,id)
          ("error" . ,error-obj)))
       s))))

;;; MCP Protocol Handlers

(defvar *server-info* nil
  "Current server info (set during server initialization).")

(defvar *mcp-handlers* (make-hash-table :test 'equal)
  "Registry of MCP method handlers.")

(defun register-mcp-handler (method handler)
  "Register a handler function for an MCP method."
  (setf (gethash method *mcp-handlers*) handler))

(defun get-mcp-handler (method)
  "Get the handler for an MCP method."
  (gethash method *mcp-handlers*))

;;; Built-in MCP handlers

(defun handle-initialize (params)
  "Handle initialize request."
  (declare (ignore params))
  (let ((capabilities `(("tools" . (("listChanged" . ,yason:false))))))
    ;; Add resources capability if any resources registered
    ;; Use fboundp to handle case where resources.lisp not yet loaded
    (when (and (fboundp 'list-resources) (funcall 'list-resources))
      (push `("resources" . (("subscribe" . ,yason:false)
                             ("listChanged" . ,yason:false)))
            capabilities))
    ;; Add prompts capability if any prompts registered
    ;; Use fboundp to handle case where prompts.lisp not yet loaded
    (when (and (fboundp 'list-prompts) (funcall 'list-prompts))
      (push `("prompts" . (("listChanged" . ,yason:false)))
            capabilities))
    `(("protocolVersion" . "2024-11-05")
      ("capabilities" . ,capabilities)
      ("serverInfo" . ,(or *server-info*
                           `(("name" . "mcp-framework")
                             ("version" . "1.0.0")))))))

(defun handle-initialized (params)
  "Handle initialized notification."
  (declare (ignore params))
  nil)

(defun handle-tools-list (params)
  "Handle tools/list request."
  (declare (ignore params))
  (let ((tools nil))
    (dolist (name (list-tools))
      (let* ((tool (get-tool name))
             (info (funcall tool :inspect)))
        (push `(("name" . ,name)
                ("description" . ,(or (getf info :documentation) ""))
                ("inputSchema" . ,(funcall tool :schema)))
              tools)))
    `(("tools" . ,(coerce (nreverse tools) 'vector)))))

(defun handle-tools-call (params)
  "Handle tools/call request."
  (let ((name (gethash "name" params))
        (arguments (gethash "arguments" params (make-hash-table :test 'equal))))
    (unless name
      (error 'invalid-params :data "Missing tool name"))
    (let ((tool (get-tool name)))
      (unless tool
        (error 'tool-not-found :tool-name name :data name))
      (handler-case
          (let* ((result (call-with-hooks name arguments
                           (lambda () (funcall tool :call arguments))))
                 (content (normalize-content result)))
            `(("content" . ,(coerce (mapcar #'content-to-json content) 'vector))))
        (error (e)
          (let ((detail (handler-case (format nil "~A" e)
                          (error () (format nil "~S" (type-of e))))))
            (error 'tool-execution-error
                   :tool-name name
                   :message (format nil "~A: ~A" name detail)
                   :data detail)))))))

;;; Register built-in handlers

(register-mcp-handler "initialize" #'handle-initialize)
(register-mcp-handler "notifications/initialized" #'handle-initialized)
(register-mcp-handler "tools/list" #'handle-tools-list)
(register-mcp-handler "tools/call" #'handle-tools-call)

;;; Main dispatch

(defun handle-mcp-message (json-string)
  "Handle a complete MCP message and return response string (or nil for notifications).
   Disables Swank debugger hook during processing so errors propagate to
   handler-case instead of entering the interactive debugger."
  (let ((id nil)
        ;; Disable Swank debugger hook so errors are caught by handler-case
        ;; rather than entering the interactive debugger (which hangs MCP)
        (*debugger-hook* nil))
    (handler-case
        (let ((message (parse-jsonrpc-message json-string)))
          (multiple-value-bind (method params msg-id)
              (validate-jsonrpc-request message)
            (setf id msg-id)
            (let ((handler (get-mcp-handler method)))
              (unless handler
                (error 'method-not-found :data method))
              (let ((result (funcall handler params)))
                ;; Notifications (no id) don't get responses
                (when id
                  (encode-jsonrpc-response id result))))))
      (jsonrpc-parse-error (e)
        ;; Parse errors always return error response with id=null
        (encode-jsonrpc-error nil
                              (mcp-error-code e)
                              (mcp-error-message e)
                              (mcp-error-data e)))
      (mcp-error (e)
        (when id
          (encode-jsonrpc-error id
                                (mcp-error-code e)
                                (mcp-error-message e)
                                (mcp-error-data e))))
      (error (e)
        (when id
          (encode-jsonrpc-error id -32603 "Internal error"
                                (format nil "~A" e)))))))
