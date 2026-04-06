;;; MCP Framework - Error conditions
;;; JSON-RPC 2.0 error codes and MCP-specific errors

(in-package #:mcp-framework)

;;; Base condition

(define-condition mcp-error (error)
  ((code :initarg :code :reader mcp-error-code)
   (message :initarg :message :reader mcp-error-message)
   (data :initarg :data :reader mcp-error-data :initform nil))
  (:report (lambda (c s)
             (format s "MCP Error ~A: ~A~@[ (~A)~]"
                     (mcp-error-code c)
                     (mcp-error-message c)
                     (mcp-error-data c)))))

;;; JSON-RPC 2.0 standard errors (-32700 to -32600)

(define-condition jsonrpc-parse-error (mcp-error)
  ()
  (:default-initargs :code -32700 :message "Parse error"))

(define-condition invalid-request (mcp-error)
  ()
  (:default-initargs :code -32600 :message "Invalid Request"))

(define-condition method-not-found (mcp-error)
  ()
  (:default-initargs :code -32601 :message "Method not found"))

(define-condition invalid-params (mcp-error)
  ()
  (:default-initargs :code -32602 :message "Invalid params"))

(define-condition internal-error (mcp-error)
  ()
  (:default-initargs :code -32603 :message "Internal error"))

;;; MCP-specific errors

(define-condition tool-not-found (mcp-error)
  ((tool-name :initarg :tool-name :reader tool-not-found-name))
  (:default-initargs :code -32001 :message "Tool not found"))

(define-condition tool-execution-error (mcp-error)
  ((tool-name :initarg :tool-name :reader tool-execution-error-name))
  (:default-initargs :code -32002 :message "Tool execution failed"))

(define-condition resource-not-found (mcp-error)
  ((uri :initarg :uri :reader resource-not-found-uri))
  (:default-initargs :code -32003 :message "Resource not found"))

(define-condition prompt-not-found (mcp-error)
  ((prompt-name :initarg :prompt-name :reader prompt-not-found-name))
  (:default-initargs :code -32004 :message "Prompt not found"))

;;; Helper to create error response data

(defun error-to-json (condition)
  "Convert an MCP-ERROR condition to JSON-compatible alist."
  (let ((result `(("code" . ,(mcp-error-code condition))
                  ("message" . ,(mcp-error-message condition)))))
    (when (mcp-error-data condition)
      (push (cons "data" (mcp-error-data condition)) result))
    result))
