;;;; JSON-RPC dispatch over the membrane, plus the stdio serve loop. Tool faults
;;;; map to isError results; JSON-RPC errors are reserved for malformed requests
;;;; and unknown methods. stdout carries JSON-RPC only; logging belongs on stderr.

(in-package #:kli/runtime/mcp-server)

(defparameter *protocol-version* "2025-11-25"
  "The server's latest protocol revision, and the version returned when a
client requests one the server does not speak.")
(defparameter *supported-protocol-versions*
  '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05")
  "Revisions the server will speak. A client's requested version is echoed when
listed here; otherwise the server replies its latest and the client decides.")
(defparameter *server-name* "kli")
(defparameter *server-version* "0.0.0"
  "Reported in initialize serverInfo; the app sets this to the running version.")

(defun negotiated-version (params)
  "The protocol version to report: the client's requested version when the
server speaks it, else the server's latest."
  (let ((requested (gethash "protocolVersion" params)))
    (if (and requested
             (member requested *supported-protocol-versions* :test #'equal))
        requested
        *protocol-version*)))

(defun dispatch (surface context msg)
  "MSG is a parsed JSON-RPC request object. Returns (values KIND id payload...),
KIND one of :ok (payload = result object), :err (payload = code message), or
:none (a notification, or an id-less unknown method -- no reply)."
  (let ((method (gethash "method" msg))
        (id (gethash "id" msg))
        (params (or (gethash "params" msg) (make-hash-table :test #'equal))))
    (cond
      ((equal method "initialize")
       (values :ok id (obj "protocolVersion" (negotiated-version params)
                           "capabilities" (obj "tools" (obj)
                                               "prompts" (obj)
                                               "resources" (obj))
                           "serverInfo" (obj "name" *server-name*
                                             "version" *server-version*))))
      ((equal method "notifications/initialized") (values :none))
      ((equal method "tools/list") (values :ok id (tools-list-result surface)))
      ((equal method "tools/call")
       (values :ok id (tools-call-result surface context params)))
      ((equal method "prompts/list") (values :ok id (prompts-list-result surface)))
      ((equal method "prompts/get")
       (multiple-value-bind (kind a b) (prompt-get-result surface params)
         (ecase kind
           (:ok (values :ok id a))
           (:err (values :err id a b)))))
      ((equal method "resources/list") (values :ok id (resources-list-result surface)))
      ((equal method "resources/read")
       (multiple-value-bind (kind a b) (resource-read-result surface params)
         (ecase kind
           (:ok (values :ok id a))
           (:err (values :err id a b)))))
      ((null id) (values :none))
      (t (values :err id -32601 (format nil "method not found: ~A" method))))))

(defun serve-stream (surface context in out)
  "Read newline-framed JSON-RPC requests from IN, dispatch, write framed
responses to OUT. A single blocking loop: one process serves one client. A
malformed line yields a parse error and the loop continues."
  (loop
    (let ((msg (handler-case (rpc:read-jsonrpc-message in)
                 (error (condition)
                   (rpc:write-jsonrpc-error
                    out nil -32700 (format nil "parse error: ~A" condition))
                   :parse-error))))
      (cond
        ((eq msg :eof) (return))
        ((eq msg :parse-error) nil)
        (t (multiple-value-bind (kind id a b) (dispatch surface context msg)
             (ecase kind
               (:none nil)
               (:ok  (rpc:write-jsonrpc-response out id a))
               (:err (rpc:write-jsonrpc-error out id a b)))))))))
