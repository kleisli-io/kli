(in-package #:kli/runtime/isolated)

;;; A Model Context Protocol client riding the persistent transport: handshake,
;;; tools/list+call, resources/list+read, and server notifications re-emitted as
;;; kli events. Standalone -- connect, list, call, read, observe -- with the
;;; membrane mapping every wire shape onto its kli analogue.

(defparameter +mcp-protocol-version+ "2025-11-25"
  "MCP wire protocol version this client proposes at initialize. The server may
echo another it supports; the negotiated value is stored, not hard-gated.")

(defparameter *default-client-name* "kli"
  "clientInfo name sent at initialize when the caller gives none.")

(defparameter *default-client-version* "0.0.0"
  "clientInfo version sent at initialize when the caller gives none.")

(defstruct (mcp-client (:constructor %make-mcp-client))
  (process nil)
  (server-info nil)
  (server-capabilities nil)
  (protocol-version nil)
  (context nil))

;;; MCP keys are camelCase, but the transport's plist coercion downcases keys,
;;; so every request object is built as an explicit string-keyed hash that
;;; jsonify passes through unchanged.

(defun %string-object (&rest keys-and-values)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          when value do (setf (gethash key object) value))
    object))

(defun %initialize-params (client-name client-version)
  (%string-object "protocolVersion" +mcp-protocol-version+
                  "capabilities" (make-hash-table :test #'equal)
                  "clientInfo" (%string-object "name" client-name
                                               "version" client-version)))

(defun %mcp-notification-handler (context)
  "A transport notification handler that re-emits each id-less server message
as a kli event on CONTEXT. Runs on the transport reader thread."
  (lambda (message)
    (multiple-value-bind (type payload) (notification->event message)
      (emit-event context (make-event type :payload payload :source :mcp)))))

(defun mcp-connect (command &key arguments directory environment context
                                 (client-name *default-client-name*)
                                 (client-version *default-client-version*)
                                 (timeout *default-isolated-timeout*))
  "Spawn an MCP server subprocess and run the initialize handshake: propose the
protocol version, capture serverInfo/capabilities/negotiated version, then send
notifications/initialized. Returns an mcp-client, or NIL if the spawn faults.
Server notifications are re-emitted as kli events on CONTEXT when given. A
handshake failure tears the subprocess down before propagating."
  (let ((process (spawn-isolated-process
                  command
                  :arguments arguments
                  :directory directory
                  :environment environment
                  :notification-handler (when context
                                          (%mcp-notification-handler context)))))
    (when process
      (let ((connected nil))
        (unwind-protect
             (let ((result (call-isolated process "initialize"
                                          (%initialize-params client-name
                                                              client-version)
                                          :timeout timeout)))
               (notify-isolated process "notifications/initialized" nil)
               (prog1
                   (%make-mcp-client
                    :process process
                    :server-info (and (hash-table-p result)
                                      (gethash "serverInfo" result))
                    :server-capabilities (and (hash-table-p result)
                                              (gethash "capabilities" result))
                    :protocol-version (and (hash-table-p result)
                                           (gethash "protocolVersion" result))
                    :context context)
                 (setf connected t)))
          (unless connected (teardown-isolated process)))))))

(declaim (ftype (function (t t t &optional t) t) mcp-call-tool))

(defun %mcp-tool->kli (client entry)
  "Build a kli tool from one tools/list entry: mapped parameters, the raw
inputSchema kept in metadata, and a runner that closes over CLIENT to call
tools/call when the model invokes the tool."
  (let ((name (gethash "name" entry))
        (schema (gethash "inputSchema" entry)))
    (make-tool
     :name name
     :description (or (gethash "description" entry) "")
     :parameters (inputschema->parameters schema)
     :metadata (list :mcp/input-schema schema)
     :runner (lambda (tool parameters context &key call-id on-update)
               (declare (ignore call-id on-update))
               (mcp-call-tool client (tool-name tool) parameters context)))))

(defun mcp-list-tools (client)
  "List the server's tools as kli tool objects whose runners call tools/call."
  (let* ((result (call-isolated (mcp-client-process client) "tools/list" nil))
         (tools (and (hash-table-p result) (gethash "tools" result)))
         (entries (when (vectorp tools) (coerce tools 'list))))
    (loop for entry in entries
          when (hash-table-p entry)
            collect (%mcp-tool->kli client entry))))

(defun mcp-call-tool (client name arguments &optional context)
  "Invoke a server tool by NAME with ARGUMENTS (a hash for exact keys, or a
plist whose keys the transport downcases), returning a kli tool-result."
  (let ((params (%string-object "name" name)))
    (when arguments
      (setf (gethash "arguments" params) (jsonify arguments)))
    (content->tool-result
     (call-isolated (mcp-client-process client) "tools/call" params)
     (or context (mcp-client-context client)))))

(defun mcp-list-resources (client)
  "List the server's resources as transport-local resource plists."
  (let* ((result (call-isolated (mcp-client-process client) "resources/list" nil))
         (resources (and (hash-table-p result) (gethash "resources" result)))
         (entries (when (vectorp resources) (coerce resources 'list))))
    (loop for entry in entries
          when (hash-table-p entry)
            collect (mcp-resource->plist entry))))

(defun mcp-read-resource (client uri)
  "Read a server resource by URI, returning the list of content plists."
  (read-contents->plists
   (call-isolated (mcp-client-process client) "resources/read"
                  (%string-object "uri" uri))))

(defun mcp-disconnect (client)
  "Tear down the server subprocess. Idempotent."
  (teardown-isolated (mcp-client-process client)))
