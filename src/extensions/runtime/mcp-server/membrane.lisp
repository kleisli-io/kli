;;;; One extension's tool surface mapped onto MCP. A surface exposes exactly the
;;;; tools an extension contributed, under a subject carrying their capabilities
;;;; and the gate atoms of any coordinate-gated tools -- scope and authority
;;;; derived from one provenance projection.

(in-package #:kli/runtime/mcp-server)

(defun obj (&rest kvs)
  "A jzon-ready string-keyed object from alternating key/value arguments."
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on kvs by #'cddr do (setf (gethash k h) v)
        finally (return h)))

(defstruct (surface (:constructor %make-surface))
  "The exposed slice of a protocol: its tools, their names, the subject the
serve loop runs under, and the optional MCP prompts and resources served
alongside. PROMPTS is a list of prompt descriptor plists
(:name :description :arguments :expand), where :arguments is a list of
(:name :description :required) plists and :expand a function of the request
arguments hash-table returning the expanded text. RESOURCES is a list of
(:uri :name :description :mime :text) plists. Both default empty -- a surface
built without them serves tools only, exactly as before."
  protocol tools names subject prompts resources)

(defun tools-capabilities (tools)
  "The union of :capabilities declared across TOOLS."
  (let ((caps '()))
    (dolist (tool tools (nreverse caps))
      (dolist (c (getf (ext:tool-metadata tool) :capabilities))
        (pushnew c caps)))))

(defun tools-coordinate-atoms (tools)
  "The gate atoms of TOOLS that carry a :coordinate. A coordinate-gated tool
demands its atom through its coordinate, not its :capabilities, so a surface
subject built from capabilities alone cannot cover it."
  (let ((atoms '()))
    (dolist (tool tools (nreverse atoms))
      (let ((atom (getf (getf (ext:tool-metadata tool) :coordinate) :atom)))
        (when atom (pushnew atom atoms))))))

(defun extension-surface (protocol exposure-ids &key prompts resources)
  "Expose exactly EXPOSURE-IDS' tools, under a subject carrying their capabilities
and the gate atoms of any coordinate-gated tools, so the serve loop holds
authority covering every tool it exposes. PROMPTS and RESOURCES, when supplied,
are the MCP prompt and resource descriptors (see the SURFACE struct) served
alongside the tools."
  (let* ((tools (ext:list-tools-from protocol exposure-ids))
         (names (mapcar #'ext:tool-name tools)))
    (%make-surface :protocol protocol
                   :tools tools
                   :names names
                   :subject (ext:make-subject
                             :capabilities (append (tools-capabilities tools)
                                                   (tools-coordinate-atoms tools)))
                   :prompts prompts
                   :resources resources)))

(defun tool->descriptor (tool)
  (obj "name" (ext:tool-name tool)
       "description" (ext:tool-description tool)
       "inputSchema" (tx:tool-parameters->json-schema (ext:tool-parameters tool))))

(defun tools-list-result (surface)
  (obj "tools" (map 'vector #'tool->descriptor (surface-tools surface))))

(defun content->block (c)
  (obj "type" (string-downcase (string (getf c :type)))
       "text" (princ-to-string (getf c :text))))

(defun error-result (datum)
  "Tool-level failure -- unexposed tool, capability denial, runner error -- as an
MCP isError result, never a JSON-RPC error."
  (obj "content" (vector (obj "type" "text" "text" (princ-to-string datum)))
       "isError" t))

(defun tools-call-result (surface context params)
  "Route tools/call only for a tool in SURFACE; a name outside it never reaches
invoke-tool. The MCP arguments hash-table has string keys, which tool-parameter
reads directly. Capability gating happens inside invoke-tool against *call-subject*."
  (handler-case
      (let ((name (gethash "name" params)))
        (if (member name (surface-names surface) :test #'equal)
            (let* ((arguments (or (gethash "arguments" params)
                                  (make-hash-table :test #'equal)))
                   (res (ext:invoke-tool (surface-protocol surface)
                                         name arguments context)))
              (obj "content" (map 'vector #'content->block
                                  (ext:tool-result-content res))
                   "isError" (if (ext:tool-result-error-p res) t nil)))
            (error-result (format nil "tool not exposed: ~A" name))))
    (error (condition) (error-result condition))))

;;; Prompts. Each prompt descriptor renders to one MCP Prompt; prompts/get
;;; expands it to a single user-role message. The corpus is a single page, so
;;; the list results omit nextCursor entirely (never a null).

(defun prompt-argument->descriptor (argument)
  (let ((d (obj "name" (getf argument :name))))
    (when (getf argument :description)
      (setf (gethash "description" d) (getf argument :description)))
    (when (getf argument :required)
      (setf (gethash "required" d) t))
    d))

(defun prompt->descriptor (prompt)
  (let ((d (obj "name" (getf prompt :name))))
    (when (getf prompt :description)
      (setf (gethash "description" d) (getf prompt :description)))
    (let ((arguments (getf prompt :arguments)))
      (when arguments
        (setf (gethash "arguments" d)
              (map 'vector #'prompt-argument->descriptor arguments))))
    d))

(defun prompts-list-result (surface)
  (obj "prompts" (map 'vector #'prompt->descriptor (surface-prompts surface))))

(defun find-surface-prompt (surface name)
  (find name (surface-prompts surface)
        :key (lambda (p) (getf p :name)) :test #'equal))

(defun prompt-get-result (surface params)
  "PROMPTS/GET dispatch. Returns (values :ok result) for a known prompt, or
(values :err code message) -- -32602 for an unknown name."
  (let* ((name (gethash "name" params))
         (prompt (and name (find-surface-prompt surface name))))
    (if (null prompt)
        (values :err -32602 (format nil "unknown prompt: ~A" name))
        (let* ((arguments (or (gethash "arguments" params)
                              (make-hash-table :test #'equal)))
               (text (funcall (getf prompt :expand) arguments)))
          (values :ok
                  (obj "description" (or (getf prompt :description) "")
                       "messages"
                       (vector (obj "role" "user"
                                    "content" (obj "type" "text"
                                                   "text" text)))))))))

;;; Resources. Each prompt/skill is also a markdown resource read straight from
;;; its prebuilt body.

(defun resource->descriptor (resource)
  (let ((d (obj "uri" (getf resource :uri)
                "name" (getf resource :name))))
    (when (getf resource :description)
      (setf (gethash "description" d) (getf resource :description)))
    (when (getf resource :mime)
      (setf (gethash "mimeType" d) (getf resource :mime)))
    d))

(defun resources-list-result (surface)
  (obj "resources" (map 'vector #'resource->descriptor (surface-resources surface))))

(defun find-surface-resource (surface uri)
  (find uri (surface-resources surface)
        :key (lambda (r) (getf r :uri)) :test #'equal))

(defun resource-read-result (surface params)
  "RESOURCES/READ dispatch. Returns (values :ok result) for a known uri, or
(values :err code message) -- -32002 for an unknown uri."
  (let* ((uri (gethash "uri" params))
         (resource (and uri (find-surface-resource surface uri))))
    (if (null resource)
        (values :err -32002 (format nil "resource not found: ~A" uri))
        (values :ok
                (obj "contents"
                     (vector (obj "uri" (getf resource :uri)
                                  "mimeType" (or (getf resource :mime) "text/markdown")
                                  "text" (getf resource :text))))))))
