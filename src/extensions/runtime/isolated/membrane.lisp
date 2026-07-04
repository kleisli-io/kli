(in-package #:kli/runtime/isolated)

;;; Pure mappings from MCP wire shapes (parsed jzon hash-tables, string keys)
;;; onto their kli analogues. No transport here, so each function is exercised
;;; in-process over literal hashes.

(defparameter +json-type-map+
  '(("object" . :object) ("string" . :string) ("number" . :number)
    ("integer" . :integer) ("boolean" . :boolean) ("array" . :array)
    ("null" . :null))
  "MCP JSON Schema scalar type names paired with their kli parameter keywords.")

(defun json-type->kli (type)
  "Map a JSON Schema `type` string to a kli parameter keyword, defaulting to
:string for an absent or unrecognized type."
  (or (and (stringp type) (cdr (assoc type +json-type-map+ :test #'string=)))
      :string))

(defun inputschema->parameters (schema)
  "Map an MCP tool inputSchema to the kli opaque parameter s-expr
(:object (NAME TYPE [:optional t]) ...). Top-level properties only, sorted by
name for a stable shape; a property is optional unless listed in `required`.
Nested structure is summarized here and preserved verbatim in tool metadata."
  (let* ((properties (and (hash-table-p schema) (gethash "properties" schema)))
         (required (and (hash-table-p schema) (gethash "required" schema)))
         (required-names (when (vectorp required) (coerce required 'list)))
         (names '()))
    (when (hash-table-p properties)
      (maphash (lambda (name subschema)
                 (declare (ignore subschema))
                 (push name names))
               properties))
    (cons :object
          (loop for name in (sort names #'string<)
                for subschema = (gethash name properties)
                for type = (and (hash-table-p subschema)
                                (gethash "type" subschema))
                for entry = (list (intern name :keyword) (json-type->kli type))
                collect (if (member name required-names :test #'string=)
                            entry
                            (append entry (list :optional t)))))))

(defparameter *mcp-tool-result-character-limit* (* 1024 1024)
  "Largest tool-result an external MCP tool relays into the model's context, in
characters, summed across its blocks. A server's text blocks are otherwise passed
through verbatim, so an unbounded or pathological result could flood the window.")

(defun bound-tool-result-content (pieces)
  "Aggregate char backstop across rendered text PIECES: keep whole pieces within
*mcp-tool-result-character-limit*, front-truncate the one that crosses it, drop
the rest. Always emits at least one piece, so the result is never empty. Returns
(values pieces truncated-p)."
  (let ((limit *mcp-tool-result-character-limit*)
        (spent 0)
        (kept '())
        (truncated nil))
    (dolist (piece pieces)
      (let* ((text (getf piece :text))
             (len (length text)))
        (cond
          (truncated)
          ((null kept)
           (if (> len limit)
               (setf kept (list (make-tool-text-content
                                 (format nil "~A[+~D chars]"
                                         (subseq text 0 limit) (- len limit))))
                     spent limit truncated t)
               (setf kept (list piece) spent len)))
          ((<= (+ spent len) limit)
           (push piece kept) (incf spent len))
          (t
           (let ((room (- limit spent)))
             (push (make-tool-text-content
                    (format nil "~A[+~D chars]"
                            (subseq text 0 room) (- len room)))
                   kept)
             (setf spent limit truncated t))))))
    (values (nreverse kept) truncated)))

(defun rendered-tool-result-text (pieces)
  "Text representation retained behind an MCP result handle."
  (with-output-to-string (stream)
    (loop for piece in pieces
          for first-p = t then nil
          do (unless first-p (terpri stream))
             (write-string (getf piece :text "") stream))))

(defun mcp-result-spill-notice (protocol rendered truncated-p)
  "Return the model-facing spill marker and structured handle details for RENDERED."
  (when truncated-p
    (let ((entry (and protocol
                      (write-string-spill
                       protocol (rendered-tool-result-text rendered)
                       :producer-uuid "mcp-tool-result"))))
      (if entry
          (values (format-spill-marker "MCP tool result"
                                       :shown *mcp-tool-result-character-limit*
                                       :total (spill-entry-bytes entry)
                                       :handle (spill-entry-token entry)
                                       :unit "byte")
                  (list :result-handle (spill-entry-token entry)
                        :result-bytes (spill-entry-bytes entry)))
          (values (format-spill-marker "MCP tool result"
                                       :shown *mcp-tool-result-character-limit*
                                       :degraded protocol)
                  nil)))))

(defun content-block->content (block)
  "Render one MCP content block as a kli text content plist. Text is faithful but
per-line render-capped; image/audio/resource_link/resource collapse to a typed
text placeholder so the surface stays forward compatible; an unknown type yields
NIL to be skipped."
  (when (hash-table-p block)
    (let ((type (gethash "type" block)))
      (cond
        ((equal type "text")
         (make-tool-text-content
          (render-bounded-lines (or (gethash "text" block) ""))))
        ((equal type "image")
         (make-tool-text-content
          (format nil "[image ~A]" (or (gethash "mimeType" block) "?"))))
        ((equal type "audio")
         (make-tool-text-content
          (format nil "[audio ~A]" (or (gethash "mimeType" block) "?"))))
        ((equal type "resource_link")
         (make-tool-text-content
          (format nil "[resource_link ~A]" (or (gethash "uri" block) "?"))))
        ((equal type "resource")
         (let ((resource (gethash "resource" block)))
           (make-tool-text-content
            (format nil "[resource ~A]"
                    (or (and (hash-table-p resource) (gethash "uri" resource))
                        "?")))))
        (t nil)))))

(defun content->tool-result (call-result &optional context)
  "Map an MCP tools/call result to a kli tool-result. Unknown content blocks
are skipped; a true isError sets error-p (a tool failure is isError, not a
JSON-RPC error, so it arrives here as an ordinary result). Oversized rendered
content keeps a bounded window in-band and records a handle to the full rendered
text when an active spill registry is available."
  (let* ((content (and (hash-table-p call-result)
                       (gethash "content" call-result)))
         (blocks (when (vectorp content) (coerce content 'list)))
         (rendered (loop for block in blocks
                         for piece = (content-block->content block)
                         when piece collect piece))
         (error-p (and (hash-table-p call-result)
                       (eq (gethash "isError" call-result) t)))
         (protocol (and context (active-protocol context))))
    (multiple-value-bind (bounded truncated) (bound-tool-result-content rendered)
      (multiple-value-bind (notice handle-details)
          (mcp-result-spill-notice protocol rendered truncated)
        (make-tool-result :content (if notice
                                       (append bounded
                                               (list (make-tool-text-content notice)))
                                       bounded)
                          :error-p error-p
                          :details (when truncated
                                     (append (list :truncated t)
                                             handle-details)))))))

(defun mcp-resource->plist (resource)
  "Map one MCP resources/list entry to a transport-local resource plist."
  (when (hash-table-p resource)
    (list :uri (gethash "uri" resource)
          :name (gethash "name" resource)
          :description (gethash "description" resource)
          :mime-type (gethash "mimeType" resource))))

(defun read-contents->plists (read-result)
  "Map an MCP resources/read result to a list of transport-local content
plists, one per returned content entry."
  (let* ((contents (and (hash-table-p read-result)
                        (gethash "contents" read-result)))
         (entries (when (vectorp contents) (coerce contents 'list))))
    (loop for entry in entries
          when (hash-table-p entry)
            collect (list :uri (gethash "uri" entry)
                          :mime-type (gethash "mimeType" entry)
                          :text (gethash "text" entry)
                          :blob (gethash "blob" entry)))))

(defun notification->event (message)
  "Map a server JSON-RPC notification to the kli event constituents: a generic
:mcp/notification type and a (:method :params) payload. Pure; the client emits."
  (values :mcp/notification
          (list :method (message-method message)
                :params (message-params message))))
