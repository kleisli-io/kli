(in-package #:kli/tests)

(in-suite all)

;;; A fixture extension with one gated and one ungated tool, exercised through
;;; the generic membrane -- no real extension, proving the server carries
;;; whatever the protocol contributes.

(defun run-mcp-echo (tool parameters context &key call-id on-update)
  (declare (ignore tool context call-id on-update))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content
                   (format nil "echo:~A" (ext:tool-parameter parameters :text))))))

(ext:defextension mcp-server-test-tools
  (:provides
   (tool mcp_echo
     :label "Echo"
     :description "Echo the text argument."
     :parameters '(:object (:text :string) (:n :integer :optional t))
     :runner #'run-mcp-echo)
   (tool mcp_write
     :label "Write"
     :description "A gated writer."
     :parameters '(:object (:text :string))
     :runner #'run-mcp-echo
     :metadata '(:capabilities (:mcp-test/write)))))

;;; A second fixture from a different extension, present in the protocol but not
;;; in the served surface -- the over-exposure the scoping must prevent.

(defun run-mcp-other (tool parameters context &key call-id on-update)
  (declare (ignore tool parameters context call-id on-update))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "other"))))

(ext:defextension mcp-server-other-tools
  (:provides
   (tool other_tool
     :label "Other"
     :description "A tool from a different extension."
     :parameters '(:object (:x :string))
     :runner #'run-mcp-other
     :metadata '(:capabilities (:mcp-test/other)))))

;;; A coordinate-gated fixture: its gate atom lives in :coordinate, not
;;; :capabilities -- the lifted/re-bridged MCP tool shape on the outbound surface.

(ext:defextension mcp-server-coordinate-tools
  (:provides
   (tool coord_tool
     :label "Coord"
     :description "A coordinate-gated tool."
     :parameters '(:object (:text :string) (:mode :string))
     :runner #'run-mcp-echo
     :metadata '(:coordinate (:atom :mcp-test/coord :constraint :enum :arg :mode)))))

(defun mcp-server-protocol ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *mcp-server-test-tools-extension-manifest*)
    (values protocol context)))

(defun mcp-server-protocol-both ()
  (multiple-value-bind (protocol context) (mcp-server-protocol)
    (install-extension context *mcp-server-other-tools-extension-manifest*)
    (values protocol context)))

(defun mcp-server-protocol-coordinate ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *mcp-server-coordinate-tools-extension-manifest*)
    (values protocol context)))

(defun mcp-server-run-on (surface context requests &key subject)
  "Run REQUESTS through serve-stream over SURFACE and return the parsed response
objects, one per emitted line. Without SUBJECT the serve loop runs under the
surface's own subject -- the production posture."
  (let ((input (format nil "~{~A~%~}" requests)))
    (flet ((serve (out)
             (with-input-from-string (in input)
               (mcp-server:serve-stream surface context in out))))
      (let ((raw (let ((ext:*call-subject* (or subject (mcp-server:surface-subject surface))))
                   (with-output-to-string (out) (serve out)))))
        (loop for line in (uiop:split-string raw :separator '(#\Newline))
              when (plusp (length line))
                collect (com.inuoe.jzon:parse line))))))

(defun mcp-server-run (requests &key subject)
  "Run REQUESTS through a surface exposing the test fixture extension."
  (multiple-value-bind (protocol context) (mcp-server-protocol)
    (mcp-server-run-on (mcp-server:extension-surface protocol '(:mcp-server-test-tools))
                       context requests :subject subject)))

(defun mcp-req (id method &optional params)
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" object) "2.0"
          (gethash "id" object) id
          (gethash "method" object) method)
    (when params (setf (gethash "params" object) params))
    (com.inuoe.jzon:stringify object)))

(test mcp-server-initialize-reports-server-info-and-tools
  (let* ((responses (mcp-server-run (list (mcp-req 1 "initialize"))))
         (result (gethash "result" (first responses))))
    (is (= 1 (length responses)))
    (is (string= "kli" (gethash "name" (gethash "serverInfo" result))))
    (is (hash-table-p (gethash "tools" (gethash "capabilities" result)))
        "the tools capability is advertised")))

(test mcp-server-initialized-notification-is-silent
  (is (null (mcp-server-run (list (mcp-req nil "notifications/initialized"))))
      "a notification produces no response line"))

(test mcp-server-tools-list-maps-schema-with-optionality
  (let* ((responses (mcp-server-run (list (mcp-req 2 "tools/list"))))
         (tools (gethash "tools" (gethash "result" (first responses)))))
    (is (= 2 (length tools)))
    (let* ((echo (find "mcp_echo" tools
                       :key (lambda (d) (gethash "name" d)) :test #'string=))
           (schema (gethash "inputSchema" echo))
           (required (gethash "required" schema)))
      (is (string= "object" (gethash "type" schema)))
      (is (equalp #("text") required)
          "the required argument is listed, the optional one is not")
      (is (hash-table-p (gethash "n" (gethash "properties" schema)))
          "the optional argument is still a declared property"))))

(defun mcp-call-params (name &rest arg-pairs)
  (let ((p (make-hash-table :test #'equal))
        (a (make-hash-table :test #'equal)))
    (loop for (k v) on arg-pairs by #'cddr do (setf (gethash k a) v))
    (setf (gethash "name" p) name
          (gethash "arguments" p) a)
    p))

(test mcp-server-tools-call-round-trips-result
  (let* ((responses (mcp-server-run
                     (list (mcp-req 3 "tools/call"
                                    (mcp-call-params "mcp_echo" "text" "hi")))))
         (result (gethash "result" (first responses)))
         (content (gethash "content" result)))
    (is (not (eq t (gethash "isError" result))) "a successful call is not an error")
    (is (string= "echo:hi" (gethash "text" (aref content 0)))
        "the tool ran and its text round-tripped through the membrane")))

(test mcp-server-capability-gate-fires-as-iserror
  (let ((call (mcp-req 4 "tools/call"
                       (let ((p (make-hash-table :test #'equal))
                             (a (make-hash-table :test #'equal)))
                         (setf (gethash "text" a) "x"
                               (gethash "name" p) "mcp_write"
                               (gethash "arguments" p) a)
                         p))))
    (let* ((denied (mcp-server-run (list call)
                                   :subject (ext:make-subject :capabilities '())))
           (denied-result (gethash "result" (first denied))))
      (is (eq t (gethash "isError" denied-result))
          "a subject lacking the capability is denied as an isError result")
      (is (null (gethash "error" (first denied)))
          "the denial is a tool result, not a JSON-RPC error"))
    (let* ((allowed (mcp-server-run
                     (list call)
                     :subject (ext:make-subject
                               :capabilities '(:mcp-test/write))))
           (allowed-result (gethash "result" (first allowed))))
      (is (eq 'nil (gethash "isError" allowed-result))
          "a capable subject is allowed through the same membrane"))))

(test mcp-server-coordinate-gated-tool-is-served-under-folded-subject
  "A re-served tool that gates on its :coordinate atom -- the lifted-tool shape --
is callable over the membrane: the surface subject folds the coordinate atom in
alongside declared capabilities, so the serve loop holds authority the tool
actually demands, instead of denying it as the capability vocabularies being
disjoint once did."
  (multiple-value-bind (protocol context) (mcp-server-protocol-coordinate)
    (let* ((surface (mcp-server:extension-surface
                     protocol '(:mcp-server-coordinate-tools)))
           (called (mcp-server-run-on
                    surface context
                    (list (mcp-req 1 "tools/call"
                                   (mcp-call-params "coord_tool"
                                                    "text" "hi" "mode" "x")))))
           (result (gethash "result" (first called))))
      (is (member :mcp-test/coord
                  (ext:subject-capabilities (mcp-server:surface-subject surface)))
          "the surface subject carries the exposed tool's coordinate atom")
      (is (null (gethash "error" (first called)))
          "a coordinate-gated call is not a JSON-RPC error")
      (is (not (eq t (gethash "isError" result)))
          "the coordinate-gated tool is served, not capability-denied")
      (is (string= "echo:hi"
                   (gethash "text" (aref (gethash "content" result) 0)))
          "the tool ran and its content returned through the membrane"))))

(test mcp-server-unknown-tool-is-iserror-not-jsonrpc-error
  (let* ((responses (mcp-server-run
                     (list (mcp-req 5 "tools/call"
                                    (let ((p (make-hash-table :test #'equal)))
                                      (setf (gethash "name" p) "nope")
                                      p)))))
         (response (first responses)))
    (is (null (gethash "error" response)) "an unknown tool is not a JSON-RPC error")
    (is (eq t (gethash "isError" (gethash "result" response)))
        "an unknown tool surfaces as an isError result")))

(test mcp-server-unknown-method-is-jsonrpc-error
  (let* ((responses (mcp-server-run (list (mcp-req 6 "frobnicate"))))
         (error-object (gethash "error" (first responses))))
    (is (not (null error-object)) "an unknown method is a JSON-RPC error")
    (is (= -32601 (gethash "code" error-object)) "method-not-found code")))

(test mcp-server-malformed-line-yields-parse-error-and-continues
  (let* ((responses (mcp-server-run (list "{ not json" (mcp-req 7 "tools/list"))))
         (parse-error (gethash "error" (first responses))))
    (is (= 2 (length responses)) "the malformed line replies and the loop continues")
    (is (= -32700 (gethash "code" parse-error)) "parse-error code")
    (is (gethash "result" (second responses))
        "the well-formed request after it still gets served")))

(test mcp-server-surface-scopes-to-named-extension
  (multiple-value-bind (protocol context) (mcp-server-protocol-both)
    (declare (ignore context))
    (let ((surface (mcp-server:extension-surface protocol '(:mcp-server-test-tools))))
      (is (= 3 (length (ext:list-tools protocol)))
          "both extensions' tools are installed in the protocol")
      (is (= 2 (length (mcp-server:surface-tools surface)))
          "the surface exposes only the named extension's tools")
      (is (null (member "other_tool"
                        (mapcar #'ext:tool-name (mcp-server:surface-tools surface))
                        :test #'string=))
          "a tool from another extension is not exposed"))))

(test mcp-server-surface-subject-carries-only-scoped-capabilities
  (multiple-value-bind (protocol context) (mcp-server-protocol-both)
    (declare (ignore context))
    (let ((scoped (mcp-server:extension-surface protocol '(:mcp-server-test-tools)))
          (union (mcp-server:extension-surface
                  protocol '(:mcp-server-test-tools :mcp-server-other-tools))))
      (is (equal '(:mcp-test/write)
                 (ext:subject-capabilities (mcp-server:surface-subject scoped)))
          "the scoped subject carries only the scoped extension's capabilities")
      (is (member :mcp-test/other
                  (ext:subject-capabilities (mcp-server:surface-subject union)))
          "the union surface picks up the other extension's capability"))))

(test mcp-server-unexposed-tool-is-iserror-and-unlisted
  (multiple-value-bind (protocol context) (mcp-server-protocol-both)
    (let* ((surface (mcp-server:extension-surface protocol '(:mcp-server-test-tools)))
           (called (mcp-server-run-on
                    surface context
                    (list (mcp-req 8 "tools/call" (mcp-call-params "other_tool" "x" "y")))))
           (call-response (first called))
           (listed (mcp-server-run-on surface context (list (mcp-req 9 "tools/list"))))
           (tools (gethash "tools" (gethash "result" (first listed)))))
      (is (null (gethash "error" call-response))
          "an unexposed tool is not a JSON-RPC error")
      (is (eq t (gethash "isError" (gethash "result" call-response)))
          "a tool from an unexposed extension surfaces as an isError result")
      (is (= 2 (length tools)) "the unexposed extension's tool is never listed"))))

;;; Prompts and resources. The surface carries prebuilt descriptors -- the same
;;; contract the app layer fills from a bundled corpus -- so these exercise the
;;; membrane and dispatch without the prompts/skills extensions installed.

(defun mcp-test-prompt (name description)
  (list :name name :description description
        :arguments (list (list :name "arguments" :description "the args" :required nil))
        :expand (lambda (arguments)
                  (format nil "EXPANDED:~A"
                          (or (and (hash-table-p arguments)
                                   (gethash "arguments" arguments))
                              "")))))

(defun mcp-test-resource (uri name text)
  (list :uri uri :name name :description "a doc" :mime "text/markdown" :text text))

(defun mcp-server-run-corpus (requests)
  "Run REQUESTS through a surface exposing the test tools plus one prompt and one
resource."
  (multiple-value-bind (protocol context) (mcp-server-protocol)
    (let ((surface (mcp-server:extension-surface
                    protocol '(:mcp-server-test-tools)
                    :prompts (list (mcp-test-prompt "research" "Do research"))
                    :resources (list (mcp-test-resource
                                      "cairn://prompts/research" "research" "BODY")))))
      (mcp-server-run-on surface context requests))))

(defun mcp-init-params (version)
  (let ((p (make-hash-table :test #'equal)))
    (setf (gethash "protocolVersion" p) version)
    p))

(test mcp-server-initialize-advertises-prompts-and-resources
  (let* ((responses (mcp-server-run (list (mcp-req 1 "initialize"))))
         (capabilities (gethash "capabilities" (gethash "result" (first responses)))))
    (is (hash-table-p (gethash "tools" capabilities)) "tools advertised")
    (is (hash-table-p (gethash "prompts" capabilities)) "prompts advertised")
    (is (hash-table-p (gethash "resources" capabilities)) "resources advertised")))

(test mcp-server-initialize-negotiates-protocol-version
  (let ((latest (gethash "protocolVersion"
                         (gethash "result"
                                  (first (mcp-server-run
                                          (list (mcp-req 1 "initialize"))))))))
    (is (string= "2025-11-25" latest)
        "with no requested version the server reports its latest"))
  (let ((echoed (gethash "protocolVersion"
                         (gethash "result"
                                  (first (mcp-server-run
                                          (list (mcp-req 1 "initialize"
                                                         (mcp-init-params "2024-11-05")))))))))
    (is (string= "2024-11-05" echoed)
        "a supported requested version is echoed (down-negotiation)"))
  (let ((replied (gethash "protocolVersion"
                          (gethash "result"
                                   (first (mcp-server-run
                                           (list (mcp-req 1 "initialize"
                                                          (mcp-init-params "1999-01-01")))))))))
    (is (string= "2025-11-25" replied)
        "an unknown requested version falls back to the server's latest")))

(test mcp-server-prompts-list-maps-descriptors-without-cursor
  (let* ((responses (mcp-server-run-corpus (list (mcp-req 2 "prompts/list"))))
         (result (gethash "result" (first responses)))
         (prompts (gethash "prompts" result)))
    (is (= 1 (length prompts)) "the surface's one prompt is listed")
    (is (null (gethash "nextCursor" result)) "a single page omits nextCursor")
    (let* ((prompt (aref prompts 0))
           (arguments (gethash "arguments" prompt)))
      (is (string= "research" (gethash "name" prompt)) "the prompt name maps")
      (is (= 1 (length arguments)) "its one declared argument maps")
      (is (string= "arguments" (gethash "name" (aref arguments 0)))
          "the argument descriptor carries its name"))))

(test mcp-server-prompts-get-returns-one-user-message
  (let* ((responses (mcp-server-run-corpus
                     (list (mcp-req 3 "prompts/get"
                                    (mcp-call-params "research" "arguments" "kernels")))))
         (messages (gethash "messages" (gethash "result" (first responses)))))
    (is (= 1 (length messages)) "a prompt expands to one message")
    (let* ((message (aref messages 0))
           (content (gethash "content" message)))
      (is (string= "user" (gethash "role" message)) "the message is user-role")
      (is (string= "text" (gethash "type" content)) "its content is text")
      (is (string= "EXPANDED:kernels" (gethash "text" content))
          "the prompt expanded with the supplied argument"))))

(test mcp-server-prompts-get-unknown-name-is-invalid-params
  (let* ((responses (mcp-server-run-corpus
                     (list (mcp-req 4 "prompts/get" (mcp-call-params "nope")))))
         (error-object (gethash "error" (first responses))))
    (is (not (null error-object)) "an unknown prompt is a JSON-RPC error")
    (is (= -32602 (gethash "code" error-object)) "invalid-params code")))

(test mcp-server-resources-list-and-read-round-trip
  (let* ((listed (mcp-server-run-corpus (list (mcp-req 5 "resources/list"))))
         (list-result (gethash "result" (first listed)))
         (resources (gethash "resources" list-result)))
    (is (= 1 (length resources)) "the surface's one resource is listed")
    (is (null (gethash "nextCursor" list-result)) "a single page omits nextCursor")
    (is (string= "cairn://prompts/research" (gethash "uri" (aref resources 0)))
        "the resource uri maps"))
  (let* ((read-params (let ((p (make-hash-table :test #'equal)))
                        (setf (gethash "uri" p) "cairn://prompts/research")
                        p))
         (responses (mcp-server-run-corpus (list (mcp-req 6 "resources/read" read-params))))
         (contents (gethash "contents" (gethash "result" (first responses)))))
    (is (= 1 (length contents)) "read returns one content item")
    (is (string= "BODY" (gethash "text" (aref contents 0)))
        "the resource body round-trips")
    (is (string= "text/markdown" (gethash "mimeType" (aref contents 0)))
        "the markdown mime type is reported")))

(test mcp-server-resources-read-unknown-uri-is-not-found
  (let* ((read-params (let ((p (make-hash-table :test #'equal)))
                        (setf (gethash "uri" p) "cairn://prompts/missing")
                        p))
         (responses (mcp-server-run-corpus (list (mcp-req 7 "resources/read" read-params))))
         (error-object (gethash "error" (first responses))))
    (is (not (null error-object)) "an unknown resource uri is a JSON-RPC error")
    (is (= -32002 (gethash "code" error-object)) "resource-not-found code")))
