(in-package #:kli/tests)

(in-suite all)

;;; Literal jzon-shaped values: objects are equal hash-tables with string keys,
;;; arrays are vectors. These build the inputs the membrane sees after parsing.

(defun mcp-obj (&rest keys-and-values)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key object) value))
    object))

(defun mcp-key (name)
  (intern name :keyword))

;;; Membrane: pure mappings over literal hashes, no subprocess.

(test mcp-inputschema-maps-types-and-optionality
  (let* ((schema (mcp-obj "type" "object"
                          "properties" (mcp-obj "path" (mcp-obj "type" "string")
                                                "count" (mcp-obj "type" "integer"))
                          "required" (vector "path")))
         (params (isolated:inputschema->parameters schema)))
    (is (eq :object (first params)))
    (is (equal (list (mcp-key "path") :string)
               (assoc (mcp-key "path") (rest params)))
        "a required property has no :optional marker")
    (is (equal (list (mcp-key "count") :integer :optional t)
               (assoc (mcp-key "count") (rest params)))
        "an absent-from-required property is optional")))

(test mcp-inputschema-tolerates-empty-schema
  (is (equal '(:object) (isolated:inputschema->parameters (mcp-obj))))
  (is (equal '(:object) (isolated:inputschema->parameters nil))))

(test mcp-content-maps-text-and-sets-error
  (let* ((result (mcp-obj "content" (vector (mcp-obj "type" "text" "text" "hello")
                                            (mcp-obj "type" "text" "text" "world")
                                            (mcp-obj "type" "mystery"))
                          "isError" t))
         (tr (isolated:content->tool-result result))
         (content (ext:tool-result-content tr)))
    (is (ext:tool-result-error-p tr) "isError true maps to error-p")
    (is (= 2 (length content)) "the unknown block is skipped, not errored")
    (is (string= "hello" (getf (first content) :text)))
    (is (string= "world" (getf (second content) :text)))))

(test mcp-content-media-becomes-placeholder
  (let* ((result (mcp-obj "content"
                          (vector (mcp-obj "type" "image" "mimeType" "image/png"))))
         (tr (isolated:content->tool-result result))
         (content (ext:tool-result-content tr)))
    (is (not (ext:tool-result-error-p tr)))
    (is (= 1 (length content)))
    (is (search "image/png" (getf (first content) :text))
        "a media block collapses to a typed text placeholder")))

;;;; Context-pollution bounds: a relayed text block is front-truncated per line,
;;;; and the whole result is held under an aggregate character cap across blocks.

(test mcp-content-per-line-front-truncates-a-long-line
  "A text block with one structurally-long line is front-truncated per line, so a
single relayed block cannot flood the context."
  (let* ((text:*render-line-limit* 10)
         (result (mcp-obj "content"
                          (vector (mcp-obj "type" "text"
                                           "text" "0123456789ABCDEFGHIJ"))))
         (tr (isolated:content->tool-result result))
         (content (ext:tool-result-content tr)))
    (is (= 1 (length content)))
    (is (string= "0123456789[+10 chars]" (getf (first content) :text)))))

(test mcp-content-aggregate-backstop-drops-overflow
  "Across blocks, once the aggregate character limit is spent the crossing block is
cut with a [+N chars] marker, the rest are dropped, at least one block survives,
and :truncated rides the details. Without an active protocol the marker degrades
without advertising a handle."
  (let* ((isolated:*mcp-tool-result-character-limit* 10)
         (result (mcp-obj "content"
                          (vector (mcp-obj "type" "text" "text" "01234")
                                  (mcp-obj "type" "text" "text" "56789ABCDE")
                                  (mcp-obj "type" "text" "text" "dropped"))))
         (tr (isolated:content->tool-result result))
         (content (ext:tool-result-content tr)))
    (is (= 3 (length content)) "two blocks plus a marker emitted, the third data block dropped")
    (is (string= "01234" (getf (first content) :text)))
    (is (string= "56789[+5 chars]" (getf (second content) :text))
        "the crossing block is cut at the remaining budget")
    (is (search "MCP tool result truncated" (getf (third content) :text))
        "the truncation is visible in content")
    (is (getf (ext:tool-result-details tr) :truncated)
        ":truncated rides the details")
    (is (null (getf (ext:tool-result-details tr) :result-handle))
        "no handle is reported without a protocol-backed store")))

(test mcp-resource-list-and-read-shapes
  (let ((entry (isolated:mcp-resource->plist
                (mcp-obj "uri" "file:///a" "name" "A" "mimeType" "text/plain"))))
    (is (string= "file:///a" (getf entry :uri)))
    (is (string= "A" (getf entry :name)))
    (is (string= "text/plain" (getf entry :mime-type))))
  (let ((contents (isolated:read-contents->plists
                   (mcp-obj "contents"
                            (vector (mcp-obj "uri" "file:///a"
                                             "mimeType" "text/plain"
                                             "text" "body"))))))
    (is (= 1 (length contents)))
    (is (string= "file:///a" (getf (first contents) :uri)))
    (is (string= "body" (getf (first contents) :text)))))

(test mcp-notification-maps-to-event
  (let ((message (mcp-obj "method" "notifications/message"
                          "params" (mcp-obj "level" "info"))))
    (multiple-value-bind (type payload) (isolated:notification->event message)
      (is (eq :mcp/notification type))
      (is (string= "notifications/message" (getf payload :method)))
      (is (hash-table-p (getf payload :params))))))

;;; Integration against a real child. The fixture is a POSIX shell MCP server
;;; that branches on the request method with canned per-method JSON, using only
;;; shell builtins (the id/method/name extraction mirrors the transport tests).
;;; tools/call branches further on the tool name so one fixture serves both a
;;; success and an isError reply. It emits one unsolicited log notification at
;;; startup.

(defun mcp-fixture-script ()
  (format nil "~{~A~%~}"
          (list
           "printf '{\"jsonrpc\":\"2.0\",\"method\":\"notifications/message\",\"params\":{\"level\":\"info\"}}\\n'"
           "while IFS= read -r line; do"
           "case \"$line\" in"
           "*'\"id\":'*)"
           "rest=${line#*'\"id\":'}"
           "id=${rest%%[!0-9]*}"
           "rest=${line#*'\"method\":\"'}"
           "method=${rest%%'\"'*}"
           "case \"$method\" in"
           "initialize)"
           "printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"protocolVersion\":\"2025-11-25\",\"serverInfo\":{\"name\":\"fixture\",\"version\":\"1\"},\"capabilities\":{\"tools\":{}}}}\\n' \"$id\""
           ";;"
           "tools/list)"
           "printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"tools\":[{\"name\":\"echo\",\"description\":\"d\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}},\"required\":[\"text\"]}}]}}\\n' \"$id\""
           ";;"
           "tools/call)"
           "rest=${line#*'\"name\":\"'}"
           "name=${rest%%'\"'*}"
           "case \"$name\" in"
           "fail)"
           "printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"boom\"}],\"isError\":true}}\\n' \"$id\""
           ";;"
           "*)"
           "printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"called\"}]}}\\n' \"$id\""
           ";;"
           "esac"
           ";;"
           "resources/list)"
           "printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"resources\":[{\"uri\":\"file:///a\",\"name\":\"A\",\"mimeType\":\"text/plain\"}]}}\\n' \"$id\""
           ";;"
           "resources/read)"
           "printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"contents\":[{\"uri\":\"file:///a\",\"mimeType\":\"text/plain\",\"text\":\"body\"}]}}\\n' \"$id\""
           ";;"
           "esac"
           ";;"
           "esac"
           "done")))

(defun call-with-mcp-fixture (thunk &key context)
  (let ((client (isolated:mcp-connect "sh"
                                      :arguments (list "-c" (mcp-fixture-script))
                                      :context context
                                      :timeout 5)))
    (unwind-protect (funcall thunk client)
      (when client (isolated:mcp-disconnect client)))))

(test mcp-connect-handshakes-and-captures-server-info
  (call-with-mcp-fixture
   (lambda (client)
     (is (not (null client)) "connect returns a client")
     (let ((info (isolated:mcp-client-server-info client)))
       (is (hash-table-p info))
       (is (string= "fixture" (gethash "name" info))))
     (is (hash-table-p (isolated:mcp-client-server-capabilities client)))
     (is (string= "2025-11-25" (isolated:mcp-client-protocol-version client))
         "the negotiated protocol version is stored"))))

(test mcp-list-tools-maps-parameters
  (call-with-mcp-fixture
   (lambda (client)
     (let ((tools (isolated:mcp-list-tools client)))
       (is (= 1 (length tools)))
       (let ((tool (first tools)))
         (is (string= "echo" (ext:tool-name tool)))
         (is (equal (list :object (list (mcp-key "text") :string))
                    (ext:tool-parameters tool))
             "inputSchema maps to the kli parameter s-expr")
         (is (hash-table-p (getf (ext:tool-metadata tool) :mcp/input-schema))
             "the raw inputSchema is kept in metadata for fidelity"))))))

(test mcp-call-tool-maps-result-and-error
  (call-with-mcp-fixture
   (lambda (client)
     (let ((ok (isolated:mcp-call-tool client "echo" nil)))
       (is (not (ext:tool-result-error-p ok)))
       (is (string= "called"
                    (getf (first (ext:tool-result-content ok)) :text))))
     (let ((bad (isolated:mcp-call-tool client "fail" nil)))
       (is (ext:tool-result-error-p bad) "an isError reply yields error-p")
       (is (string= "boom"
                    (getf (first (ext:tool-result-content bad)) :text)))))))

(test mcp-resources-list-and-read
  (call-with-mcp-fixture
   (lambda (client)
     (let ((resources (isolated:mcp-list-resources client)))
       (is (= 1 (length resources)))
       (is (string= "file:///a" (getf (first resources) :uri))))
     (let ((contents (isolated:mcp-read-resource client "file:///a")))
       (is (= 1 (length contents)))
       (is (string= "body" (getf (first contents) :text)))))))

(test mcp-disconnect-leaves-no-orphan
  (let ((client (isolated:mcp-connect "sh"
                                      :arguments (list "-c" (mcp-fixture-script))
                                      :timeout 5)))
    (is (isolated:isolated-process-alive-p (isolated:mcp-client-process client))
        "the server child is running")
    (isolated:mcp-disconnect client)
    (is (not (isolated:isolated-process-alive-p
              (isolated:mcp-client-process client)))
        "disconnect reaps the child")))

;;; Server notification surfaces as a kli event. A minimal extension protocol
;;; with the events extension installed plus a recording handler captures the
;;; fixture's startup log, emitted on the transport reader thread.

(defun call-with-mcp-event-recorder (thunk)
  (let* ((context (kli:make-kernel-host))
         (recorded (make-array 0 :adjustable t :fill-pointer 0))
         (lock (sb-thread:make-mutex)))
    (switch-to-extension-protocol context)
    (install-extensions context
                        obj:*standard-object-extension-manifest*
                        event:*events-extension-manifest*)
    (ext:install-contribution
     (kli:active-protocol context)
     (event:make-event-handler-contribution
      :name :mcp-test-recorder
      :event-type :mcp/notification
      :handler (lambda (event ctx)
                 (declare (ignore ctx))
                 (sb-thread:with-mutex (lock)
                   (vector-push-extend event recorded)))
      :source :test)
     context)
    (funcall thunk context recorded)))

(test mcp-server-notification-becomes-kli-event
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (call-with-mcp-fixture
      (lambda (client)
        (declare (ignore client))
        (let ((deadline (+ (get-internal-real-time)
                           (* 5 internal-time-units-per-second))))
          (loop until (or (plusp (length recorded))
                          (>= (get-internal-real-time) deadline))
                do (sleep 0.02)))
        (is (plusp (length recorded)) "the server log surfaced as an event")
        (let ((event (aref recorded 0)))
          (is (eq :mcp/notification (event:event-type event)))
          (is (string= "notifications/message"
                       (getf (event:event-payload event) :method)))))
      :context context))))

(test mcp-content-spills-truncated-result-to-handle
  "With an active protocol, truncated MCP content records the full rendered text
behind a read-result/search-result handle, including blocks dropped from the
inline window."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context spill:*output-spill-extension-manifest*)
           (let* ((isolated:*mcp-tool-result-character-limit* 10)
                  (result (mcp-obj "content"
                                   (vector (mcp-obj "type" "text" "text" "01234")
                                           (mcp-obj "type" "text" "text" "56789ABCDE")
                                           (mcp-obj "type" "text" "text" "dropped-tail"))))
                  (tr (isolated:content->tool-result result context))
                  (text (format nil "~{~A~^~%~}"
                                (mapcar (lambda (piece) (getf piece :text))
                                        (ext:tool-result-content tr))))
                  (details (ext:tool-result-details tr))
                  (handle (getf details :result-handle)))
             (is (stringp handle) "a handle is minted for the retained MCP result")
             (is (search "handle" text) "the marker is in content text")
             (is (getf details :result-bytes) "the details carry the retained byte size")
             (let ((page (spill-result-text
                          (spill::run-read-result-tool
                           nil (list :handle handle :start 0 :limit 10) context))))
               (is (search "dropped-tail" page)
                   "read-result reaches a block dropped from the inline window"))))
      (ignore-errors
        (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                    :validate t :if-does-not-exist :ignore)))))
