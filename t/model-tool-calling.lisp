(in-package #:kli/tests)

(in-suite all)

(defun json-obj (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun tool-calling-completions-chunk (&key index id name arguments)
  "One chat.completion.chunk carrying a single tool-call fragment."
  (let ((fn (json-obj "arguments" arguments)))
    (when name (setf (gethash "name" fn) name))
    (let ((tc (json-obj "index" index "function" fn)))
      (when id (setf (gethash "id" tc) id))
      (com.inuoe.jzon:stringify
       (json-obj "choices"
                 (vector (json-obj "delta" (json-obj "tool_calls" (vector tc)))))))))

(defun responses-output-item-added-event (&key index call-id name)
  (com.inuoe.jzon:stringify
   (json-obj "type" "response.output_item.added"
             "output_index" index
             "item" (json-obj "type" "function_call"
                              "call_id" call-id
                              "name" name))))

(defun responses-args-delta-event (&key index delta)
  (com.inuoe.jzon:stringify
   (json-obj "type" "response.function_call_arguments.delta"
             "output_index" index
             "delta" delta)))

(defun aggregate-completions-tool-calls (chunks)
  (let ((stream (rt:make-model-stream nil))
        (state (transports:make-completions-state)))
    (flet ((emit (d) (rt:handle-model-delta stream d nil)))
      (dolist (chunk chunks) (transports:map-completions-chunk chunk state #'emit))
      (transports:finish-completions state #'emit))
    (rt::stream-tool-calls stream)))

(defun aggregate-responses-tool-calls (events)
  (let ((stream (rt:make-model-stream nil)))
    (flet ((emit (d) (rt:handle-model-delta stream d nil)))
      (dolist (data events) (transports:map-responses-event nil data #'emit)))
    (rt::stream-tool-calls stream)))

(defparameter +forbidden-file-result-wire-keys+
  '("\"old\":" "\"new\":" "\"preview-old\":" "\"preview-new\":"
    "\"patched\":" "\"repaired\":"))

(defun plain-tool-result-text (result)
  (apply #'concatenate 'string
         (loop for item in (ext:tool-result-content result)
               collect (or (getf item :text) ""))))

(defun tool-result-responses-wire (tool-name result)
  (responses-tool-result-wire
   (list :role :tool-result
         :content (plain-tool-result-text result)
         :tool-call-id "call_1"
         :tool-name tool-name
         :error-p (ext:tool-result-error-p result)
         :details (ext:tool-result-details result))))

(defun assert-compact-file-result-wire (wire path)
  (is (search "<tool-result-details>" wire))
  (is (search path wire))
  (is (search "\"added\":" wire))
  (is (search "\"removed\":" wire))
  (is (search "\"changed-ranges\":" wire))
  (is (search "\"new-sha256\":" wire))
  (assert-no-bulk-file-result-wire wire))

(defun assert-no-bulk-file-result-wire (wire)
  (dolist (key +forbidden-file-result-wire-keys+)
    (is (not (search key wire))
        (format nil "wire output must not contain bulk detail key ~A" key))))

(test tool-parameters-convert-to-json-schema
  "The kli param DSL converts to JSON Schema."
  (let ((schema (transports:tool-parameters->json-schema
                 '(:object (:form :string) (:package :string :optional t)))))
    (is (string= "object" (gethash "type" schema)))
    (is (string= "string"
                 (gethash "type" (gethash "form" (gethash "properties" schema)))))
    (is (equalp #("form") (gethash "required" schema))))
  (let ((schema (transports:tool-parameters->json-schema
                 '(:object (:path :string) (:old :string) (:new :string)
                   (:replace-all :boolean :optional t)))))
    (is (string= "boolean"
                 (gethash "type"
                          (gethash "replace-all" (gethash "properties" schema)))))
    (is (equalp #("path" "old" "new") (gethash "required" schema))))
  (let ((schema (transports:tool-parameters->json-schema nil)))
    (is (string= "object" (gethash "type" schema)))
    (is (zerop (hash-table-count (gethash "properties" schema))))
    (is (equalp #() (gethash "required" schema)))))

(test responses-body-encodes-flat-tools
  "A Responses request carries flat function tools."
  (let* ((tools '((:name "get_weather" :description "Get weather"
                   :parameters (:object (:city :string)))))
         (parsed (com.inuoe.jzon:parse
                  (transports:build-responses-body
                   "gpt-5.3-codex" '((:role :user :content "hi")) :tools tools)))
         (tool-vec (gethash "tools" parsed)))
    (is (= 1 (length tool-vec)))
    (let ((tool (aref tool-vec 0)))
      (is (string= "function" (gethash "type" tool)))
      (is (string= "get_weather" (gethash "name" tool)))
      (is (string= "Get weather" (gethash "description" tool)))
      (is (string= "object" (gethash "type" (gethash "parameters" tool))))
      (is (null (gethash "strict" tool)))))
  (let ((parsed (com.inuoe.jzon:parse
                 (transports:build-responses-body
                  "gpt-5.3-codex" '((:role :user :content "hi"))))))
    (is (null (gethash "tools" parsed)))))

(test completions-body-encodes-nested-tools
  "A Completions request carries nested function tools."
  (let* ((tools '((:name "get_weather" :description "Get weather"
                   :parameters (:object (:city :string)))))
         (parsed (com.inuoe.jzon:parse
                  (transports:build-completions-body
                   "llama-3.3-70b" '((:role :user :content "hi")) :tools tools)))
         (tool-vec (gethash "tools" parsed)))
    (is (= 1 (length tool-vec)))
    (let* ((tool (aref tool-vec 0))
           (fn (gethash "function" tool)))
      (is (string= "function" (gethash "type" tool)))
      (is (string= "get_weather" (gethash "name" fn)))
      (is (string= "Get weather" (gethash "description" fn)))
      (is (string= "object" (gethash "type" (gethash "parameters" fn))))
      (is (null (gethash "strict" fn)))))
  (let ((parsed (com.inuoe.jzon:parse
                 (transports:build-completions-body
                  "llama-3.3-70b" '((:role :user :content "hi"))))))
    (is (null (gethash "tools" parsed)))))

(test tool-enumeration-yields-registered-descriptors
  "Enumeration surfaces every registered tool with its DSL params."
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extensions context
                        tools-filesystem:*read-tool-extension-manifest*
                        tools-filesystem:*write-tool-extension-manifest*
                        tools-filesystem:*edit-tool-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((descriptors (rt::enumerate-request-tools context))
           (names (mapcar (lambda (d) (getf d :name)) descriptors)))
      (is (= 4 (length descriptors)))
      (dolist (name '("read" "write" "edit" "eval"))
        (is (member name names :test #'string=)))
      (let ((eval-desc (find "eval" descriptors
                             :key (lambda (d) (getf d :name)) :test #'string=)))
        (is (equal '(:object (:form :string) (:package :string :optional t)
                     (:timeout :integer :optional t) (:on-error :string :optional t))
                   (getf eval-desc :parameters)))))))

(test completions-decode-aggregates-tool-call
  "Completions decode aggregates split fragments into one tool call."
  (let ((calls (aggregate-completions-tool-calls
                (list (tool-calling-completions-chunk
                       :index 0 :id "call_1" :name "get_weather"
                       :arguments "{\"city\":")
                      (tool-calling-completions-chunk
                       :index 0 :arguments "\"NYC\"}")))))
    (is (= 1 (length calls)))
    (let ((call (first calls)))
      (is (equal "call_1" (getf call :id)))
      (is (equal "get_weather" (getf call :name)))
      (is (equal "{\"city\":\"NYC\"}" (getf call :arguments-json)))
      (is (string= "NYC"
                   (gethash "city"
                            (rt:parse-tool-call-arguments
                             (getf call :arguments-json))))))))

(test parse-tool-call-arguments-reports-invalid-json
  "Invalid arguments JSON still yields an empty object but surfaces the parse
failure as a second value. Blank or missing JSON stays a silent empty object,
so only present-but-unparseable arguments report a cause."
  (multiple-value-bind (arguments parse-error)
      (rt:parse-tool-call-arguments "{\"city\": tru")
    (is (hash-table-p arguments))
    (is (zerop (hash-table-count arguments)))
    (is (stringp parse-error)))
  (multiple-value-bind (arguments parse-error)
      (rt:parse-tool-call-arguments "   ")
    (is (hash-table-p arguments))
    (is (null parse-error)))
  (multiple-value-bind (arguments parse-error)
      (rt:parse-tool-call-arguments nil)
    (is (hash-table-p arguments))
    (is (null parse-error))))

(test responses-decode-aggregates-tool-call
  "Responses decode recovers tool-call identity on output_item.added."
  (let ((calls (aggregate-responses-tool-calls
                (list (responses-output-item-added-event
                       :index 0 :call-id "call_abc" :name "get_weather")
                      (responses-args-delta-event :index 0 :delta "{\"city\":")
                      (responses-args-delta-event :index 0 :delta "\"NYC\"}")))))
    (is (= 1 (length calls)))
    (let ((call (first calls)))
      (is (equal "call_abc" (getf call :id)))
      (is (equal "get_weather" (getf call :name)))
      (is (equal "{\"city\":\"NYC\"}" (getf call :arguments-json))))))

(test convert-agent-message-surfaces-assistant-tool-calls
  "convert-agent-message surfaces assistant tool calls from message metadata."
  (let* ((tool-calls '((:id "call_1" :name "read" :arguments-json "{}")))
         (message (sess:make-assistant-message
                   "" :metadata (list :tool-calls tool-calls)))
         (converted (rt::convert-agent-message message)))
    (is (eq :assistant (getf converted :role)))
    (is (equal tool-calls (getf converted :tool-calls)))))

(test convert-agent-message-surfaces-tool-result-details-first-class
  "A tool result's structured details surface as a first-class field on the
converted plist, leaving the text content untouched for each transport to encode."
  (let ((with-details
          (rt::convert-agent-message
           (sess:make-tool-result-message
            "ran" :tool-call-id "call_1" :tool-name "eval"
            :metadata (list :details '(:package "KLI-USER" :exit-code 0)))))
        (without
          (rt::convert-agent-message
           (sess:make-tool-result-message
            "ran" :tool-call-id "call_2" :tool-name "eval"
            :metadata (list :details nil)))))
    (is (equal '(:package "KLI-USER" :exit-code 0) (getf with-details :details))
        "details surface verbatim, not folded into content")
    (is (string= "ran" (getf with-details :content))
        "content stays the plain tool text")
    (is (null (getf without :details))
        "a detail-less result carries no details")))

(defun anthropic-tool-result-wire (m)
  (let* ((msgs (transports:convert-anthropic-messages (list m)))
         (blocks (gethash "content" (aref msgs 0))))
    (gethash "content" (first blocks))))

(defun responses-tool-result-wire (m)
  (gethash "output" (aref (transports:convert-responses-input (list m)) 0)))

(defun completions-tool-result-wire (m)
  (gethash "content" (aref (transports:convert-completions-messages (list m)) 0)))

(test transports-encode-tool-result-details-onto-wire-content
  "Each transport appends the structured details as a labeled JSON block on the
tool-result's string content, so the model perceives the structured outcome.
A detail-less result keeps its plain text."
  (let ((m '(:role :tool-result :content "ran" :tool-call-id "call_1" :error-p nil
             :details (:package "KLI-USER" :exit-code 0)))
        (plain '(:role :tool-result :content "ran" :tool-call-id "call_2"
                 :error-p nil :details nil)))
    (dolist (probe (list (cons "anthropic" #'anthropic-tool-result-wire)
                         (cons "responses" #'responses-tool-result-wire)
                         (cons "completions" #'completions-tool-result-wire)))
      (let ((wire (funcall (cdr probe) m))
            (label (car probe)))
        (is (search "<tool-result-details>" wire)
            (format nil "~A appends the details block" label))
        (is (search "\"exit-code\":0" wire)
            (format nil "~A carries the structured value" label))
        (is (string= "ran" (funcall (cdr probe) plain))
            (format nil "~A keeps a detail-less result's plain text" label))))))

(test file-mutation-tool-result-wire-rejects-forbidden-bulk-detail-keys
  "Built-in file mutation tools fail closed if public details regress to whole
file bodies."
  (let ((message '(:role :tool-result :content "Edited /tmp/x"
                   :tool-call-id "call_1"
                   :tool-name "edit"
                   :error-p nil
                   :details (:files ((:path "/tmp/x" :old "a" :new "b"))))))
    (signals error
      (responses-tool-result-wire message))))

(test responses-converts-assistant-tool-use
  "Assistant tool_use round-trips on Responses."
  (let* ((messages '((:role :user :content "weather?")
                     (:role :assistant :content ""
                      :tool-calls ((:id "call_1" :name "get_weather"
                                    :arguments-json "{\"city\":\"NYC\"}")))
                     (:role :tool-result :tool-call-id "call_1"
                      :content "sunny")))
         (items (transports:convert-responses-input messages)))
    (is (= 3 (length items)))
    (is (string= "message" (gethash "type" (aref items 0))))
    (let ((call (aref items 1)))
      (is (string= "function_call" (gethash "type" call)))
      (is (string= "call_1" (gethash "call_id" call)))
      (is (string= "get_weather" (gethash "name" call)))
      (is (string= "{\"city\":\"NYC\"}" (gethash "arguments" call))))
    (let ((output (aref items 2)))
      (is (string= "function_call_output" (gethash "type" output)))
      (is (string= "call_1" (gethash "call_id" output)))
      (is (string= "sunny" (gethash "output" output))))))

(test completions-converts-assistant-tool-use
  "Assistant tool_use round-trips on Completions."
  (let* ((messages '((:role :user :content "weather?")
                     (:role :assistant :content ""
                      :tool-calls ((:id "call_1" :name "get_weather"
                                    :arguments-json "{\"city\":\"NYC\"}")))
                     (:role :tool-result :tool-call-id "call_1"
                      :content "sunny")))
         (msgs (transports:convert-completions-messages messages)))
    (is (= 3 (length msgs)))
    (is (string= "user" (gethash "role" (aref msgs 0))))
    (let* ((assistant (aref msgs 1))
           (tcs (gethash "tool_calls" assistant)))
      (is (string= "assistant" (gethash "role" assistant)))
      (is (= 1 (length tcs)))
      (let* ((tc (aref tcs 0))
             (fn (gethash "function" tc)))
        (is (string= "call_1" (gethash "id" tc)))
        (is (string= "function" (gethash "type" tc)))
        (is (string= "get_weather" (gethash "name" fn)))
        (is (string= "{\"city\":\"NYC\"}" (gethash "arguments" fn)))))
    (let ((tool (aref msgs 2)))
      (is (string= "tool" (gethash "role" tool)))
      (is (string= "call_1" (gethash "tool_call_id" tool)))
      (is (string= "sunny" (gethash "content" tool))))))

(test (agent-loop-tool-round-trip-carries-assistant-and-result :fixture interactive-authority)
  "Full round-trip -- a tool call executes, then the next request would carry assistant tool_use plus the tool result. The first assistant message carries the tool calls, and the loop runs a second turn so the next request would replay both."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context "round-trip-provider" "round-trip-model"
                       :metadata '(:fake-deltas ("calling tool")
                                   :fake-tool-call
                                   (:id :call-echo
                                    :name :agent-echo-swap
                                    :arguments (:partial-json
                                                "{\"message\":\"tool ok\"}")))))
           (reply-selection (agent-loop-register-model
                             context "round-trip-reply-provider" "round-trip-reply-model"
                             :metadata '(:fake-deltas ("all done"))))
           (agent (make-agent-loop-session-agent context selection)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context)
        (is (agents:agent-idle-p agent))
        (is (equal '(:user :assistant :tool-result :assistant)
                   (agent-session-message-roles agent)))
        (is (equal "tool ok"
                   (third (agent-session-message-contents agent))))
        (let* ((converted (rt::convert-messages (agent-session-messages agent)))
               (assistant (find :assistant converted
                                :key (lambda (m) (getf m :role))))
               (tool-result (find :tool-result converted
                                  :key (lambda (m) (getf m :role)))))
          (is (not (null (getf assistant :tool-calls))))
          (is (not (null tool-result))))))))
