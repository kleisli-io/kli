(in-package #:kli/model/transports)

(define-condition openai-api-error (error)
  ((status :initarg :status :initform nil :reader openai-api-error-status)
   (body   :initarg :body   :initform nil :reader openai-api-error-body))
  (:documentation "A provider failure from either OpenAI-shaped transport
(Responses or Chat Completions), in-stream or HTTP. Compatible providers
speaking the same wire shape signal it too.")
  (:report (lambda (c s)
             (format s "OpenAI API error~@[ (HTTP ~A)~]~@[: ~A~]"
                     (openai-api-error-status c)
                     (provider-error-display (openai-api-error-body c))))))

(defmethod kli/ext:condition-category ((condition openai-api-error))
  :provider)

(defmethod kli/ext:condition-http-status ((condition openai-api-error))
  (openai-api-error-status condition))

(defparameter *openai-prompt-cache-key-max-length* 64)

(defparameter +terminal-openai-usage-limit-markers+
  '("usage_limit_reached"
    "usage_not_included"
    "insufficient_quota"
    "out of budget"
    "quota exceeded"
    "monthly usage limit reached"
    "gousagelimiterror"
    "freeusagelimiterror"
    "workspace_member_usage_limit_reached"
    "workspace_owner_credits_depleted"
    "usage limit has been reached"))

(defun %contains-ci-p (needle haystack)
  (and (stringp needle)
       (stringp haystack)
       (search needle haystack :test #'char-equal)))

(defun %clamp-openai-prompt-cache-key (key)
  (if (and (stringp key)
           (> (length key) *openai-prompt-cache-key-max-length*))
      (subseq key 0 *openai-prompt-cache-key-max-length*)
      key))

(defun terminal-openai-usage-limit-error-p (condition)
  (and (typep condition 'openai-api-error)
       (eql 429 (openai-api-error-status condition))
       (let ((body (openai-api-error-body condition)))
         (and (stringp body)
              (not (null (some (lambda (marker) (%contains-ci-p marker body))
                               +terminal-openai-usage-limit-markers+)))))))

(defun %item-kind (item)
  (let ((ty (and (hash-table-p item) (gethash "type" item))))
    (cond ((equal ty "reasoning") :thinking)
          ((equal ty "message") :text)
          ((equal ty "function_call") :toolcall)
          (t :text))))

(defun %map-stop-reason (status)
  (cond ((equal status "completed") :stop)
        ((equal status "incomplete") :length)
        ((member status '("failed" "cancelled") :test #'equal) :error)
        (t :stop)))

(defun %responses-truncated-p (resp)
  "True when the final response object stopped at the max-output-token limit.
An incomplete status with an explicit non-token reason (content filter) is
not truncation."
  (when (hash-table-p resp)
    (let* ((details (gethash "incomplete_details" resp))
           (reason (and (hash-table-p details) (gethash "reason" details))))
      (and (eq :length (%map-stop-reason (gethash "status" resp)))
           (or (null reason) (equal reason "max_output_tokens"))))))

(defun %usage-detail-tokens (usage details-key token-key)
  "Nested USAGE[DETAILS-KEY][TOKEN-KEY], or NIL when either object is absent."
  (let ((details (and (hash-table-p usage) (gethash details-key usage))))
    (and (hash-table-p details) (gethash token-key details))))

(defun %usage-plist (usage)
  "Normalize a Responses usage object to a token plist. cached_tokens is cache
READ, a subset of input and not additive. total_tokens is reported directly
and falls back to input+output. OpenAI has no cache-write figure, so that key
is left for downstream to default."
  (when (hash-table-p usage)
    (let ((input (gethash "input_tokens" usage))
          (output (gethash "output_tokens" usage))
          (cached (%usage-detail-tokens usage "input_tokens_details" "cached_tokens")))
      (nconc (list :input-tokens input
                   :output-tokens output
                   :total-tokens (or (gethash "total_tokens" usage)
                                     (+ (or input 0) (or output 0))))
             (when cached (list :cache-read-tokens cached))))))

(defun %function-call-identity-delta (item idx)
  "For a function_call output item, a tool-call-delta carrying its call_id+name."
  (when (and (hash-table-p item)
             (equal (gethash "type" item) "function_call"))
    (make-tool-call-delta (gethash "name" item)
                          (list :partial-json (or (gethash "arguments" item) ""))
                          :call-id (gethash "call_id" item)
                          :content-index idx)))

(defun map-responses-event (event-name data-string emit)
  "Parse DATA-STRING and EMIT the corresponding runtime delta(s), if any.
Keys off the JSON \"type\" field -- the SSE event line is redundant, so
EVENT-NAME is ignored. Maps Responses stream events to block-aware deltas."
  (declare (ignore event-name))
  (let* ((obj (parse-sse-payload data-string))
         (type (and (hash-table-p obj) (gethash "type" obj)))
         (idx  (and (hash-table-p obj) (gethash "output_index" obj))))
    (cond
      ((equal type "response.created") nil)
      ((equal type "response.output_item.added")
       (let ((item (gethash "item" obj)))
         (funcall emit (make-block-start-delta (%item-kind item) :content-index idx))
         (let ((identity (%function-call-identity-delta item idx)))
           (when identity (funcall emit identity)))))
      ((member type '("response.reasoning_text.delta"
                      "response.reasoning_summary_text.delta") :test #'equal)
       (funcall emit (make-thinking-delta (gethash "delta" obj) :content-index idx)))
      ((member type '("response.output_text.delta"
                      "response.refusal.delta") :test #'equal)
       (funcall emit (make-assistant-delta (gethash "delta" obj) :content-index idx)))
      ((equal type "response.function_call_arguments.delta")
       (funcall emit (make-tool-call-delta nil
                                           (list :partial-json (gethash "delta" obj))
                                           :content-index idx)))
      ((equal type "response.output_item.done")
       (funcall emit (make-block-end-delta (%item-kind (gethash "item" obj))
                                           :content-index idx)))
      ((member type '("response.completed" "response.incomplete") :test #'equal)
       (let* ((resp (gethash "response" obj))
              (usage (and (hash-table-p resp) (gethash "usage" resp))))
         (when (%responses-truncated-p resp)
           (funcall emit (make-stop-reason-delta :length)))
         (when (%usage-plist usage)
           (funcall emit (make-usage-delta (%usage-plist usage))))))
      ((member type '("error" "response.failed") :test #'equal)
       (error 'openai-api-error :body data-string))
      (t nil))))

(defun %responses-tool-spec (descriptor)
  "Generic tool descriptor -> Responses flat function tool object."
  (%obj "type" "function"
        "name" (getf descriptor :name)
        "description" (or (getf descriptor :description) "")
        "parameters" (tool-parameters->json-schema (getf descriptor :parameters))
        "strict" nil))

(defun %responses-function-call-item (tc)
  (%obj "type" "function_call"
        "call_id" (getf tc :id)
        "name" (getf tc :name)
        "arguments" (%tool-call-arguments-string tc)))

(defun convert-responses-input (messages &key developer-role-p)
  "Converted kli message plists to Responses `input` vector. A kli message
`:content` is a string. The system prompt is not a message and travels in the
top-level `instructions` field. Harness operator content takes the developer
role when DEVELOPER-ROLE-P, else a <harness-context> user wrap; reference content
lowers to an untrusted_text fence."
  (let ((has-reference (%has-reference-p messages)))
    (coerce
     (loop for m in messages
           for content = (getf m :content)
           append (case (getf m :role)
                    (:user (list (%obj "type" "message" "role" "user"
                                       "content" (list (%obj "type" "input_text"
                                                             "text" (princ-to-string content))))))
                    (:assistant
                     (let ((items '()))
                       (unless (%blankp (princ-to-string content))
                         (push (%obj "type" "message" "role" "assistant"
                                     "content" (list (%obj "type" "output_text"
                                                           "text" (princ-to-string content)
                                                           "annotations" #())))
                               items))
                       (dolist (tc (getf m :tool-calls))
                         (push (%responses-function-call-item tc) items))
                       (nreverse items)))
                    (:tool-result (list (%obj "type" "function_call_output"
                                              "call_id" (getf m :tool-call-id)
                                              "output" (%tool-result-content m))))
                    (:harness-context
                     (let ((trust (getf m :trust))
                           (text (princ-to-string content)))
                       (if (eq trust :operator)
                           (let ((op (%operator-content text +untrusted-anchor+ has-reference)))
                             (list (%obj "type" "message"
                                         "role" (if developer-role-p "developer" "user")
                                         "content" (list (%obj "type" "input_text"
                                                               "text" (if developer-role-p op
                                                                          (%wrap-tag
                                                                           (%escape-fence-delimiters
                                                                            op +harness-context-open+ +harness-context-close+)
                                                                           +harness-context-open+
                                                                           +harness-context-close+)))))))
                           (list (%obj "type" "message" "role" "user"
                                       "content" (list (%obj "type" "input_text"
                                                             "text" (%untrusted-text-block text))))))))
                    (t nil)))
     'vector)))

(defparameter +reasoning-effort+
  '(:minimal "minimal" :low "low" :medium "medium" :high "high" :xhigh "xhigh"))

(defun build-responses-body (model-id messages
                             &key (instructions "You are a helpful assistant.")
                               reasoning-effort tools session-id text-verbosity
                               service-tier prompt-cache-retention
                               developer-role-p)
  (let ((body (%obj "model" model-id
                    "store" nil "stream" t
                    "instructions" instructions
                    "input" (convert-responses-input messages :developer-role-p developer-role-p)
                    "include" (list "reasoning.encrypted_content")
                    "tool_choice" "auto"
                    "parallel_tool_calls" t)))
    (when tools
      (setf (gethash "tools" body) (%tools-vector tools #'%responses-tool-spec)))
    (when session-id
      (setf (gethash "prompt_cache_key" body)
            (%clamp-openai-prompt-cache-key session-id)))
    (when text-verbosity
      (setf (gethash "text" body) (%obj "verbosity" (%wire-option-value text-verbosity))))
    (when service-tier
      (setf (gethash "service_tier" body) (%wire-option-value service-tier)))
    (let ((retention (getf +prompt-cache-retention+ prompt-cache-retention)))
      (when retention
        (setf (gethash "prompt_cache_retention" body) retention)))
    (let ((effort (getf +reasoning-effort+ reasoning-effort)))
      (when effort
        (setf (gethash "reasoning" body) (%obj "effort" effort "summary" "auto"))))
    (com.inuoe.jzon:stringify body)))

(defun responses-url (base-url &optional (path "/responses"))
  (concatenate 'string
               (string-right-trim "/" (or base-url "https://api.openai.com/v1"))
               path))

(defun build-responses-headers (token account-id &rest arguments)
  (let* ((extra-headers (and arguments
                             (not (keywordp (first arguments)))
                             (pop arguments)))
         (session-id (getf arguments :session-id))
         (user-agent (getf arguments :user-agent))
         (session-header (or (getf arguments :session-header) "session-id"))
         (account-id-header (or (getf arguments :account-id-header)
                                "chatgpt-account-id")))
    (append (list (cons "authorization" (format nil "Bearer ~A" token))
                  (cons "accept" "text/event-stream"))
            (when account-id (list (cons account-id-header account-id)))
            (when user-agent (list (cons "user-agent" user-agent)))
            (when session-id (list (cons session-header session-id)
                                   (cons "x-client-request-id" session-id)))
            extra-headers)))

(defun %responses-endpoint (cfg &optional profile)
  "Resolve the Responses URL from CFG plus structured transport PROFILE."
  (let ((path (%transport-profile-value profile :url-path)))
    (responses-url (and cfg (provider-config-base-url cfg)) (or path "/responses"))))

(defun %responses-developer-role-p (provider-profile model-profile)
  (or (%transport-profile-value provider-profile :developer-role)
      (%transport-profile-value model-profile :developer-role)))

(defvar *responses-http* nil
  "Test seam: (url body headers) -> (values char-stream status). NIL routes
through a real streaming drakma POST.")

(defun %responses-request (request url body headers)
  (if *responses-http*
      (multiple-value-bind (stream status) (funcall *responses-http* url body headers)
        (setf (model-request-stream-closer request)
              (lambda () (shutdown-request-stream stream)))
        (values stream status))
      (multiple-value-bind (raw status)
          (open-cancellable-stream request url body headers)
        (values (flexi-streams:make-flexi-stream raw :external-format :utf-8)
                status))))

(defun %resolve-token-and-account (provider context)
  (let* ((auth (require-capability-provider (active-protocol context)
                                            :auth :contract :auth/v1))
         (store (find-live-object (context-registry context) :credential-store))
         (cred-pid (model-provider-credential-provider-id provider))
         (resolved (provider-call auth :resolve-credential store cred-pid context))
         (ref (find-if (lambda (r) (typep r 'oauth-credential-reference))
                       (provider-call auth :find-credential-references store cred-pid))))
    (values (resolved-credential-value resolved)
            (and ref (oauth-credential-reference-account-id ref)))))

(defun %codex-user-agent ()
  (format nil "kli (~A ~A; ~A)"
          (software-type) (software-version) (machine-type)))

(defun openai-responses-adapter (provider request context &key emit)
  "Stream PROVIDER's Responses-API reply for REQUEST, emitting deltas to EMIT.
Codex compatibility facts (session identity, user agent, text verbosity) come from
the provider transport profile."
  (multiple-value-bind (token account-id) (%resolve-token-and-account provider context)
    (let* ((cfg (model-provider-config provider))
           (provider-profile (%provider-transport-profile provider))
           (model-meta (%request-model-metadata request context))
           (model-profile (%metadata-transport-profile model-meta))
           (url (%responses-endpoint cfg provider-profile))
           (extra (and cfg (provider-config-headers cfg)))
           (instructions (or (model-request-instructions request)
                             (getf (model-provider-metadata provider) :instructions)
                             "You are a helpful assistant."))
           (selection (model-request-selection request))
           (session-id (and (%transport-profile-value provider-profile :session-identity)
                            (model-request-session-id request)
                            (princ-to-string (model-request-session-id request))))
           (session-header (%transport-profile-value provider-profile
                                                     :session-header "session-id"))
           (user-agent (and (%transport-profile-value provider-profile :user-agent)
                            (%codex-user-agent)))
           (account-id-header (%transport-profile-value provider-profile
                                                       :account-id-header
                                                       "chatgpt-account-id"))
           (developer-role-p (%responses-developer-role-p provider-profile
                                                          model-profile))
           (body (build-responses-body (model-request-model-id request)
                                       (model-request-model-messages request)
                                       :instructions instructions
                                       :reasoning-effort (model-selection-option-value selection "reasoning-effort")
                                       :tools (model-request-tool-schemas request)
                                       :session-id session-id
                                       :text-verbosity (or (model-selection-option-value selection "text-verbosity")
                                                           (%transport-profile-value provider-profile :text-verbosity))
                                       :service-tier (model-selection-option-value selection "service-tier")
                                       :prompt-cache-retention (model-selection-option-value selection "prompt-cache-retention")
                                       :developer-role-p developer-role-p)))
      (multiple-value-bind (stream status)
          (%responses-request request url body
                              (build-responses-headers token account-id extra
                                                        :session-id session-id
                                                        :session-header session-header
                                                        :account-id-header account-id-header
                                                        :user-agent user-agent))
        (unwind-protect
             (progn
               (unless (and (integerp status) (<= 200 status 299))
                 (error 'openai-api-error
                        :status status
                        :body (drain-capped-body stream)))
               (stream-sse-events stream
                                  (lambda (ev data) (map-responses-event ev data emit))))
          (setf (model-request-stream-closer request) nil)
          (ignore-errors (close stream)))))))
