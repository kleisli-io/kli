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

(defun %json-array-values (value)
  (cond ((vectorp value) (loop for item across value collect item))
        ((listp value) value)
        (t nil)))

(defun %responses-text-parts-text (parts)
  (let ((texts
          (loop for part in (%json-array-values parts)
                for text = (and (hash-table-p part) (gethash "text" part))
                when (and (stringp text) (plusp (length text)))
                  collect text)))
    (when texts
      (format nil "~{~A~^~%~%~}" texts))))

(defun %responses-reasoning-item-text (item)
  (when (hash-table-p item)
    (let ((content (%responses-text-parts-text (gethash "content" item))))
      (if content
          (values content :raw)
          (let ((summary (%responses-text-parts-text (gethash "summary" item))))
            (when summary
              (values summary :summary)))))))

(defun %responses-reasoning-item-signature (item)
  (when (hash-table-p item)
    (ignore-errors (com.inuoe.jzon:stringify item))))

(defun %responses-final-reasoning-delta (item idx)
  (when (and (hash-table-p item)
             (equal (gethash "type" item) "reasoning"))
    (multiple-value-bind (text source) (%responses-reasoning-item-text item)
      (when text
        (make-thinking-delta
         text
         :content-index idx
         :replacement-p t
         :source source
         :signature (%responses-reasoning-item-signature item))))))

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
      ((equal type "response.reasoning_text.delta")
       (funcall emit (make-thinking-delta (gethash "delta" obj)
                                          :content-index idx
                                          :source :raw)))
      ((equal type "response.reasoning_summary_text.delta")
       (funcall emit (make-thinking-delta (gethash "delta" obj)
                                          :content-index idx
                                          :source :summary)))
      ((equal type "response.reasoning_summary_part.done")
       (funcall emit (make-thinking-delta (format nil "~%~%")
                                          :content-index idx
                                          :source :summary)))
      ((member type '("response.output_text.delta"
                      "response.refusal.delta") :test #'equal)
       (funcall emit (make-assistant-delta (gethash "delta" obj) :content-index idx)))
      ((equal type "response.function_call_arguments.delta")
       (funcall emit (make-tool-call-delta nil
                                           (list :partial-json (gethash "delta" obj))
                                           :content-index idx)))
      ((equal type "response.output_item.done")
       (let ((item (gethash "item" obj)))
         (let ((reasoning (%responses-final-reasoning-delta item idx)))
           (when reasoning (funcall emit reasoning)))
         (funcall emit (make-block-end-delta (%item-kind item)
                                             :content-index idx))))
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
  (let ((tc (%normalize-tool-call-for-wire tc)))
    (%obj "type" "function_call"
          "call_id" (getf tc :id)
          "name" (getf tc :name)
          "arguments" (%tool-call-arguments-string tc))))

(defun %responses-assistant-output-items (content tool-calls)
  (let ((items '()))
    (unless (%blankp content)
      (push (%obj "type" "message" "role" "assistant"
                  "content" (list (%obj "type" "output_text"
                                        "text" content
                                        "annotations" #())))
            items))
    (dolist (tc tool-calls)
      (push (%responses-function-call-item tc) items))
    (nreverse items)))

(defun convert-responses-input (messages &key developer-role-p)
  "Converted kli message plists to Responses `input` vector. A kli message
`:content` is a string. The system prompt is not a message and travels in the
top-level `instructions` field. Harness operator content takes the developer
role when DEVELOPER-ROLE-P, else a <harness-context> user wrap; reference content
lowers to an untrusted_text fence."
  (let* ((messages (%assert-provider-message-sequence messages))
         (has-reference (%has-reference-p messages)))
    (coerce
     (loop for m in messages
           for content = (%provider-text-content (getf m :content))
           append (case (getf m :role)
                    ((:user :summary)
                     (list (%obj "type" "message" "role" "user"
                                 "content" (list (%obj "type" "input_text"
                                                       "text" content)))))
                    (:assistant
                     (%responses-assistant-output-items
                      content (getf m :tool-calls)))
                    (:tool-result (list (%obj "type" "function_call_output"
                                              "call_id" (%tool-result-call-id-for-wire m)
                                              "output" (%tool-result-content m))))
                    (:harness-context
                     (let ((trust (getf m :trust))
                           (text content))
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

(defparameter +reasoning-summary+
  '(:auto "auto" :concise "concise" :detailed "detailed" :none "none"))

(defun %responses-reasoning-summary-value (reasoning-summary)
  (or (getf +reasoning-summary+ reasoning-summary)
      (and reasoning-summary (%wire-option-value reasoning-summary))
      "auto"))

(defun build-responses-body-object (model-id messages
                                    &key (instructions "You are a helpful assistant.")
                                      reasoning-effort reasoning-summary
                                      tools session-id text-verbosity
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
        (setf (gethash "reasoning" body)
              (%obj "effort" effort
                    "summary" (%responses-reasoning-summary-value
                               reasoning-summary)))))
    body))

(defun build-responses-body (model-id messages
                             &key (instructions "You are a helpful assistant.")
                               reasoning-effort reasoning-summary
                               tools session-id text-verbosity
                               service-tier prompt-cache-retention
                               developer-role-p)
  (com.inuoe.jzon:stringify
   (build-responses-body-object model-id messages
                                :instructions instructions
                                :reasoning-effort reasoning-effort
                                :reasoning-summary reasoning-summary
                                :tools tools
                                :session-id session-id
                                :text-verbosity text-verbosity
                                :service-tier service-tier
                                :prompt-cache-retention prompt-cache-retention
                                :developer-role-p developer-role-p)))

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

(defparameter +codex-websocket-beta+ "responses_websockets=2026-02-06")

(defvar *codex-websocket-stream* nil
  "Test seam: (url body headers request) -> JSON-line character stream.
NIL routes through websocket-driver-client.")

(defstruct codex-websocket-continuation
  last-request-body
  last-response-id
  last-response-items)

(defstruct codex-websocket-session
  socket
  continuation
  busy
  opened-at
  last-used-at
  (requests 0)
  (full-requests 0)
  (delta-requests 0)
  (connections-created 0)
  (connections-reused 0))

(defstruct codex-websocket-acquisition
  state
  socket
  reused
  cached
  busy-fallback
  retired)

(defstruct responses-output-collector
  response-id
  text-fragments
  tool-deltas)

(defvar *codex-websocket-sessions* (make-hash-table :test #'equal))
(defvar *codex-websocket-sessions-lock*
  (sb-thread:make-mutex :name "codex-websocket-sessions"))

(defparameter *codex-websocket-idle-ttl-seconds* 300)
(defparameter *codex-websocket-max-age-seconds* 3300)

(defun %now-seconds ()
  (get-universal-time))

(defun %copy-json-object-except (object keys)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (unless (member key keys :test #'string=)
                 (setf (gethash key copy) value)))
             object)
    copy))

(defun %request-body-without-input (body)
  (%copy-json-object-except body '("input" "previous_response_id")))

(defun %request-bodies-match-except-input-p (current previous)
  (equalp (%request-body-without-input current)
          (%request-body-without-input previous)))

(defun %json-input-vector (body)
  (let ((input (and (hash-table-p body) (gethash "input" body))))
    (cond ((vectorp input) input)
          ((listp input) (coerce input 'vector))
          (t #()))))

(defun %json-input-count (body)
  (length (%json-input-vector body)))

(defun %append-json-vectors (&rest vectors)
  (coerce (loop for vector in vectors
                append (coerce vector 'list))
          'vector))

(defun %json-prefix-equal-p (vector prefix)
  (and (<= (length prefix) (length vector))
       (loop for idx below (length prefix)
             always (equalp (aref vector idx) (aref prefix idx)))))

(defun %cached-websocket-input-delta (body continuation)
  (when (%request-bodies-match-except-input-p
         body (codex-websocket-continuation-last-request-body continuation))
    (let* ((current (%json-input-vector body))
           (baseline (%append-json-vectors
                      (%json-input-vector
                       (codex-websocket-continuation-last-request-body continuation))
                      (codex-websocket-continuation-last-response-items continuation))))
      (when (%json-prefix-equal-p current baseline)
        (subseq current (length baseline))))))

(defun %copy-json-object (object)
  (%copy-json-object-except object '()))

(defun %websocket-request-body (state full-body &optional (cached-context-p t))
  (unless cached-context-p
    (when state
      (setf (codex-websocket-session-continuation state) nil))
    (return-from %websocket-request-body
      (values (%copy-json-object full-body) :full)))
  (let ((continuation (and state
                           (codex-websocket-session-continuation state))))
    (cond
      ((null continuation)
       (values (%copy-json-object full-body) :full))
      (t
       (let ((delta (%cached-websocket-input-delta full-body continuation)))
         (cond
           ((and delta
                 (codex-websocket-continuation-last-response-id continuation))
            (let ((body (%copy-json-object full-body)))
              (setf (gethash "previous_response_id" body)
                    (codex-websocket-continuation-last-response-id continuation)
                    (gethash "input" body) delta)
              (values body :delta)))
           (t
            (setf (codex-websocket-session-continuation state) nil)
            (values (%copy-json-object full-body) :full))))))))

(defun %websocket-wire-body (body)
  (let ((wire (%copy-json-object body)))
    (setf (gethash "type" wire) "response.create")
    (com.inuoe.jzon:stringify wire)))

(defun %record-codex-websocket-request (state request-kind)
  (when state
    (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
      (incf (codex-websocket-session-requests state))
      (ecase request-kind
        (:full (incf (codex-websocket-session-full-requests state)))
        (:delta (incf (codex-websocket-session-delta-requests state))))
      (list :session-requests (codex-websocket-session-requests state)
            :session-full-requests (codex-websocket-session-full-requests state)
            :session-delta-requests (codex-websocket-session-delta-requests state)))))

(defun %record-codex-websocket-connection (state reused)
  (when state
    (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
      (if reused
          (incf (codex-websocket-session-connections-reused state))
          (incf (codex-websocket-session-connections-created state)))
      (list :session-connections-created
            (codex-websocket-session-connections-created state)
            :session-connections-reused
            (codex-websocket-session-connections-reused state)))))

(defun %first-present (values)
  (find-if #'identity values))

(defun %collector-note-delta (collector delta)
  (typecase delta
    (assistant-delta
     (push (assistant-delta-text delta)
           (responses-output-collector-text-fragments collector)))
    (tool-call-delta
     (push delta (responses-output-collector-tool-deltas collector))))
  delta)

(defun %collector-text (collector)
  (apply #'concatenate 'string
         (reverse (responses-output-collector-text-fragments collector))))

(defun %collector-tool-arguments (deltas)
  (with-output-to-string (out)
    (dolist (delta deltas)
      (let ((fragment (getf (tool-call-delta-arguments delta) :partial-json)))
        (when (stringp fragment)
          (write-string fragment out))))))

(defun %collector-tool-calls (collector)
  (let ((order '())
        (groups (make-hash-table :test #'eql)))
    (dolist (delta (reverse (responses-output-collector-tool-deltas collector)))
      (let ((idx (model-delta-content-index delta)))
        (unless (nth-value 1 (gethash idx groups))
          (push idx order))
        (push delta (gethash idx groups))))
    (loop for idx in (nreverse order)
          for deltas = (nreverse (gethash idx groups))
          collect (list :id (%first-present
                             (mapcar #'tool-call-delta-call-id deltas))
                        :name (%first-present
                               (mapcar #'tool-call-delta-name deltas))
                        :arguments-json (%collector-tool-arguments deltas)))))

(defun %collector-response-items (collector)
  (coerce (%responses-assistant-output-items
           (%collector-text collector)
           (%collector-tool-calls collector))
          'vector))

(defun %note-completed-response-id (collector data-string)
  (let* ((obj (parse-sse-payload data-string))
         (response (and (hash-table-p obj) (gethash "response" obj)))
         (response-id (and (hash-table-p response) (gethash "id" response))))
    (when response-id
      (setf (responses-output-collector-response-id collector) response-id))))

(defun %codex-websocket-url (responses-url)
  (let* ((uri (puri:parse-uri responses-url))
         (scheme (ecase (puri:uri-scheme uri)
                   (:https "wss")
                   (:http "ws")))
         (port (or (puri:uri-port uri)
                   (if (eq (puri:uri-scheme uri) :https) 443 80)))
         (path (or (puri:uri-path uri) "/"))
         (query (puri:uri-query uri)))
    (format nil "~A://~A:~D~A~@[?~A~]"
            scheme (puri:uri-host uri) port path query)))

(defun %filtered-websocket-extra-headers (headers)
  (remove-if (lambda (header)
               (member (car header) '("accept" "content-type" "openai-beta")
                       :test #'string-equal))
             headers))

(defun build-codex-websocket-headers (token account-id extra-headers
                                      &key session-id user-agent
                                        (account-id-header "chatgpt-account-id"))
  (append (list (cons "authorization" (format nil "Bearer ~A" token))
                (cons "openai-beta" +codex-websocket-beta+))
          (when account-id (list (cons account-id-header account-id)))
          (when user-agent (list (cons "user-agent" user-agent)))
          (when session-id (list (cons "session_id" session-id)
                                 (cons "x-client-request-id" session-id)))
          (%filtered-websocket-extra-headers extra-headers)))

(defun %websocket-message-string (message)
  (typecase message
    (string message)
    ((vector (unsigned-byte 8))
     (flexi-streams:octets-to-string message :external-format :utf-8))
    (t (princ-to-string message))))

(defun %completion-websocket-event-p (type)
  (member type '("response.completed" "response.done" "response.incomplete")
          :test #'string=))

(defun %websocket-error-event-p (type)
  (member type '("error" "response.failed") :test #'string=))

(defun %process-codex-websocket-event (request data-string collector emit on-start)
  (let* ((obj (parse-sse-payload data-string))
         (type (and (hash-table-p obj) (gethash "type" obj))))
    (unless (%websocket-error-event-p type)
      (funcall on-start))
    (when (%completion-websocket-event-p type)
      (%note-completed-response-id collector data-string))
    (%note-provider-event request (or type "message") data-string)
    (map-responses-event type data-string
                         (lambda (delta)
                           (%collector-note-delta collector delta)
                           (funcall emit delta)))
    type))

(defun %stream-codex-websocket-lines (request stream collector emit on-start)
  (unwind-protect
       (loop for line = (read-line stream nil nil)
             while line
             unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
               do (%process-codex-websocket-event request line collector emit on-start))
    (ignore-errors (close stream))))

(defun %get-codex-websocket-session (session-id)
  (when session-id
    (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
      (or (gethash session-id *codex-websocket-sessions*)
          (setf (gethash session-id *codex-websocket-sessions*)
                (make-codex-websocket-session))))))

(defun %mark-codex-websocket-opened (state)
  (when state
    (let ((now (%now-seconds)))
      (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
        (setf (codex-websocket-session-opened-at state) now
              (codex-websocket-session-last-used-at state) now)))))

(defun %mark-codex-websocket-used (state)
  (when state
    (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
      (setf (codex-websocket-session-last-used-at state) (%now-seconds)))))

(defun %codex-websocket-session-expired-p (state)
  (when state
    (let ((now (%now-seconds))
          (opened-at (codex-websocket-session-opened-at state))
          (last-used-at (codex-websocket-session-last-used-at state)))
      (or (and last-used-at
               (> (- now last-used-at)
                  *codex-websocket-idle-ttl-seconds*))
          (and opened-at
               (> (- now opened-at)
                  *codex-websocket-max-age-seconds*))))))

(defun %websocket-open-p (socket)
  (and socket
       (ignore-errors (eq (websocket-driver:ready-state socket) :open))))

(defun %codex-websocket-stream (socket)
  (ignore-errors (websocket-driver:socket socket)))

(defun %shutdown-codex-websocket (socket)
  (let ((stream (%codex-websocket-stream socket)))
    (when stream
      (shutdown-request-stream stream)
      t)))

(defun %discard-codex-websocket-session (state)
  (let ((socket (and state (codex-websocket-session-socket state))))
    (when socket
      (%shutdown-codex-websocket socket)))
  (when state
    (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
      (setf (codex-websocket-session-socket state) nil
            (codex-websocket-session-continuation state) nil
            (codex-websocket-session-opened-at state) nil
            (codex-websocket-session-last-used-at state) nil))))

(defun %close-codex-websocket-session (state)
  (%discard-codex-websocket-session state))

(defun %make-codex-websocket (url headers)
  (let ((socket (websocket-driver:make-client url :additional-headers headers)))
    (websocket-driver:start-connection socket)
    socket))

(defun %claim-codex-websocket-session (state)
  (if state
      (let ((claimed nil))
        (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
          (unless (codex-websocket-session-busy state)
            (setf (codex-websocket-session-busy state) t
                  claimed t)))
        (make-codex-websocket-acquisition
         :state state
         :cached claimed
         :busy-fallback (not claimed)))
      (make-codex-websocket-acquisition)))

(defun %open-codex-websocket-acquisition (acquisition url headers)
  (handler-case
      (if (codex-websocket-acquisition-cached acquisition)
          (let ((state (codex-websocket-acquisition-state acquisition)))
            (when (%codex-websocket-session-expired-p state)
              (%discard-codex-websocket-session state))
            (let ((reused (%websocket-open-p
                           (codex-websocket-session-socket state))))
              (unless reused
                (%close-codex-websocket-session state)
                (setf (codex-websocket-session-socket state)
                      (%make-codex-websocket url headers))
                (%mark-codex-websocket-opened state))
              (setf (codex-websocket-acquisition-socket acquisition)
                    (codex-websocket-session-socket state)
                    (codex-websocket-acquisition-reused acquisition)
                    reused)
              acquisition))
          (progn
            (setf (codex-websocket-acquisition-socket acquisition)
                  (%make-codex-websocket url headers)
                  (codex-websocket-acquisition-reused acquisition)
                  nil)
            acquisition))
    (error (condition)
      (when (codex-websocket-acquisition-cached acquisition)
        (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
          (setf (codex-websocket-session-busy
                 (codex-websocket-acquisition-state acquisition))
                nil)))
      (error condition))))

(defun %acquire-codex-websocket (state url headers)
  (%open-codex-websocket-acquisition
   (%claim-codex-websocket-session state)
   url headers))

(defun %retire-codex-websocket-acquisition (acquisition)
  (let ((socket (and acquisition
                     (codex-websocket-acquisition-socket acquisition))))
    (when acquisition
      (setf (codex-websocket-acquisition-retired acquisition) t))
    (when socket
      (%shutdown-codex-websocket socket))
    (when (and acquisition
               (codex-websocket-acquisition-cached acquisition))
      (let ((state (codex-websocket-acquisition-state acquisition)))
        (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
          (when (eq (codex-websocket-session-socket state) socket)
            (setf (codex-websocket-session-socket state) nil
                  (codex-websocket-session-continuation state) nil
                  (codex-websocket-session-opened-at state) nil
                  (codex-websocket-session-last-used-at state) nil)))))))

(defun %release-codex-websocket (acquisition keep)
  (when acquisition
    (if (codex-websocket-acquisition-cached acquisition)
        (let ((state (codex-websocket-acquisition-state acquisition)))
          (unless (or keep
                      (codex-websocket-acquisition-retired acquisition))
            (%close-codex-websocket-session state)
            (setf (codex-websocket-session-continuation state) nil))
          (when (and keep
                     (not (codex-websocket-acquisition-retired acquisition)))
            (%mark-codex-websocket-used state))
          (sb-thread:with-mutex (*codex-websocket-sessions-lock*)
            (setf (codex-websocket-session-busy state) nil)))
        (let ((socket (codex-websocket-acquisition-socket acquisition)))
          (when (and socket
                     (not (codex-websocket-acquisition-retired acquisition)))
            (%shutdown-codex-websocket socket))))))

(defun %codex-websocket-effective-cached-context-p (acquisition cached-context-p)
  (and cached-context-p
       (codex-websocket-acquisition-cached acquisition)))

(defun %codex-websocket-body-state (state acquisition cached-context-p)
  (and state
       (or (not cached-context-p)
           (codex-websocket-acquisition-cached acquisition))
       state))

(defun %parse-websocket-event-string (message)
  (let ((text (%websocket-message-string message)))
    (values text (parse-sse-payload text))))

(defun %stream-real-codex-websocket (request url body headers acquisition collector emit on-start)
  (let* ((opened nil)
         (socket nil)
         (queue '())
         (done nil)
         (failure nil)
         (close-requested nil)
         (saw-completion nil)
         (lock (sb-thread:make-mutex :name "codex-websocket-stream"))
         (ready (sb-thread:make-semaphore :name "codex-websocket-stream" :count 0))
         (on-message nil)
         (on-error nil)
         (on-close nil)
         (keep nil))
    (labels ((wake () (sb-thread:signal-semaphore ready))
             (request-close ()
               (sb-thread:with-mutex (lock)
                 (setf close-requested t))
               (wake))
             (close-requested-p ()
               (sb-thread:with-mutex (lock)
                 close-requested))
             (abort-if-close-requested ()
               (when (close-requested-p)
                 (%retire-codex-websocket-acquisition opened)
                 (error "Codex WebSocket request aborted")))
             (push-message (message)
               (handler-case
                   (multiple-value-bind (text obj)
                       (%parse-websocket-event-string message)
                     (let ((type (and (hash-table-p obj) (gethash "type" obj))))
                       (sb-thread:with-mutex (lock)
                         (when (%completion-websocket-event-p type)
                           (setf saw-completion t
                                 done t))
                         (setf queue (nconc queue (list text))))
                       (wake)))
                 (error (condition)
                   (sb-thread:with-mutex (lock)
                     (setf failure condition
                           done t))
                   (wake)))))
      (setf on-message #'push-message
            on-error (lambda (condition)
                       (sb-thread:with-mutex (lock)
                         (setf failure condition
                               done t))
                       (wake))
            on-close (lambda (&rest args)
                       (declare (ignore args))
                       (sb-thread:with-mutex (lock)
                         (unless saw-completion
                           (setf failure (make-condition
                                          'simple-error
                                          :format-control
                                          "Codex WebSocket closed before response.completed")))
                         (setf done t))
                       (wake)))
      (unwind-protect
           (progn
             (setf (model-request-stream-closer request) #'request-close)
             (setf opened (%open-codex-websocket-acquisition acquisition url headers)
                   socket (codex-websocket-acquisition-socket opened))
             (let* ((reused (codex-websocket-acquisition-reused opened))
                    (state (and (codex-websocket-acquisition-cached opened)
                                (codex-websocket-acquisition-state opened)))
                    (connection-stats (%record-codex-websocket-connection state reused)))
               (note-model-stream-timing
                (model-request-stream request)
                :websocket-connection
                :detail (list* :reused (and reused t)
                               :created (not reused)
                               connection-stats)))
             (abort-if-close-requested)
             (event-emitter:on :message socket on-message)
             (event-emitter:on :error socket on-error)
             (event-emitter:on :close socket on-close)
             (abort-if-close-requested)
             (websocket-driver:send-text socket body)
             (loop
               (let ((next nil)
                     (failed nil)
                     (close-now nil)
                     (finished nil))
                 (sb-thread:with-mutex (lock)
                   (when queue
                     (setf next (pop queue)))
                   (setf failed failure
                         close-now close-requested
                         finished (and done (null queue))))
                 (cond
                   (next
                    (%process-codex-websocket-event request next collector emit on-start))
                   (close-now
                    (%retire-codex-websocket-acquisition opened)
                    (error "Codex WebSocket request aborted"))
                   (failed (error failed))
                   (finished (return))
                   (t (sb-thread:wait-on-semaphore ready)))))
             (setf keep t))
        (ignore-errors (event-emitter:remove-listener socket :message on-message))
        (ignore-errors (event-emitter:remove-listener socket :error on-error))
        (ignore-errors (event-emitter:remove-listener socket :close on-close))
        (setf (model-request-stream-closer request) nil)
        (when opened
          (%release-codex-websocket opened keep))))))

(defun %stream-codex-websocket (request url body headers acquisition collector emit on-start)
  (if *codex-websocket-stream*
      (let ((stream nil)
            (keep nil))
        (unwind-protect
             (progn
               (setf stream
                     (funcall *codex-websocket-stream*
                              url body headers request))
               (setf (model-request-stream-closer request)
                     (lambda () (shutdown-request-stream stream)))
               (%stream-codex-websocket-lines request stream collector emit on-start)
               (setf keep t))
          (setf (model-request-stream-closer request) nil)
          (%release-codex-websocket acquisition keep)))
      (%stream-real-codex-websocket request url body headers acquisition collector emit on-start)))

(defun %prepare-codex-websocket-attempt (state cached-context-p body-object
                                         &key force-full)
  (let* ((acquisition (%claim-codex-websocket-session state))
         (body-state (%codex-websocket-body-state
                      state acquisition cached-context-p))
         (effective-cached-context-p
           (and (not force-full)
                (%codex-websocket-effective-cached-context-p
                 acquisition cached-context-p))))
    (multiple-value-bind (request-body request-kind)
        (%websocket-request-body body-state body-object
                                 effective-cached-context-p)
      (values request-body
              request-kind
              acquisition
              effective-cached-context-p
              (%websocket-wire-body request-body)))))

(defun %remember-codex-websocket-continuation (state full-body collector)
  (when (and state (responses-output-collector-response-id collector))
    (setf (codex-websocket-session-continuation state)
          (make-codex-websocket-continuation
           :last-request-body full-body
           :last-response-id (responses-output-collector-response-id collector)
           :last-response-items (%collector-response-items collector)))))

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

(defun %stream-responses-sse (request url body headers emit)
  (multiple-value-bind (stream status)
      (%responses-request request url body headers)
    (unwind-protect
         (progn
           (%note-http-response request status)
           (unless (and (integerp status) (<= 200 status 299))
             (error 'openai-api-error
                    :status status
                    :body (drain-capped-body stream)))
           (stream-sse-events stream
                              (lambda (ev data)
                                (%note-provider-event request ev data)
                                (map-responses-event ev data emit))))
      (setf (model-request-stream-closer request) nil)
      (ignore-errors (close stream)))))

(defun %note-responses-payload (request api body url messages
                                &key include reasoning-effort reasoning-summary
                                  service-tier
                                  prompt-cache-retention session-id
                                  text-verbosity transport-mode
                                  cached-context request-kind input-items
                                  full-input-items delta-input-items
                                  websocket-stats)
  (%note-request-payload request api body
                         :url url
                         :message-count (length messages)
                         :include include
                         :reasoning-effort reasoning-effort
                         :reasoning-summary (and (getf +reasoning-effort+
                                                        reasoning-effort)
                                                 (%responses-reasoning-summary-value
                                                  reasoning-summary))
                         :service-tier service-tier
                         :prompt-cache-retention prompt-cache-retention
                         :session-id-present (not (null session-id))
                         :text-verbosity text-verbosity
                         :transport-mode transport-mode
                         :cached-context cached-context
                         :request-kind request-kind
                         :input-items input-items
                         :full-input-items full-input-items
                         :delta-input-items delta-input-items
                         :websocket-stats websocket-stats))

(defun %json-error-code (object)
  (when (hash-table-p object)
    (let ((error (gethash "error" object)))
      (or (gethash "code" object)
          (and (hash-table-p error)
               (or (gethash "code" error)
                   (gethash "type" error)))
          (gethash "type" object)))))

(defun %condition-error-body (condition)
  (cond
    ((typep condition 'openai-api-error)
     (openai-api-error-body condition))
    ((typep condition 'model-network-error)
     (princ-to-string (model-network-error-cause condition)))
    (t (princ-to-string condition))))

(defun %condition-error-code (condition)
  (let ((body (%condition-error-body condition)))
    (cond
      ((hash-table-p body) (%json-error-code body))
      ((stringp body)
       (or (ignore-errors (%json-error-code (parse-sse-payload body)))
           (find-if (lambda (code) (%contains-ci-p code body))
                    '("previous_response_not_found"
                      "websocket_connection_limit_reached"))))
      (t nil))))

(defun %websocket-error-code-p (condition code)
  (equal code (%condition-error-code condition)))

(defun %previous-response-not-found-p (condition)
  (%websocket-error-code-p condition "previous_response_not_found"))

(defun %websocket-connection-limit-reached-p (condition)
  (%websocket-error-code-p condition "websocket_connection_limit_reached"))

(defun %fallbackable-websocket-error-p (condition)
  (not (eq (ignore-errors (kli/ext:condition-category condition)) :provider)))

(defun %websocket-transport-mode-p (mode)
  (not (null (member mode '(:auto :websocket :websocket-cached) :test #'eq))))

(defun %cached-websocket-transport-mode-p (mode)
  (not (null (member mode '(:auto :websocket-cached) :test #'eq))))

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
           (reasoning-effort (model-selection-option-value selection "reasoning-effort"))
           (reasoning-summary (model-selection-option-value selection "reasoning-summary"))
           (text-verbosity (or (model-selection-option-value selection "text-verbosity")
                               (%transport-profile-value provider-profile :text-verbosity)))
           (service-tier (model-selection-option-value selection "service-tier"))
           (prompt-cache-retention
             (model-selection-option-value selection "prompt-cache-retention"))
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
           (messages (model-request-model-messages request))
           (body-object (build-responses-body-object
                         (model-request-model-id request)
                         messages
                         :instructions instructions
                         :reasoning-effort reasoning-effort
                         :reasoning-summary reasoning-summary
                         :tools (model-request-tool-schemas request)
                         :session-id session-id
                         :text-verbosity text-verbosity
                         :service-tier service-tier
                         :prompt-cache-retention prompt-cache-retention
                         :developer-role-p developer-role-p))
           (body (com.inuoe.jzon:stringify body-object))
           (sse-headers (build-responses-headers token account-id extra
                                                  :session-id session-id
                                                  :session-header session-header
                                                  :account-id-header account-id-header
                                                  :user-agent user-agent))
           (websocket-capable-p
             (%transport-profile-value provider-profile
                                       :websocket-continuation))
           (transport-mode
             (model-selection-option-value selection "transport"
                                           (if websocket-capable-p :auto :sse)))
           (websocket-p (and websocket-capable-p
                             (%websocket-transport-mode-p transport-mode)))
           (cached-context-p (and websocket-p
                                  (%cached-websocket-transport-mode-p
                                   transport-mode)))
           (state (and websocket-p
                       (%get-codex-websocket-session session-id))))
      (if websocket-p
          (let* ((ws-url (%codex-websocket-url url))
                 (headers (build-codex-websocket-headers
                           token account-id extra
                           :session-id (or session-id
                                           (princ-to-string (object-id request)))
                           :account-id-header account-id-header
                           :user-agent user-agent)))
            (labels ((discard-idle-state ()
                       (when (and state
                                  (not (codex-websocket-session-busy state)))
                         (%discard-codex-websocket-session state)))
                     (run-attempt (&key force-full)
                       (let ((collector (make-responses-output-collector))
                             (started nil))
                         (multiple-value-bind (request-body request-kind
                                               acquisition
                                               effective-cached-context-p
                                               wire-body)
                             (%prepare-codex-websocket-attempt
                              state cached-context-p body-object
                              :force-full force-full)
                           (let* ((body-state (%codex-websocket-body-state
                                               state acquisition
                                               cached-context-p))
                                  (websocket-stats
                                    (%record-codex-websocket-request
                                     body-state request-kind))
                                  (input-items (%json-input-count request-body))
                                  (full-input-items
                                    (%json-input-count body-object))
                                  (delta-input-items
                                    (and (eq request-kind :delta)
                                         input-items)))
                             (%note-responses-payload
                              request :openai-codex-websocket wire-body ws-url messages
                              :include '("reasoning.encrypted_content")
                              :reasoning-effort reasoning-effort
                              :reasoning-summary reasoning-summary
                              :service-tier service-tier
                              :prompt-cache-retention prompt-cache-retention
                              :session-id session-id
                              :text-verbosity text-verbosity
                              :transport-mode transport-mode
                              :cached-context effective-cached-context-p
                              :request-kind request-kind
                              :input-items input-items
                              :full-input-items full-input-items
                              :delta-input-items delta-input-items
                              :websocket-stats websocket-stats)
                             (handler-case
                                 (progn
                                   (%stream-codex-websocket
                                    request ws-url wire-body headers acquisition
                                    collector emit
                                    (lambda () (setf started t)))
                                   (when (and cached-context-p
                                              (codex-websocket-acquisition-cached
                                               acquisition))
                                     (%remember-codex-websocket-continuation
                                      state body-object collector))
                                   (list :ok t :started started))
                               (error (condition)
                                 (when (and state
                                            (codex-websocket-acquisition-cached
                                             acquisition))
                                   (setf (codex-websocket-session-continuation
                                          state)
                                         nil))
                                 (list :ok nil
                                       :started started
                                       :condition condition
                                       :request-kind request-kind)))))))
                     (fallback-or-error (result)
                       (let ((condition (getf result :condition)))
                         (discard-idle-state)
                         (when (or (getf result :started)
                                   (not (%fallbackable-websocket-error-p
                                         condition)))
                           (error condition))
                         (%stream-responses-sse
                          request url body sse-headers emit))))
              (let ((result (run-attempt)))
                (cond
                  ((getf result :ok) nil)
                  ((getf result :started) (error (getf result :condition)))
                  ((%previous-response-not-found-p (getf result :condition))
                   (when state
                     (setf (codex-websocket-session-continuation state) nil))
                   (let ((retry (run-attempt :force-full t)))
                     (if (getf retry :ok)
                         nil
                         (fallback-or-error retry))))
                  ((%websocket-connection-limit-reached-p
                    (getf result :condition))
                   (discard-idle-state)
                   (let ((retry (run-attempt :force-full t)))
                     (if (getf retry :ok)
                         nil
                         (fallback-or-error retry))))
                  (t (fallback-or-error result))))))
          (progn
            (%note-responses-payload
             request :openai-responses body url messages
             :include '("reasoning.encrypted_content")
             :reasoning-effort reasoning-effort
             :reasoning-summary reasoning-summary
             :service-tier service-tier
             :prompt-cache-retention prompt-cache-retention
             :session-id session-id
             :text-verbosity text-verbosity
             :transport-mode transport-mode
             :cached-context nil
             :request-kind nil
             :input-items (%json-input-count body-object)
             :full-input-items (%json-input-count body-object))
            (%stream-responses-sse request url body sse-headers emit))))))
