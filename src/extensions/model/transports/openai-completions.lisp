(in-package #:kli/model/transports)

(defstruct (completions-state (:conc-name cs-))
  "Block-synthesis state for the chunk-to-delta mapper. Chat Completions has no
explicit block boundaries, so ensure-*-block lazily opens a block on the first
delta of a kind and finish-completions closes every open block in creation
order, then usage. TOOL-INDICES is an alist of stream-index to content-index.
OPEN-BLOCKS is a list of (content-index . kind) in reverse creation order."
  (next-index 0)
  (text-index nil)
  (thinking-index nil)
  (tool-indices nil)
  (open-blocks nil)
  (usage nil))

(defun %open-block (state kind emit)
  (let ((idx (cs-next-index state)))
    (incf (cs-next-index state))
    (push (cons idx kind) (cs-open-blocks state))
    (funcall emit (make-block-start-delta kind :content-index idx))
    idx))

(defun %ensure-text-block (state emit)
  (or (cs-text-index state)
      (setf (cs-text-index state) (%open-block state :text emit))))

(defun %ensure-thinking-block (state emit)
  (or (cs-thinking-index state)
      (setf (cs-thinking-index state) (%open-block state :thinking emit))))

(defun %ensure-tool-block (state stream-index emit)
  (let ((cell (assoc stream-index (cs-tool-indices state))))
    (if cell
        (cdr cell)
        (let ((idx (%open-block state :toolcall emit)))
          (push (cons stream-index idx) (cs-tool-indices state))
          idx))))

(defparameter +completions-reasoning-fields+
  '("reasoning_content" "reasoning" "reasoning_text"))

(defun %first-reasoning (delta)
  (loop for f in +completions-reasoning-fields+
        for v = (gethash f delta)
        when (and (stringp v) (plusp (length v))) return v))

(defun %completions-usage-plist (usage)
  "Normalize a Chat Completions usage object to a token plist.
prompt_tokens_details.cached_tokens is cache READ, a subset of prompt.
total_tokens is reported directly and falls back to prompt+completion."
  (when (hash-table-p usage)
    (let ((input (gethash "prompt_tokens" usage))
          (output (gethash "completion_tokens" usage))
          (cached (%usage-detail-tokens usage "prompt_tokens_details" "cached_tokens")))
      (nconc (list :input-tokens input
                   :output-tokens output
                   :total-tokens (or (gethash "total_tokens" usage)
                                     (+ (or input 0) (or output 0))))
             (when cached (list :cache-read-tokens cached))))))

(defun %handle-completions-tool-call (tc state emit)
  (let* ((sidx (gethash "index" tc))
         (id   (gethash "id" tc))
         (fn   (gethash "function" tc))
         (name (and (hash-table-p fn) (gethash "name" fn)))
         (args (and (hash-table-p fn) (gethash "arguments" fn)))
         (cidx (%ensure-tool-block state sidx emit)))
    (funcall emit (make-tool-call-delta name (list :partial-json (or args ""))
                                        :call-id id :content-index cidx))))

(defun map-completions-chunk (data-string state emit)
  "Parse one chat.completion.chunk and EMIT runtime delta(s), mutating STATE."
  (let ((obj (parse-sse-payload data-string)))
    (when (hash-table-p obj)
      (when (gethash "error" obj)
        (error 'openai-api-error :body data-string))
      (let ((usage (gethash "usage" obj)))
        (when (hash-table-p usage)
          (setf (cs-usage state) (%completions-usage-plist usage))))
      (let* ((choices (gethash "choices" obj))
             (choice (and (vectorp choices) (plusp (length choices)) (aref choices 0)))
             (delta (and (hash-table-p choice) (gethash "delta" choice))))
        (when (and (hash-table-p choice)
                   (equal (gethash "finish_reason" choice) "length"))
          (funcall emit (make-stop-reason-delta :length)))
        (when (hash-table-p delta)
          (let ((content (gethash "content" delta)))
            (when (and (stringp content) (plusp (length content)))
              (funcall emit (make-assistant-delta
                             content :content-index (%ensure-text-block state emit)))))
          (let ((r (%first-reasoning delta)))
            (when r
              (funcall emit (make-thinking-delta
                             r :content-index (%ensure-thinking-block state emit)))))
          (let ((tcs (gethash "tool_calls" delta)))
            (when (vectorp tcs)
              (loop for tc across tcs when (hash-table-p tc)
                    do (%handle-completions-tool-call tc state emit)))))))))

(defun finish-completions (state emit)
  "Close every open block in creation order, then emit usage if known."
  (dolist (entry (reverse (cs-open-blocks state)))
    (funcall emit (make-block-end-delta (cdr entry) :content-index (car entry))))
  (when (cs-usage state)
    (funcall emit (make-usage-delta (cs-usage state)))))

(defun %completions-tool-spec (descriptor)
  "Generic tool descriptor -> Chat Completions nested function tool object."
  (%obj "type" "function"
        "function" (%obj "name" (getf descriptor :name)
                         "description" (or (getf descriptor :description) "")
                         "parameters" (tool-parameters->json-schema
                                       (getf descriptor :parameters))
                         "strict" nil)))

(defun %completions-tool-call-item (tc)
  (let ((tc (%normalize-tool-call-for-wire tc)))
    (%obj "id" (getf tc :id)
          "type" "function"
          "function" (%obj "name" (getf tc :name)
                           "arguments" (%tool-call-arguments-string tc)))))

(defun convert-completions-messages (messages &key instructions developer-role-p)
  "kli message plists to Chat Completions `messages` vector. Chat Completions
carries the system prompt as the first message (role \"system\") and has no
top-level instructions field. Harness operator content takes the developer role
when DEVELOPER-ROLE-P, else a <harness-context> user wrap; reference content
lowers to an untrusted_text fence."
  (let ((messages (%assert-provider-message-sequence messages))
        (out '())
        (has-reference (%has-reference-p messages)))
    (when (and instructions (plusp (length instructions)))
      (push (%obj "role" "system" "content" instructions) out))
    (dolist (m messages)
      (case (getf m :role)
        ((:user :summary)
         (push (%obj "role" "user"
                     "content" (%provider-text-content (getf m :content))) out))
        (:assistant
         (let ((msg (%obj "role" "assistant"
                          "content" (%provider-text-content
                                     (getf m :content))))
               (tool-calls (getf m :tool-calls)))
           (when tool-calls
             (setf (gethash "tool_calls" msg)
                   (coerce (mapcar #'%completions-tool-call-item tool-calls) 'vector)))
           (push msg out)))
        (:tool-result (push (%obj "role" "tool"
                                  "tool_call_id" (%tool-result-call-id-for-wire m)
                                  "content" (%tool-result-content m)) out))
        (:harness-context
         (let ((content (%provider-text-content (getf m :content)))
               (trust (getf m :trust)))
           (if (eq trust :operator)
               (let ((op (%operator-content content +untrusted-anchor+ has-reference)))
                 (if developer-role-p
                     (push (%obj "role" "developer" "content" op) out)
                     (push (%obj "role" "user"
                                 "content" (%wrap-tag
                                            (%escape-fence-delimiters
                                             op +harness-context-open+ +harness-context-close+)
                                            +harness-context-open+ +harness-context-close+)) out)))
               (push (%obj "role" "user" "content" (%untrusted-text-block content)) out))))))
    (coerce (nreverse out) 'vector)))

(defun build-completions-body (model-id messages
                               &key (instructions "You are a helpful assistant.")
                                 reasoning-effort text-verbosity service-tier
                                 prompt-cache-retention
                                 (include-usage t) tools developer-role-p)
  (let ((body (%obj "model" model-id
                    "messages" (convert-completions-messages
                                messages :instructions instructions
                                         :developer-role-p developer-role-p)
                    "stream" t)))
    (when include-usage
      (setf (gethash "stream_options" body) (%obj "include_usage" t)))
    (when tools
      (setf (gethash "tools" body) (%tools-vector tools #'%completions-tool-spec)))
    (let ((effort (getf +reasoning-effort+ reasoning-effort)))
      (when effort (setf (gethash "reasoning_effort" body) effort)))
    (when text-verbosity
      (setf (gethash "verbosity" body) (%wire-option-value text-verbosity)))
    (when service-tier
      (setf (gethash "service_tier" body) (%wire-option-value service-tier)))
    (let ((retention (getf +prompt-cache-retention+ prompt-cache-retention)))
      (when retention
        (setf (gethash "prompt_cache_retention" body) retention)))
    (com.inuoe.jzon:stringify body)))

(defun build-completions-headers (token &optional extra-headers)
  (append (list (cons "authorization" (format nil "Bearer ~A" token))
                (cons "accept" "text/event-stream"))
          extra-headers))

(defun completions-url (base-url)
  (concatenate 'string
               (string-right-trim "/" (or base-url "https://api.openai.com/v1"))
               "/chat/completions"))

(defvar *completions-http* nil
  "Test seam taking (url body headers) to (values char-stream status). NIL means real drakma POST.")

(defun %completions-request (request url body headers)
  (if *completions-http*
      (multiple-value-bind (stream status) (funcall *completions-http* url body headers)
        (setf (model-request-stream-closer request)
              (lambda () (shutdown-request-stream stream)))
        (values stream status))
      (multiple-value-bind (raw status)
          (open-cancellable-stream request url body headers)
        (values (flexi-streams:make-flexi-stream raw :external-format :utf-8) status))))

(defun openai-completions-adapter (provider request context &key emit)
  (multiple-value-bind (token account-id) (%resolve-token-and-account provider context)
    (declare (ignore account-id))
    (let* ((cfg (model-provider-config provider))
           (base (and cfg (provider-config-base-url cfg)))
           (extra (and cfg (provider-config-headers cfg)))
           (instructions (or (model-request-instructions request)
                             (getf (model-provider-metadata provider) :instructions)
                             "You are a helpful assistant."))
           (selection (model-request-selection request))
           (reasoning-effort (model-selection-option-value selection "reasoning-effort"))
           (text-verbosity (model-selection-option-value selection "text-verbosity"))
           (service-tier (model-selection-option-value selection "service-tier"))
           (prompt-cache-retention
             (model-selection-option-value selection "prompt-cache-retention"))
           (model-meta (%request-model-metadata request context))
           (model-profile (%metadata-transport-profile model-meta))
           (developer-role-p (%transport-profile-value model-profile :developer-role))
           (body (build-completions-body (model-request-model-id request)
                                         (model-request-model-messages request)
                                         :instructions instructions
                                         :reasoning-effort reasoning-effort
                                         :text-verbosity text-verbosity
                                         :service-tier service-tier
                                         :prompt-cache-retention prompt-cache-retention
                                         :tools (model-request-tool-schemas request)
                                         :developer-role-p developer-role-p))
           (state (make-completions-state)))
      (%note-request-payload request :openai-completions body
                             :url (completions-url base)
                             :message-count (length (model-request-model-messages request))
                             :reasoning-effort reasoning-effort
                             :service-tier service-tier
                             :prompt-cache-retention prompt-cache-retention
                             :text-verbosity text-verbosity)
      (multiple-value-bind (stream status)
          (%completions-request request (completions-url base) body
                                (build-completions-headers token extra))
        (unwind-protect
            (progn
              (%note-http-response request status)
              (unless (and (integerp status) (<= 200 status 299))
                (error 'openai-api-error :status status
                       :body (drain-capped-body stream)))
              (stream-sse-events stream
                                 (lambda (ev data)
                                   (%note-provider-event request ev data)
                                   (map-completions-chunk data state emit)))
              (finish-completions state emit))
          (setf (model-request-stream-closer request) nil)
          (ignore-errors (close stream)))))))

(defun ensure-stream-adapter (rt-provider runtime api context)
  "Register and reference the stream adapter for API on RUNTIME. Every installer
calls this unconditionally: register-model-stream-adapter increments the per-api
refcount, so providers sharing an api each hold a reference and one provider's
retract no longer strands the rest (the fn is dropped only when the last
reference releases). References both adapters, so it loads after
openai-responses.lisp."
  (provider-call rt-provider :register-model-stream-adapter runtime api
                 (ecase api
                   (:openai-responses   #'openai-responses-adapter)
                   (:openai-completions #'openai-completions-adapter)
                   (:anthropic-messages #'anthropic-messages-adapter))
                 context))
