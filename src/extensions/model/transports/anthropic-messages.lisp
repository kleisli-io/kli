(in-package #:kli/model/transports)

(define-condition anthropic-api-error (error)
  ((status :initarg :status :initform nil :reader anthropic-api-error-status)
   (body   :initarg :body   :initform nil :reader anthropic-api-error-body))
  (:report (lambda (c s)
             (format s "Anthropic API error~@[ (HTTP ~A)~]~@[: ~A~]"
                     (anthropic-api-error-status c)
                     (provider-error-display (anthropic-api-error-body c))))))

(defmethod kli/ext:condition-category ((condition anthropic-api-error))
  :provider)

(defmethod kli/ext:condition-http-status ((condition anthropic-api-error))
  (anthropic-api-error-status condition))

(defstruct (anthropic-state (:conc-name as-))
  "Stream state for the Messages event mapper. content_block_stop carries no
block type, so BLOCK-KINDS remembers what kind each index opened as. Usage is
split across message_start (input and cache counts) and message_delta (output),
accumulated here and emitted at message_start and message_stop."
  (block-kinds nil)
  (input-tokens nil)
  (output-tokens nil)
  (cache-read nil)
  (cache-write nil))

(defun %merge-anthropic-usage (state usage)
  "Fold non-null fields of a wire usage object into STATE. Proxies may send
explicit nulls, which must not clobber earlier counts."
  (flet ((merge-field (key current)
           (let ((v (gethash key usage)))
             (if (integerp v) v current))))
    (setf (as-input-tokens state)
          (merge-field "input_tokens" (as-input-tokens state))
          (as-output-tokens state)
          (merge-field "output_tokens" (as-output-tokens state))
          (as-cache-read state)
          (merge-field "cache_read_input_tokens" (as-cache-read state))
          (as-cache-write state)
          (merge-field "cache_creation_input_tokens" (as-cache-write state)))))

(defun %anthropic-usage-plist (state)
  "Anthropic reports no total_tokens, so the four-component sum stands in."
  (let ((input (or (as-input-tokens state) 0))
        (output (or (as-output-tokens state) 0))
        (cache-read (or (as-cache-read state) 0))
        (cache-write (or (as-cache-write state) 0)))
    (list :input-tokens input
          :output-tokens output
          :total-tokens (+ input output cache-read cache-write)
          :cache-read-tokens cache-read
          :cache-write-tokens cache-write)))

(defun %record-anthropic-block (state idx kind emit)
  (push (cons idx kind) (as-block-kinds state))
  (funcall emit (make-block-start-delta kind :content-index idx)))

(defun %start-anthropic-block (state idx block emit)
  (let ((type (and (hash-table-p block) (gethash "type" block))))
    (cond
      ((equal type "thinking")
       (%record-anthropic-block state idx :thinking emit))
      ((equal type "redacted_thinking")
       (%record-anthropic-block state idx :thinking emit)
       (let ((data (gethash "data" block)))
         (funcall emit (make-thinking-delta "[reasoning redacted]"
                                            :content-index idx
                                            :signature (and (stringp data) data)
                                            :redacted t))))
      ((equal type "tool_use")
       (%record-anthropic-block state idx :toolcall emit)
       (funcall emit (make-tool-call-delta (gethash "name" block)
                                           (list :partial-json "")
                                           :call-id (gethash "id" block)
                                           :content-index idx)))
      (t
       (%record-anthropic-block state idx :text emit)))))

(defun %anthropic-content-delta (idx delta emit)
  (let ((dtype (and (hash-table-p delta) (gethash "type" delta))))
    (cond
      ((equal dtype "text_delta")
       (funcall emit (make-assistant-delta (gethash "text" delta)
                                           :content-index idx)))
      ((equal dtype "thinking_delta")
       (funcall emit (make-thinking-delta (gethash "thinking" delta)
                                          :content-index idx)))
      ((equal dtype "input_json_delta")
       (funcall emit (make-tool-call-delta nil
                                           (list :partial-json
                                                 (gethash "partial_json" delta))
                                           :content-index idx)))
      ((equal dtype "signature_delta")
       (funcall emit (make-thinking-delta ""
                                          :content-index idx
                                          :signature (gethash "signature" delta))))
      (t nil))))

(defun map-anthropic-event (event-name data-string state emit)
  "Parse one Messages SSE payload and EMIT runtime delta(s), mutating STATE.
Keys off the JSON type field -- the SSE event line is redundant, so EVENT-NAME
is ignored. An early usage delta at message_start keeps input counts on
abort, and the final merged one at message_stop wins."
  (declare (ignore event-name))
  (let* ((obj (parse-sse-payload data-string))
         (type (and (hash-table-p obj) (gethash "type" obj)))
         (idx (and (hash-table-p obj) (gethash "index" obj))))
    (cond
      ((equal type "message_start")
       (let* ((msg (gethash "message" obj))
              (usage (and (hash-table-p msg) (gethash "usage" msg))))
         (when (hash-table-p usage)
           (%merge-anthropic-usage state usage)
           (funcall emit (make-usage-delta (%anthropic-usage-plist state))))))
      ((equal type "content_block_start")
       (%start-anthropic-block state idx (gethash "content_block" obj) emit))
      ((equal type "content_block_delta")
       (%anthropic-content-delta idx (gethash "delta" obj) emit))
      ((equal type "content_block_stop")
       (funcall emit (make-block-end-delta
                      (or (cdr (assoc idx (as-block-kinds state))) :text)
                      :content-index idx)))
      ((equal type "message_delta")
       (let ((delta (gethash "delta" obj)))
         (when (and (hash-table-p delta)
                    (equal (gethash "stop_reason" delta) "max_tokens"))
           (funcall emit (make-stop-reason-delta :length))))
       (let ((usage (gethash "usage" obj)))
         (when (hash-table-p usage)
           (%merge-anthropic-usage state usage))))
      ((equal type "message_stop")
       (funcall emit (make-usage-delta (%anthropic-usage-plist state))))
      ((equal type "error")
       (let* ((err (gethash "error" obj))
              (etype (and (hash-table-p err) (gethash "type" err))))
         ;; In-stream overload carries no HTTP status; stamping 529 makes it
         ;; retryable like its out-of-stream twin.
         (error 'anthropic-api-error
                :status (when (equal etype "overloaded_error") 529)
                :body data-string)))
      (t nil))))

(defun %cache-control ()
  (%obj "type" "ephemeral"))

(defun %parse-tool-input (json)
  "Parse aggregated tool-call arguments into the wire input object.
Blank or invalid JSON yields an empty object."
  (if (%blankp json)
      (make-hash-table :test #'equal)
      (handler-case (com.inuoe.jzon:parse json)
        (error () (make-hash-table :test #'equal)))))

(defun %anthropic-tool-use-item (tc)
  (let ((tc (%normalize-tool-call-for-wire tc)))
    (%obj "type" "tool_use"
          "id" (getf tc :id)
          "name" (getf tc :name)
          "input" (%parse-tool-input (getf tc :arguments-json)))))

(defun %anthropic-thinking-item (tb)
  "Wire block for a replayed thinking-block plist, NIL when it cannot travel.
Unsigned thinking cannot be replayed as a thinking block and downgrades to
plain text. Redacted blocks carry their opaque data in the signature slot."
  (let ((text (getf tb :thinking))
        (signature (getf tb :signature)))
    (cond
      ((getf tb :redacted)
       (%obj "type" "redacted_thinking" "data" signature))
      ((and signature (not (%blankp text)))
       (%obj "type" "thinking" "thinking" text "signature" signature))
      ((not (%blankp text))
       (%obj "type" "text" "text" text))
      (t nil))))

(defun %anthropic-tool-result-item (m)
  (let ((item (%obj "type" "tool_result"
                    "tool_use_id" (%tool-result-call-id-for-wire m)
                    "content" (%tool-result-content m))))
    (when (getf m :error-p)
      (setf (gethash "is_error" item) t))
    item))

(defun %stamp-cache-control-on (msg)
  "Ephemeral cache_control on the final block of MSG. String content becomes a
one-text-block array to carry it."
  (let ((content (gethash "content" msg)))
    (when (stringp content)
      (setf content (list (%obj "type" "text" "text" content))
            (gethash "content" msg) content))
    (setf (gethash "cache_control" (car (last content))) (%cache-control))))

(defun convert-anthropic-messages (messages &key midstream-system-p)
  "kli message plists to the Messages `messages` vector. Tool results are
tool_result blocks inside a user message and a consecutive run coalesces into
one such message -- some Anthropic-compatible endpoints reject splits. Replayed
thinking blocks render before assistant text and tool_use blocks. Harness
operator content lowers to a midstream system entry only when MIDSTREAM-SYSTEM-P
and the message is ephemeral (a durable operator would persist a role:system with
no guaranteed following assistant turn); otherwise a <harness-context> user wrap
with the fence delimiters escaped. Reference content always lowers to a fenced,
datamarked user turn. The cache breakpoint pins to the last non-ephemeral user
turn so ephemeral harness content never shifts the cached prefix."
  (let ((messages (%assert-provider-message-sequence messages))
        (entries '())
        (pending-results '())
        (has-reference (%has-reference-p messages)))
    (labels ((emit (wire &key ephemeral)
               (push (list :wire wire :ephemeral ephemeral
                           :user-role (equal "user" (gethash "role" wire)))
                     entries))
             (flush-results ()
               (when pending-results
                 (emit (%obj "role" "user" "content" (nreverse pending-results)))
                 (setf pending-results '()))))
      (dolist (m messages)
        (case (getf m :role)
          ((:user :summary)
           (flush-results)
           (let ((content (%provider-text-content (getf m :content))))
             (unless (%blankp content)
               (emit (%obj "role" "user" "content" content)))))
          (:assistant
           (flush-results)
           (let* ((content (%provider-text-content (getf m :content)))
                  (blocks (append
                           (loop for tb in (getf m :thinking-blocks)
                                 for item = (%anthropic-thinking-item tb)
                                 when item collect item)
                           (unless (%blankp content)
                             (list (%obj "type" "text" "text" content)))
                           (mapcar #'%anthropic-tool-use-item
                                   (getf m :tool-calls)))))
             (when blocks
               (emit (%obj "role" "assistant" "content" blocks)))))
          (:tool-result
           (push (%anthropic-tool-result-item m) pending-results))
          (:harness-context
           (flush-results)
           (let ((trust (getf m :trust))
                 (content (%provider-text-content (getf m :content)))
                 (eph (and (getf m :ephemeral) t)))
             (if (eq trust :operator)
                 (let ((op (%operator-content content +reference-anchor+ has-reference)))
                   (if (and midstream-system-p eph)
                       (emit (%obj "role" "system" "content" op) :ephemeral eph)
                       (emit (%obj "role" "user"
                                   "content" (%wrap-tag
                                              (%escape-fence-delimiters
                                               op +harness-context-open+ +harness-context-close+)
                                              +harness-context-open+ +harness-context-close+))
                             :ephemeral eph)))
                 (emit (%obj "role" "user" "content" (%task-memory-block content))
                       :ephemeral eph))))))
      (flush-results))
    (let ((ordered (nreverse entries)))
      (loop for e in (reverse ordered)
            when (and (getf e :user-role) (not (getf e :ephemeral)))
              do (%stamp-cache-control-on (getf e :wire)) (return))
      (coerce (mapcar (lambda (e) (getf e :wire)) ordered) 'vector))))

(defun %anthropic-tool-spec (descriptor)
  "Generic tool descriptor -> Anthropic flat tool object (no nesting envelope)."
  (%obj "name" (getf descriptor :name)
        "description" (or (getf descriptor :description) "")
        "input_schema" (tool-parameters->json-schema
                        (getf descriptor :parameters))))

(defparameter +thinking-budgets+
  '(:minimal 1024 :low 2048 :medium 8192 :high 16384 :xhigh 16384))

(defparameter +adaptive-efforts+
  '(:minimal "low" :low "low" :medium "medium" :high "high"))

(defun %anthropic-thinking-config (reasoning-effort max-output thinking-mode
                                   xhigh-effort)
  "Wire thinking configuration -> (values thinking output-config max-tokens
beta-p). Base max_tokens is min(MAX-OUTPUT, 32000). Budget mode grows
max_tokens by the budget capped at MAX-OUTPUT, shrinking the budget when the
cap leaves no generation room. The interleaved-thinking beta applies only to
budget-mode thinking. All registered models reason, so off is an explicit
disable."
  (let ((base (min max-output 32000)))
    (cond
      ((or (null reasoning-effort) (eq reasoning-effort :off))
       (values (%obj "type" "disabled") nil base nil))
      ((eq thinking-mode :adaptive)
       (let ((effort (if (eq reasoning-effort :xhigh)
                         (or xhigh-effort "high")
                         (getf +adaptive-efforts+ reasoning-effort "medium"))))
         (values (%obj "type" "adaptive") (%obj "effort" effort) base nil)))
      (t
       (let* ((budget (getf +thinking-budgets+ reasoning-effort 8192))
              (max-tokens (min (+ base budget) max-output)))
         (when (<= max-tokens budget)
           (setf budget (- max-tokens 1024)))
         (values (%obj "type" "enabled" "budget_tokens" budget)
                 nil max-tokens t))))))

(defun build-anthropic-body (model-id messages
                             &key (instructions "You are a helpful assistant.")
                               reasoning-effort tools (max-output 32000)
                               (thinking-mode :budget) xhigh-effort
                               midstream-system-p)
  "Messages request JSON. The second value is true when the request must carry
the interleaved-thinking beta header. Ephemeral cache_control rides on the
system block, the last tool, and the last user block. max_tokens is required
by the API and has no default. MIDSTREAM-SYSTEM-P lets harness operator content
lower to a midstream system entry."
  (multiple-value-bind (thinking output-config max-tokens beta-p)
      (%anthropic-thinking-config reasoning-effort max-output thinking-mode
                                  xhigh-effort)
    (let ((body (%obj "model" model-id
                      "messages" (convert-anthropic-messages
                                  messages :midstream-system-p midstream-system-p)
                      "max_tokens" max-tokens
                      "stream" t
                      "thinking" thinking)))
      (when (and instructions (plusp (length instructions)))
        (setf (gethash "system" body)
              (list (%obj "type" "text" "text" instructions
                          "cache_control" (%cache-control)))))
      (when output-config
        (setf (gethash "output_config" body) output-config))
      (when tools
        (let ((specs (%tools-vector tools #'%anthropic-tool-spec)))
          (setf (gethash "cache_control" (aref specs (1- (length specs))))
                (%cache-control))
          (setf (gethash "tools" body) specs)))
      (values (com.inuoe.jzon:stringify body) beta-p))))

(defun build-anthropic-headers (token &optional extra-headers &key beta-p)
  (append (list (cons "x-api-key" token)
                (cons "anthropic-version" "2023-06-01")
                (cons "accept" "text/event-stream"))
          (when beta-p
            (list (cons "anthropic-beta" "interleaved-thinking-2025-05-14")))
          extra-headers))

(defun anthropic-url (base-url)
  (concatenate 'string
               (string-right-trim "/" (or base-url "https://api.anthropic.com"))
               "/v1/messages"))

(defvar *anthropic-http* nil
  "Test seam taking (url body headers) to (values char-stream status). NIL means real drakma POST.")

(defun %anthropic-request (request url body headers)
  (if *anthropic-http*
      (multiple-value-bind (stream status) (funcall *anthropic-http* url body headers)
        (setf (model-request-stream-closer request)
              (lambda () (shutdown-request-stream stream)))
        (values stream status))
      (multiple-value-bind (raw status)
          (open-cancellable-stream request url body headers)
        (values (flexi-streams:make-flexi-stream raw :external-format :utf-8)
                status))))

(defun %model-output-facts (request context)
  "Output and thinking facts for the request's model from its transport profile."
  (let* ((meta (%request-model-metadata request context))
         (profile (%metadata-transport-profile meta)))
    (values (%transport-profile-value profile :max-output 32000)
            (%transport-profile-value profile :thinking-mode :budget)
            (%transport-profile-value profile :xhigh-effort)
            (%transport-profile-value profile :midstream-system))))

(defun anthropic-messages-adapter (provider request context &key emit)
  "Stream PROVIDER's Messages-API reply for REQUEST, emitting deltas to EMIT.
No finish pass is needed -- Anthropic always closes its blocks on the wire."
  (multiple-value-bind (token account-id) (%resolve-token-and-account provider context)
    (declare (ignore account-id))
    (multiple-value-bind (max-output thinking-mode xhigh-effort midstream-system-p)
        (%model-output-facts request context)
      (let* ((cfg (model-provider-config provider))
             (base (and cfg (provider-config-base-url cfg)))
             (extra (and cfg (provider-config-headers cfg)))
             (instructions (or (model-request-instructions request)
                               (getf (model-provider-metadata provider) :instructions)
                               "You are a helpful assistant."))
             (selection (model-request-selection request))
             (reasoning-effort (model-selection-option-value selection "reasoning-effort"))
             (state (make-anthropic-state)))
        (multiple-value-bind (body beta-p)
            (build-anthropic-body (model-request-model-id request)
                                  (model-request-model-messages request)
                                  :instructions instructions
                                  :reasoning-effort reasoning-effort
                                  :tools (model-request-tool-schemas request)
                                  :max-output max-output
                                  :thinking-mode thinking-mode
                                  :xhigh-effort xhigh-effort
                                  :midstream-system-p midstream-system-p)
          (%note-request-payload request :anthropic-messages body
                                 :url (anthropic-url base)
                                 :message-count (length (model-request-model-messages request))
                                 :reasoning-effort reasoning-effort
                                 :thinking-mode thinking-mode
                                 :max-output max-output
                                 :interleaved-thinking-beta beta-p)
          (multiple-value-bind (stream status)
              (%anthropic-request request (anthropic-url base) body
                                  (build-anthropic-headers token extra
                                                           :beta-p beta-p))
            (unwind-protect
                 (progn
                   (%note-http-response request status)
                   (unless (and (integerp status) (<= 200 status 299))
                     (error 'anthropic-api-error
                            :status status
                            :body (drain-capped-body stream)))
                   (stream-sse-events stream
                                      (lambda (ev data)
                                        (%note-provider-event request ev data)
                                        (map-anthropic-event ev data state emit))))
              (setf (model-request-stream-closer request) nil)
              (ignore-errors (close stream)))))))))
