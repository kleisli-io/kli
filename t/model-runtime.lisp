(in-package #:kli/tests)

(defun model-runtime-provider (protocol)
  (ext:require-capability-provider protocol
                                   :model/runtime
                                   :contract :model/runtime/v1))

(defun model-runtime-service (context)
  (kli:find-live-object (kli:context-registry context)
                        :model-runtime-service))

(defun model-runtime-test-context ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (values context protocol)))

(defun make-runtime-session-and-context (context &key include-tool-result
                                                 include-custom-message)
  (let* ((store (session-log-store context))
         (session (sess:create-session store context :id (gensym "RUNTIME-SESSION-")))
         (agent-context nil))
    (sess:append-session-entry
     store
     session
     (sess:make-message-entry
      (sess:make-user-message "hello" :id (gensym "RUNTIME-USER-")))
     context)
    (when include-custom-message
      (sess:append-session-entry
       store
       session
       (sess:make-custom-message-entry
        :runtime-note
        "ui-only"
        :id (gensym "RUNTIME-CUSTOM-"))
       context))
    (when include-tool-result
      (sess:append-session-entry
       store
       session
       (sess:make-message-entry
        (sess:make-assistant-message
         ""
         :id (gensym "RUNTIME-ASSISTANT-TOOL-")
         :metadata
         (list :tool-calls
               (list (list :id :call-1
                           :name "read"
                           :arguments-json "{}")))))
       context)
      (sess:append-session-entry
       store
       session
       (sess:make-message-entry
        (sess:make-tool-result-message "tool output"
                                       :id (gensym "RUNTIME-TOOL-MESSAGE-")
                                       :tool-call-id :call-1
                                       :tool-name "read"))
       context))
    (setf agent-context (ctx:make-agent-context session store context))
    (values session
            agent-context
            (ctx:seal-context-projection agent-context context))))

(defun register-runtime-model (context provider-id model-id &key
                                       (auth-required-p t) metadata api
                                       option-schemas options)
  (let* ((registry (model-registry context))
         (provider (models:register-model-provider
                    registry
                    (models:make-model-provider provider-id
                                                (or api :fake)
                                                :auth-required-p
                                                auth-required-p
                                                :metadata metadata)
                    context))
         (model (models:register-model-definition
                 registry
                 (models:make-model-definition
                  provider-id model-id (or api :fake)
                  :context-window (getf metadata :context-window)
                  :option-schemas option-schemas)
                 context))
         (selection (models:select-model registry model context
                                         :options options)))
    (values provider model selection)))

(test model-runtime-registers-provider-and-service
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*)
    (signals error
      (install-extension context rt:*model-runtime-extension-manifest*)))
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (is (typep (model-runtime-service context) 'rt:model-runtime))
    (is (rt:find-model-stream-adapter (model-runtime-service context) :fake))
    (is (model-runtime-provider protocol))))

(test model-runtime-tables-are-synchronized
  "Requests, streams, and responses register from the agent worker thread
while inspection reads from the TUI loop thread; adapters install from the
loop thread and resolve from the worker."
  (multiple-value-bind (context protocol) (model-runtime-test-context)
    (declare (ignore protocol))
    (let ((runtime (model-runtime-service context)))
      (is (sb-ext:hash-table-synchronized-p (rt::runtime-requests runtime)))
      (is (sb-ext:hash-table-synchronized-p (rt::runtime-streams runtime)))
      (is (sb-ext:hash-table-synchronized-p (rt::runtime-responses runtime)))
      (is (sb-ext:hash-table-synchronized-p
           (rt::runtime-stream-adapters runtime)))
      (is (sb-ext:hash-table-synchronized-p
           (rt::runtime-stream-adapter-refcounts runtime))))))

(test model-runtime-has-no-built-in-fake-adapter
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*)
    (is (null (rt:find-model-stream-adapter
               (model-runtime-service context)
               :fake)))))

(test (model-runtime-streams-deterministic-fake-deltas :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (provider _model selection)
          (register-runtime-model context
                                  "fake-provider"
                                  "fake-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("hello" " world")))
        (declare (ignore _model))
        (let* ((runtime (model-runtime-service context))
               (request (rt:make-model-request runtime
                                               selection
                                               sealed-context
                                               context
                                               :id :fake-request))
               (response (rt:stream-model-response provider request context))
               (stream (rt:model-request-stream request)))
          (is (equal "hello world" (rt:model-response-content response)))
          (is (eq :completed (rt:model-request-state request)))
          (is (= (ctx:sealed-context-epoch sealed-context)
                 (rt:model-request-sealed-epoch request)))
          (is (equal '(:message-start
                       :message-end)
                     (mapcar (lambda (event) (getf event :type))
                             (getf (rt:inspect-model-stream stream)
                                   :events)))))))))

(test (model-runtime-threads-instructions-into-request :fixture interactive-authority)
  "The agent system prompt rides on the request as :instructions, which transports turn into the leading system message or top-level instructions field."
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "fake-provider" "fake-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let ((runtime (model-runtime-service context)))
          (let ((with (rt:make-model-request runtime selection sealed-context
                                             context
                                             :id :req-with-instructions
                                             :instructions "You are kli.")))
            (is (string= "You are kli."
                         (rt:model-request-instructions with))))
          (let ((without (rt:make-model-request runtime selection sealed-context
                                                context
                                                :id :req-without-instructions)))
            (is (null (rt:model-request-instructions without)))))))))

(test (model-runtime-threads-session-id-into-request :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "fake-provider" "fake-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let ((runtime (model-runtime-service context)))
          (let ((with (rt:make-model-request runtime selection sealed-context
                                             context
                                             :id :req-with-session
                                             :session-id :session-7)))
            (is (eq :session-7 (rt:model-request-session-id with))))
          (let ((without (rt:make-model-request runtime selection sealed-context
                                                context
                                                :id :req-without-session)))
            (is (null (rt:model-request-session-id without)))))))))

(test (model-runtime-streams-block-aware-deltas :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (provider _model selection)
          (register-runtime-model
           context
           "block-provider"
           "block-model"
           :auth-required-p nil
           :metadata '(:fake-blocks
                       ((:kind :thinking :text ("Let me think. " "Considering."))
                        (:kind :text :text ("Here is " "the answer."))
                        (:kind :toolcall
                         :tool-call (:id :call-1
                                     :name "read"
                                     :arguments (:partial-json "{\"path\":\"README.md\"}"))))))
        (declare (ignore _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection
                                               sealed-context
                                               context))
               (response (rt:stream-model-response provider request context))
               (stream (rt:model-request-stream request))
               (events (getf (rt:inspect-model-stream stream) :events)))
          (is (equal "Here is the answer." (rt:model-response-content response)))
          (is (equal '((:id :call-1 :name "read" :arguments-json "{\"path\":\"README.md\"}"))
                     (rt:model-response-tool-calls response)))
          (is (equal '((:thinking "Let me think. Considering."
                        :signature nil :redacted nil))
                     (getf (rt:model-response-metadata response)
                           :thinking-blocks)))
          (is (equal '(:message-start :message-end)
                     (mapcar (lambda (event) (getf event :type)) events))))))))

(test (model-runtime-inspection-reports-stream-timings :fixture interactive-authority)
  (let ((rt:*capture-model-timings* t))
    (multiple-value-bind (context protocol)
        (model-runtime-test-context)
      (declare (ignore protocol))
      (multiple-value-bind (_session _agent-context sealed-context)
          (make-runtime-session-and-context context)
        (declare (ignore _session _agent-context))
        (multiple-value-bind (provider _model selection)
            (register-runtime-model
             context
             "timing-provider"
             "timing-model"
             :auth-required-p nil
             :metadata '(:fake-blocks
                         ((:kind :thinking :text ("Thinking."))
                          (:kind :text :text ("Visible.")))))
          (declare (ignore _model))
          (let* ((request (rt:make-model-request (model-runtime-service context)
                                                 selection
                                                 sealed-context
                                                 context))
                 (response (rt:stream-model-response provider request context))
                 (stream (rt:model-request-stream request))
                 (inspection (rt:inspect-model-stream stream))
                 (timings (getf inspection :timings))
                 (keys (mapcar (lambda (entry) (getf entry :key)) timings)))
            (dolist (key '(:request-start :first-block-start
                           :first-thinking-delta :first-visible-delta
                           :completion))
              (is (member key keys)))
            (is (every (lambda (entry)
                         (let ((elapsed (getf entry :elapsed-ms)))
                           (and (integerp elapsed) (not (minusp elapsed)))))
                       timings))
            (is (equal timings (getf (rt:model-response-metadata response)
                                     :timings)))
            (is (equal timings (getf (getf (rt:inspect-model-response response)
                                           :metadata)
                                     :timings)))))))))

(test (model-runtime-fake-provider-can-emit-tool-call :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (provider _model selection)
          (register-runtime-model
           context
           "tool-provider"
           "tool-model"
           :auth-required-p nil
           :metadata '(:fake-deltas ("before tool")
                       :fake-tool-call (:id :call-1
                                        :name "read"
                                        :arguments (:partial-json "{\"path\":\"README.md\"}"))))
        (declare (ignore _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection
                                               sealed-context
                                               context))
               (response (rt:stream-model-response provider request context)))
          (is (equal "before tool" (rt:model-response-content response)))
          (is (equal '((:id :call-1
                        :name "read"
                        :arguments-json "{\"path\":\"README.md\"}"))
                     (rt:model-response-tool-calls response))))))))

(test (model-runtime-request-is-stable-after-context-edit :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context
                                  "stable-provider"
                                  "stable-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection
                                               sealed-context
                                               context))
               (initial (copy-list (rt:model-request-model-messages request)))
               (patch (ctx:make-append-message-patch
                       (sess:make-user-message "after seal"))))
          (ctx:stage-context-patch agent-context patch)
          (ctx:commit-context-patches agent-context context)
          (is (equal initial (rt:model-request-model-messages request)))
          (is (equal '("hello" "after seal")
                     (mapcar #'sess:message-content
                             (ctx:context-projected-messages
                              agent-context)))))))))

(defun request-view-contents (view)
  (loop for item in (ctx:context-view-items view)
        for value = (ctx:context-view-item-payload-value item)
        when (typep value 'sess:agent-message)
          collect (sess:message-content value)))

(defun request-replay-item-ids (request)
  (mapcar #'ctx:context-view-item-id
          (ctx:context-view-items
           (rt:model-request-provider-replay-view request))))

(test (model-runtime-request-audit-snapshots-survive-context-patches
       :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context
                                  "audit-provider"
                                  "audit-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection
                                               sealed-context
                                               context))
               (wire (copy-tree (rt:model-request-provider-wire-input request)))
               (accounting (rt:model-request-accounting-result request))
               (total (rt:request-accounting-result-total accounting))
               (inspection (rt:inspect-model-request request))
               (patch (ctx:make-append-message-patch
                       (sess:make-user-message "after audit seal"))))
          (is (equal '("hello")
                     (request-view-contents
                      (rt:model-request-sealed-editable-view request))))
          (is (equal '("hello")
                     (request-view-contents
                      (rt:model-request-provider-replay-view request))))
          (is (getf (getf inspection :audit) :provider-wire-input))
          (is (eq :tokens (getf (getf inspection :audit) :accounting-units)))
          (ctx:stage-context-patch agent-context patch)
          (ctx:commit-context-patches agent-context context)
          (is (equal wire (rt:model-request-provider-wire-input request)))
          (is (= total
                 (rt:request-accounting-result-total
                  (rt:model-request-accounting-result request))))
          (is (equal '("hello")
                     (request-view-contents
                      (rt:model-request-sealed-editable-view request))))
          (is (equal '("hello" "after audit seal")
                     (mapcar #'sess:message-content
                             (ctx:context-projected-messages
                              agent-context)))))))))

(test (model-runtime-records-transport-wire-input-in-request-audit
       :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "wire-provider" "wire-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let ((request (rt:make-model-request (model-runtime-service context)
                                              selection
                                              sealed-context
                                              context)))
          (rt:record-model-request-wire-input
           request :openai-responses "{\"model\":\"wire-model\",\"input\":[]}")
          (is (string= "{\"model\":\"wire-model\",\"input\":[]}"
                       (rt:model-request-provider-wire-input request)))
          (is (eq :openai-responses
                  (rt:request-accounting-result-api
                   (rt:model-request-accounting-result request))))
          (is (some (lambda (attr)
                      (equal '(:message)
                             (mapcar (lambda (id) (and (consp id) (first id)))
                                     (getf attr :item-ids))))
                    (rt:request-accounting-result-attributions
                     (rt:model-request-accounting-result request)))))))))

(test (model-runtime-request-audit-does-not-write-wire-to-session-log
       :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "log-audit-provider" "log-audit-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let* ((before (sess:session-entry-count session))
               (request (rt:make-model-request (model-runtime-service context)
                                               selection
                                               sealed-context
                                               context)))
          (rt:record-model-request-wire-input
           request :openai-responses
           "{\"model\":\"log-audit-model\",\"input\":[{\"role\":\"user\"}]}")
          (is (= before (sess:session-entry-count session)))
          (is (every (lambda (entry)
                       (not (search "log-audit-model"
                                    (prin1-to-string entry))))
                     (sess:session-entries session))))))))

(test (complete-text-and-normal-requests-share-audit-boundary
       :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "shared-provider" "shared-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("ok")))
        (declare (ignore _provider _model))
        (let* ((runtime (model-runtime-service context))
               (normal (rt:make-model-request runtime selection sealed-context
                                              context))
               (isolated nil))
          (rt:complete-text runtime selection sealed-context context
                            :on-request (lambda (request)
                                          (setf isolated request)))
          (is (equal (request-replay-item-ids normal)
                     (request-replay-item-ids isolated)))
          (is (equal (rt:model-request-provider-wire-input normal)
                     (rt:model-request-provider-wire-input isolated)))
          (is (= (rt:request-accounting-result-total
                  (rt:model-request-accounting-result normal))
                 (rt:request-accounting-result-total
                  (rt:model-request-accounting-result isolated)))))))))

(test (model-runtime-resolves-auth-without-leaking-secret :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (let* ((auth-store (credential-store context))
           (path (sb-ext:posix-getenv "PATH")))
      (register-path-auth auth-store context :provider-id "path-runtime")
      (multiple-value-bind (_session _agent-context sealed-context)
          (make-runtime-session-and-context context)
        (declare (ignore _session _agent-context))
        (multiple-value-bind (provider _model selection)
            (register-runtime-model context "path-runtime" "path-model")
          (declare (ignore _model))
          (let ((request (rt:make-model-request (model-runtime-service context)
                                                selection
                                                sealed-context
                                                context)))
            (rt:stream-model-response provider request context)
            (let ((inspection (rt:inspect-model-request request)))
              (is (rt:model-request-credential-reference-id request))
              (is (not (search path
                               (prin1-to-string inspection)
                               :test #'char=))))))))))

(test (model-runtime-resolves-auth-before-non-fake-adapter-error :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (let ((auth-store (credential-store context)))
      (register-path-auth auth-store context :provider-id "non-fake")
      (multiple-value-bind (_session _agent-context sealed-context)
          (make-runtime-session-and-context context)
        (declare (ignore _session _agent-context))
        (multiple-value-bind (provider _model selection)
            (register-runtime-model context
                                    "non-fake"
                                    "non-fake-model"
                                    :api :not-implemented)
          (declare (ignore _model))
          (let ((request (rt:make-model-request (model-runtime-service context)
                                                selection
                                                sealed-context
                                                context)))
            (let ((condition
                    (handler-case
                        (progn
                          (rt:stream-model-response provider request context)
                          nil)
                      (error (c) c))))
              (is (not (null condition)))
              (is (rt:model-request-credential-reference-id request)))))))))

(test stream-thinking-blocks-aggregates-replayable-blocks
  "Blocks group by content-index in arrival order. Text and signature
fragments concatenate per group, the redacted flag survives, and groups
carrying neither text nor a signature are dropped."
  (let ((stream (rt:make-model-stream nil)))
    (dolist (d (list (rt:make-thinking-delta "Th" :content-index 0)
                     (rt:make-thinking-delta "inking" :content-index 0)
                     (rt:make-thinking-delta "" :content-index 0
                                             :signature "sig")
                     (rt:make-thinking-delta "" :content-index 0
                                             :signature "==")
                     (rt:make-thinking-delta "[reasoning redacted]"
                                             :content-index 1
                                             :signature "opaque" :redacted t)
                     (rt:make-thinking-delta "" :content-index 2)))
      (rt:handle-model-delta stream d nil))
    (is (equal '((:thinking "Thinking" :signature "sig==" :redacted nil)
                 (:thinking "[reasoning redacted]" :signature "opaque"
                  :redacted t))
	               (rt:stream-thinking-blocks stream)))))

(test stream-thinking-blocks-replaces-finalized-thinking-text
  (let ((stream (rt:make-model-stream nil)))
    (dolist (d (list (rt:make-thinking-delta "Head" :content-index 0)
                     (rt:make-thinking-delta "er" :content-index 0)
                     (rt:make-thinking-delta "Full final thinking"
                                             :content-index 0
                                             :replacement-p t
                                             :signature "final")))
      (rt:handle-model-delta stream d nil))
    (is (equal '((:thinking "Full final thinking"
                  :signature "final"
                  :redacted nil))
               (rt:stream-thinking-blocks stream)))))

(test stream-thinking-blocks-prefers-raw-over-summary
  (let ((stream (rt:make-model-stream nil)))
    (dolist (d (list (rt:make-thinking-delta "Header"
                                             :content-index 0
                                             :source :summary)
                     (rt:make-thinking-delta (format nil "~%~%")
                                             :content-index 0
                                             :source :summary)
                     (rt:make-thinking-delta "Raw streamed"
                                             :content-index 0
                                             :source :raw)
                     (rt:make-thinking-delta "Short summary"
                                             :content-index 0
                                             :source :summary
                                             :replacement-p t
                                             :signature "final")))
      (rt:handle-model-delta stream d nil))
    (is (equal '((:thinking "Raw streamed"
                  :signature "final"
                  :redacted nil))
               (rt:stream-thinking-blocks stream)))))

(test stream-thinking-blocks-replaces-raw-with-final-raw-content
  (let ((stream (rt:make-model-stream nil)))
    (dolist (d (list (rt:make-thinking-delta "Header"
                                             :content-index 0
                                             :source :summary)
                     (rt:make-thinking-delta "Raw streamed"
                                             :content-index 0
                                             :source :raw)
                     (rt:make-thinking-delta "Full raw final"
                                             :content-index 0
                                             :source :raw
                                             :replacement-p t
                                             :signature "final")))
      (rt:handle-model-delta stream d nil))
    (is (equal '((:thinking "Full raw final"
                  :signature "final"
                  :redacted nil))
               (rt:stream-thinking-blocks stream)))))

(test (model-response-metadata-carries-thinking-blocks-when-collected :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (provider _model selection)
          (register-runtime-model context "thinking-provider" "thinking-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-blocks
                                              ((:kind :thinking
                                                :text ("Let me think."))
                                               (:kind :text
                                                :text ("Answer.")))))
        (declare (ignore _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection sealed-context context))
               (response (rt:stream-model-response provider request context)))
          (is (equal '((:thinking "Let me think." :signature nil :redacted nil))
                     (getf (rt:model-response-metadata response)
                           :thinking-blocks)))))
      (multiple-value-bind (provider _model selection)
          (register-runtime-model context "plain-provider" "plain-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("just text")))
        (declare (ignore _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection sealed-context context))
               (response (rt:stream-model-response provider request context)))
          (is (eq :missing (getf (rt:model-response-metadata response)
                                 :thinking-blocks :missing))))))))

(test (model-runtime-conversion-filters-custom-and-preserves-tool-results :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context
                                          :include-tool-result t
                                          :include-custom-message t)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context
                                  "conversion-provider"
                                  "conversion-model"
                                  :auth-required-p nil)
        (declare (ignore _provider _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection
                                               sealed-context
                                               context))
               (messages (rt:model-request-model-messages request)))
          (is (equal '(:user :assistant :tool-result)
                     (mapcar (lambda (message) (getf message :role))
                             messages)))
          (is (equal "read" (getf (third messages) :tool-name)))
          (is (equal :call-1 (getf (third messages) :tool-call-id))))))))

(test (model-runtime-materializes-provider-replay-items :fixture interactive-authority)
  "Request construction keeps semantic provider replay items inspectable and
lowers them only at the provider materialization boundary."
  (multiple-value-bind (context protocol)
      (model-runtime-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (sess:create-session store context :id (gensym "SUMMARY-SESSION-"))))
      (flet ((append! (entry)
               (sess:append-session-entry store session entry context)))
        (append! (sess:make-message-entry
                  (sess:make-user-message "write artifact")
                  :id :summary-user))
        (append! (sess:make-message-entry
                  (sess:make-assistant-message
                   ""
                   :metadata
                   (list :tool-calls
                         (list (list :id "call_summary"
                                     :name "write"
                                     :arguments-json "{\"path\":\"a.txt\"}"))))
                  :id :summary-assistant))
        (append! (sess:make-message-entry
                  (sess:make-tool-result-message
                   "Wrote a.txt."
                   :tool-call-id "call_summary"
                   :tool-name "write")
                  :id :summary-result))
        (append! (sess:make-compaction-entry
                  "The artifact was written to a.txt."
                  :summary-assistant
                  :id :summary-compaction)))
      (let* ((agent-context (ctx:make-agent-context session store context))
             (sealed-context (ctx:seal-context-projection agent-context context)))
        (multiple-value-bind (_provider _model selection)
            (register-runtime-model context
                                    "summary-provider"
                                    "summary-model"
                                    :auth-required-p nil)
          (declare (ignore _provider _model))
          (let* ((request (rt:make-model-request (model-runtime-service context)
                                                 selection
                                                 sealed-context
                                                 context))
                 (items (rt:model-request-provider-replay-items request))
                 (messages (rt:model-request-model-messages request)))
            (is (equal '(:summary)
                       (mapcar #'ctx:context-view-item-payload-kind items)))
            (is (equal '(:summary)
                       (mapcar (lambda (message) (getf message :role))
                               messages)))
            (is (string= "The artifact was written to a.txt."
                         (getf (first messages) :content)))))))))

(test (complete-text-returns-text-and-registers-no-loop-request :fixture interactive-authority)
  "An isolated completion yields the response text and never registers a loop
request, so it leaks nothing into the runtime request table."
  (multiple-value-bind (context protocol) (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "fake-provider" "fake-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("summary " "text")))
        (declare (ignore _provider _model))
        (let* ((runtime (model-runtime-service context))
               (before (hash-table-count (rt:runtime-requests runtime)))
               (text (rt:complete-text runtime selection sealed-context context
                                       :instructions "SYSTEM")))
          (is (string= "summary text" text))
          (is (= before (hash-table-count (rt:runtime-requests runtime)))))))))

(test (complete-text-returns-usage-as-second-value :fixture interactive-authority)
  "The isolated completion grades its result: the provider-reported usage plist
rides as the second value, NIL when the provider omits usage."
  (multiple-value-bind (context protocol) (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "graded-provider" "graded-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("graded")
                                              :fake-usage (:input-tokens 30
                                                           :output-tokens 5)))
        (declare (ignore _provider _model))
        (multiple-value-bind (text usage)
            (rt:complete-text (model-runtime-service context)
                              selection sealed-context context)
          (is (string= "graded" text))
          (is (equal '(:input-tokens 30 :output-tokens 5) usage))))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "ungraded-provider" "ungraded-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("ungraded")))
        (declare (ignore _provider _model))
        (multiple-value-bind (text usage)
            (rt:complete-text (model-runtime-service context)
                              selection sealed-context context)
          (is (string= "ungraded" text))
          (is (null usage)))))))

(test (complete-text-threads-instructions-onto-request :fixture interactive-authority)
  "INSTRUCTIONS ride on the isolated request as the system prompt for the call."
  (let ((captured nil))
    (multiple-value-bind (context protocol) (model-runtime-test-context)
      (declare (ignore protocol))
      (rt:register-model-stream-adapter
       (model-runtime-service context) :fake
       (lambda (provider request context &key emit)
         (declare (ignore provider context))
         (setf captured (rt:model-request-instructions request))
         (funcall emit (rt:make-assistant-delta "ok")))
       context)
      (multiple-value-bind (_session _agent-context sealed-context)
          (make-runtime-session-and-context context)
        (declare (ignore _session _agent-context))
        (multiple-value-bind (_provider _model selection)
            (register-runtime-model context "fake-provider" "fake-model"
                                    :auth-required-p nil)
          (declare (ignore _provider _model))
          (rt:complete-text (model-runtime-service context)
                            selection sealed-context context
                            :instructions "SUMMARIZE THIS")
          (is (string= "SUMMARIZE THIS" captured)))))))

(test model-delta-ids-are-uninterned
  "Delta ids are per-token ephemera -- interning a keyword per delta grows the
keyword package for the life of the image. Tool-call ids stay interned because
they serialize into session entries."
  (is (null (symbol-package (kli:object-id (rt:make-assistant-delta "x")))))
  (is (null (symbol-package (kli:object-id (rt:make-thinking-delta "x")))))
  (is (null (symbol-package (kli:object-id (rt:make-usage-delta '())))))
  (is (keywordp (rt:tool-call-delta-call-id
                 (rt:make-tool-call-delta "read" '())))))

(test (stream-buffers-clear-once-response-is-built :fixture interactive-authority)
  "The response owns the aggregated content after completion -- the stream's
per-token buffers and transient request fields are dropped, while request audit
snapshots remain available for inspection."
  (multiple-value-bind (context protocol) (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (provider _model selection)
          (register-runtime-model
           context
           "downsize-provider"
           "downsize-model"
           :auth-required-p nil
           :metadata '(:fake-blocks
                       ((:kind :thinking :text ("Let me think."))
                        (:kind :text :text ("Here is " "the answer."))
                        (:kind :toolcall
                         :tool-call (:id :call-1
                                     :name "read"
                                     :arguments (:partial-json "{\"path\":\"README.md\"}"))))))
        (declare (ignore _model))
        (let* ((request (rt:make-model-request (model-runtime-service context)
                                               selection
                                               sealed-context
                                               context))
               (response (rt:stream-model-response provider request context))
               (stream (rt:model-request-stream request)))
          (is (equal "Here is the answer." (rt:model-response-content response)))
          (is (equal '((:id :call-1 :name "read" :arguments-json "{\"path\":\"README.md\"}"))
                     (rt:model-response-tool-calls response)))
          (is (equal '((:thinking "Let me think." :signature nil :redacted nil))
                     (getf (rt:model-response-metadata response)
                           :thinking-blocks)))
          (is (null (rt::model-stream-text-fragments stream)))
          (is (null (rt:model-stream-thinking-deltas stream)))
          (is (null (rt::model-stream-tool-call-deltas stream)))
          (is (null (rt:model-request-model-messages request)))
          (is (null (rt:model-request-sealed-context request)))
          (is (equal '("hello")
                     (request-view-contents
                      (rt:model-request-sealed-editable-view request))))
          (is (rt:model-request-provider-wire-input request))
          (is (rt:model-request-accounting-result request)))))))

(test (completed-requests-evict-beyond-history-limit :fixture interactive-authority)
  "Completions beyond the history limit evict the oldest request, stream, and
response from the runtime tables and the live registry, so the registries stay
bounded for the life of the image."
  (multiple-value-bind (context protocol) (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (provider _model selection)
          (register-runtime-model context "evict-provider" "evict-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("ok")))
        (declare (ignore _model))
        (let ((runtime (model-runtime-service context))
              (rt::*completed-request-history-limit* 2)
              (requests '()))
          (dotimes (i 4)
            (let ((request (rt:make-model-request runtime
                                                  selection
                                                  sealed-context
                                                  context)))
              (push request requests)
              (rt:stream-model-response provider request context)))
          (is (<= (hash-table-count (rt:runtime-requests runtime)) 2))
          (is (<= (hash-table-count (rt:runtime-streams runtime)) 2))
          (is (<= (hash-table-count (rt:runtime-responses runtime)) 2))
          (let* ((ordered (nreverse requests))
                 (oldest (first ordered))
                 (newest (car (last ordered))))
            (is (null (gethash (kli:object-id oldest)
                               (rt:runtime-requests runtime))))
            (is (null (kli:find-live-object (kli:context-registry context)
                                            (kli:object-id oldest))))
            (is (eq newest (gethash (kli:object-id newest)
                                    (rt:runtime-requests runtime))))
            (is (kli:find-live-object (kli:context-registry context)
                                      (kli:object-id newest)))))))))

(test (complete-text-streams-and-responses-evict-with-history :fixture interactive-authority)
  "Isolated completions never register their requests, but their streams and
responses do register -- the eviction history drops those triples all the same."
  (multiple-value-bind (context protocol) (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (multiple-value-bind (_provider _model selection)
          (register-runtime-model context "fake-provider" "fake-model"
                                  :auth-required-p nil
                                  :metadata '(:fake-deltas ("summary")))
        (declare (ignore _provider _model))
        (let ((runtime (model-runtime-service context))
              (rt::*completed-request-history-limit* 1))
          (dotimes (i 3)
            (rt:complete-text runtime selection sealed-context context))
          (is (<= (hash-table-count (rt:runtime-streams runtime)) 1))
          (is (<= (hash-table-count (rt:runtime-responses runtime)) 1)))))))

(test (failed-request-joins-eviction-history :fixture image-session-authority)
  "A request whose adapter signals never reaches completion, but it still joins
the eviction history so it evicts instead of pinning the tables forever."
  (multiple-value-bind (context protocol) (model-runtime-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (_session _agent-context sealed-context)
        (make-runtime-session-and-context context)
      (declare (ignore _session _agent-context))
      (rt:register-model-stream-adapter
       (model-runtime-service context) :failing
       (lambda (provider request context &key emit)
         (declare (ignore provider request context emit))
         (error "transport down"))
       context)
      (multiple-value-bind (failing-provider _fm failing-selection)
          (register-runtime-model context "failing-provider" "failing-model"
                                  :auth-required-p nil
                                  :api :failing)
        (declare (ignore _fm))
        (multiple-value-bind (ok-provider _om ok-selection)
            (register-runtime-model context "fake-provider" "fake-model"
                                    :auth-required-p nil
                                    :metadata '(:fake-deltas ("ok")))
          (declare (ignore _om))
          (let ((runtime (model-runtime-service context))
                (rt::*completed-request-history-limit* 1))
            (let ((failed (rt:make-model-request runtime
                                                 failing-selection
                                                 sealed-context
                                                 context)))
              (signals error
                (rt:stream-model-response failing-provider failed context))
              (let ((ok (rt:make-model-request runtime
                                               ok-selection
                                               sealed-context
                                               context)))
                (rt:stream-model-response ok-provider ok context)
                (is (null (gethash (kli:object-id failed)
                                   (rt:runtime-requests runtime))))
                (is (null (kli:find-live-object (kli:context-registry context)
                                                (kli:object-id failed))))
                (is (eq ok (gethash (kli:object-id ok)
                                    (rt:runtime-requests runtime))))))))))))

(test convert-agent-message-lowers-harness-context-and-drops-unknown-roles
  "The :harness-context arm carries trust/content/ephemeral into the plist; an
unrecognized role still drops (otherwise -> nil)."
  (let ((plist (rt::convert-agent-message
                (sess:make-harness-context-message "BODY" :trust :operator :ephemeral t))))
    (is (eq :harness-context (getf plist :role)))
    (is (eq :operator (getf plist :trust)))
    (is (equal "BODY" (getf plist :content)))
    (is (eq t (getf plist :ephemeral))))
  (is (null (rt::convert-agent-message
             (sess:make-agent-message 'sess:user-message :bogus "x")))
      "an unrecognized role drops"))
