(in-package #:kli/model/runtime)

(defgeneric make-model-request (runtime model-selection sealed-context context
                                &key id actor metadata instructions session-id))
(defgeneric complete-text (runtime model-selection sealed-context context
                           &key instructions on-request)
  (:documentation "Run one isolated synchronous completion over SEALED-CONTEXT and
return the response text. The request is built but never registered as a loop
request, and the response is never appended to a session, so callers obtain
summary or title text without mutating conversation state. INSTRUCTIONS rides on
the request as the system prompt for the isolated call. ON-REQUEST, when given,
is called with the request before streaming starts, handing callers the handle
abort-model-request needs to interrupt the otherwise-anonymous call."))
(defgeneric register-model-stream-adapter (runtime api adapter context))
(defgeneric unregister-model-stream-adapter (runtime api context))
(defgeneric find-model-stream-adapter (runtime api))
(defgeneric stream-model-response (provider request context &key on-delta))
(defgeneric handle-model-delta (stream delta context))
(defgeneric abort-model-request (request context))
(defgeneric inspect-model-request (request))
(defgeneric inspect-model-response (response))
(defgeneric inspect-model-stream (stream))

(defun runtime-registry-provider (context)
  (require-capability-provider (active-protocol context)
                               :model/registry
                               :contract :model/registry/v1))

(defun runtime-auth-provider (context)
  (require-capability-provider (active-protocol context)
                               :auth
                               :contract :auth/v1))

(defun runtime-model-registry (context)
  (or (find-live-object (context-registry context) :model-registry-service)
      (error "No model registry service is loaded.")))

(defun runtime-auth-store (context)
  (find-live-object (context-registry context) :credential-store))

(defun runtime-service (context)
  (or (find-live-object (context-registry context) :model-runtime-service)
      (error "No model runtime service is loaded.")))

(defun runtime-register-live-object (context object)
  (unless (find-live-object (context-registry context) (object-id object))
    (register-live-object (context-registry context) object))
  object)

(defun register-runtime-request (runtime request context)
  (setf (gethash (object-id request) (runtime-requests runtime)) request)
  (runtime-register-live-object context request))

(defun register-runtime-stream (runtime stream context)
  (setf (gethash (object-id stream) (runtime-streams runtime)) stream)
  (runtime-register-live-object context stream))

(defun register-runtime-response (runtime response context)
  (setf (gethash (object-id response) (runtime-responses runtime)) response)
  (runtime-register-live-object context response))

(defmethod register-model-stream-adapter ((runtime model-runtime)
                                          api
                                          adapter
                                          context)
  (declare (ignore context))
  (unless (functionp adapter)
    (error "Model stream adapter is not a function: ~S" adapter))
  (incf (gethash api (runtime-stream-adapter-refcounts runtime) 0))
  (setf (gethash api (runtime-stream-adapters runtime)) adapter)
  adapter)

(defmethod unregister-model-stream-adapter ((runtime model-runtime)
                                            api
                                            context)
  (declare (ignore context))
  (let* ((refcounts (runtime-stream-adapter-refcounts runtime))
         (n (gethash api refcounts 0)))
    (cond
      ((<= n 1)
       (remhash api refcounts)
       (remhash api (runtime-stream-adapters runtime)))
      (t (setf (gethash api refcounts) (1- n)))))
  runtime)

(defmethod find-model-stream-adapter ((runtime model-runtime) api)
  (gethash api (runtime-stream-adapters runtime)))

(defun make-request-instance (selection provider sealed-context model-messages
                              provider-id model-id
                              &key id metadata tool-schemas instructions session-id)
  (make-instance 'model-request
                 :id (or id (next-model-request-id))
                 :selection selection
                 :provider provider
                 :provider-id provider-id
                 :model-id model-id
                 :sealed-context sealed-context
                 :model-messages model-messages
                 :instructions instructions
                 :session-id session-id
                 :tool-schemas tool-schemas
                 :sealed-epoch (sealed-context-epoch sealed-context)
                 :sealed-context-id (object-id sealed-context)
                 :source-context-id (sealed-context-source-context-id sealed-context)
                 :leaf-id (sealed-context-leaf-id sealed-context)
                 :metadata metadata
                 :created-at (get-universal-time)))

(defun enumerate-request-tools (context)
  "Generic tool descriptors (:name :description :parameters) for every registered
tool. Param specs stay in the kli DSL -- transports convert to JSON Schema."
  (let ((protocol (active-protocol context)))
    (when protocol
      (loop for tool in (kli/ext:list-tools protocol)
            collect (list :name (kli/ext:tool-name tool)
                          :description (kli/ext:tool-description tool)
                          :parameters (kli/ext:tool-parameters tool))))))

(defun parse-tool-call-arguments (json)
  "Parse a tool-call arguments JSON string into a jzon hash-table.
Blank or missing JSON yields an empty object. Invalid JSON also yields an
empty object plus a second value describing the parse failure, so callers
can surface the real cause instead of a missing-parameter error."
  (if (and (stringp json)
           (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) json))))
      (handler-case (values (com.inuoe.jzon:parse json) nil)
        (error (condition)
          (values (make-hash-table :test #'equal)
                  (princ-to-string condition))))
      (make-hash-table :test #'equal)))

(defmethod make-model-request ((runtime model-runtime)
                               selection
                               sealed-context
                               context
                               &key id actor metadata instructions session-id)
  (let* ((metadata (if actor (list* :actor actor metadata) metadata))
         (registry-provider (runtime-registry-provider context))
         (registry (runtime-model-registry context))
         (provider-id (model-selection-provider-id selection))
         (model-id (model-selection-model-id selection))
         (provider (provider-call registry-provider
                                  :find-model-provider
                                  registry
                                  provider-id)))
    (unless provider
      (error "No model provider registered for selection: ~S" provider-id))
    (let* ((messages (context-model-messages sealed-context))
           (converted (convert-messages messages))
           (request (make-request-instance selection
                                           provider
                                           sealed-context
                                           converted
                                           provider-id
                                           model-id
                                           :id id
                                           :metadata metadata
                                           :instructions instructions
                                           :session-id session-id
                                           :tool-schemas (enumerate-request-tools context))))
      (register-runtime-request runtime request context))))

(defmethod complete-text ((runtime model-runtime)
                          selection
                          sealed-context
                          context
                          &key instructions on-request)
  (let* ((registry-provider (runtime-registry-provider context))
         (registry (runtime-model-registry context))
         (provider-id (model-selection-provider-id selection))
         (model-id (model-selection-model-id selection))
         (provider (provider-call registry-provider
                                  :find-model-provider
                                  registry
                                  provider-id)))
    (unless provider
      (error "No model provider registered for selection: ~S" provider-id))
    (let* ((messages (context-model-messages sealed-context))
           (converted (convert-messages messages))
           (request (make-request-instance selection
                                           provider
                                           sealed-context
                                           converted
                                           provider-id
                                           model-id
                                           :instructions instructions)))
      (when on-request
        (funcall on-request request))
      (model-response-content
       (stream-model-response provider request context)))))

(defun convert-agent-message (message)
  (let ((role (message-role message)))
    (case role
      (:user
       (list :role :user
             :content (message-content message)
             :metadata (copy-list (message-metadata message))))
      (:assistant
       (list :role :assistant
             :content (message-content message)
             :tool-calls (getf (message-metadata message) :tool-calls)
             :thinking-blocks (getf (message-metadata message) :thinking-blocks)
             :metadata (copy-list (message-metadata message))))
      (:tool-result
       (list :role :tool-result
             :content (message-content message)
             :tool-call-id (tool-call-id message)
             :tool-name (tool-name message)
             :error-p (tool-error-p message)
             :details (getf (message-metadata message) :details)
             :metadata (copy-list (message-metadata message))))
      (:harness-context
       (list :role :harness-context
             :trust (kli/session/log:message-trust message)
             :content (message-content message)
             :ephemeral (getf (message-metadata message) :ephemeral)
             :metadata (copy-list (message-metadata message))))
      (otherwise nil))))

(defun convert-messages (messages)
  (loop for message in messages
        for converted = (convert-agent-message message)
        when converted
          collect converted))

(defun resolve-request-credential (provider request context auth-provider)
  (when (model-provider-auth-required-p provider)
    (let* ((auth-store (or (runtime-auth-store context)
                           (error "No auth store is loaded.")))
           (credential (provider-call
                        auth-provider
                        :resolve-credential
                        auth-store
                        (model-provider-credential-provider-id provider)
                        context)))
      (setf (model-request-credential-reference-id request)
            (resolved-credential-reference-id credential)
            (model-request-credential-source-kind request)
            (resolved-credential-source-kind credential))))
  request)

(defun ensure-request-stream (request context)
  (or (model-request-stream request)
      (let* ((runtime (find-live-object (context-registry context)
                                        :model-runtime-service))
             (stream (make-model-stream request)))
        (setf (model-request-stream request) stream)
        (when runtime
          (register-runtime-stream runtime stream context))
        stream)))

(defun record-stream-event (stream event)
  "Record a lifecycle event (message-start, message-end, request-aborted).
Per-delta events are deliberately not recorded -- they would retain every
streamed token a second time for the life of the stream."
  (push event (model-stream-events stream))
  event)

(defmethod handle-model-delta ((stream model-stream)
                               (delta usage-delta)
                               context)
  (declare (ignore context))
  (setf (model-stream-usage stream) (copy-list (usage-delta-usage delta)))
  delta)

(defmethod handle-model-delta ((stream model-stream)
                               (delta assistant-delta)
                               context)
  (declare (ignore context))
  (push (assistant-delta-text delta) (model-stream-text-fragments stream))
  delta)

(defmethod handle-model-delta ((stream model-stream)
                               (delta thinking-delta)
                               context)
  (declare (ignore context))
  (push delta (model-stream-thinking-deltas stream))
  delta)

(defmethod handle-model-delta ((stream model-stream)
                               (delta tool-call-delta)
                               context)
  (declare (ignore context))
  (push delta (model-stream-tool-call-deltas stream))
  delta)

(defmethod handle-model-delta ((stream model-stream)
                               (delta block-boundary-delta)
                               context)
  (declare (ignore context))
  delta)

(defmethod handle-model-delta ((stream model-stream)
                               (delta stop-reason-delta)
                               context)
  (declare (ignore context))
  (setf (model-stream-provider-stop-reason stream)
        (stop-reason-delta-reason delta))
  delta)

(defun stream-text-content (stream)
  (let ((fragments (reverse (model-stream-text-fragments stream))))
    (if fragments
        (apply #'concatenate 'string fragments)
        "")))

(defun %first-non-nil (values)
  (find-if #'identity values))

(defun %tool-call-arguments-json (deltas)
  "Concatenate the :partial-json fragments across DELTAS in order."
  (with-output-to-string (out)
    (dolist (d deltas)
      (let ((frag (getf (tool-call-delta-arguments d) :partial-json)))
        (when (stringp frag) (write-string frag out))))))

(defun stream-tool-calls (stream)
  "Aggregate tool-call deltas by content-index into coherent, serialization-safe
tool calls (:id :name :arguments-json). The first delta of each content-index
group carries identity, later deltas carry argument fragments."
  (let ((order '())
        (groups (make-hash-table :test #'eql)))
    (dolist (delta (reverse (model-stream-tool-call-deltas stream)))
      (let ((idx (model-delta-content-index delta)))
        (unless (nth-value 1 (gethash idx groups))
          (push idx order))
        (push delta (gethash idx groups))))
    (loop for idx in (nreverse order)
          for deltas = (nreverse (gethash idx groups))
          collect (list :id (%first-non-nil (mapcar #'tool-call-delta-call-id deltas))
                        :name (%first-non-nil (mapcar #'tool-call-delta-name deltas))
                        :arguments-json (%tool-call-arguments-json deltas)))))

(defun %thinking-block-text (deltas)
  (with-output-to-string (out)
    (dolist (d deltas)
      (let ((text (thinking-delta-text d)))
        (when (stringp text) (write-string text out))))))

(defun %thinking-block-signature (deltas)
  "Concatenate signature fragments across DELTAS, NIL when none arrived."
  (let ((signature (with-output-to-string (out)
                     (dolist (d deltas)
                       (let ((frag (thinking-delta-signature d)))
                         (when (stringp frag) (write-string frag out)))))))
    (when (plusp (length signature)) signature)))

(defun stream-thinking-blocks (stream)
  "Aggregate thinking deltas by content-index into replayable, serialization-safe
blocks (:thinking :signature :redacted). Groups carrying neither text nor a
signature are dropped."
  (let ((order '())
        (groups (make-hash-table :test #'eql)))
    (dolist (delta (reverse (model-stream-thinking-deltas stream)))
      (let ((idx (model-delta-content-index delta)))
        (unless (nth-value 1 (gethash idx groups))
          (push idx order))
        (push delta (gethash idx groups))))
    (loop for idx in (nreverse order)
          for deltas = (nreverse (gethash idx groups))
          for text = (%thinking-block-text deltas)
          for signature = (%thinking-block-signature deltas)
          unless (and (zerop (length text)) (null signature))
            collect (list :thinking text
                          :signature signature
                          :redacted (and (some #'thinking-delta-redacted deltas) t)))))

(defun evict-completed-triple (runtime triple context)
  (destructuring-bind (request-id stream-id response-id) triple
    (let ((registry (context-registry context)))
      (remhash request-id (runtime-requests runtime))
      (remove-live-object registry request-id)
      (when stream-id
        (remhash stream-id (runtime-streams runtime))
        (remove-live-object registry stream-id))
      (when response-id
        (remhash response-id (runtime-responses runtime))
        (remove-live-object registry response-id)))))

(defun note-completed-request (runtime request stream response context)
  "Push REQUEST's triple onto the completed history and evict the oldest
entries beyond *completed-request-history-limit*. Components a caller never
registered (complete-text requests, failed streams) evict as no-ops."
  (let ((triple (list (object-id request)
                      (and stream (object-id stream))
                      (and response (object-id response))))
        (evicted '()))
    (sb-thread:with-mutex ((runtime-completed-history-lock runtime))
      (let ((history (cons triple (runtime-completed-history runtime))))
        (setf evicted (nthcdr *completed-request-history-limit* history)
              (runtime-completed-history runtime)
              (if evicted (ldiff history evicted) history))))
    (dolist (old evicted)
      (evict-completed-triple runtime old context))))

(defun complete-model-request (request stream context &key stop-reason metadata)
  "Finalize REQUEST from STREAM. A cross-thread abort unwinds the stream as
clean EOF and lands here, so the :aborted state the aborter set is preserved
rather than masked as :completed."
  (let* ((runtime (find-live-object (context-registry context)
                                    :model-runtime-service))
         (stream-usage (model-stream-usage stream))
         (thinking-blocks (stream-thinking-blocks stream))
         (with-thinking (if thinking-blocks
                            (list* :thinking-blocks thinking-blocks metadata)
                            metadata))
         (effective-metadata (if stream-usage
                                 (list* :usage stream-usage with-thinking)
                                 with-thinking))
         (response (make-model-response request
                                        (stream-text-content stream)
                                        (stream-tool-calls stream)
                                        stop-reason
                                        :metadata effective-metadata)))
    (setf (model-stream-state stream) :completed
          (model-request-response request) response
          (model-request-completed-at request) (get-universal-time))
    (unless (eq (model-request-state request) :aborted)
      (setf (model-request-state request) :completed))
    (record-stream-event
     stream
     (list :type :message-end
           :request-id (object-id request)
           :response-id (object-id response)
           :stop-reason stop-reason))
    ;; The response owns the aggregated content from here -- drop the
    ;; per-token buffers and the conversation snapshot they were built from,
    ;; so retained references (agent turns) no longer pin every streamed token.
    (setf (model-stream-text-fragments stream) '()
          (model-stream-thinking-deltas stream) '()
          (model-stream-tool-call-deltas stream) '()
          (model-request-sealed-context request) nil
          (model-request-model-messages request) '())
    (when runtime
      (register-runtime-response runtime response context)
      (note-completed-request runtime request stream response context))
    response))

(defmethod stream-model-response (provider (request model-request) context
                                  &key on-delta)
  "Stream PROVIDER's response for REQUEST, dispatching deltas to ON-DELTA. A
mid-stream abort closes the socket, so the adapter read unwinds with a stream
error -- what already streamed is kept and finalized as a partial. A genuine
non-abort error still propagates."
  (when (eq (model-request-state request) :aborted)
    (return-from stream-model-response (model-request-response request)))
  (let ((auth-provider (runtime-auth-provider context)))
    (resolve-request-credential provider request context auth-provider)
    (let* ((runtime (runtime-service context))
           (api (model-provider-api provider))
           (adapter (or (find-model-stream-adapter runtime api)
                        (error "No stream adapter for model API ~S." api)))
           (stream (ensure-request-stream request context)))
      (setf (model-request-state request) :streaming
            (model-stream-state stream) :streaming)
      (record-stream-event
       stream
       (list :type :message-start
             :request-id (object-id request)
             :provider-id (model-request-provider-id request)
             :model-id (model-request-model-id request)))
      (let ((emit (lambda (delta)
                    (handle-model-delta stream delta context)
                    (when on-delta
                      (kli/ext:safely-invoke
                       on-delta
                       :model-stream (list :on-delta (object-id request))
                       delta))
                    delta)))
        (handler-case
            (funcall adapter provider request context :emit emit)
          (error (c)
            (unless (eq (model-request-state request) :aborted)
              ;; A failed request never reaches complete-model-request, and a
              ;; retry builds a fresh request -- note this one here so it
              ;; evicts instead of pinning the tables forever.
              (note-completed-request runtime request stream nil context)
              (error c)))))
      (complete-model-request request stream context
                              :stop-reason (cond
                                             ((eq (model-request-state request)
                                                  :aborted)
                                              :aborted)
                                             ((model-stream-provider-stop-reason
                                               stream))
                                             (t :end))))))

(defmethod abort-model-request ((request model-request) context)
  "Mark REQUEST aborted and unblock the worker parked in read-line by invoking
the stream closer, which shuts down the socket."
  (declare (ignore context))
  (let ((stream (model-request-stream request))
        (closer (model-request-stream-closer request)))
    (setf (model-request-state request) :aborted
          (model-request-error request) "Request aborted."
          (model-request-completed-at request) (get-universal-time))
    (when closer (ignore-errors (funcall closer)))
    (when stream
      (setf (model-stream-state stream) :aborted)
      (record-stream-event stream
                           (list :type :request-aborted
                                 :request-id (object-id request)))))
  request)

(defmethod inspect-model-request ((request model-request))
  (list :id (object-id request)
        :state (model-request-state request)
        :provider-id (model-request-provider-id request)
        :model-id (model-request-model-id request)
        :sealed-context-id (model-request-sealed-context-id request)
        :sealed-epoch (model-request-sealed-epoch request)
        :source-context-id (model-request-source-context-id request)
        :leaf-id (model-request-leaf-id request)
        :credential-reference-id (model-request-credential-reference-id request)
        :credential-source-kind (model-request-credential-source-kind request)
        :message-count (length (model-request-model-messages request))
        :stream-id (and (model-request-stream request)
                        (object-id (model-request-stream request)))
        :response-id (and (model-request-response request)
                          (object-id (model-request-response request)))
        :error (model-request-error request)
        :created-at (model-request-created-at request)
        :completed-at (model-request-completed-at request)))

(defmethod inspect-model-response ((response model-response))
  (list :id (object-id response)
        :request-id (model-response-request-id response)
        :content (model-response-content response)
        :tool-calls (copy-list (model-response-tool-calls response))
        :stop-reason (model-response-stop-reason response)
        :timestamp (model-response-timestamp response)))

(defmethod inspect-model-stream ((stream model-stream))
  (list :id (object-id stream)
        :request-id (object-id (model-stream-request stream))
        :state (model-stream-state stream)
        :events (reverse (copy-list (model-stream-events stream)))))
