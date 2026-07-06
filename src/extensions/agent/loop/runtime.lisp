(in-package #:kli/agent/loop)

(defun agent-loop-service (context)
  (find-live-object (context-registry context) :agent-loop-service))

(defun queue-empty-p (queue)
  (sb-thread:with-mutex ((agent-queue-lock queue))
    (null (agent-queue-items queue))))

(defun enqueue-agent-item (queue item)
  (sb-thread:with-mutex ((agent-queue-lock queue))
    (setf (agent-queue-items queue)
          (append (agent-queue-items queue) (list item))))
  item)

(defun drain-agent-queue (queue)
  (sb-thread:with-mutex ((agent-queue-lock queue))
    (prog1 (copy-list (agent-queue-items queue))
      (setf (agent-queue-items queue) '()))))

(defun pop-agent-queue-item (queue)
  (sb-thread:with-mutex ((agent-queue-lock queue))
    (pop (agent-queue-items queue))))

(defun deliver-all-queue-items (queue)
  "Canonical :all delivery policy: every queued item in one pass."
  (drain-agent-queue queue))

(defun deliver-one-queue-item (queue)
  "Canonical :one-at-a-time delivery policy: exactly one queued item per
pass. The rest wait for the next delivery point."
  (let ((item (pop-agent-queue-item queue)))
    (when item
      (list item))))

(defun agent-behavior-value (agent name)
  (let ((key (etypecase name
               (keyword name)
               (symbol (intern (symbol-name name) :keyword)))))
    (getf (behavior-state (agent-behavior agent)) key)))

(defun recode-agent-loop-behavior (agent &key tool-scheduling-policy
                                           steering-delivery-policy
                                           follow-up-delivery-policy
                                           error-to-tool-result-policy
                                           in-flight-context-edit-policy)
  ;; Live-swaps the loop's control-flow funcalls; gate like every other recode.
  (kli/ext:require-capability :behavior/hotpatch)
  (let ((state (behavior-state (agent-behavior agent))))
    (setf (agent-behavior agent)
          (make-agent-loop-behavior
           :tool-scheduling-policy (or tool-scheduling-policy
                                       (getf state :tool-scheduling-policy))
           :steering-delivery-policy (or steering-delivery-policy
                                         (getf state :steering-delivery-policy))
           :follow-up-delivery-policy (or follow-up-delivery-policy
                                          (getf state :follow-up-delivery-policy))
           :error-to-tool-result-policy
           (or error-to-tool-result-policy
               (getf state :error-to-tool-result-policy))
           :in-flight-context-edit-policy
           (or in-flight-context-edit-policy
               (getf state :in-flight-context-edit-policy)))))
  agent)

(defun agent-events-provider (context)
  (let ((protocol (active-protocol context)))
    (and protocol
         (find-capability-provider protocol :events :contract :events/v1))))

(defun agent-loop-context-lens-provider (context)
  (require-capability-provider (active-protocol context)
                               :context/lens
                               :contract :context/lens/v1))

(defun agent-loop-runtime-provider (context)
  (require-capability-provider (active-protocol context)
                               :model/runtime
                               :contract :model/runtime/v1))

(defun agent-loop-log-provider (context)
  (require-capability-provider (active-protocol context)
                               :session/log
                               :contract :session/log/v1))

(defun agent-loop-entries-provider (context)
  (require-capability-provider (active-protocol context)
                               :session/entries
                               :contract :session/entries/v1))

(defun turn-response-usage (turn)
  (let ((response (agent-turn-response turn)))
    (and response (getf (model-response-metadata response) :usage))))

(defun turn-response-timings (turn)
  (let ((response (agent-turn-response turn)))
    (and response (getf (model-response-metadata response) :timings))))

(defun latest-turn-usage (turns)
  "Usage of the most recent turn that reported any, or NIL. This is the context-occupancy snapshot, not a sum. TURNS is newest-first, and each turn's input-tokens already subsumes every prior turn's messages, so the newest turn's total reflects the whole in-flight context. Summing across turns would re-count that context once per turn and overshoot the window. Returns a fresh list so the event payload does not alias the response metadata."
  (loop for turn in turns
        for usage = (turn-response-usage turn)
        when usage
          return (copy-list usage)))

(defun latest-turn-timings (turns)
  (loop for turn in turns
        for timings = (turn-response-timings turn)
        when timings
          return (copy-tree timings)))

(defun run-final-response-text (run)
  "Final assistant text of RUN: the newest completed turn's response content on a completed run, NIL when the run aborted, errored, or produced no completed response. Empty final content yields the empty string, not NIL, so callers can distinguish a silent reply from no reply. RUN turns are newest-first."
  (and (eq (agent-run-state run) :completed)
       (let ((turn (first (agent-run-turns run))))
         (and turn
              (agent-turn-completed-at turn)
              (let ((response (agent-turn-response turn)))
                (and response (model-response-content response)))))))

(defun emit-agent-event (agent context type &key payload)
  (let ((events (agent-events-provider context)))
    (when events
      (provider-call events
                     :emit-event
                     context
                     (provider-call events
                                    :make-event
                                    type
                                    :payload payload
                                    :source (object-id agent))))))

(defun normalize-agent-input-message (entries-provider message kind)
  (cond
    ((agent-message-p message)
     message)
    ((stringp message)
     (provider-call entries-provider :make-user-message message
                    :metadata (list :agent-input kind)))
    (t
     (error "Unsupported agent input message: ~S" message))))

(defun normalize-agent-input-messages (entries-provider messages kind)
  (cond
    ((agent-message-p messages)
     (list messages))
    ((stringp messages)
     (list (normalize-agent-input-message entries-provider messages kind)))
    ((listp messages)
     (mapcar (lambda (message)
               (normalize-agent-input-message entries-provider message kind))
             messages))
    (t
     (error "Unsupported agent input messages: ~S" messages))))

(defun append-agent-message (agent message context &key kind metadata)
  (let ((log-provider (agent-loop-log-provider context))
        (entries-provider (agent-loop-entries-provider context)))
    (provider-call log-provider
                   :append-session-entry
                   (agent-store agent)
                   (agent-session agent)
                   (provider-call entries-provider
                                  :make-message-entry message
                                  :source (object-id agent)
                                  :metadata (append (list :agent-input kind)
                                                    metadata))
                   context)))

(defun emit-user-message-appended (agent context message entry kind)
  (let ((content (message-content message)))
    (when (stringp content)
      (emit-agent-event agent context :agent/user-message-appended
                        :payload (list :kind kind
                                       :entry-id (object-id entry)
                                       :text content)))))

(defun append-agent-input-messages (agent messages kind context)
  (let ((entries-provider (agent-loop-entries-provider context))
        (lens-provider (agent-loop-context-lens-provider context))
        (entries '()))
    (dolist (message (normalize-agent-input-messages entries-provider
                                                     messages
                                                     kind))
      (let ((entry (append-agent-message agent message context :kind kind)))
        (push entry entries)
        (emit-user-message-appended agent context message entry kind)))
    (provider-call lens-provider
                   :rebuild-context-projection (agent-context agent))
    (nreverse entries)))

(defun tool-result-text-content (item)
  (cond
    ((and (listp item) (eq (getf item :type) :text))
     (getf item :text))
    (t
     (prin1-to-string item))))

(defun tool-result-text (result)
  (with-output-to-string (stream)
    (loop for item in (tool-result-content result)
          for first-p = t then nil
          do (unless first-p (terpri stream))
             (write-string (tool-result-text-content item) stream))))

(defun tool-call-from-provider-plist (tool-call)
  "Build a tool-call from a provider plist. Arguments JSON that fails to
parse marks the call with :arguments-parse-error so execution can feed the
real cause back to the model instead of running the tool with the
silently-empty fallback arguments."
  (multiple-value-bind (arguments parse-error)
      (parse-tool-call-arguments (getf tool-call :arguments-json))
    (make-tool-call (getf tool-call :name)
                    arguments
                    :call-id (getf tool-call :id)
                    :metadata (if parse-error
                                  (list* :arguments-parse-error parse-error
                                         tool-call)
                                  tool-call))))

(defun skipped-tool-result ()
  (make-tool-result
   :content (list (make-tool-text-content
                   "Skipped due to queued user message."))
   :details '(:reason :steering-queued)
   :error-p t))

(defun register-agent-live-object (context object)
  (unless (find-live-object (context-registry context) (object-id object))
    (register-live-object (context-registry context) object))
  object)

(defun register-agent-turn (agent turn context)
  (push turn (agent-turns agent))
  (setf (agent-current-turn agent) turn)
  (register-agent-live-object context turn))

(defun register-tool-call (turn tool-call context)
  (push tool-call (agent-turn-tool-calls turn))
  (register-agent-live-object context tool-call))

(defun register-tool-execution (agent execution context)
  (push execution (agent-tool-executions agent))
  (register-agent-live-object context execution))

(defun append-tool-result-message (agent tool-call result context
                                   &key presentation)
  "PRESENTATION is the tool-owned view term for the result. It rides in message
metadata under :presentation -- a key the model wire never reads -- so the TUI
and replay render from it without the model ever seeing it."
  (let ((entries-provider (agent-loop-entries-provider context)))
    (append-agent-message
     agent
     (provider-call entries-provider
                    :make-tool-result-message (tool-result-text result)
                    :tool-call-id (agent-tool-call-id tool-call)
                    :tool-name (agent-tool-call-name tool-call)
                    :error-p (tool-result-error-p result)
                    :metadata (list :details
                                    (tool-result-details result)
                                    :presentation presentation))
     context
     :kind :tool-result)))

(defun skip-agent-tool-call (agent tool-call context)
  (let ((execution (make-tool-execution tool-call))
        (result (skipped-tool-result)))
    (register-tool-execution agent execution context)
    (setf (agent-tool-call-state tool-call) :skipped
          (tool-execution-result execution) result
          (tool-execution-state execution) :skipped
          (tool-execution-completed-at execution) (get-universal-time))
    (emit-agent-event agent
                      context
                      :agent/tool-execution-start
                      :payload (list :execution-id (object-id execution)
                                     :tool-call-id
                                     (agent-tool-call-id tool-call)
                                     :tool-name
                                     (agent-tool-call-name tool-call)
                                     :arguments
                                     (agent-tool-call-arguments tool-call)
                                     :skipped-p t))
    (append-tool-result-message agent tool-call result context)
    (emit-agent-event agent
                      context
                      :agent/tool-execution-end
                      :payload (payload-with-duration
                                (list :execution-id (object-id execution)
                                      :tool-call-id
                                      (agent-tool-call-id tool-call)
                                      :error-p t
                                      :skipped-p t
                                      :result-text
                                      (tool-result-text result))
                                (tool-execution-started-at-real execution)))
    execution))

(defun skip-agent-tool-calls (agent tool-calls context)
  (dolist (tool-call tool-calls)
    (skip-agent-tool-call agent tool-call context)))

(defun record-tool-update (agent execution payload context)
  (let ((update (make-tool-execution-update execution payload)))
    (push update (tool-execution-updates execution))
    (register-agent-live-object context update)
    (emit-agent-event agent
                      context
                      :agent/tool-execution-update
                      :payload (list :execution-id (object-id execution)
                                     :update-id (object-id update)
                                     :payload payload))
    update))

(defun finish-agent-turn (agent turn state context)
  (setf (agent-turn-state turn) state
        (agent-turn-completed-at turn) (get-universal-time))
  (emit-agent-event agent
                    context
                    :agent/turn-end
                    :payload (payload-with-duration
                              (list :turn-id (object-id turn)
                                    :state state)
                              (agent-turn-started-at-real turn)))
  turn)

(defun abort-agent-turn (agent turn context)
  "Seal the turn's registered-but-unresolved tool calls with skipped results,
then finish the turn :aborted. Every abort path routes here: an interrupted turn
that left a declared tool_call without a tool_result unbalances the assistant
message, and providers reject every later request over that context."
  (skip-agent-tool-calls
   agent
   (remove-if (lambda (tool-call)
                (member (agent-tool-call-state tool-call)
                        '(:completed :error :skipped)))
              (reverse (agent-turn-tool-calls turn)))
   context)
  (finish-agent-turn agent turn :aborted context))

(defun assistant-tool-call-presentations (tool-calls context)
  "Call-terms for an assistant message's TOOL-CALLS, as ((call-id term) ...).
Persisted separately from the model-facing :tool-calls and keyed by call-id, so
replay renders each call the way the live execution did -- a read hidden, a bash
as a command line -- without re-running the tool, and the term never reaches the
wire."
  (let ((protocol (active-protocol context)))
    (loop for call in tool-calls
          for id = (getf call :id)
          for tool = (and protocol (kli/ext:find-tool protocol (getf call :name)))
          for arguments = (kli/model/runtime:parse-tool-call-arguments
                           (getf call :arguments-json))
          for term = (if tool
                         (or (ignore-errors (kli/ext:present-call tool arguments))
                             (kli/ext:default-call-term arguments))
                         (kli/ext:default-call-term arguments))
          when id collect (list id term))))

(defun append-assistant-response (agent response context)
  "Append the assistant response to the session. A :aborted stop-reason on a mid-stream cancel keeps the partial in context, tagged."
  (let* ((entries-provider (agent-loop-entries-provider context))
         (content (model-response-content response))
         (tool-calls (model-response-tool-calls response))
         (thinking-blocks (getf (model-response-metadata response)
                                :thinking-blocks))
         (metadata (append (list :tool-calls tool-calls
                                 :stop-reason (model-response-stop-reason response))
                           (when tool-calls
                             (list :tool-call-presentations
                                   (assistant-tool-call-presentations tool-calls
                                                                      context)))
                           (when thinking-blocks
                             (list :thinking-blocks thinking-blocks)))))
    (when (or (plusp (length content)) tool-calls)
      (append-agent-message
       agent
       (provider-call entries-provider
                      :make-assistant-message
                      content
                      :metadata metadata)
       context
       :kind :assistant
       :metadata metadata))))

(defun drain-steering-into-session (agent context)
  (let ((messages (funcall (agent-loop-behavior-steering-delivery
                            (agent-behavior agent))
                           (agent-steering-queue agent))))
    (when messages
      (append-agent-input-messages agent messages :steer context)
      t)))

(defun drain-follow-up-into-session (agent context)
  (let ((messages (funcall (agent-loop-behavior-follow-up-delivery
                            (agent-behavior agent))
                           (agent-follow-up-queue agent))))
    (when messages
      (append-agent-input-messages agent messages :follow-up context)
      t)))

(defun apply-in-flight-context-edits (agent context)
  "Canonical :apply in-flight-context-edit policy: rebuild the context
projection before each model turn, so results appended during the run and
any in-flight committed edits enter the next request."
  (provider-call (agent-loop-context-lens-provider context)
                 :rebuild-context-projection
                 (agent-context agent)))
