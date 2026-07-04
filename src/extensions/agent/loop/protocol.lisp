(in-package #:kli/agent/loop)

(defgeneric register-agent (service agent context))
(defgeneric deregister-agent (service agent context))
(defgeneric prompt-agent (agent messages context))
(defgeneric continue-agent (agent context))
(defgeneric steer-agent (agent message context))
(defgeneric queue-agent-steer (agent message context))
(defgeneric follow-up-agent (agent message context))
(defgeneric abort-agent (agent context))
(defgeneric run-agent-loop (agent context))
(defgeneric run-one-agent-turn (agent context)
  (:documentation "Run a single model turn, returning :continue when the loop
should run another turn or NIL when it should stop. A generic function so a
behavior extension can wrap the per-turn continuation decision via :around."))
(defgeneric execute-agent-tool-call (agent tool-call context))
(defgeneric agent-idle-p (agent))
(defgeneric agent-active-p (agent))
(defgeneric drain-pending-agent-work (agent context))
(defgeneric inspect-agent (agent))
(defgeneric inspect-agent-turn (turn))
(defgeneric inspect-tool-execution (execution))

(defmethod register-agent ((service agent-loop-service)
                           (agent agent)
                           context)
  (setf (gethash (object-id agent) (agent-loop-service-agents service))
        agent)
  (register-agent-live-object context agent)
  (register-agent-live-object context (agent-steering-queue agent))
  (register-agent-live-object context (agent-follow-up-queue agent))
  agent)

(defmethod deregister-agent ((service agent-loop-service)
                             (agent agent)
                             context)
  (let ((registry (context-registry context)))
    (remhash (object-id agent) (agent-loop-service-agents service))
    (dolist (turn (agent-turns agent))
      (dolist (tool-call (agent-turn-tool-calls turn))
        (remove-live-object registry (object-id tool-call)))
      (remove-live-object registry (object-id turn)))
    (dolist (execution (agent-tool-executions agent))
      (dolist (update (tool-execution-updates execution))
        (remove-live-object registry (object-id update)))
      (remove-live-object registry (object-id execution)))
    (sb-thread:with-mutex ((agent-loop-runs-lock agent))
      (setf (agent-loop-runs agent)
            (remove-if (lambda (run)
                         (unless (eq (agent-run-state run) :running)
                           (remove-live-object registry (object-id run))
                           t))
                       (agent-loop-runs agent))))
    (remove-live-object registry (object-id (agent-steering-queue agent)))
    (remove-live-object registry (object-id (agent-follow-up-queue agent)))
    (remove-live-object registry (object-id agent)))
  agent)

(defmethod agent-idle-p ((agent agent))
  (eq (agent-state-value (agent-state agent)) :idle))

(defmethod agent-active-p ((agent agent))
  "True while a turn is in flight (:running, :streaming, :tooling) or the session layer is backing off before a retry (:retrying) or compacting the session (:compacting). :idle, :aborted, and :error are inactive. Unlike (not (agent-idle-p ...)) a just-aborted agent is not reported busy. :retrying and :compacting count as active so those windows still steer submits into the queue and keep the busy indicator live."
  (and (member (agent-state-value (agent-state agent))
               '(:running :streaming :tooling :retrying :compacting))
       t))

(defmethod drain-pending-agent-work ((agent agent) context)
  "Run steering or follow-up messages that queued while the agent sat outside
its loop -- e.g. during a session compaction, when submits steer into the queue
with no turn boundary left to drain them. No-op unless the agent is idle."
  (when (agent-idle-p agent)
    (when (or (drain-steering-into-session agent context)
              (drain-follow-up-into-session agent context))
      (run-agent-loop agent context)))
  agent)

(defmethod prompt-agent ((agent agent) messages context)
  (setf (agent-abort-requested-p agent) nil)
  (append-agent-input-messages agent messages :prompt context)
  (emit-agent-event agent context :agent/start)
  (run-agent-loop agent context))

(defmethod continue-agent ((agent agent) context)
  (setf (agent-abort-requested-p agent) nil)
  (run-agent-loop agent context))

(defmethod queue-agent-steer ((agent agent) message context)
  "Enqueue MESSAGE for the next steering boundary without ever running the
loop on this thread. Callers that must not run a turn synchronously (the TUI
loop thread) pair this with drain-pending-agent-work on a worker, while
steer-agent keeps the synchronous idle catch-up."
  (enqueue-agent-item (agent-steering-queue agent) message)
  (emit-agent-event agent
                    context
                    :agent/steer
                    :payload (list :queued (length (agent-queue-items
                                                    (agent-steering-queue
                                                     agent)))))
  agent)

(defmethod steer-agent ((agent agent) message context)
  (queue-agent-steer agent message context)
  (when (agent-idle-p agent)
    (drain-steering-into-session agent context)
    (run-agent-loop agent context))
  agent)

 (defvar *defer-agent-turn-p* nil
   "Bound true around a dispatch path whose own caller drains queued turns
 after acknowledging the command, so follow-up-agent leaves the message
  enqueued and returns without running the loop on this thread. The TUI's
  slash-command path binds this so :command/result is visible before any
  follow-up model output; headless paths never bind it and keep the
  synchronous idle catch-up.")
 
(defmethod follow-up-agent ((agent agent) message context)
  (enqueue-agent-item (agent-follow-up-queue agent) message)
  (emit-agent-event agent
                    context
                    :agent/follow-up
                    :payload (list :queued (length (agent-queue-items
                                                    (agent-follow-up-queue
                                                     agent)))))
  (when (and (agent-idle-p agent)
             (not *defer-agent-turn-p*))
    (drain-follow-up-into-session agent context)
    (run-agent-loop agent context))
  agent)

(defmethod abort-agent ((agent agent) context)
  (setf (agent-abort-requested-p agent) t)
  (let* ((turn (agent-current-turn agent))
         (request (and turn (agent-turn-request turn))))
    (when request
      (provider-call (agent-loop-runtime-provider context)
                     :abort-model-request request context)))
  (set-agent-state agent :aborted :reason :abort-requested)
  (emit-agent-event agent context :agent/aborted)
  agent)

(defun make-agent-model-request (agent sealed-context context)
  (provider-call (agent-loop-runtime-provider context)
                 :make-model-request
                 (agent-model-runtime agent)
                 (agent-model-selection agent)
                 sealed-context
                 context
                 :actor (agent-principal agent)
                 :instructions (agent-system-prompt agent)
                 :session-id (session-uuid (agent-session agent))))

(defun turn-context-extras (agent)
  "This turn's ephemeral context messages, recomputed fresh and never persisted."
  (let ((extras-fn (agent-context-extras-fn agent)))
    (and extras-fn (funcall extras-fn))))

(defun run-agent-model-turn (agent turn context)
  (let* ((lens-provider (agent-loop-context-lens-provider context))
         (runtime-provider (agent-loop-runtime-provider context)))
    (funcall (agent-loop-behavior-in-flight-context-edit (agent-behavior agent))
             agent
             context)
    (let* ((sealed-context (provider-call lens-provider
                                          :seal-context-projection
                                          (agent-context agent)
                                          context
                                          :extra-messages
                                          (turn-context-extras agent)))
           (request (make-agent-model-request agent sealed-context context)))
      (setf (agent-turn-sealed-context turn) sealed-context
            (agent-turn-request turn) request)
      (set-agent-state agent :streaming)
      (emit-agent-event agent
                        context
                        :agent/message-start
                        :payload (list :turn-id (object-id turn)
                                       :request-id (object-id request)))
      (when (agent-abort-requested-p agent)
        (abort-agent agent context)
        (return-from run-agent-model-turn nil))
      (let* ((reasoning-effort (kli/model/registry:model-selection-option-value
                              (agent-model-selection agent)
                              "reasoning-effort"))
             (on-delta (lambda (delta)
                         (typecase delta
                           (assistant-delta
                            (emit-agent-event
                             agent
                             context
                             :agent/delta
                             :payload (list :turn-id (object-id turn)
                                            :request-id (object-id request)
                                            :text (assistant-delta-text
                                                   delta))))
                           (thinking-delta
                            (emit-agent-event
                             agent
                             context
                             :agent/thinking-delta
                             :payload (list :turn-id (object-id turn)
                                            :request-id (object-id request)
                                            :text (thinking-delta-text delta)
                                            :level reasoning-effort)))
                           (usage-delta
                            (emit-agent-event
                             agent
                             context
                             :agent/usage
                             :payload (list :turn-id (object-id turn)
                                            :request-id (object-id request)
                                            :usage (usage-delta-usage delta)))))))
             (response (provider-call runtime-provider
                                      :stream-model-response
                                      (model-request-provider request)
                                      request
                                      context
                                      :on-delta on-delta)))
        (setf (agent-turn-response turn) response)
        (append-assistant-response agent response context)
        (emit-agent-event agent
                          context
                          :agent/message-end
                          :payload (list :turn-id (object-id turn)
                                         :request-id (object-id request)
                                         :stop-reason (model-response-stop-reason
                                                       response)))
        response))))

(defun register-response-tool-calls (turn response context)
  (loop for provider-tool-call in (model-response-tool-calls response)
        for tool-call = (tool-call-from-provider-plist provider-tool-call)
        collect (register-tool-call turn tool-call context)))

(defun execute-tool-calls-serially (agent turn tool-calls context)
  "Canonical :serial tool-scheduling policy: one call at a time, checking
for abort and queued steering between calls. Queued steering skips the
remaining calls and delivers at the boundary."
  (loop for (tool-call . remaining-tool-calls) on tool-calls
        do
    (execute-agent-tool-call agent tool-call context)
    (cond
      ((agent-abort-requested-p agent)
       (abort-agent-turn agent turn context)
       (return-from execute-tool-calls-serially :aborted))
      ((not (queue-empty-p (agent-steering-queue agent)))
       (skip-agent-tool-calls agent remaining-tool-calls context)
       (drain-steering-into-session agent context)
       (finish-agent-turn agent turn :steered context)
       (return-from execute-tool-calls-serially :continue)))))

(defun execute-response-tool-calls (agent turn response context)
  (set-agent-state agent :tooling)
  (funcall (agent-loop-behavior-tool-scheduling (agent-behavior agent))
           agent
           turn
           (register-response-tool-calls turn response context)
           context))

(defvar *agent-error-supervised-p* nil
  "Bound true while a supervisor (the session retry loop) will observe the
agent's error after the prompt returns and either retry it or re-signal it
into a visible channel. Stamped into the :agent/error payload so renderers
skip supervised errors -- they would otherwise show twice -- while errors on
unsupervised paths (steer drains, worker catch-ups) still surface.")

(defmethod run-one-agent-turn ((agent agent) context)
  "Run a single model turn. When tool calls executed, loops back so the model reacts to their results before the turn completes. A steer queued during a tool-less turn has no tool-call boundary to drain at, so it is consumed here to run as the next turn rather than stranding in the queue. A stream torn down by abort surfaces as a parse or stream error -- that is the unwind, not a failure, and run-agent-loop's epilogue sets the :aborted run state and emits :agent/aborted."
  (let ((turn (make-agent-turn)))
    (register-agent-turn agent turn context)
    (set-agent-state agent :running)
    (emit-agent-event agent
                      context
                      :agent/turn-start
                      :payload (list :turn-id (object-id turn)))
    (handler-case
        (let ((response (run-agent-model-turn agent turn context)))
          (cond
            ((agent-abort-requested-p agent)
             (when response
               (register-response-tool-calls turn response context))
             (abort-agent-turn agent turn context)
             nil)
            (response
             (let ((tool-status (execute-response-tool-calls agent
                                                             turn
                                                             response
                                                             context)))
               (case tool-status
                 (:continue :continue)
                 (:aborted nil)
                 (otherwise
                  (cond
                    ((model-response-tool-calls response)
                     (finish-agent-turn agent turn :completed context)
                     :continue)
                    ((drain-steering-into-session agent context)
                     (finish-agent-turn agent turn :steered context)
                     :continue)
                    ((drain-follow-up-into-session agent context)
                     (finish-agent-turn agent turn :follow-up context)
                     :continue)
                    (t
                     (finish-agent-turn agent turn :completed context)
                     nil))))))
            (t
             (abort-agent-turn agent turn context)
             nil)))
      (error (condition)
        (cond
          ((agent-abort-requested-p agent)
           (abort-agent-turn agent turn context)
           nil)
          (t
           (setf (agent-last-error agent) condition
                 (agent-turn-error turn) condition)
           (set-agent-state agent :error :reason condition)
           (finish-agent-turn agent turn :error context)
           ;; Events are data: the payload carries the printed condition,
           ;; while the in-memory agent-last-error slot keeps the raw
           ;; condition for inspect-agent.
           (emit-agent-event agent
                             context
                             :agent/error
                             :payload (list :turn-id (object-id turn)
                                            :category
                                            (kli/ext:condition-category
                                             condition)
                                            :supervised
                                            (and *agent-error-supervised-p* t)
                                            :condition
                                            (princ-to-string condition)))
           nil))))))

(defmethod run-agent-loop ((agent agent) context)
  (let ((run (make-agent-run agent))
        (continue-p t)
        (turn-count 0))
    (register-agent-live-object context run)
    (sb-thread:with-mutex ((agent-loop-runs-lock agent))
      (push run (agent-loop-runs agent)))
    (loop while (and continue-p
                     (not (agent-abort-requested-p agent)))
          do (incf turn-count)
             (setf continue-p (run-one-agent-turn agent context)))
    (setf (agent-run-turns run)
          (subseq (agent-turns agent) 0 (min turn-count
                                             (length (agent-turns agent))))
          (agent-run-completed-at run)
          (get-universal-time))
    (cond
      ((agent-abort-requested-p agent)
       (set-agent-state agent :aborted :reason :abort-requested)
       (setf (agent-run-state run) :aborted)
       (emit-agent-event agent context :agent/aborted))
      ((not (eq (agent-state-value (agent-state agent)) :error))
       (set-agent-state agent :idle)
       (setf (agent-run-state run) :completed)))
    (emit-agent-event agent
                      context
                      :agent/end
                      :payload (let ((base (list :run-id (object-id run)
                                                 :state (agent-run-state run)
                                                 :text (run-final-response-text run)))
                                     (usage (latest-turn-usage
                                             (agent-run-turns run))))
                                 (payload-with-duration
                                  (if usage
                                      (append base (list :usage usage))
                                      base)
                                  (agent-run-started-at-real run))))
    ;; A steer or follow-up can land after the final turn's boundary drain
    ;; but before the :idle transition above -- no boundary remains to
    ;; deliver it. Check the queues once more on the way out; this no-ops
    ;; unless the agent settled at :idle, so aborted and errored runs never
    ;; auto-run queued work.
    (drain-pending-agent-work agent context)
    ;; Second value is this run's final assistant text; drain never mutates `run'.
    (values agent (run-final-response-text run))))

(defun feed-back-error-tool-result (agent tool-call result context)
  "Canonical :feed-back error-to-tool-result policy: record the error result
unchanged so the model receives it and can self-correct."
  (declare (ignore agent tool-call context))
  result)

(defun arguments-parse-error-result (parse-error)
  "Error result for a tool call whose arguments JSON failed to parse.
Invoking the tool with the silently-empty fallback arguments would
misreport the failure as a missing required parameter, so the result
names the real cause for the model to correct."
  (make-tool-result
   :content (list (make-tool-text-content
                   (format nil "Tool call arguments were not valid JSON: ~A"
                           parse-error)))
   :details (list :reason :arguments-parse-error)
   :error-p t))

(defun agent-call-subject (agent protocol)
  "The subject a tool call runs under: AGENT's principal projected from
PROTOCOL's grant-set when it records one, else AGENT's own subject slot. Routes
delegated authority through the single snapshot-able grant-set without changing
how an un-delegated agent is gated."
  (let ((principal (agent-principal agent)))
    (if (and principal (grant-set-has-p protocol principal))
        (principal-subject protocol principal)
        (agent-subject agent))))

(defun seed-agent-principal-grant (agent protocol context)
  "Record AGENT's own configured authority in PROTOCOL's grant-set under its
principal, as a reversible grant-contribution. This makes agent-call-subject
project through the snapshot-able grant-set rather than the slot fallback,
without widening authority: the seeded grant equals the configured subject, so
an un-delegated agent gates identically. No-op when no principal or protocol."
  (let ((principal (agent-principal agent)))
    (when (and principal protocol)
      (install-contribution
       protocol
       (make-grant-contribution :principal principal
                                :grant (subject-grant (agent-subject agent))
                                :source :agent/principal)
       context))))

(defmethod execute-agent-tool-call ((agent agent) (tool-call tool-call) context)
  (let* ((protocol (or (active-protocol context)
                       (error "No active protocol is installed.")))
         (execution (make-tool-execution tool-call))
         (tool (kli/ext:find-tool protocol (agent-tool-call-name tool-call)))
         (arguments (agent-tool-call-arguments tool-call))
         (call-term (if tool
                        (or (ignore-errors (kli/ext:present-call tool arguments))
                            (kli/ext:default-call-term arguments))
                        (kli/ext:default-call-term arguments)))
         (*call-subject* (agent-call-subject agent protocol))
         (kli/ext:*tool-abort-predicate*
          (lambda () (agent-abort-requested-p agent))))
    (register-tool-execution agent execution context)
    (setf (agent-tool-call-state tool-call) :running
          (tool-execution-state execution) :running)
    (emit-agent-event agent
                      context
                      :agent/tool-execution-start
                      :payload (list :execution-id (object-id execution)
                                     :tool-call-id
                                     (agent-tool-call-id tool-call)
                                     :tool-name
                                     (agent-tool-call-name tool-call)
                                     :arguments
                                     arguments
                                     :call-term call-term))
    (let* ((parse-error (getf (agent-tool-call-metadata tool-call)
                              :arguments-parse-error))
           (result
            (if parse-error
                (arguments-parse-error-result parse-error)
                (handler-case
                    (invoke-tool protocol
                                 (agent-tool-call-name tool-call)
                                 (agent-tool-call-arguments tool-call)
                                 context
                                 :call-id (agent-tool-call-id tool-call)
                                 :on-update (lambda (payload)
                                              (kli/ext:safely-invoke
                                               #'record-tool-update
                                               :tool-update
                                               (agent-tool-call-id tool-call)
                                               agent execution payload context)))
                  (kli/ext:capability-denied (condition)
                    (make-tool-result
                     :content (list (make-tool-text-content
                                     (format nil "~A" condition)))
                     :details (list :condition-type
                                    (princ-to-string (type-of condition))
                                    :capability
                                    (kli/ext:capability-denied-capability
                                     condition))
                     :error-p t))
                  ;; The tool_use is already in the session log; a condition
                  ;; escaping here -- an unknown (hallucinated) tool name being
                  ;; the live case -- leaves it dangling with no tool_result,
                  ;; and providers reject every subsequent request over that
                  ;; context. Every failure must come back as an error result
                  ;; the model can react to.
                  (error (condition)
                    (tool-error-result condition)))))
           (result (if (tool-result-error-p result)
                       (funcall (agent-loop-behavior-error-to-tool-result
                                 (agent-behavior agent))
                                agent tool-call result context)
                       result)))
      (setf (tool-execution-result execution) result
            (tool-execution-state execution) :completed
            (tool-execution-completed-at execution) (get-universal-time)
            (agent-tool-call-state tool-call)
            (if (tool-result-error-p result) :error :completed))
      (let ((result-term (if tool
                             (or (ignore-errors (kli/ext:present-result tool result))
                                 (kli/ext:result-box))
                             (kli/ext:result-box))))
        (append-tool-result-message agent tool-call result context
                                    :presentation result-term)
        (emit-agent-event agent
                          context
                          :agent/tool-execution-end
                          :payload (payload-with-duration
                                    (list :execution-id (object-id execution)
                                          :tool-call-id
                                          (agent-tool-call-id tool-call)
                                          :error-p
                                          (tool-result-error-p result)
                                          :result-text
                                          (tool-result-text result)
                                          :details
                                          (tool-result-details result)
                                          :result-term result-term)
                                    (tool-execution-started-at-real execution))))
      execution)))

(defmethod inspect-agent ((agent agent))
  (list :id (object-id agent)
        :session-id (object-id (agent-session agent))
        :state (agent-state-value (agent-state agent))
        :turn-count (length (agent-turns agent))
        :tool-execution-count (length (agent-tool-executions agent))
        :steering-queued (length (agent-queue-items
                                  (agent-steering-queue agent)))
        :follow-up-queued (length (agent-queue-items
                                   (agent-follow-up-queue agent)))
        :abort-requested-p (agent-abort-requested-p agent)
        :behavior (behavior-state (agent-behavior agent))
        :last-error (agent-last-error agent)))

(defmethod inspect-agent-turn ((turn agent-turn))
  (list :id (object-id turn)
        :state (agent-turn-state turn)
        :request-id (and (agent-turn-request turn)
                         (object-id (agent-turn-request turn)))
        :response-id (and (agent-turn-response turn)
                          (object-id (agent-turn-response turn)))
        :tool-call-ids (mapcar #'object-id (reverse (agent-turn-tool-calls
                                                     turn)))
        :error (agent-turn-error turn)))

(defmethod inspect-tool-execution ((execution tool-execution))
  (let ((call (tool-execution-call execution)))
    (list :id (object-id execution)
          :state (tool-execution-state execution)
          :tool-call-id (agent-tool-call-id call)
          :tool-name (agent-tool-call-name call)
          :error-p (and (tool-execution-result execution)
                        (tool-result-error-p
                         (tool-execution-result execution)))
          :updates (mapcar #'object-id
                           (reverse (tool-execution-updates execution))))))
