(in-package #:kli/tests)

(defvar *agent-loop-current-agent* nil)
(defvar *agent-loop-reply-selection* nil)
(defvar *agent-loop-steering-used-p* nil)
(defvar *agent-loop-first-tool-runs* 0)
(defvar *agent-loop-second-tool-runs* 0)

(defun run-agent-loop-echo-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool context call-id on-update))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content
                   (or (ext:tool-parameter parameters :message) "")))))

(defun run-agent-loop-echo-and-swap-tool (tool parameters context
                                          &key call-id on-update)
  "Echo, then swap the agent onto *agent-loop-reply-selection* so the next turn
returns plain text and the loop terminates instead of re-emitting a tool call."
  (declare (ignore tool context call-id on-update))
  (when (and *agent-loop-current-agent* *agent-loop-reply-selection*)
    (setf (agents:agent-model-selection *agent-loop-current-agent*)
          *agent-loop-reply-selection*))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content
                   (or (ext:tool-parameter parameters :message) "")))))

(defun run-agent-loop-steer-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool parameters call-id on-update))
  (incf *agent-loop-first-tool-runs*)
  (unless *agent-loop-steering-used-p*
    (setf *agent-loop-steering-used-p* t
          (agents:agent-model-selection *agent-loop-current-agent*)
          *agent-loop-reply-selection*)
    (agents:steer-agent *agent-loop-current-agent*
                        "steered while tooling"
                        context))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "first tool done"))))

(defun run-agent-loop-second-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool parameters context call-id on-update))
  (incf *agent-loop-second-tool-runs*)
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "second tool done"))))

(defun run-agent-loop-abort-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool parameters call-id on-update))
  (agents:abort-agent *agent-loop-current-agent* context)
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "aborted"))))

(defparameter *agent-loop-details-fixture* '(:path "/tmp/x" :old "a" :new "b"))

(defun run-agent-loop-details-tool (tool parameters context &key call-id on-update)
  "Echo with a structured details plist, then swap to the reply selection so
the loop terminates."
  (declare (ignore tool parameters context call-id on-update))
  (when (and *agent-loop-current-agent* *agent-loop-reply-selection*)
    (setf (agents:agent-model-selection *agent-loop-current-agent*)
          *agent-loop-reply-selection*))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "edited"))
   :details *agent-loop-details-fixture*))

(defun run-agent-loop-explode-tool (tool parameters context &key call-id on-update)
  "Swap to the reply selection, then signal a plain error: the loop must hand
the error result back to the model rather than kill the turn."
  (declare (ignore tool parameters context call-id on-update))
  (when (and *agent-loop-current-agent* *agent-loop-reply-selection*)
    (setf (agents:agent-model-selection *agent-loop-current-agent*)
          *agent-loop-reply-selection*))
  (error "tool exploded mid-call"))

(defun run-agent-loop-double-steer-tool (tool parameters context
                                         &key call-id on-update)
  "Queue two steering messages during tool execution, then swap to the reply
selection so later turns return plain text."
  (declare (ignore tool parameters call-id on-update))
  (setf (agents:agent-model-selection *agent-loop-current-agent*)
        *agent-loop-reply-selection*)
  (agents:steer-agent *agent-loop-current-agent* "steer one" context)
  (agents:steer-agent *agent-loop-current-agent* "steer two" context)
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "double steered"))))

(defun run-agent-loop-observe-abort-tool (tool parameters context
                                          &key call-id on-update)
  (declare (ignore tool parameters context call-id on-update))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content
                   (if (ext:tool-abort-requested-p)
                       "abort-requested"
                       "no-abort")))))

(ext:defextension agent-loop-test-tools
  (:provides
   (tool agent-echo
     :label "Agent Echo"
     :description "Echo a message for agent-loop tests."
     :parameters '(:object (:message :string))
     :runner #'run-agent-loop-echo-tool)
   (tool agent-echo-swap
     :label "Agent Echo Swap"
     :description "Echo, then swap to the reply selection so the loop terminates."
     :parameters '(:object (:message :string))
     :runner #'run-agent-loop-echo-and-swap-tool)
   (tool agent-steer
     :label "Agent Steer"
     :description "Inject steering during tool execution."
     :runner #'run-agent-loop-steer-tool)
   (tool agent-second
     :label "Agent Second"
     :description "Fails the steering skip test if invoked."
     :runner #'run-agent-loop-second-tool)
   (tool agent-abort
     :label "Agent Abort"
     :description "Requests agent abort during tool execution."
     :runner #'run-agent-loop-abort-tool)
   (tool agent-details
     :label "Agent Details"
     :description "Returns a result carrying a structured details plist."
     :runner #'run-agent-loop-details-tool)
   (tool agent-explode
     :label "Agent Explode"
     :description "Swaps to the reply selection, then signals a plain error."
     :runner #'run-agent-loop-explode-tool)
   (tool agent-double-steer
     :label "Agent Double Steer"
     :description "Queues two steering messages during tool execution."
     :runner #'run-agent-loop-double-steer-tool)
   (tool agent-observe-abort
     :label "Agent Observe Abort"
     :description "Reports whether the tool abort predicate fires."
     :runner #'run-agent-loop-observe-abort-tool)))

(defun agent-loop-provider (protocol)
  (ext:require-capability-provider protocol
                                   :agent/loop
                                   :contract :agent/loop/v1))

(defun agent-loop-service (context)
  (kli:find-live-object (kli:context-registry context)
                        :agent-loop-service))

(defun agent-loop-test-context (&key tools)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*
                        agents:*agent-loop-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (when tools
      (install-extension context *agent-loop-test-tools-extension-manifest*))
    (values context protocol)))

(defun make-agent-loop-session-agent (context selection &key id behavior)
  (let* ((store (session-log-store context))
         (session (sess:create-session store context :id (gensym "AGENT-SESSION-")))
         (agent-context (ctx:make-agent-context session store context))
         (agent (agents:make-agent session
                                   store
                                   agent-context
                                   selection
                                   (model-runtime-service context)
                                   :id id
                                   :behavior behavior)))
    (agents:register-agent (agent-loop-service context) agent context)))

(defun make-agent-loop-file-store-agent (context selection root &key id subject)
  "Agent whose session lives in a file store under ROOT, so every appended
entry passes through the durable serializer synchronously."
  (let* ((store (sess:make-file-session-store root
                                              :id (gensym "FILE-SESSION-STORE-")))
         (session (sess:create-session store context :id (gensym "AGENT-SESSION-")))
         (agent-context (ctx:make-agent-context session store context))
         (agent (agents:make-agent session
                                   store
                                   agent-context
                                   selection
                                   (model-runtime-service context)
                                   :id id
                                   :subject subject)))
    (agents:register-agent (agent-loop-service context) agent context)))

(defun agent-loop-register-model (context provider-id model-id &key metadata
                                          option-schemas options)
  (multiple-value-bind (_provider _model selection)
      (register-runtime-model context
                              provider-id
                              model-id
                              :auth-required-p nil
                              :metadata metadata
                              :option-schemas option-schemas
                              :options options)
    (declare (ignore _provider _model))
    selection))

(defun agent-session-messages (agent)
  (sess:session-context-messages
   (sess:build-session-context (agents:agent-store agent)
                               (agents:agent-session agent))))

(defun agent-session-message-roles (agent)
  (mapcar #'sess:message-role (agent-session-messages agent)))

(defun agent-session-message-contents (agent)
  (mapcar #'sess:message-content (agent-session-messages agent)))

(defun agent-wire-items (agent)
  "AGENT's current session rendered to the provider-wire item vector."
  (transports:convert-responses-input
   (mapcar #'rt::convert-agent-message
           (agent-session-messages agent))))

(defun wire-call-ids-of-type (wire type)
  (loop for i below (length wire)
        for item = (aref wire i)
        when (string= type (gethash "type" item))
          collect (princ-to-string (gethash "call_id" item))))

(defun wire-tool-calls-balanced-p (wire)
  "Every function_call carries a matching function_call_output -- the balance a
provider requires. NIL when there are no calls, so it cannot pass vacuously."
  (let ((calls (sort (wire-call-ids-of-type wire "function_call") #'string<))
        (outputs (sort (wire-call-ids-of-type wire "function_call_output")
                       #'string<)))
    (and calls (equal calls outputs))))

(test agent-loop-registers-provider-service-and-agent
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*)
    (signals error
      (install-extension context agents:*agent-loop-extension-manifest*)))
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (let* ((selection (agent-loop-register-model context
                                                 "agent-provider"
                                                 "agent-model"))
           (agent (make-agent-loop-session-agent context selection :id :agent-1)))
      (is (typep (agent-loop-service context) 'agents:agent-loop-service))
      (is (agent-loop-provider protocol))
      (is (eq agent
              (gethash :agent-1
                       (agents:agent-loop-service-agents
                        (agent-loop-service context))))))))

(test agent-loop-retrying-counts-as-active
  "A session-layer backoff window must read as busy so submits steer into
the queue and Esc-abort stays live."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model context
                                                 "retry-provider"
                                                 "retry-model"))
           (agent (make-agent-loop-session-agent context selection)))
      (is (not (agents:agent-active-p agent)))
      (agents:set-agent-state agent :retrying)
      (is (eq t (agents:agent-active-p agent)))
      (agents:set-agent-state agent :aborted)
      (is (not (agents:agent-active-p agent))))))

(test (agent-loop-prompt-streams-fake-assistant :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "prompt-provider"
                       "prompt-model"
                       :metadata '(:fake-deltas ("hello agent"))))
           (agent (make-agent-loop-session-agent context selection)))
      (agents:prompt-agent agent "hello" context)
      (is (agents:agent-idle-p agent))
      (is (= 1 (length (agents:agent-turns agent))))
      (is (equal '(:user :assistant)
                 (agent-session-message-roles agent)))
      (is (equal '("hello" "hello agent")
                 (agent-session-message-contents agent))))))

(test (agent-loop-threads-thinking-blocks-into-message-metadata :fixture interactive-authority)
  "Collected thinking blocks ride response metadata into the assistant
message metadata and surface on the converted model-message plist, the same
round-trip :tool-calls takes. Without thinking the key is absent."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "thinking-agent-provider"
                       "thinking-agent-model"
                       :metadata '(:fake-blocks
                                   ((:kind :thinking :text ("Pondering."))
                                    (:kind :text :text ("Done."))))))
           (agent (make-agent-loop-session-agent context selection)))
      (agents:prompt-agent agent "think" context)
      (let* ((assistant (find :assistant (agent-session-messages agent)
                              :key #'sess:message-role))
             (blocks (getf (sess:message-metadata assistant)
                           :thinking-blocks)))
        (is (equal '((:thinking "Pondering." :signature nil :redacted nil))
                   blocks))
        (is (equal blocks
                   (getf (rt::convert-agent-message assistant)
                         :thinking-blocks)))))
    (let* ((selection (agent-loop-register-model
                       context
                       "plain-agent-provider"
                       "plain-agent-model"
                       :metadata '(:fake-deltas ("no thinking"))))
           (agent (make-agent-loop-session-agent context selection)))
      (agents:prompt-agent agent "plain" context)
      (let ((assistant (find :assistant (agent-session-messages agent)
                             :key #'sess:message-role)))
        (is (eq :missing (getf (sess:message-metadata assistant)
                               :thinking-blocks :missing)))))))

(test (agent-loop-tool-call-invokes-tool-and-appends-result :fixture interactive-authority)
  "The trailing :assistant proves the loop re-invoked the model with the tool
result rather than stopping after the tool call."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "tool-provider"
                       "tool-model"
                       :metadata '(:fake-deltas ("calling tool")
                                   :fake-tool-call
                                   (:id :call-echo
                                    :name :agent-echo-swap
                                    :arguments (:partial-json "{\"message\":\"tool ok\"}")))))
           (reply-selection (agent-loop-register-model
                             context
                             "tool-reply-provider"
                             "tool-reply-model"
                             :metadata '(:fake-deltas ("all done"))))
           (agent (make-agent-loop-session-agent context selection)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context)
        (is (agents:agent-idle-p agent))
        (is (= 1 (length (agents:agent-tool-executions agent))))
        (is (= 2 (length (agents:agent-turns agent))))
        (is (equal '(:user :assistant :tool-result :assistant)
                   (agent-session-message-roles agent)))
        (is (equal "tool ok"
                   (third (agent-session-message-contents agent))))
        (is (equal "all done"
                   (car (last (agent-session-message-contents agent)))))))))

(test agent-loop-unknown-tool-name-feeds-back-error-result
  "A hallucinated tool name must come back as an :error-p tool result with a
tool_result message appended, not escape and kill the turn. The tool_use is
already in the session log when execution starts, so a turn dying there
leaves it dangling with no tool_result -- providers reject every subsequent
request over that context, wedging the session."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model context
                                                 "ghost-provider"
                                                 "ghost-model"))
           (agent (make-agent-loop-session-agent context selection))
           (execution (agents:execute-agent-tool-call
                       agent
                       (agents:make-tool-call :no-such-tool '()
                                              :call-id "call-ghost")
                       context))
           (result (agents:tool-execution-result execution)))
      (is (ext:tool-result-error-p result))
      (is (stringp (getf (ext:tool-result-details result) :condition-type)))
      (is (eq :completed (agents:tool-execution-state execution)))
      (is (equal '(:tool-result) (agent-session-message-roles agent))
          "the error still produced a tool_result message")
      (is (search "No tool named"
                  (first (agent-session-message-contents agent)))))))

(test agent-loop-malformed-tool-arguments-feed-back-parse-error
  "Tool-call arguments that fail to parse as JSON must come back as an
:error-p tool result naming the JSON failure. Running the tool with the
silently-empty fallback arguments would misreport the real cause as a
missing required parameter."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model context
                                                 "garbled-provider"
                                                 "garbled-model"))
           (agent (make-agent-loop-session-agent context selection))
           (tool-call (agents::tool-call-from-provider-plist
                       '(:id "call-garbled"
                         :name :agent-echo-swap
                         :arguments-json "{\"message\": tr")))
           (execution (agents:execute-agent-tool-call agent tool-call context))
           (result (agents:tool-execution-result execution)))
      (is (ext:tool-result-error-p result))
      (is (eq :arguments-parse-error
              (getf (ext:tool-result-details result) :reason)))
      (is (eq :completed (agents:tool-execution-state execution)))
      (is (equal '(:tool-result) (agent-session-message-roles agent))
          "the parse error still produced a tool_result message")
      (is (search "not valid JSON"
                  (first (agent-session-message-contents agent)))))))

(test agent-loop-binds-the-tool-abort-predicate
  "execute-agent-tool-call must bind the tool abort predicate to the
agent's abort flag, so long-running tool runners can poll it inside their
wait and walk loops and exit early -- abort is otherwise only checked
between tool calls."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model context
                                                 "abort-observer-provider"
                                                 "abort-observer-model"))
           (agent (make-agent-loop-session-agent context selection)))
      (flet ((observed ()
               (let* ((execution (agents:execute-agent-tool-call
                                  agent
                                  (agents:make-tool-call
                                   :agent-observe-abort '()
                                   :call-id (string (gensym "CALL-OBSERVE-")))
                                  context))
                      (result (agents:tool-execution-result execution)))
                 (getf (first (ext:tool-result-content result)) :text))))
        (is (string= "no-abort" (observed))
            "without an abort request the predicate stays false")
        (setf (agents:agent-abort-requested-p agent) t)
        (is (string= "abort-requested" (observed))
            "the runner sees the agent's abort flag through the predicate")))))

(test (agent-loop-tool-error-result-survives-durable-store :fixture interactive-authority)
  "A non-capability-denied tool error must reach the model as an :error-p tool
result, not kill the turn: on a durable store the result is serialized
synchronously on append, so its :details must be durably representable. The
trailing :assistant proves the loop re-invoked the model with the error
result."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "explode-provider"
                       "explode-model"
                       :metadata '(:fake-deltas ("calling exploder")
                                   :fake-tool-call
                                   (:id :call-explode
                                    :name :agent-explode
                                    :arguments (:partial-json "{}")))))
           (reply-selection (agent-loop-register-model
                             context
                             "explode-reply-provider"
                             "explode-reply-model"
                             :metadata '(:fake-deltas ("recovered"))))
           (root (temp-session-root))
           (agent (make-agent-loop-file-store-agent context selection root)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context))
      (is (agents:agent-idle-p agent))
      (is (equal '(:user :assistant :tool-result :assistant)
                 (agent-session-message-roles agent)))
      (let ((tool-result (find :tool-result (agent-session-messages agent)
                               :key #'sess:message-role)))
        (is (sess:tool-error-p tool-result))
        (is (stringp (getf (getf (sess:message-metadata tool-result) :details)
                           :condition-type))))
      (is (equal "recovered"
                 (car (last (agent-session-message-contents agent)))))
      (let* ((path (sess:session-file-path
                    (agents:agent-store agent)
                    (kli:object-id (agents:agent-session agent))))
             (fresh (sess:make-file-session-store root))
             (loaded (sess:load-session-file fresh path (kli:make-kernel-host))))
        (is (equal '(:user :assistant :tool-result :assistant)
                   (mapcar #'sess:message-role
                           (sess:session-context-messages
                            (sess:build-session-context fresh loaded)))))
        (ignore-errors (delete-file path))))))

(test agent-loop-capability-denied-result-survives-durable-store
  "The capability-denied tool result has the same durable-leaf obligation as
the generic error path: its :details condition type must arrive as a string
while :capability stays a keyword."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (install-extension context *test-gated-tool-extension-manifest*)
    (let* ((selection (agent-loop-register-model context
                                                 "denied-provider"
                                                 "denied-model"))
           (root (temp-session-root))
           (agent (make-agent-loop-file-store-agent
                   context selection root
                   :subject (ext:make-subject :capabilities '())))
           (execution (agents:execute-agent-tool-call
                       agent
                       (agents:make-tool-call :gated '() :call-id "call-gated")
                       context))
           (result (agents:tool-execution-result execution)))
      (is (ext:tool-result-error-p result))
      (let ((details (ext:tool-result-details result)))
        (is (stringp (getf details :condition-type)))
        (is (keywordp (getf details :capability))))
      (let* ((path (sess:session-file-path
                    (agents:agent-store agent)
                    (kli:object-id (agents:agent-session agent))))
             (fresh (sess:make-file-session-store root))
             (loaded (sess:load-session-file fresh path (kli:make-kernel-host))))
        (is (= 1 (length (sess:session-branch fresh loaded nil))))
        (ignore-errors (delete-file path))))))

(defvar *captured-agent-end-events*)

(defun capture-agent-end-handler (event _context)
  (declare (ignore _context))
  (push (event:event-payload event) *captured-agent-end-events*))

(ext:defextension agent-loop-end-capture
  (:requires
   (:capability :events :contract :events/v1))
  (:provides
   (event-handler :capture-agent-end
     :event-type :agent/end
     :handler #'capture-agent-end-handler)))

(defvar *captured-tool-end-events*)

(defun capture-tool-end-handler (event _context)
  (declare (ignore _context))
  (push (event:event-payload event) *captured-tool-end-events*))

(ext:defextension agent-loop-tool-end-capture
  (:requires
   (:capability :events :contract :events/v1))
  (:provides
   (event-handler :capture-tool-end
     :event-type :agent/tool-execution-end
     :handler #'capture-tool-end-handler)))

(test (agent-loop-tool-execution-end-carries-result-details :fixture interactive-authority)
  "The end event payload carries the tool result's details plist alongside the
model-visible result text, so the TUI can render a structured view without the
model seeing it."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (install-extension context *agent-loop-tool-end-capture-extension-manifest*)
    (let* ((selection (agent-loop-register-model
                       context "details-provider" "details-model"
                       :metadata '(:fake-deltas ("calling tool")
                                   :fake-tool-call
                                   (:id :call-details
                                    :name :agent-details
                                    :arguments (:partial-json "{}")))))
           (reply-selection (agent-loop-register-model
                             context "details-reply-provider"
                             "details-reply-model"
                             :metadata '(:fake-deltas ("all done"))))
           (agent (make-agent-loop-session-agent context selection))
           (*captured-tool-end-events* '()))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "edit something" context))
      (is (= 1 (length *captured-tool-end-events*)))
      (let ((payload (first *captured-tool-end-events*)))
        (is (equal "edited" (getf payload :result-text)))
        (is (equal *agent-loop-details-fixture* (getf payload :details)))))))

(test (agent-loop-end-usage-reports-latest-turn-not-cross-turn-sum :fixture interactive-authority)
  "A tool-looping run reports the LATEST turn's usage as the context-occupancy
snapshot, never the cross-turn sum. Each turn's input already subsumes the prior
turns, so summing would overshoot the window. Turn 1 (tool call) reports total
100, turn 2 (reply) reports total 250 -- the run reports 250, not 350."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (install-extension context *agent-loop-end-capture-extension-manifest*)
    (let* ((selection (agent-loop-register-model
                       context "usage-tool-provider" "usage-tool-model"
                       :metadata '(:fake-deltas ("calling tool")
                                   :fake-usage (:input-tokens 90 :output-tokens 10
                                                :total-tokens 100)
                                   :fake-tool-call
                                   (:id :call-echo
                                    :name :agent-echo-swap
                                    :arguments (:partial-json "{\"message\":\"ok\"}")))))
           (reply-selection (agent-loop-register-model
                             context "usage-reply-provider" "usage-reply-model"
                             :metadata '(:fake-deltas ("all done")
                                         :fake-usage (:input-tokens 230 :output-tokens 20
                                                      :total-tokens 250))))
           (agent (make-agent-loop-session-agent context selection))
           (*captured-agent-end-events* '()))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context))
      (is (= 2 (length (agents:agent-turns agent))))
      (is (= 1 (length *captured-agent-end-events*)))
      (let ((usage (getf (first *captured-agent-end-events*) :usage)))
        (is (not (null usage)))
        (is (= 250 (getf usage :total-tokens)))
        (is (= 230 (getf usage :input-tokens)))
        (is (= 20 (getf usage :output-tokens)))))))

(defvar *captured-turn-end-events*)

(defun capture-turn-end-handler (event _context)
  (declare (ignore _context))
  (push (event:event-payload event) *captured-turn-end-events*))

(ext:defextension agent-loop-turn-end-capture
  (:requires
   (:capability :events :contract :events/v1))
  (:provides
   (event-handler :capture-turn-end
     :event-type :agent/turn-end
     :handler #'capture-turn-end-handler)))

(test (agent-loop-bracketed-end-events-carry-monotonic-duration :fixture interactive-authority)
  "Each bracketed boundary -- turn-end, tool-execution-end, agent-end -- carries
a :duration-ms measured with the monotonic clock, reported as a non-negative
integer of milliseconds."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (install-extension context *agent-loop-turn-end-capture-extension-manifest*)
    (install-extension context *agent-loop-tool-end-capture-extension-manifest*)
    (install-extension context *agent-loop-end-capture-extension-manifest*)
    (let* ((selection (agent-loop-register-model
                       context "duration-provider" "duration-model"
                       :metadata '(:fake-deltas ("calling tool")
                                   :fake-tool-call
                                   (:id :call-details
                                    :name :agent-details
                                    :arguments (:partial-json "{}")))))
           (reply-selection (agent-loop-register-model
                             context "duration-reply-provider"
                             "duration-reply-model"
                             :metadata '(:fake-deltas ("all done"))))
           (agent (make-agent-loop-session-agent context selection))
           (*captured-turn-end-events* '())
           (*captured-tool-end-events* '())
           (*captured-agent-end-events* '()))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context))
      (flet ((duration-ok (payload)
               (let ((ms (getf payload :duration-ms)))
                 (and (integerp ms) (>= ms 0)))))
        (is (plusp (length *captured-turn-end-events*)))
        (is (every #'duration-ok *captured-turn-end-events*))
        (is (plusp (length *captured-tool-end-events*)))
        (is (every #'duration-ok *captured-tool-end-events*))
        (is (= 1 (length *captured-agent-end-events*)))
        (is (every #'duration-ok *captured-agent-end-events*))))))

(test agent-loop-duration-omitted-when-unstamped
  "The honesty contract for unstamped events: no start stamp yields no
duration, never a fabricated zero, and the payload omits :duration-ms
entirely."
  (is (null (agents::event-duration-ms nil)))
  (is (equal '(:a 1) (agents::payload-with-duration '(:a 1) nil)))
  (let ((payload (agents::payload-with-duration '(:a 1)
                                                (get-internal-real-time))))
    (is (integerp (getf payload :duration-ms)))))

(test agent-loop-duration-derives-from-monotonic-clock
  "The duration value is arithmetic on get-internal-real-time stamps, so a
regression to the jumpable wall clock cannot satisfy both bounds."
  (let ((start (get-internal-real-time)))
    (sleep 0.02)
    (is (>= (agents::event-duration-ms start) 10)))
  (let ((future (+ (get-internal-real-time)
                   (* 10 internal-time-units-per-second))))
    (is (<= (agents::event-duration-ms future) 0))))

(test (agent-loop-continues-after-tool-call-until-model-stops :fixture interactive-authority)
  "One tool call, two model turns, terminating on the turn with no tool calls.
The loop ran a second model turn carrying the tool result."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "continue-provider"
                       "continue-model"
                       :metadata '(:fake-deltas ("on it")
                                   :fake-tool-call
                                   (:id :call-cont
                                    :name :agent-echo-swap
                                    :arguments (:partial-json "{\"message\":\"did it\"}")))))
           (reply-selection (agent-loop-register-model
                             context
                             "continue-reply-provider"
                             "continue-reply-model"
                             :metadata '(:fake-deltas ("final answer"))))
           (agent (make-agent-loop-session-agent context selection)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "do a thing" context)
        (is (agents:agent-idle-p agent))
        (is (= 1 (length (agents:agent-tool-executions agent))))
        (is (= 2 (length (agents:agent-turns agent))))
        (is (equal '("do a thing" "on it" "did it" "final answer")
                   (agent-session-message-contents agent)))
        (is (equal '(:user :assistant :tool-result :assistant)
                   (agent-session-message-roles agent)))))))

(test (agent-loop-steering-during-tool-skips-remaining-and-starts-new-turn :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((tool-selection
             (agent-loop-register-model
              context
              "steer-provider"
              "steer-model"
              :metadata '(:fake-deltas ("needs tools")
                          :fake-tool-calls
                          ((:id :call-first
                            :name :agent-steer
                           :arguments ())
                           (:id :call-second
                            :name :agent-second
                            :arguments ())))))
           (reply-selection
             (agent-loop-register-model
              context
              "steer-reply-provider"
              "steer-reply-model"
              :metadata '(:fake-deltas ("after steer"))))
           (agent (make-agent-loop-session-agent context tool-selection)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection)
            (*agent-loop-steering-used-p* nil)
            (*agent-loop-first-tool-runs* 0)
            (*agent-loop-second-tool-runs* 0))
        (agents:prompt-agent agent "start" context)
        (is (= 1 *agent-loop-first-tool-runs*))
        (is (= 0 *agent-loop-second-tool-runs*))
        (is (= 2 (length (agents:agent-tool-executions agent))))
        (is (member "Skipped due to queued user message."
                    (agent-session-message-contents agent)
                    :test #'string=))
        (is (equal '("start"
                     "needs tools"
                     "first tool done"
                     "Skipped due to queued user message."
                     "steered while tooling"
                     "after steer")
                   (agent-session-message-contents agent)))
        (is (equal '(:user :assistant :tool-result :tool-result :user
                     :assistant)
                   (agent-session-message-roles agent)))
        (is (= 2 (length (agents:agent-turns agent))))
        (is (member "steered while tooling"
                    (agent-session-message-contents agent)
                    :test #'string=))
        (is (member "after steer"
                    (agent-session-message-contents agent)
                    :test #'string=))))))

(test (agent-loop-follow-up-after-idle-starts-new-turn :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "follow-provider"
                       "follow-model"
                       :metadata '(:fake-deltas ("ack"))))
           (agent (make-agent-loop-session-agent context selection)))
      (agents:prompt-agent agent "first" context)
      (agents:follow-up-agent agent "second" context)
      (is (agents:agent-idle-p agent))
      (is (= 2 (length (agents:agent-turns agent))))
      (is (equal '("first" "ack" "second" "ack")
                 (agent-session-message-contents agent))))))
(test (agent-loop-follow-up-deferred-skips-inline-run :fixture interactive-authority)
  "Under *defer-agent-turn-p*, follow-up-agent enqueues the message and emits
:agent/follow-up but does NOT run the loop on the calling thread -- the TUI's
slash-command path relies on this so :command/result lands before any model
output. A later drain-pending-agent-work runs the queued turn, proving the
message was enqueued rather than dropped. Headless paths never bind the flag
and keep the synchronous idle catch-up (covered by the test above)."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "defer-provider"
                       "defer-model"
                       :metadata '(:fake-deltas ("deferred-reply"))))
           (agent (make-agent-loop-session-agent context selection)))
      (agents:prompt-agent agent "first" context)
      (is (agents:agent-idle-p agent))
      ;; Deferred: enqueue without running. The follow-up queue holds the
      ;; message and the agent stays idle -- no turn ran on this thread.
      (let ((agents:*defer-agent-turn-p* t))
        (agents:follow-up-agent agent "second" context)
        (is (agents:agent-idle-p agent)
            "follow-up-agent did not run the loop under *defer-agent-turn-p*")
        (is (= 1 (length (agents:agent-queue-items
                          (agents:agent-follow-up-queue agent))))
            "the follow-up message was enqueued"))
      ;; Drain runs the queued turn, proving the message survived.
      (agents:drain-pending-agent-work agent context)
      (is (agents:agent-idle-p agent))
      (is (= 2 (length (agents:agent-turns agent)))
          "drain ran the deferred follow-up as a second turn")
      (is (equal '("first" "deferred-reply" "second" "deferred-reply")
                 (agent-session-message-contents agent))))))

(test (agent-loop-abort-during-tool-leaves-inspectable-state :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "abort-provider"
                       "abort-model"
                       :metadata '(:fake-deltas ("before abort")
                                   :fake-tool-call
                                   (:id :call-abort
                                    :name :agent-abort
                                    :arguments ()))))
           (agent (make-agent-loop-session-agent context selection)))
      (let ((*agent-loop-current-agent* agent))
        (agents:prompt-agent agent "abort now" context)
        (is (eq :aborted
                (agents:agent-state-value (agents:agent-state agent))))
        (is (getf (agents:inspect-agent agent) :abort-requested-p))
        (is (eq :aborted
                (agents:agent-turn-state
                 (first (agents:agent-turns agent)))))))))

(defun run-agent-loop-abort-then-error-adapter (provider request context
                                                &key emit)
  "Stand in for a stream torn down mid-flight by a cross-thread abort: request the
abort, then signal as a truncated SSE payload would."
  (declare (ignore provider request emit))
  (agents:abort-agent *agent-loop-current-agent* context)
  (error "stream torn down by abort"))

(test (agent-loop-error-after-abort-classifies-aborted-not-error :fixture interactive-authority)
  "The error is the abort unwinding, not a failure. Turn and agent end :aborted
never :error, no error is recorded, and the loop stops."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context "abort-err-provider" "abort-err-model"))
           (agent (make-agent-loop-session-agent context selection)))
      (rt:register-model-stream-adapter (model-runtime-service context)
                                        :fake
                                        #'run-agent-loop-abort-then-error-adapter
                                        context)
      (let ((*agent-loop-current-agent* agent))
        (agents:prompt-agent agent "go" context)
        (is (eq :aborted
                (agents:agent-state-value (agents:agent-state agent))))
        (is (eq :aborted
                (agents:agent-turn-state
                 (first (agents:agent-turns agent)))))
        (is (getf (agents:inspect-agent agent) :abort-requested-p))
        (is (null (getf (agents:inspect-agent agent) :last-error)))
        (is (= 1 (length (agents:agent-turns agent))))))))

(defun run-agent-loop-partial-then-abort-adapter (provider request context
                                                  &key emit)
  "A stream torn down after some text streamed: emit a delta, request the abort,
then signal as a truncated SSE payload would."
  (declare (ignore provider request))
  (funcall emit (rt:make-assistant-delta "partial reply"))
  (agents:abort-agent *agent-loop-current-agent* context)
  (error "stream torn down by abort"))

(test (agent-loop-mid-stream-abort-persists-the-partial-reply :fixture interactive-authority)
  "A turn aborted after some text streamed keeps the partial as an assistant message
   tagged :stop-reason :aborted, so the model sees what it had said."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context "abort-partial-provider" "abort-partial-model"))
           (agent (make-agent-loop-session-agent context selection)))
      (rt:register-model-stream-adapter (model-runtime-service context)
                                        :fake
                                        #'run-agent-loop-partial-then-abort-adapter
                                        context)
      (let ((*agent-loop-current-agent* agent))
        (agents:prompt-agent agent "write an essay" context)
        (is (eq :aborted (agents:agent-state-value (agents:agent-state agent))))
        (let ((assistant (find :assistant (agent-session-messages agent)
                               :key #'sess:message-role)))
          (is (not (null assistant))
              "the aborted partial is persisted as an assistant message")
          (is (equal "partial reply" (sess:message-content assistant)))
          (is (eq :aborted
                  (getf (sess:message-metadata assistant) :stop-reason))))))))

(test (agent-loop-abort-during-tool-seals-remaining-calls :fixture interactive-authority)
  "Esc-abort mid tool-turn must seal the un-executed calls with skipped results.
An assistant message whose declared tool_calls outnumber its tool_results is
unbalanced on the wire and the provider rejects every later request over it."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection
             (agent-loop-register-model
              context "abort-seal-provider" "abort-seal-model"
              :metadata '(:fake-deltas ("calling tools")
                          :fake-tool-calls
                          ((:id :call-abort :name :agent-abort :arguments ())
                           (:id :call-second :name :agent-second :arguments ())))))
           (agent (make-agent-loop-session-agent context selection)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-second-tool-runs* 0))
        (agents:prompt-agent agent "use tools" context)
        (is (eq :aborted (agents:agent-state-value (agents:agent-state agent))))
        (is (= 0 *agent-loop-second-tool-runs*)
            "the sealed call's runner never executes")
        (is (= 2 (length (agents:agent-tool-executions agent)))
            "one real execution plus one skip-seal")
        (is (wire-tool-calls-balanced-p (agent-wire-items agent))
            "both declared calls have matching tool_results")))))

(defun run-agent-loop-toolcalls-then-abort-adapter (provider request context
                                                    &key emit)
  "Stream a two-call response to completion, then flip abort before the loop
registers and dispatches the calls -- the window where a clean stream's calls
would orphan with no tool_result."
  (declare (ignore provider request))
  (funcall emit (rt:make-assistant-delta "calling tools"))
  (funcall emit (rt:make-tool-call-delta :agent-second '()
                                         :call-id :win2-call-a :content-index 0))
  (funcall emit (rt:make-tool-call-delta :agent-second '()
                                         :call-id :win2-call-b :content-index 1))
  (agents:abort-agent *agent-loop-current-agent* context))

(test (agent-loop-abort-after-clean-stream-seals-unregistered-calls :fixture interactive-authority)
  "A stream that completed cleanly persists the assistant message with its
tool_calls, but an abort flipping before dispatch left them unregistered and
unsealed. The abort path registers then seals them so the wire balances."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context "win2-provider" "win2-model"))
           (agent (make-agent-loop-session-agent context selection)))
      (rt:register-model-stream-adapter
       (model-runtime-service context)
       :fake
       #'run-agent-loop-toolcalls-then-abort-adapter
       context)
      (let ((*agent-loop-current-agent* agent))
        (agents:prompt-agent agent "go" context))
      (is (eq :aborted (agents:agent-state-value (agents:agent-state agent))))
      (is (equal '(:user :assistant :tool-result :tool-result)
                 (agent-session-message-roles agent))
          "both clean-stream calls get sealed results")
      (is (wire-tool-calls-balanced-p (agent-wire-items agent))))))

(test (agent-loop-recode-preserves-state-turns-and-queues :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "recode-provider"
                       "recode-model"
                       :metadata '(:fake-deltas ("ok"))))
           (agent (make-agent-loop-session-agent
                   context
                   selection
                   :behavior (agents:make-agent-loop-behavior
                              :tool-scheduling-policy :serial))))
      (agents:prompt-agent agent "first" context)
      (setf (agents:agent-queue-items (agents:agent-follow-up-queue agent))
            '("queued"))
      (agents:recode-agent-loop-behavior agent
                                         :steering-delivery-policy :one-at-a-time)
      (is (= 1 (length (agents:agent-turns agent))))
      (is (equal '("queued")
                 (agents:agent-queue-items
                  (agents:agent-follow-up-queue agent))))
      (is (eq :serial
              (agents:agent-behavior-value agent :tool-scheduling-policy)))
      (is (eq :one-at-a-time
              (agents:agent-behavior-value agent
                                           :steering-delivery-policy))))))

(test (agent-loop-queue-steer-never-runs-the-loop :fixture interactive-authority)
  "queue-agent-steer only enqueues -- unlike steer-agent it must not run the
loop synchronously on an idle agent. drain-pending-agent-work later runs the
queued work."
  (multiple-value-bind (context protocol) (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "queue-steer-provider"
                       "queue-steer-model"
                       :metadata '(:fake-deltas ("ok"))))
           (agent (make-agent-loop-session-agent context selection)))
      (agents:queue-agent-steer agent "parked" context)
      (is (agents:agent-idle-p agent))
      (is (zerop (length (agents:agent-turns agent)))
          "the enqueue never runs a turn, even on an idle agent")
      (is (equal '("parked")
                 (agents:agent-queue-items (agents:agent-steering-queue agent))))
      (agents:drain-pending-agent-work agent context)
      (is (null (agents:agent-queue-items (agents:agent-steering-queue agent))))
      (is (= 1 (length (agents:agent-turns agent))))
      (let ((contents (agent-session-message-contents agent)))
        (is (find "parked" contents :test #'string=))
        (is (find "ok" contents :test #'string=))))))

(test (agent-loop-one-at-a-time-steering-drains-one-per-pass :fixture interactive-authority)
  "With :one-at-a-time steering delivery each delivery point hands the model
exactly one queued steer, so two steers queued during one tool execution
arrive over two passes; with the :all default the whole queue arrives at the
first boundary."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "steer-one-provider"
                       "steer-one-model"
                       :metadata '(:fake-deltas ("calling double steer")
                                   :fake-tool-call
                                   (:id :call-double-steer
                                    :name :agent-double-steer
                                    :arguments (:partial-json "{}")))))
           (reply-selection (agent-loop-register-model
                             context
                             "steer-one-reply-provider"
                             "steer-one-reply-model"
                             :metadata '(:fake-deltas ("ok"))))
           (agent (make-agent-loop-session-agent
                   context
                   selection
                   :behavior (agents:make-agent-loop-behavior
                              :steering-delivery-policy :one-at-a-time))))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context))
      (is (agents:agent-idle-p agent))
      (is (= 3 (length (agents:agent-turns agent))))
      (is (equal '(:user :assistant :tool-result
                   :user :assistant :user :assistant)
                 (agent-session-message-roles agent)))
      (is (equal '("use tool" "calling double steer" "double steered"
                   "steer one" "ok" "steer two" "ok")
                 (agent-session-message-contents agent))))
    (let* ((selection (agent-loop-register-model
                       context
                       "steer-all-provider"
                       "steer-all-model"
                       :metadata '(:fake-deltas ("calling double steer")
                                   :fake-tool-call
                                   (:id :call-double-steer-all
                                    :name :agent-double-steer
                                    :arguments (:partial-json "{}")))))
           (reply-selection (agent-loop-register-model
                             context
                             "steer-all-reply-provider"
                             "steer-all-reply-model"
                             :metadata '(:fake-deltas ("ok"))))
           (agent (make-agent-loop-session-agent context selection)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context))
      (is (agents:agent-idle-p agent))
      (is (= 2 (length (agents:agent-turns agent))))
      (is (equal '(:user :assistant :tool-result :user :user :assistant)
                 (agent-session-message-roles agent))))))

(test (agent-loop-recode-error-policy-governs-live-behavior :fixture interactive-authority)
  "A recoded error-to-tool-result function governs the very next tool error,
and inspect-agent reports the actual function rather than a canonical name it
no longer runs."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "intercept-provider"
                       "intercept-model"
                       :metadata '(:fake-deltas ("calling exploder")
                                   :fake-tool-call
                                   (:id :call-intercept
                                    :name :agent-explode
                                    :arguments (:partial-json "{}")))))
           (reply-selection (agent-loop-register-model
                             context
                             "intercept-reply-provider"
                             "intercept-reply-model"
                             :metadata '(:fake-deltas ("recovered"))))
           (agent (make-agent-loop-session-agent context selection))
           (interceptor (lambda (agent tool-call result context)
                          (declare (ignore agent tool-call context))
                          (ext:make-tool-result
                           :content (list (ext:make-tool-text-content
                                           "intercepted"))
                           :details (ext:tool-result-details result)
                           :error-p t))))
      (agents:recode-agent-loop-behavior
       agent
       :error-to-tool-result-policy interceptor)
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context))
      (is (agents:agent-idle-p agent))
      (is (equal "intercepted"
                 (third (agent-session-message-contents agent))))
      (let ((behavior (getf (agents:inspect-agent agent) :behavior)))
        (is (eq interceptor (getf behavior :error-to-tool-result-policy)))
        (is (eq :serial (getf behavior :tool-scheduling-policy)))
        (is (eq :all (getf behavior :steering-delivery-policy)))
        (is (eq :apply (getf behavior :in-flight-context-edit-policy)))))))

(test (agent-loop-scheduling-and-context-edit-policies-govern :fixture interactive-authority)
  "The tool-scheduling and in-flight-context-edit slots are funcalled at
their decision points: recoded wrappers observe one scheduling decision per
model response and one context-projection refresh per model turn."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context :tools t)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context
                       "wired-provider"
                       "wired-model"
                       :metadata '(:fake-deltas ("calling tool")
                                   :fake-tool-call
                                   (:id :call-wired
                                    :name :agent-echo-swap
                                    :arguments (:partial-json "{\"message\":\"hi\"}")))))
           (reply-selection (agent-loop-register-model
                             context
                             "wired-reply-provider"
                             "wired-reply-model"
                             :metadata '(:fake-deltas ("all done"))))
           (agent (make-agent-loop-session-agent context selection))
           (scheduled 0)
           (rebuilds 0))
      (agents:recode-agent-loop-behavior
       agent
       :tool-scheduling-policy
       (lambda (agent turn tool-calls context)
         (incf scheduled)
         (agents::execute-tool-calls-serially agent turn tool-calls context))
       :in-flight-context-edit-policy
       (lambda (agent context)
         (incf rebuilds)
         (agents::apply-in-flight-context-edits agent context)))
      (let ((*agent-loop-current-agent* agent)
            (*agent-loop-reply-selection* reply-selection))
        (agents:prompt-agent agent "use tool" context))
      (is (agents:agent-idle-p agent))
      (is (= 2 scheduled))
      (is (= 2 rebuilds))
      (is (equal '(:user :assistant :tool-result :assistant)
                 (agent-session-message-roles agent))))))

(defparameter *agent-loop-declared-event-types*
  '(:agent/start :agent/end
    :agent/turn-start :agent/turn-end
    :agent/message-start :agent/message-end
    :agent/delta
    :agent/thinking-delta
    :agent/user-message-appended
    :agent/aborted :agent/error
    :agent/steer :agent/follow-up
    :agent/tool-execution-start :agent/tool-execution-end
    :agent/tool-execution-update))

(test agent-loop-declares-emitted-event-types-in-manifest
  (multiple-value-bind (context protocol) (agent-loop-test-context)
    (declare (ignore context))
    (let ((declared (event:protocol-event-types protocol)))
      (dolist (kind *agent-loop-declared-event-types*)
        (is (typep (gethash kind declared) 'event:event-type-contribution)
            "missing event-type declaration: ~S" kind)))))

(test (agent-loop-event-types-retract-when-extension-deactivates :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-loop-test-context)
    (let ((extension (kli:find-live-object (kli:context-registry context)
                                           :agent-loop)))
      (is (gethash :agent/delta (event:protocol-event-types protocol)))
      (ext:deactivate-extension protocol extension context)
      (let ((after (event:protocol-event-types protocol)))
        (dolist (kind *agent-loop-declared-event-types*)
          (is (null (gethash kind after))
              "event-type lingered after retract: ~S" kind))))))

(defvar *captured-agent-deltas*)

(defun capture-agent-delta-handler (event _context)
  (declare (ignore _context))
  (push (event:event-payload event) *captured-agent-deltas*))

(ext:defextension agent-loop-delta-capture
  (:requires
   (:capability :events :contract :events/v1))
  (:provides
   (event-handler :capture-agent-delta
     :event-type :agent/delta
     :handler #'capture-agent-delta-handler)))

(test (agent-loop-emits-agent-delta-per-text-chunk :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (install-extension context *agent-loop-delta-capture-extension-manifest*)
    (let* ((selection (agent-loop-register-model
                       context
                       "delta-provider"
                       "delta-model"
                       :metadata '(:fake-deltas ("hel" "lo " "world"))))
           (agent (make-agent-loop-session-agent context selection))
           (*captured-agent-deltas* '()))
      (agents:prompt-agent agent "stream" context)
      (let ((payloads (reverse *captured-agent-deltas*)))
        (is (= 3 (length payloads)))
        (is (equal '("hel" "lo " "world")
                   (mapcar (lambda (p) (getf p :text)) payloads)))
        (let ((turn-id (kli:object-id (first (agents:agent-turns agent)))))
          (dolist (p payloads)
            (is (eq turn-id (getf p :turn-id))))))
      (is (equal '("hello world")
                 (last (agent-session-message-contents agent)))))))

(defvar *captured-agent-thinking-deltas*)

(defun capture-agent-thinking-delta-handler (event _context)
  (declare (ignore _context))
  (push (event:event-payload event) *captured-agent-thinking-deltas*))

(ext:defextension agent-loop-thinking-delta-capture
  (:requires
   (:capability :events :contract :events/v1))
  (:provides
   (event-handler :capture-agent-thinking-delta
     :event-type :agent/thinking-delta
     :handler #'capture-agent-thinking-delta-handler)))

(test (agent-loop-emits-agent-thinking-delta-with-level :fixture interactive-authority)
  "Thinking deltas re-emit as :agent/thinking-delta carrying the turn id and the
   selection's effort level. The assistant text deltas of the same turn stay on
   :agent/delta and are not captured here."
  (multiple-value-bind (context protocol) (agent-loop-test-context)
    (declare (ignore protocol))
    (install-extension context *agent-loop-thinking-delta-capture-extension-manifest*)
    (multiple-value-bind (_provider model _selection)
        (register-runtime-model context
                                "thinking-delta-provider"
                                "thinking-delta-model"
                                :auth-required-p nil
                                :option-schemas (list (test-reasoning-effort-schema))
                                :metadata '(:fake-blocks
                                            ((:kind :thinking :text ("Pon" "dering."))
                                             (:kind :text :text ("Done.")))))
      (declare (ignore _provider _selection))
      (let* ((selection (models:select-model (model-registry context) model context
                                             :options (test-reasoning-options :high)))
             (agent (make-agent-loop-session-agent context selection))
             (*captured-agent-thinking-deltas* '()))
        (agents:prompt-agent agent "think" context)
        (let ((payloads (reverse *captured-agent-thinking-deltas*)))
          (is (= 2 (length payloads)))
          (is (equal '("Pon" "dering.")
                     (mapcar (lambda (p) (getf p :text)) payloads)))
          (is (every (lambda (p) (eq :high (getf p :level))) payloads)
              "each thinking delta carries the selection's effort level")
          (let ((turn-id (kli:object-id (first (agents:agent-turns agent)))))
            (dolist (p payloads)
              (is (eq turn-id (getf p :turn-id))))))))))

(defvar *captured-user-message-events*)

(defun capture-user-message-appended-handler (event _context)
  (declare (ignore _context))
  (push (event:event-payload event) *captured-user-message-events*))

(ext:defextension agent-loop-user-message-capture
  (:requires
   (:capability :events :contract :events/v1))
  (:provides
   (event-handler :capture-user-message-appended
     :event-type :agent/user-message-appended
     :handler #'capture-user-message-appended-handler)))

(test agent-loop-emits-user-message-appended-on-prompt
  (multiple-value-bind (context protocol) (agent-loop-test-context)
    (declare (ignore protocol))
    (install-extension context
                       *agent-loop-user-message-capture-extension-manifest*)
    (let* ((selection (agent-loop-register-model
                       context "user-msg-provider" "user-msg-model"
                       :metadata '(:fake-deltas ("ok"))))
           (agent (make-agent-loop-session-agent context selection))
           (*captured-user-message-events* '()))
      (agents:prompt-agent agent "hello there" context)
      (let ((payloads (reverse *captured-user-message-events*)))
        (is (= 1 (length payloads)))
        (let ((p (first payloads)))
          (is (eq :prompt (getf p :kind)))
          (is (string= "hello there" (getf p :text)))
          (is (not (null (getf p :entry-id)))))))))

(test agent-queue-concurrent-enqueue-drain-conserves-every-item
  "Enqueue and drain are read-modify-write on the items slot and run on different
   threads (steer from the input thread, drain from the worker). The queue's lock
   must make them atomic: under a producer racing a consumer, every enqueued item
   drains exactly once — none lost, none duplicated."
  (let* ((queue (agents::make-agent-queue))
         (n 5000)
         (drained '())
         (producer (sb-thread:make-thread
                    (lambda () (dotimes (i n) (agents::enqueue-agent-item queue i)))
                    :name "queue-safety-producer")))
    (loop until (and (not (sb-thread:thread-alive-p producer))
                     (agents::queue-empty-p queue))
          do (setf drained (nconc drained (agents::drain-agent-queue queue))))
    (sb-thread:join-thread producer)
    (setf drained (nconc drained (agents::drain-agent-queue queue)))
    (is (= n (length drained))
        "no steered item is lost or duplicated under contention")
    (is (equal (loop for i below n collect i)
               (sort (copy-list drained) #'<))
        "every enqueued item drained exactly once")))

(test (agent-loop-steer-queued-during-text-turn-runs-as-the-next-turn :fixture interactive-authority)
  "A steer that arrives during a plain-text (tool-less) turn has no tool boundary
   to drain at. It must be consumed at turn-end so it runs as the next turn,
   rather than being stranded in the queue with the agent gone idle."
  (multiple-value-bind (context protocol) (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model
                       context "steer-text-provider" "steer-text-model"
                       :metadata '(:fake-deltas ("essay"))))
           (agent (make-agent-loop-session-agent context selection)))
      (agents::enqueue-agent-item (agents:agent-steering-queue agent)
                                  "make it shorter")
      (agents:prompt-agent agent "write an essay" context)
      (is (agents:agent-idle-p agent))
      (is (= 2 (length (agents:agent-turns agent)))
          "the queued steer runs as a second turn")
      (is (agents::queue-empty-p (agents:agent-steering-queue agent))
          "the steer was drained, not stranded")
      (is (equal '(:user :assistant :user :assistant)
                 (agent-session-message-roles agent)))
      (is (equal '("write an essay" "essay" "make it shorter" "essay")
                 (agent-session-message-contents agent))))))

(test (agent-loop-broken-protocol-recovers-to-boot-instead-of-killing-the-agent
       :fixture interactive-authority)
  "A protocol that throws on dispatch makes a turn's own error-event emission
re-throw and escape the loop -- which would unwind the agent thread. The control
plane's :around barrier catches that escape and recovers the kernel to boot, so
the loop returns and the next turn runs under a working protocol."
  (multiple-value-bind (context protocol)
      (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model context
                                                 "recover-loop-provider"
                                                 "recover-loop-model"))
           (agent (make-agent-loop-session-agent context selection))
           (boot (kli:find-live-object (kli:context-registry context)
                                       :boot-protocol))
           (broken (make-instance 'broken-dispatch-protocol
                                  :id :loop-broken-protocol)))
      (load-control-plane context)
      (kli:register-live-object (kli:context-registry context) broken)
      (setf (kli:active-protocol context) broken)
      (finishes (agents:run-agent-loop agent context))
      (is (eq boot (kli:active-protocol context))))))
