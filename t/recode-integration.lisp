(in-package #:kli/tests)
(in-suite all)

(test (recode-preserves-session-patches-queues-pending-tool-sealed-and-transcript :fixture image-session-authority)
  "Holistic recode witness -- recoding both the context capsule and the agent loop behavior preserves session entries, staged patches, queues, pending tool state, sealed request provenance, and transcript-visible history in one composed cycle. settle joins the agent worker and drains renders so the agent is idle before inspection. follow-up on an idle agent drains immediately, so the queue is set directly. No public constructor leaves a tool execution pending, so genuine pending state is built directly."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (app (tui-app:make-tui-app :context context :columns 40))
         (service (agent-session-service context))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding)))
         (agent-context (agents:agent-context agent)))
    (tui-app:tui-app-feed app "hello there" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (settle-tui-app app)
    (ctx:stage-context-patch
     agent-context
     (ctx:make-append-message-patch (sess:make-user-message "staged note")))
    (setf (agents:agent-queue-items (agents:agent-follow-up-queue agent))
          '("queued follow-up"))
    (setf (agents:agent-queue-items (agents:agent-steering-queue agent))
          '("queued steer"))
    (let ((pending (agents::make-tool-execution
                    (agents:make-tool-call "agent-echo" '(:message "later")
                                           :call-id "pending-call-1"))))
      (agents::register-tool-execution agent pending context))
    (let* ((sealed (ctx:seal-context-projection agent-context context))
           (roles-before (agent-session-message-roles agent))
           (staged-before (length (ctx:context-staged-patches agent-context)))
           (transcript-before (length (tui-app:tui-app-transcript-events app)))
           (pending-before (agents:inspect-tool-execution
                            (first (agents:agent-tool-executions agent))))
           (epoch-before (ctx:sealed-context-epoch sealed))
           (source-before (ctx:sealed-context-source-context-id sealed))
           (leaf-before (ctx:sealed-context-leaf-id sealed)))
      (ctx:recode-context-capsule agent-context)
      (agents:recode-agent-loop-behavior agent
                                         :steering-delivery-policy :one-at-a-time)
      (is (eq :one-at-a-time
              (agents:agent-behavior-value agent :steering-delivery-policy)))
      (is (equal roles-before (agent-session-message-roles agent)))
      (is (= staged-before (length (ctx:context-staged-patches agent-context))))
      (is (equal '("queued follow-up")
                 (agents:agent-queue-items (agents:agent-follow-up-queue agent))))
      (is (equal '("queued steer")
                 (agents:agent-queue-items (agents:agent-steering-queue agent))))
      (is (equal pending-before
                 (agents:inspect-tool-execution
                  (first (agents:agent-tool-executions agent)))))
      (is (eq :pending
              (getf (agents:inspect-tool-execution
                     (first (agents:agent-tool-executions agent)))
                    :state)))
      (is (eql epoch-before (ctx:sealed-context-epoch sealed)))
      (is (eql source-before (ctx:sealed-context-source-context-id sealed)))
      (is (eql leaf-before (ctx:sealed-context-leaf-id sealed)))
      (is (= transcript-before
             (length (tui-app:tui-app-transcript-events app)))))))
