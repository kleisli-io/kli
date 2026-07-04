(in-package #:kli/agent/loop)

(defun make-agent-loop-contract ()
  (make-provider-contract
   :id :agent/loop/v1
   :capability :agent/loop
   :required-entries
   '(:register-agent
     :deregister-agent
     :prompt-agent
     :continue-agent
     :steer-agent
     :queue-steer
     :follow-up-agent
     :abort-agent
     :run-agent-loop
     :execute-agent-tool-call
     :agent-idle-p
     :agent-active-p
     :drain-pending-work
     :inspect-agent
     :inspect-agent-turn
     :inspect-tool-execution
     :recode-agent-loop-behavior)))

(defun make-agent-loop-provider ()
  (make-provider
   :id :agent-loop-provider
   :capability :agent/loop
   :contracts '(:agent/loop/v1)
   :entries
   (list :register-agent #'register-agent
         :deregister-agent #'deregister-agent
         :prompt-agent #'prompt-agent
         :continue-agent #'continue-agent
         :steer-agent #'steer-agent
         :queue-steer #'queue-agent-steer
         :follow-up-agent #'follow-up-agent
         :abort-agent #'abort-agent
         :run-agent-loop #'run-agent-loop
         :execute-agent-tool-call #'execute-agent-tool-call
         :agent-idle-p #'agent-idle-p
         :agent-active-p #'agent-active-p
         :drain-pending-work #'drain-pending-agent-work
         :inspect-agent #'inspect-agent
         :inspect-agent-turn #'inspect-agent-turn
         :inspect-tool-execution #'inspect-tool-execution
         :recode-agent-loop-behavior #'recode-agent-loop-behavior)))

(defextension agent-loop
  (:requires
   (capability events :contract events/v1)
   (capability session/log :contract session/log/v1)
   (capability session/entries :contract session/entries/v1)
   (capability context/lens :contract context/lens/v1)
   (capability model/runtime :contract model/runtime/v1))
  (:provides
   (contract agent/loop/v1
     (make-agent-loop-contract))
   (capability agent/loop (make-agent-loop-provider))
   (live-object agent-loop-service
     (make-agent-loop-service))
   (event-type :agent/start)
   (event-type :agent/end)
   (event-type :agent/turn-start)
   (event-type :agent/turn-end)
   (event-type :agent/message-start)
   (event-type :agent/message-end)
   (event-type :agent/delta)
   (event-type :agent/thinking-delta)
   (event-type :agent/usage)
   (event-type :agent/user-message-appended)
   (event-type :agent/aborted)
   (event-type :agent/error)
   (event-type :agent/steer)
   (event-type :agent/follow-up)
   (event-type :agent/tool-execution-start)
   (event-type :agent/tool-execution-end)
   (event-type :agent/tool-execution-update)))
