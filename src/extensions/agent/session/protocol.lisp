(in-package #:kli/agent/session)

(defgeneric submit-agent-session-prompt (agent-session mode-id message context))
(defgeneric steer-agent-session         (agent-session mode-id message context))
(defgeneric queue-agent-session-steer   (agent-session mode-id message context))
(defgeneric drain-agent-session-work    (agent-session mode-id context))
(defgeneric follow-up-agent-session     (agent-session mode-id message context))
(defgeneric switch-agent-session        (agent-session mode-id session-id context))
(defgeneric branch-agent-session        (agent-session mode-id entry-id context))
(defgeneric rewind-agent-session        (agent-session mode-id context
                                         &key entry-id))
(defgeneric rewind-agent-session-available-p (agent-session mode-id context))
(defgeneric list-rewind-targets         (agent-session mode-id context))
(defgeneric reset-agent-session         (agent-session mode-id context &key reason))
(defgeneric clear-agent-session         (agent-session mode-id context))
(defgeneric abort-agent-session         (agent-session mode-id context))
(defgeneric agent-session-busy-p        (agent-session mode-id context))
(defgeneric agent-session-context       (agent-session mode-id context))
(defgeneric current-agent-session-leaf-id (agent-session mode-id context))
(defgeneric retract-agent-session-pending-prompt
    (agent-session mode-id context pre-turn-leaf-id))

(defgeneric register-session-event-listener   (agent-session listener context))
(defgeneric unregister-session-event-listener (agent-session listener-id context))

(defgeneric persist-agent-event      (agent-session event context))
(defgeneric persist-context-patch    (agent-session event context))
(defgeneric execute-session-compaction (agent-session event context
                                        &key custom-instructions))

(defgeneric set-agent-session-model    (agent-session mode-id selection context))
(defgeneric set-agent-session-option   (agent-session mode-id option-id value context))

(defgeneric inspect-agent-session (agent-session))

(defgeneric session-mode-info     (agent-session mode-id context))
(defgeneric rename-agent-session  (agent-session mode-id name context))
(defgeneric resume-agent-session  (agent-session mode-id session-id context))
(defgeneric compact-agent-session (agent-session mode-id context
                                   &key custom-instructions))
(defgeneric focus-agent-session-mode (agent-session mode-id context))

(defmethod submit-agent-session-prompt
    ((agent-session agent-session) mode-id message context)
  "Submit a prompt for MODE-ID. The agent-loop appends the user message as part of prompt-agent, so only orchestration-layer extras need explicit appendage here."
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (mode-binding-for agent-session mode-id))
         (agent   (resolve-agent-from-binding binding context)))
    (unless agent
      (error 'agent-session-error :reason :no-agent))
    (ensure-model-and-credential agent context)
    (let* ((message-text (if (stringp message) message ""))
           (message-images nil))
      (multiple-value-bind (expanded-text images cancelled-p)
          (kli/ext:with-extension-fault-barrier
              (:seam :session-policy :id '(:prompt-expansion :expand)
               :on-fault (values message-text message-images nil))
            (funcall (session-prompt-expansion-policy agent-session)
                     :expand message-text message-images))
        (when cancelled-p
          (return-from submit-agent-session-prompt (values nil nil)))
        ;; Pending model-visible command records ride in front of the user
        ;; text so the model shares the user's view of what ran between
        ;; turns. After expansion, so expanders see only what the user typed.
        (let ((pending (drain-pending-command-block agent-session)))
          (when pending
            (setf expanded-text (concatenate 'string pending expanded-text))))
        (let ((new-prompt
                (funcall (session-context-transform-policy agent-session)
                         :system-prompt
                         (kli/agent/loop:agent-base-system-prompt agent))))
          (when new-prompt
            (setf (kli/agent/loop:agent-system-prompt agent) new-prompt))
          (rebuild-context agent context)
          (call-prompt-with-retry agent-session agent
                                  (make-user-prompt-message
                                   expanded-text images context
                                   :typed-text message-text)
                                  context))))))

(defmethod steer-agent-session
    ((agent-session agent-session) mode-id message context)
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (mode-binding-for agent-session mode-id))
         (agent   (resolve-agent-from-binding binding context)))
    (unless agent
      (error 'agent-session-error :reason :no-agent))
    (provider-call (agent-loop-provider-of context)
                   :steer-agent agent message context)))

(defmethod queue-agent-session-steer
    ((agent-session agent-session) mode-id message context)
  "Queue MESSAGE as steering for MODE-ID without ever running the loop on
this thread, unlike steer-agent-session whose idle catch-up runs a full turn
synchronously. Pair with drain-agent-session-work from a worker when the
agent may have gone idle since the caller's busy check."
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (mode-binding-for agent-session mode-id))
         (agent   (resolve-agent-from-binding binding context)))
    (unless agent
      (error 'agent-session-error :reason :no-agent))
    (provider-call (agent-loop-provider-of context)
                   :queue-steer agent message context)))

(defmethod drain-agent-session-work
    ((agent-session agent-session) mode-id context)
  "Run any steering or follow-up messages stranded in MODE-ID's queues while
the agent sat idle. No-op while a turn is in flight (the loop drains its own
queues at turn boundaries) and when the mode is unbound."
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (when agent
      (provider-call (agent-loop-provider-of context)
                     :drain-pending-work agent context))))

(defmethod follow-up-agent-session
    ((agent-session agent-session) mode-id message context)
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (mode-binding-for agent-session mode-id))
         (agent   (resolve-agent-from-binding binding context)))
    (unless agent
      (error 'agent-session-error :reason :no-agent))
    (provider-call (agent-loop-provider-of context)
                   :follow-up-agent agent message context)))

(defmethod switch-agent-session
    ((agent-session agent-session) mode-id session-id context)
  (kli/ext:require-capability :agent/session/switch)
  (clear-pending-command-records agent-session)
  (let* ((existing (gethash mode-id (session-mode-bindings agent-session)))
         (old-agent (and existing (resolve-agent-from-binding existing context)))
         (old-agent-id (and existing (mode-binding-agent-id existing)))
         (old-context-id
           (and existing
                (let ((cb (mode-binding-context-binding existing)))
                  (and cb (context-binding-context-id cb)))))
         (binding (or existing
                      (let ((b (make-mode-binding :mode-id mode-id)))
                        (setf (gethash mode-id
                                       (session-mode-bindings agent-session))
                              b)
                        b))))
    (when old-agent
      (let ((loop-provider (agent-loop-provider-of context))
            (loop-service (agent-loop-service-of context)))
        (provider-call loop-provider :abort-agent old-agent context)
        (provider-call loop-provider :deregister-agent
                       loop-service old-agent context)))
    (let* ((new-session (resolve-session-by-id session-id context))
           (new-context (build-context-for-session new-session context))
           (new-agent   (construct-agent-for new-session new-context context)))
      ;; Read the live policy each turn so later recodes are picked up.
      (setf (kli/agent/loop:agent-context-extras-fn new-agent)
            (lambda ()
              (funcall (session-context-transform-policy agent-session)
                       :extra-messages)))
      (provider-call (context-lens-provider-of context)
                     :rebuild-context-projection
                     new-context
                     :leaf-id (kli/session/log:session-leaf-id new-session))
      (restore-model-and-options new-agent new-session context)
      (unbind-mode-sources agent-session old-agent-id old-context-id)
      (setf (mode-binding-session-binding binding)
            (make-instance 'agent-session-binding
                           :session-id session-id
                           :leaf-id (kli/session/log:session-leaf-id new-session))
            (mode-binding-context-binding binding)
            (make-instance 'agent-context-binding
                           :context-id (object-id new-context)
                           :rebuilt-at (get-universal-time))
            (mode-binding-agent-id binding) (object-id new-agent))
      (bind-mode-sources agent-session mode-id
                         (object-id new-agent)
                         (object-id new-context))
      (kli/event:emit-event
       context
       (kli/event:make-event :session-switch
                             :payload (list :mode mode-id
                                            :session-id session-id)
                             :source mode-id))
      binding)))

(defmethod branch-agent-session
    ((agent-session agent-session) mode-id entry-id context)
  (kli/ext:require-capability :agent/session/branch)
  (let* ((binding (mode-binding-for agent-session mode-id))
         (current-session-id
           (session-binding-session-id (mode-binding-session-binding binding)))
         (store (session-store-of context))
         (log   (session-log-provider-of context))
         (current-session (provider-call log :find-session
                                         store current-session-id))
         (branched-session
           (provider-call log :branch-session-at-entry
                          store current-session entry-id context)))
    (unless (typep branched-session 'kli/session/log:session)
      (error 'agent-session-error
             :reason :branch-failed
             :provider-id current-session-id))
    (let ((new-session-id (object-id branched-session)))
      (switch-agent-session agent-session mode-id new-session-id context)
      (kli/event:emit-event
       context
       (kli/event:make-event :session-branch
                             :payload (list :mode mode-id
                                            :branched-from-entry entry-id
                                            :new-session-id new-session-id)
                             :source mode-id))
      new-session-id)))

(defmethod rewind-agent-session-available-p
    ((agent-session agent-session) mode-id context)
  "T when the bound session has a user prompt to step past."
  (and (rewind-prompt-entries agent-session mode-id context) t))

(defmethod list-rewind-targets
    ((agent-session agent-session) mode-id context)
  "The rewind menu's rows: user prompts on MODE-ID's current branch, newest
first, as plists (:entry-id ID :text TEXT). TEXT is what the user typed
(see prompt-entry-text). Empty when there is nothing to rewind."
  (loop for entry in (reverse (rewind-prompt-entries agent-session mode-id
                                                     context))
        collect (list :entry-id (object-id entry)
                      :text (prompt-entry-text entry))))

(defmethod rewind-agent-session
    ((agent-session agent-session) mode-id context &key entry-id)
  "Step the conversation back to before a user prompt: branch at the prompt
entry's parent and switch the mode onto the branch. ENTRY-ID picks any
prompt on the current branch (a list-rewind-targets row), defaulting to the
latest. The original session is untouched, so /resume can return to it. A
first-turn prompt has no parent to branch at, so rewinding it falls back to
a fresh session via reset-agent-session. Callers gate on busy themselves --
a rewind under an in-flight turn would switch the session out from under
the worker. Returns (values new-session-id text) where TEXT is what the
user typed for the rewound prompt, or NIL when there is no matching prompt."
  (let* ((entries (rewind-prompt-entries agent-session mode-id context))
         (entry (if entry-id
                    (find entry-id entries :key #'object-id)
                    (car (last entries)))))
    (when entry
      (let* ((parent-id (kli/session/log:entry-parent-id entry))
             (text (prompt-entry-text entry))
             (new-session-id
               (if parent-id
                   (branch-agent-session agent-session mode-id parent-id
                                         context)
                   (let ((binding (reset-agent-session agent-session mode-id
                                                       context)))
                     (session-binding-session-id
                      (mode-binding-session-binding binding))))))
        (kli/event:emit-event
         context
         (kli/event:make-event :session-rewind
                               :payload (list :mode mode-id
                                              :rewound-entry (object-id entry)
                                              :new-session-id new-session-id
                                              :text text)
                               :source mode-id))
        (values new-session-id text)))))

(defmethod reset-agent-session
    ((agent-session agent-session) mode-id context &key (reason :reset))
  "Start a new conversation by creating a fresh session and switching the mode onto it. switch-agent-session aborts and deregisters any in-flight agent, then constructs a new one seeded from the registry's current model selection, so the user's model and thinking carry across while the empty new session drops the model-visible history. Binds from cold when the mode is currently unbound. REASON rides in the :session-reset event so the cold initial bind (:initial) projects no line while a user-driven reset (:reset) does."
  (kli/ext:require-capability :agent/session/switch)
  (let* ((log         (session-log-provider-of context))
         (store       (session-store-of context))
         (new-session (provider-call log :create-session store context))
         (new-session-id (object-id new-session))
         (binding (switch-agent-session agent-session mode-id
                                        new-session-id context)))
    (kli/event:emit-event
     context
     (kli/event:make-event :session-reset
                           :payload (list :mode mode-id
                                          :session-id new-session-id
                                          :reason reason)
                           :source mode-id))
    binding))

(defmethod agent-session-busy-p
    ((agent-session agent-session) mode-id context)
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (and agent (kli/agent/loop:agent-active-p agent) t)))

(defmethod abort-agent-session
    ((agent-session agent-session) mode-id context)
  "Cooperatively interrupt the in-flight turn. The agent stays bound so the
conversation continues from where it stopped. An in-flight compaction is
interrupted through the binding's compaction-interrupt thunk, which flags the
run aborted and shuts down the summarizer's model request -- abort-agent alone
only reaches the current turn's request, and a compaction has none. No-op when
nothing is running."
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (when (and agent (kli/agent/loop:agent-active-p agent))
      (provider-call (agent-loop-provider-of context) :abort-agent agent context)
      (let ((interrupt (mode-binding-compaction-interrupt binding)))
        (when interrupt
          (kli/ext:safely-invoke interrupt
                                 :session-policy '(:compaction :interrupt))))
      t)))

(defmethod clear-agent-session
    ((agent-session agent-session) mode-id context)
  (kli/ext:require-capability :agent/session/switch)
  (let ((binding (gethash mode-id (session-mode-bindings agent-session))))
    (when binding
      (let* ((agent-id   (mode-binding-agent-id binding))
             (cb         (mode-binding-context-binding binding))
             (context-id (and cb (context-binding-context-id cb)))
             (agent      (and agent-id
                              (find-live-object (context-registry context)
                                                agent-id))))
        (when agent
          (let ((loop-provider (agent-loop-provider-of context))
                (loop-service (agent-loop-service-of context)))
            (provider-call loop-provider :abort-agent agent context)
            (provider-call loop-provider :deregister-agent
                           loop-service agent context)))
        (unbind-mode-sources agent-session agent-id context-id))
      (remhash mode-id (session-mode-bindings agent-session))
      (kli/event:emit-event
       context
       (kli/event:make-event :session-cleared
                             :payload (list :mode mode-id)
                             :source mode-id)))
    binding))

(defmethod agent-session-context
    ((agent-session agent-session) mode-id context)
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (cb (and binding (mode-binding-context-binding binding))))
    (and cb (find-live-object (context-registry context)
                              (context-binding-context-id cb)))))

(defmethod current-agent-session-leaf-id
    ((agent-session agent-session) mode-id context)
  "The current leaf id, captured at submit. NIL (unbound, no agent, or empty session) reads as repoint to an empty session on retract."
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (and agent
         (kli/session/log:session-leaf-id (kli/agent/loop:agent-session agent)))))

(defmethod retract-agent-session-pending-prompt
    ((agent-session agent-session) mode-id context pre-turn-leaf-id)
  "Repoint past the unanswered user turn and rebuild. Race-free only after the
worker unwinds, so call from the :agent/turn-end projection. Refuses (returns
NIL) when the leaf has advanced past the prompt entry itself - assistant or
tool entries landed for the turn, and repointing would silently orphan that
committed work. A turn of thinking deltas and tool calls emits no text delta,
so the caller's pending-prompt heuristic alone cannot tell the two apart."
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (when agent
      (let* ((provider (session-log-provider-of context))
             (store (kli/agent/loop:agent-store agent))
             (session (kli/agent/loop:agent-session agent))
             (leaf-id (kli/session/log:session-leaf-id session))
             (leaf (and leaf-id
                        (provider-call provider :session-entry-by-id
                                       store session leaf-id))))
        (when (or (eql leaf-id pre-turn-leaf-id)
                  (and leaf (eql (kli/session/log:entry-parent-id leaf)
                                 pre-turn-leaf-id)))
          (provider-call provider :repoint-session-leaf
                         store session pre-turn-leaf-id context)
          (rebuild-context agent context)
          t)))))

(defmethod register-session-event-listener
    ((agent-session agent-session) listener context)
  (kli/ext:require-capability :agent/session/listen)
  (push listener (session-event-listeners agent-session))
  (kli/event:emit-event
   context
   (kli/event:make-event :listener-registered
                         :payload (list :listener-id
                                        (listener-id listener))
                         :source (object-id agent-session)))
  listener)

(defmethod unregister-session-event-listener
    ((agent-session agent-session) listener-id context)
  (kli/ext:require-capability :agent/session/listen)
  (let* ((listeners (session-event-listeners agent-session))
         (match (find listener-id listeners :key #'listener-id :test #'equal)))
    (when match
      (setf (session-event-listeners agent-session)
            (remove match listeners))
      (kli/event:emit-event
       context
       (kli/event:make-event :listener-unregistered
                             :payload (list :listener-id listener-id)
                             :source (object-id agent-session))))
    match))

(defmethod persist-agent-event
    ((agent-session agent-session) event context)
  "Internal, substrate-driven. Called from the :after method on dispatch-event, runs under the producer's subject and skips capability gates."
  (let ((mode-id (resolve-mode-from-event agent-session event))
        (decision (kli/ext:with-extension-fault-barrier
                      (:seam :session-policy :id '(:event-persistence :route)
                       :policy :reify)
                    (funcall (session-event-persistence-policy agent-session)
                             :route event agent-session context))))
    (when mode-id
      (dolist (listener (session-event-listeners agent-session))
        (let ((filter (listener-filter listener)))
          (when (or (null filter)
                    (kli/ext:safely-invoke filter :listener-filter
                                           (listener-id listener) event))
            (let ((handler (listener-handler listener)))
              (kli/ext:safely-invoke
               (lambda (e ctx) (funcall handler e mode-id ctx))
               :event-callback (listener-id listener)
               event context))))))
    decision))

(defmethod persist-context-patch
    ((agent-session agent-session) event context)
  (declare (ignore context))
  (let* ((mode-id (resolve-mode-from-event agent-session event))
         (binding (and mode-id
                       (gethash mode-id (session-mode-bindings agent-session)))))
    (when binding
      (let ((cb (mode-binding-context-binding binding)))
        (when cb (setf (context-binding-rebuilt-at cb) nil))))
    nil))

(defun emit-compaction-event (context type mode-id trigger &rest extra)
  (kli/event:emit-event
   context
   (kli/event:make-event
    type
    :payload (append (list :mode mode-id :trigger trigger) extra)
    :source mode-id)))

(defmethod execute-session-compaction
    ((agent-session agent-session) event context &key custom-instructions)
  "Compact the session bound to EVENT's mode. Returns (values entry status
failure) where status is :compacted, :nothing-to-compact, :aborted, or
:failed -- a summarizer fault must stay distinguishable from an up-to-date
session, and a user abort from both. While the summarizer runs, the binding
carries a compaction-interrupt thunk so abort-agent-session can flag the run
and shut down the summarizer's request -- a discarded result matters because
an aborted stream still returns its partial text, which must never be
committed as the summary. Emits :session-compaction-started/-finished so the
work is visible, and holds the agent in :compacting (an active state) so the
busy indicator stays live and concurrent submits steer into the queue instead
of racing the rebuild. The queue drains once the agent returns to :idle."
  (let* ((mode-id (resolve-mode-from-event agent-session event))
         (binding (and mode-id
                       (gethash mode-id (session-mode-bindings agent-session))))
         (agent (and binding (resolve-agent-from-binding binding context))))
    (unless agent
      (return-from execute-session-compaction nil))
    (let* ((trigger (or (getf (kli/event:event-payload event) :trigger) :auto))
           (store            (session-store-of context))
           (log-provider     (session-log-provider-of context))
           (entries-provider (session-entries-provider-of context))
           (lens-provider    (context-lens-provider-of context))
           (runtime-provider (model-runtime-provider-of context))
           (loop-provider    (agent-loop-provider-of context))
           (agent-context    (kli/agent/loop:agent-context agent))
           (session          (kli/agent/loop:agent-session agent))
           (runtime          (kli/agent/loop:agent-model-runtime agent))
           (selection        (kli/agent/loop:agent-model-selection agent))
           (policy           (session-compaction-policy agent-session))
           (policy-state     (funcall policy :inspect))
           (context-window   (selection-context-window (model-registry-of context)
                                                       selection))
           (keep-recent      (or (getf policy-state :keep-recent-tokens)
                                 (derived-compaction-keep-recent-tokens
                                  context-window)))
           (entries          (provider-call log-provider :session-branch
                                            store session
                                            (kli/session/log:session-leaf-id
                                             session)))
           (prep (kli/session/log:prepare-session-compaction entries keep-recent)))
      (unless prep
        (emit-compaction-event context :session-compaction-finished mode-id
                               trigger :status :nothing-to-compact)
        (return-from execute-session-compaction
          (values nil :nothing-to-compact)))
      (let ((was-idle (kli/agent/loop:agent-idle-p agent))
            (summarizer-request nil)
            (aborted-p nil))
        (when was-idle
          (kli/agent/loop:set-agent-state agent :compacting))
        (setf (mode-binding-compaction-interrupt binding)
              (lambda ()
                (setf aborted-p t)
                (when summarizer-request
                  (provider-call runtime-provider :abort-model-request
                                 summarizer-request context))))
        (unwind-protect
             (let ((failure nil))
               (emit-compaction-event context :session-compaction-started
                                      mode-id trigger)
               (multiple-value-bind (summary details)
                   (kli/ext:with-extension-fault-barrier
                       (:seam :session-policy :id '(:compaction :summarize)
                        :on-fault (values nil nil))
                     (handler-bind ((error (lambda (condition)
                                             (setf failure condition))))
                       (funcall policy :summarize
                                :agent-context agent-context
                                :messages (kli/session/log:compaction-preparation-messages-to-summarize
                                           prep)
                                :turn-prefix-messages
                                (kli/session/log:compaction-preparation-turn-prefix-messages prep)
                                :previous-summary
                                (kli/session/log:compaction-preparation-previous-summary prep)
                                :custom-instructions custom-instructions
                                :lens-provider lens-provider
                                :runtime-provider runtime-provider
                                :runtime runtime
                                :selection selection
                                :context context
                                :on-request (lambda (request)
                                              (setf summarizer-request
                                                    request)))))
                 (cond
                   (aborted-p
                    (emit-compaction-event context :session-compaction-finished
                                           mode-id trigger :status :aborted)
                    (values nil :aborted))
                   ((and (stringp summary)
                         (plusp (length summary)))
                    (let ((entry (provider-call
                                  entries-provider :make-compaction-entry
                                  summary
                                  (kli/session/log:compaction-preparation-first-kept-id prep)
                                  :tokens-before
                                  (kli/session/log:compaction-preparation-tokens-before prep)
                                  :data details
                                  :source (object-id agent-context))))
                      (provider-call log-provider :append-session-entry
                                     store session entry context)
                      (rebuild-context agent context)
                      (emit-compaction-event context :session-compaction-finished
                                             mode-id trigger :status :compacted)
                      (values entry :compacted)))
                   (t
                    (let ((report (if failure
                                      (princ-to-string failure)
                                      "summarizer produced no summary")))
                      (emit-compaction-event context :session-compaction-finished
                                             mode-id trigger
                                             :status :failed :error report)
                      (values nil :failed failure))))))
          (setf (mode-binding-compaction-interrupt binding) nil)
          (when (and was-idle
                     (eq (kli/agent/loop:agent-state-value
                          (kli/agent/loop:agent-state agent))
                         :compacting))
            (kli/agent/loop:set-agent-state agent :idle)
            (provider-call loop-provider :drain-pending-work agent context)))))))

(defmethod set-agent-session-model
    ((agent-session agent-session) mode-id selection context)
  "Model selection adapter. Routes registry updates, session-log appendage, and live-agent mutation through one chokepoint."
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (mode-binding-for agent-session mode-id))
         (agent   (resolve-agent-from-binding binding context))
         (provider-id (kli/model/registry:model-selection-provider-id selection))
         (registry-provider (model-registry-provider-of context))
         (registry (model-registry-of context))
         (model-provider (provider-call registry-provider :find-model-provider
                                        registry provider-id)))
    (unless agent
      (error 'agent-session-error :reason :no-agent))
    (when (and model-provider
               (kli/model/registry:model-provider-auth-required-p
                model-provider)
               (let ((auth (auth-provider-of context))
                     (store (credential-store-of context)))
                 (not (provider-call auth :credential-available-p
                                     store provider-id))))
      (error 'agent-session-error
             :reason :no-credential
             :provider-id provider-id))
    (let ((model (provider-call registry-provider
                                :find-model-definition
                                registry
                                provider-id
                                (kli/model/registry:model-selection-model-id
                                 selection))))
      (provider-call registry-provider :select-model
                     registry model context
                     :id (object-id selection)
                     :options (kli/model/registry:model-selection-options selection)
                     :store (kli/agent/loop:agent-store agent)
                     :session (kli/agent/loop:agent-session agent)))
    (setf (kli/agent/loop:agent-model-selection agent) selection)
    (kli/event:emit-event
     context
     (kli/event:make-event :model-change
                           :payload selection
                           :source (object-id agent)))
    selection))

(defmethod set-agent-session-option
    ((agent-session agent-session) mode-id option-id value context)
  "Set one semantic model option on the per-mode agent's current selection. The
model registry validates and canonicalizes VALUE against the selected model's
schema, while the durable session log records the semantic option id/value pair."
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (mode-binding-for agent-session mode-id))
         (agent (resolve-agent-from-binding binding context))
         (registry-provider (model-registry-provider-of context))
         (registry (model-registry-of context))
         (option-key (kli/model/registry:model-option-keyword option-id)))
    (unless agent
      (error 'agent-session-error :reason :no-agent))
    (let ((current (kli/agent/loop:agent-model-selection agent)))
      (unless current
        (error 'agent-session-error :reason :no-current-model))
      (let* ((model (provider-call registry-provider
                                   :find-model-definition
                                   registry
                                   (kli/model/registry:model-selection-provider-id
                                    current)
                                   (kli/model/registry:model-selection-model-id
                                    current)))
             (new-selection
              (provider-call registry-provider :select-model
                             registry model context
                             :options (list* option-key value
                                             (remove-plist-key
                                              option-key
                                              (kli/model/registry:model-selection-options
                                               current)))))
             (canonical-value (kli/model/registry:model-selection-option-value
                               new-selection option-key)))
        (provider-call (session-log-provider-of context)
                       :append-session-entry
                       (kli/agent/loop:agent-store agent)
                       (kli/agent/loop:agent-session agent)
                       (provider-call (session-entries-provider-of context)
                                      :make-option-change-entry
                                      option-key canonical-value
                                      :source (object-id new-selection))
                       context)
        (setf (kli/agent/loop:agent-model-selection agent) new-selection)
        (kli/event:emit-event
         context
         (kli/event:make-event :option-change
                               :payload (list :option-id option-key
                                              :value canonical-value
                                              :selection new-selection)
                               :source (object-id agent)))
        new-selection))))

(defmethod inspect-agent-session ((agent-session agent-session))
  (list :id (object-id agent-session)
        :mode-bindings
        (loop for mode-id being the hash-keys of (session-mode-bindings agent-session)
              collect mode-id)
        :listener-count (length (session-event-listeners agent-session))
        :source->mode-size (hash-table-count (session-source->mode agent-session))
        :policies
        (list :event-persistence
              (funcall (session-event-persistence-policy agent-session) :inspect)
              :prompt-expansion
              (funcall (session-prompt-expansion-policy agent-session) :inspect)
              :context-transform
              (funcall (session-context-transform-policy agent-session) :inspect)
              :retry
              (funcall (session-retry-policy agent-session) :inspect)
              :compaction
              (funcall (session-compaction-policy agent-session) :inspect))))

(defun mode-current-selection (agent-session mode-id context)
  "The live model selection of MODE-ID's bound agent -- what that mode runs and
displays. NIL when the mode is unbound. Distinct from the registry default, which
only seeds newly constructed agents."
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (and agent (kli/agent/loop:agent-model-selection agent))))

(defmethod focus-agent-session-mode
    ((agent-session agent-session) mode-id context)
  "Make MODE-ID the focused mode: re-point the registry's global selection to the
mode's live model (no journaling) and announce :session-focus so the TUI brings
the mode to front. The sole writer of active-mode-id, so the focus pointer
round-trips through the snapshot. A no-op when MODE-ID is already active; errors
when MODE-ID is unbound."
  (kli/ext:require-capability :agent/session/focus)
  (let ((binding (gethash mode-id (session-mode-bindings agent-session))))
    (unless binding
      (error 'agent-session-error :reason :no-mode-binding :provider-id mode-id))
    (unless (eql mode-id (session-active-mode-id agent-session))
      (setf (session-active-mode-id agent-session) mode-id)
      (let ((selection (mode-current-selection agent-session mode-id context)))
        (when selection
          (let* ((registry (model-registry-of context))
                 (provider (model-registry-provider-of context))
                 (model (provider-call provider :find-model-definition registry
                                       (kli/model/registry:model-selection-provider-id
                                        selection)
                                       (kli/model/registry:model-selection-model-id
                                        selection))))
            (when model
              (provider-call provider :select-model registry model context
                             :options
                             (kli/model/registry:model-selection-options
                              selection))))))
      (kli/event:emit-event
       context
       (kli/event:make-event
        :session-focus
        :payload (list :mode mode-id
                       :session-id (session-binding-session-id
                                    (mode-binding-session-binding binding)))
        :source mode-id)))))

(defmethod session-mode-info ((agent-session agent-session) mode-id context)
  "Live snapshot of MODE-ID's bound session, or NIL when the mode is unbound.
File is the durable path on a file store, NIL on the in-memory store."
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (when agent
      (let* ((session   (kli/agent/loop:agent-session agent))
             (store     (kli/agent/loop:agent-store agent))
             (selection (kli/agent/loop:agent-model-selection agent))
             (cb        (mode-binding-context-binding binding))
             (usage     (and cb (context-binding-usage cb))))
        (list
         :id (object-id session)
         :name (getf (kli/session/log:session-metadata session) :name)
         :leaf-id (kli/session/log:session-leaf-id session)
         :entry-count (kli/session/log:session-entry-count session)
         :file (and (typep store 'kli/session/log:file-session-store)
                    (kli/session/log:session-file-path store (object-id session)))
         :provider (and selection
                        (kli/model/registry:model-selection-provider-id selection))
         :model (and selection
                     (kli/model/registry:model-selection-model-id selection))
         :thinking (and selection
                        (kli/model/registry:model-selection-option-value
                         selection "reasoning-effort"))
         :usage usage)))))

(defmethod rename-agent-session ((agent-session agent-session) mode-id name context)
  "Write NAME into the bound session's metadata and persist the header. Returns
NAME, or NIL when the mode is unbound. A no-op persist on the in-memory store."
  (kli/ext:require-capability :agent/session/submit)
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (when agent
      (let ((session (kli/agent/loop:agent-session agent))
            (store   (kli/agent/loop:agent-store agent)))
        (setf (kli/session/log:session-metadata session)
              (list* :name name
                     (remove-plist-key
                      :name (kli/session/log:session-metadata session))))
        (kli/session/log:note-stored-session store session context)
        name))))

(defmethod resume-agent-session ((agent-session agent-session) mode-id session-id
                                 context)
  "Load SESSION-ID from disk if it is not already in the store, then switch
MODE-ID onto it. Returns (values SESSION-ID skipped-tail-p), the second value
true when the load dropped a torn trailing record. An already-loaded session
skips the load, so the second value is NIL then."
  (let ((store (session-store-of context))
        (skipped-tail-p nil))
    (unless (provider-call (session-log-provider-of context) :find-session
                           store session-id)
      (setf skipped-tail-p
            (nth-value 1 (kli/session/log:load-session-file
                          store
                          (kli/session/log:session-file-path store session-id)
                          context))))
    (switch-agent-session agent-session mode-id session-id context)
    (values session-id skipped-tail-p)))

(defmethod compact-agent-session ((agent-session agent-session) mode-id context
                                  &key custom-instructions)
  "Compact MODE-ID's history now, threading CUSTOM-INSTRUCTIONS into the shared
summarizer seam. Returns execute-session-compaction's (values entry status
failure); the :manual trigger lets consumers report \"nothing to compact\"
where an auto run would stay quiet."
  (kli/ext:require-capability :agent/session/compact)
  (execute-session-compaction
   agent-session
   (kli/event:make-event :session-compaction-needed
                         :payload (list :mode mode-id :trigger :manual)
                         :source mode-id)
   context
   :custom-instructions custom-instructions))
