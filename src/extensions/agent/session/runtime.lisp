(in-package #:kli/agent/session)

(defun credential-store-of (context)
  (find-live-object (context-registry context) :credential-store))

(defun session-store-of (context)
  (find-live-object (context-registry context) :session-store))

(defun model-registry-of (context)
  (find-live-object (context-registry context) :model-registry-service))

(defun agent-loop-service-of (context)
  (find-live-object (context-registry context) :agent-loop-service))

(defun session-log-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :session/log
                               :contract :session/log/v1))

(defun session-entries-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :session/entries
                               :contract :session/entries/v1))

(defun context-lens-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :context/lens
                               :contract :context/lens/v1))

(defun model-registry-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :model/registry
                               :contract :model/registry/v1))

(defun auth-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :auth
                               :contract :auth/v1))

(defun agent-loop-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :agent/loop
                               :contract :agent/loop/v1))

(defun model-runtime-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :model/runtime
                               :contract :model/runtime/v1))

(defun provider-accounting-wire-input (api messages)
  (case api
    (:openai-responses
     (convert-responses-input messages))
    (:openai-completions
     (convert-completions-messages messages))
    (:anthropic-messages
     (convert-anthropic-messages messages))
    (:fake
     (convert-responses-input messages))
    (otherwise
     (convert-responses-input messages))))

(defun provider-wire-token-upper-bound (wire)
  (kli/session/log:estimate-content-tokens
   (com.inuoe.jzon:stringify wire)))

(defun context-view-item-tool-call-ids (item)
  (let ((message (context-view-item-payload-value item)))
    (case (context-view-item-payload-kind item)
      (:message
       (when (and message
                  (eq (kli/session/log:message-role message) :assistant))
         (loop for call in (getf (kli/session/log:message-metadata message)
                                 :tool-calls)
               for id = (getf call :id)
               when id collect id)))
      ((:tool-result :repair)
       (let ((id (and message (kli/session/log:tool-call-id message))))
         (when id (list id))))
      (otherwise nil))))

(defun provider-accounting-entry-ids (view item)
  (let ((provenance (context-view-provenance-for-item view item)))
    (remove nil
            (list (getf provenance :entry-id)
                  (getf provenance :source-summary-entry-id))
            :test #'equal)))

(defun pending-tool-result-item-p (item call-ids)
  (and (member (context-view-item-payload-kind item)
               '(:tool-result :repair)
               :test #'eq)
       (intersection (context-view-item-tool-call-ids item)
                     call-ids
                     :test #'equal)))

(defun collect-provider-accounting-groups (items)
  (let ((remaining (copy-list items))
        (groups '()))
    (loop while remaining
          for item = (pop remaining)
          for call-ids = (context-view-item-tool-call-ids item)
          do (cond
               ((and (eq (context-view-item-payload-kind item) :message)
                     call-ids)
                (let ((members (list item))
                      (kept '()))
                  (dolist (candidate remaining)
                    (if (pending-tool-result-item-p candidate call-ids)
                        (push candidate members)
                        (push candidate kept)))
                  (setf remaining (nreverse kept))
                  (push (nreverse members) groups)))
               (t
                (push (list item) groups))))
    (nreverse groups)))

(defun provider-accounting-group-id (group)
  (or (context-view-item-group-id (first group))
      (context-view-item-id (first group))))

(defun account-provider-wire-group (api group)
  (provider-wire-token-upper-bound
   (provider-accounting-wire-input
    api
    (materialize-provider-replay-items group))))

(defun record-provider-accounting-entry-costs (entry-costs entry-ids cost)
  (when entry-ids
    (incf (gethash (first entry-ids) entry-costs 0) cost)
    (dolist (entry-id (rest entry-ids))
      (setf (gethash entry-id entry-costs 0)
            (gethash entry-id entry-costs 0)))))

(defun account-provider-replay (agent-context selection context)
  (let* ((registry-provider (model-registry-provider-of context))
         (registry (model-registry-of context))
         (provider-id (model-selection-provider-id selection))
         (provider (provider-call registry-provider :find-model-provider
                                  registry provider-id))
         (api (and provider (model-provider-api provider)))
         (model-id (model-selection-model-id selection))
         (sealed (seal-context-projection agent-context context
                                          :repair-policy :synthesize-aborted))
         (items (provider-replay-items sealed))
         (wire-input (provider-accounting-wire-input
                      api
                      (materialize-provider-replay-items items)))
         (group-costs (make-hash-table :test #'equal))
         (entry-costs (make-hash-table :test #'equal))
         (attributions '())
         (total 0))
    (dolist (group (collect-provider-accounting-groups items))
      (let* ((group-id (provider-accounting-group-id group))
             (cost (account-provider-wire-group api group))
             (item-ids (mapcar #'context-view-item-id group))
             (entry-ids
               (remove-duplicates
                (loop for item in group
                      append (provider-accounting-entry-ids sealed item))
                :test #'equal
                :from-end t)))
        (setf (gethash group-id group-costs) cost)
        (record-provider-accounting-entry-costs entry-costs entry-ids cost)
        (incf total cost)
        (push (list :group-id group-id
                    :item-ids item-ids
                    :entry-ids entry-ids
                    :total cost
                    :units :tokens
                    :exact-p nil
                    :upper-bound-p t)
              attributions)))
    (make-provider-accounting-result
     :provider-id provider-id
     :model-id model-id
     :api api
     :units :tokens
     :exact-p nil
     :upper-bound-p t
     :total total
     :attributions (nreverse attributions)
     :group-costs group-costs
     :entry-costs entry-costs
     :wire-input wire-input)))

(defun safe-account-provider-replay (agent-context selection context)
  (handler-case
      (let ((kli/ext:*call-subject* (make-unrestricted-subject)))
        (account-provider-replay agent-context selection context))
    (error () nil)))

(defun provider-accounting-entry-token-fn (accounting)
  (let ((entry-costs (and accounting
                          (provider-accounting-result-entry-costs accounting))))
    (lambda (entry)
      (or (and entry-costs
               (gethash (object-id entry) entry-costs))
          (kli/session/log:entry-token-estimate entry)))))

(defun derived-compaction-summary-reserve-tokens (context-window)
  (if (and context-window (> context-window 10000))
      (min 8192 (max 1024 (floor (* context-window 0.01))))
      0))

(defun summary-token-upper-bound (summary)
  (provider-wire-token-upper-bound
   (convert-responses-input
    (list (list :role :summary :content (or summary ""))))))

(defun bind-mode-sources (agent-session mode-id agent-id context-id)
  "Record source-to-mode entries. The reverse index covers both agent-ids (set by emit-agent-event) and agent-context-ids (set by the patch-committed augmentation in context-lens). Both id classes live in disjoint keyword namespaces so a single equal-keyed hashtable can host them without collision."
  (let ((table (session-source->mode agent-session)))
    (when agent-id   (setf (gethash agent-id   table) mode-id))
    (when context-id (setf (gethash context-id table) mode-id))))

(defun unbind-mode-sources (agent-session agent-id context-id)
  (let ((table (session-source->mode agent-session)))
    (when agent-id   (remhash agent-id   table))
    (when context-id (remhash context-id table))))

(defun resolve-mode-from-event (agent-session event)
  "Resolve the mode-id for EVENT. Producer-emitted events (agent/loop, context/lens) carry an agent-id or agent-context-id as source, looked up in the reverse index. The orchestrator's own emissions (session-switch, session-reset, and the like) stamp the active mode-id into the payload under :mode. Falls back to that so listeners can still resolve a mode for self-events whose binding may already be gone, such as :session-cleared."
  (let ((src (kli/event:event-source event)))
    (or (and src (gethash src (session-source->mode agent-session)))
        (let ((payload (kli/event:event-payload event)))
          (and (listp payload) (getf payload :mode))))))

(defun mode-binding-for (agent-session mode-id)
  (or (gethash mode-id (session-mode-bindings agent-session))
      (error 'agent-session-error :reason :no-mode-binding)))

(defun resolve-agent-from-binding (binding context)
  (let ((agent-id (mode-binding-agent-id binding)))
    (and agent-id (find-live-object (context-registry context) agent-id))))

(defun remove-plist-key (key plist)
  (loop for (k v) on plist by #'cddr
        unless (eq k key)
          append (list k v)))

(defun ensure-model-and-credential (agent context)
  (let ((selection (kli/agent/loop:agent-model-selection agent)))
    (unless selection
      (error 'agent-session-error :reason :no-model))
    (let* ((provider-id (kli/model/registry:model-selection-provider-id selection))
           (registry (model-registry-of context))
           (registry-provider (model-registry-provider-of context))
           (model-provider (provider-call registry-provider :find-model-provider
                                          registry provider-id))
           (auth-provider (auth-provider-of context))
           (store (credential-store-of context)))
      (when (and model-provider
                 (kli/model/registry:model-provider-auth-required-p
                  model-provider)
                 (not (provider-call auth-provider :credential-available-p
                                     store provider-id)))
        (error 'agent-session-error
               :reason :no-credential
               :provider-id provider-id)))))

(defun rebuild-context (agent context)
  (let ((lens (context-lens-provider-of context)))
    (provider-call lens :rebuild-context-projection
                   (kli/agent/loop:agent-context agent)
                   :leaf-id (kli/session/log:session-leaf-id
                             (kli/agent/loop:agent-session agent)))))

(defun resolve-session-by-id (session-id context)
  (let ((store (session-store-of context))
        (log   (session-log-provider-of context)))
    (provider-call log :find-session store session-id)))

(defun build-context-for-session (session context)
  (let ((store (session-store-of context))
        (lens  (context-lens-provider-of context)))
    (provider-call lens :make-agent-context session store context
                   :leaf-id (kli/session/log:session-leaf-id session))))

(defparameter *system-prompt-guidelines*
  '("Be concise in your responses."
    "Show file paths clearly when working with files."
    "Use bash for file operations like ls, grep, find.")
  "Tool-agnostic guidance appended to the default system prompt.")

(defun system-prompt-tool-lines (context)
  "One `- name: description` line per tool registered on CONTEXT's protocol."
  (let ((protocol (active-protocol context)))
    (when protocol
      (loop for tool in (list-tools protocol)
            collect (format nil "- ~A: ~A"
                            (tool-name tool)
                            (or (tool-description tool) ""))))))

(defun current-date-string ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun default-agent-system-prompt (context)
  "Minimal system prompt: identity, the live tool list, concise guidelines, and
the current date plus working directory."
  (let ((tools (system-prompt-tool-lines context)))
    (format nil
            "You are kli, an interactive coding assistant operating inside a ~
terminal harness. You help the user by reading files, running commands, ~
editing code, and writing new files.~2%~
Your name is kli. When asked who you are or what your name is, say you are ~
kli; never identify as ChatGPT, GPT, Codex, or any other assistant.~2%~
Available tools:~%~A~2%Guidelines:~%~{- ~A~^~%~}~2%~
Environment (authoritative; answer questions about it directly, without ~
running a command):~%Current date: ~A~%Current working directory: ~A"
            (if tools (format nil "~{~A~^~%~}" tools) "(none)")
            *system-prompt-guidelines*
            (current-date-string)
            (namestring (uiop:getcwd)))))

(defun delivery-policy-for-word (word)
  "Settings delivery-mode string to its policy keyword, NIL with a warning
for an unrecognised value and silently for an absent one."
  (cond ((null word) nil)
        ((equal word "all") :all)
        ((equal word "one-at-a-time") :one-at-a-time)
        (t (warn "Unknown delivery mode ~S in settings." word) nil)))

(defun configured-loop-behavior (context)
  "Agent-loop behavior carrying the configured delivery policies, NIL when
no config capability is present or neither key is set so make-agent falls
back to its defaults."
  (let ((config (find-capability-provider (active-protocol context)
                                          :config
                                          :contract :config/v1)))
    (when config
      (let ((steering (delivery-policy-for-word
                       (provider-call config :settings-value context
                                      "steeringMode")))
            (follow-up (delivery-policy-for-word
                        (provider-call config :settings-value context
                                       "followUpMode"))))
        (when (or steering follow-up)
          (kli/agent/loop:make-agent-loop-behavior
           :steering-delivery-policy steering
           :follow-up-delivery-policy follow-up))))))

(defun configured-agent-subject (context)
  "Subject from the \"capabilities\" settings key, with a second value that is
true exactly when the key is present. Returns (values NIL NIL) when no config
capability is present or the key is unset, so the caller applies its own
fallback. Profile settings overlay the config layer, so a profile carrying
capabilities restricts every agent constructed while it is active."
  (let ((config (find-capability-provider (active-protocol context)
                                          :config
                                          :contract :config/v1)))
    (if config
        (capabilities-subject
         (provider-call config :settings-value context "capabilities"))
        (values nil nil))))

(defun root-agent-subject (context)
  "The subject a root (user-facing) agent boots with: the configured
\"capabilities\" subject when that key is present -- so a profile or settings can
bound the root agent, including to nothing -- otherwise an unrestricted ordinary
subject. Capability enforcement begins only once a capabilities policy is
declared; absence means no restriction, not the narrow default (which would deny
the baseline tools a normal session needs)."
  (multiple-value-bind (subject present) (configured-agent-subject context)
    (if present subject (make-unrestricted-subject))))

(defun construct-agent-for (session agent-context context)
  (let* ((store (session-store-of context))
         (registry (model-registry-of context))
         (registry-provider (model-registry-provider-of context))
         (runtime (find-live-object (context-registry context)
                                    :model-runtime-service))
         (selection (provider-call registry-provider
                                   :current-model-selection
                                   registry))
         (service (agent-loop-service-of context))
         (subject (root-agent-subject context))
         (agent (kli/agent/loop:make-agent session store agent-context
                                           selection
                                           runtime
                                           :system-prompt
                                           (default-agent-system-prompt context)
                                           :behavior
                                           (configured-loop-behavior context)
                                           :subject subject)))
    (kli/agent/loop:seed-agent-principal-grant agent (active-protocol context)
                                               context)
    (kli/agent/loop:register-agent service agent context)
    agent))

(defun restore-model-and-options (agent session context)
  (let* ((store (session-store-of context))
         (log (session-log-provider-of context))
         (session-context (provider-call log :build-session-context store session))
         (model-state (kli/session/log:session-context-model session-context)))
    (when model-state
      (let* ((registry-provider (model-registry-provider-of context))
             (registry (model-registry-of context))
             (provider-id (getf model-state :provider))
             (model-id (getf model-state :model-id))
             (options (getf model-state :options))
             (model (provider-call registry-provider :find-model-definition
                                   registry provider-id model-id)))
        (when model
          (let ((selection (provider-call registry-provider :select-model
                                          registry model context
                                          :options options)))
            (setf (kli/agent/loop:agent-model-selection agent) selection)))))))

(defun make-user-prompt-message (text images context &key typed-text)
  "TEXT is what enters the model context (expanded, possibly led by a drained
command block). TYPED-TEXT preserves what the user actually submitted so a
rewind can restore it to the editor verbatim."
  (let ((entries (session-entries-provider-of context)))
    (provider-call entries :make-user-message text
                   :metadata (append (list :images images)
                                     (when typed-text
                                       (list :typed-text typed-text))))))

(defun user-prompt-entry-p (entry)
  "T when ENTRY is a user prompt: a user message that opened its turn. Steer
and follow-up entries belong to the turn they steered, so they are excluded.
An untagged user entry (appended outside the loop) reads as a prompt."
  (and (typep entry 'kli/session/log:message-entry)
       (let ((message (kli/session/log:entry-message entry)))
         (and message
              (eq (kli/session/log:message-role message) :user)))
       (member (getf (kli/session/log:entry-metadata entry) :agent-input)
               '(:prompt nil))))

(defun rewind-prompt-entries (agent-session mode-id context)
  "The entries a rewind can step past: user prompts on MODE-ID's bound
session branch, oldest first. Empty when the mode is unbound or no prompt
has landed."
  (let* ((binding (gethash mode-id (session-mode-bindings agent-session)))
         (agent   (and binding (resolve-agent-from-binding binding context))))
    (when agent
      (let* ((provider (session-log-provider-of context))
             (store    (kli/agent/loop:agent-store agent))
             (session  (kli/agent/loop:agent-session agent))
             (leaf-id  (kli/session/log:session-leaf-id session))
             (chain    (and leaf-id
                            (provider-call provider :session-branch
                                           store session leaf-id))))
        (remove-if-not #'user-prompt-entry-p chain)))))

(defun prompt-entry-text (entry)
  "What the user typed for prompt ENTRY: the :typed-text message metadata
when present, else the stored content when it is a string."
  (let ((message (kli/session/log:entry-message entry)))
    (or (getf (kli/session/log:message-metadata message) :typed-text)
        (let ((content (kli/session/log:message-content message)))
          (and (stringp content) content)))))

(defun agent-error-after-prompt (agent)
  "The agent's last error when it finished in :error state, else NIL. Agent-loop records adapter errors on the agent and finishes the turn in :error state rather than re-signaling, so the retry policy must see those errors as well as anything that escaped provider-call."
  (and (eq (kli/agent/loop:agent-state-value
            (kli/agent/loop:agent-state agent))
           :error)
       (kli/agent/loop:agent-last-error agent)))

(defun emit-agent-retry-event (agent condition attempt max-attempts delay-ms
                               context)
  (kli/event:emit-event
   context
   (kli/event:make-event
    :agent/retry
    :payload (list :attempt attempt
                   :max-attempts max-attempts
                   :delay-ms delay-ms
                   :category (kli/ext:condition-category condition)
                   :message (princ-to-string condition))
    :source (object-id agent))))

(defparameter *backoff-poll-seconds* 0.1
  "Slice length for the retry backoff wait. The wait re-checks for an abort
request between slices, so Esc interrupts a long exponential delay within
one slice instead of after it lapses.")

(defun wait-backoff-observing-abort (agent delay-ms)
  "Sleep DELAY-MS milliseconds in *backoff-poll-seconds* slices, returning
early as soon as AGENT has an abort requested."
  (loop with deadline = (+ (get-internal-real-time)
                           (ceiling (* delay-ms internal-time-units-per-second)
                                    1000))
        until (kli/agent/loop:agent-abort-requested-p agent)
        for remaining = (- deadline (get-internal-real-time))
        while (plusp remaining)
        do (sleep (min *backoff-poll-seconds*
                       (/ remaining internal-time-units-per-second)))))

(defun call-prompt-with-retry (agent-session agent message context)
  "Prompt AGENT with MESSAGE, retrying per the session's retry policy. Only
the first attempt prompts -- prompt-agent appends MESSAGE to the context, so
a retry must continue the loop instead or the user message would duplicate.
Between attempts the agent sits in :retrying (an active state, so submits
steer and Esc-abort stays live) and an :agent/retry event surfaces the wait.
An abort requested during the backoff stops the retry without re-signaling --
abort-agent already set the state and emitted :agent/aborted."
  (let ((loop-provider (agent-loop-provider-of context))
        (retry-policy (session-retry-policy agent-session))
        (attempt 0)
        (final-text nil))
    (loop
      (let* ((escaping (handler-case
                           (let ((kli/agent/loop:*agent-error-supervised-p* t))
                             ;; Capture the run's final assistant text (2nd value).
                             (setf final-text
                                   (nth-value 1
                                              (if (zerop attempt)
                                                  (provider-call loop-provider :prompt-agent
                                                                 agent message context)
                                                  (provider-call loop-provider :continue-agent
                                                                 agent context))))
                             nil)
                         (error (c) c)))
             (condition (or escaping (agent-error-after-prompt agent))))
        (cond
          ((null condition) (return (values agent final-text)))
          ((kli/agent/loop:agent-abort-requested-p agent) (return (values agent nil)))
          (t
           (multiple-value-bind (retry-p delay-ms)
               (funcall retry-policy :should-retry condition attempt)
             (unless retry-p (error condition))
             (emit-agent-retry-event
              agent condition (1+ attempt)
              (getf (funcall retry-policy :inspect) :max-attempts)
              delay-ms context)
             (kli/agent/loop:set-agent-state agent :retrying
                                             :reason condition)
             (when (and delay-ms (plusp delay-ms))
               (wait-backoff-observing-abort agent delay-ms))
             (when (kli/agent/loop:agent-abort-requested-p agent)
               (kli/agent/loop:set-agent-state agent :aborted
                                               :reason :abort-requested)
               (return (values agent nil)))
             (setf (kli/agent/loop:agent-last-error agent) nil)
             (incf attempt))))))))

(defun default-persist-agent-message (event)
  "No-op. Agent-loop already appends agent messages to the session log."
  (declare (ignore event))
  :persist-via-agent-loop)

(defun default-persist-model-change (event)
  "No-op. select-model already appended the model_change entry."
  (declare (ignore event))
  :persist-via-select-model)

(defun default-persist-option-change (event)
  (declare (ignore event))
  :persist-via-set-option)

(defun default-persist-custom-message (event)
  (declare (ignore event))
  :persist-via-agent-loop)

(defun default-handle-context-patch (agent-session event context)
  (persist-context-patch agent-session event context))

(defun usage-plist->context-usage (plist)
  (make-context-usage
   :input-tokens (or (getf plist :input-tokens) 0)
   :output-tokens (or (getf plist :output-tokens) 0)
   :cache-read-tokens (or (getf plist :cache-read-tokens) 0)
   :cache-write-tokens (or (getf plist :cache-write-tokens) 0)
   :total-tokens (or (getf plist :total-tokens) 0)))

(defun default-handle-agent-usage (agent-session event context)
  "Set the mode binding's live usage from a streaming usage-delta so the footer
tracks the in-flight turn. The committed usage compaction reads is left
untouched; live-usage is cleared at agent-end."
  (declare (ignore context))
  (let* ((mode-id (resolve-mode-from-event agent-session event))
         (binding (and mode-id
                       (gethash mode-id (session-mode-bindings agent-session))))
         (cb (and binding (mode-binding-context-binding binding)))
         (payload (kli/event:event-payload event))
         (usage-plist (and (listp payload) (getf payload :usage))))
    (when (and cb usage-plist)
      (setf (context-binding-live-usage cb)
            (usage-plist->context-usage usage-plist)))))

(defun default-handle-agent-end (agent-session event context)
  (let* ((mode-id (resolve-mode-from-event agent-session event))
         (binding (and mode-id
                       (gethash mode-id (session-mode-bindings agent-session))))
         (cb (and binding (mode-binding-context-binding binding)))
         (payload (kli/event:event-payload event))
         (usage-plist (and (listp payload) (getf payload :usage))))
    (when (and cb usage-plist)
      (setf (context-binding-usage cb)
            (usage-plist->context-usage usage-plist)))
    ;; The turn is over: drop live usage so the footer reads the committed
    ;; value until the next turn streams. Fires on completion, abort, and
    ;; error, since run-agent-loop always emits :agent/end.
    (when cb
      (setf (context-binding-live-usage cb) nil))
    (let* ((usage (and cb (context-binding-usage cb)))
           (agent (and binding (resolve-agent-from-binding binding context)))
           (selection (and agent (kli/agent/loop:agent-model-selection agent)))
           (accounting (and agent selection
                            (safe-account-provider-replay
                             (kli/agent/loop:agent-context agent)
                             selection
                             context)))
           (total (or (and accounting
                           (provider-accounting-result-total accounting))
                      (and usage (usage-total-tokens usage))))
           (window (and selection
                        (selection-context-window (model-registry-of context)
                                                  selection)))
           (ratio (and total window (plusp window)
                       (/ total (float window)))))
      (when (and ratio
                 (kli/ext:safely-invoke (session-compaction-policy agent-session)
                                        :session-policy '(:compaction :should-compact)
                                        :should-compact ratio))
        (kli/event:emit-event
         context
         (kli/event:make-event :session-compaction-needed
                               :payload (list :mode mode-id)
                               :source mode-id))))))

(defun default-passthrough (event)
  (declare (ignore event))
  nil)

(defun make-event-persistence-policy
    (&key (agent-message-handler  #'default-persist-agent-message)
          (model-change-handler   #'default-persist-model-change)
          (option-change-handler  #'default-persist-option-change)
          (custom-message-handler #'default-persist-custom-message)
          (context-patch-handler  #'default-handle-context-patch)
          (agent-end-handler      #'default-handle-agent-end)
          (default-handler        #'default-passthrough))
  "One of the LoL policy cells. Each follows make-FOO-policy and recode-FOO-policy with :inspect plus operation keys, matching kli/agent/loop's behavior cell. Structural compaction is fixed code, not a recodable slot, so a mis-recode cannot drop the cut, seal, and append invariant."
  (pandoriclet ((agent-message-handler  agent-message-handler)
                (model-change-handler   model-change-handler)
                (option-change-handler  option-change-handler)
                (custom-message-handler custom-message-handler)
                (context-patch-handler  context-patch-handler)
                (agent-end-handler      agent-end-handler)
                (default-handler        default-handler))
    (lambda (operation &rest arguments)
      (case operation
        (:inspect (list :agent-message-handler  agent-message-handler
                        :model-change-handler   model-change-handler
                        :option-change-handler  option-change-handler
                        :custom-message-handler custom-message-handler
                        :context-patch-handler  context-patch-handler
                        :agent-end-handler      agent-end-handler
                        :default-handler        default-handler))
        (:route
         (destructuring-bind (event agent-session context) arguments
           (case (kli/event:event-type event)
             (:agent-message
              (kli/ext:safely-invoke agent-message-handler
                                     :session-policy '(:persistence :agent-message)
                                     event))
             (:model-change
              (kli/ext:safely-invoke model-change-handler
                                     :session-policy '(:persistence :model-change)
                                     event))
             (:option-change
              (kli/ext:safely-invoke option-change-handler
                                     :session-policy '(:persistence :option-change)
                                     event))
             (:custom-message
              (kli/ext:safely-invoke custom-message-handler
                                     :session-policy '(:persistence :custom-message)
                                     event))
             (:context/patch-committed
              (kli/ext:safely-invoke context-patch-handler
                                     :session-policy '(:persistence :context-patch)
                                     agent-session event context))
             (:agent/usage
              (default-handle-agent-usage agent-session event context))
             (:agent/end
              (kli/ext:safely-invoke agent-end-handler
                                     :session-policy '(:persistence :agent-end)
                                     agent-session event context))
             (:session-compaction-needed
              (execute-session-compaction agent-session event context))
             (t (kli/ext:safely-invoke default-handler
                                       :session-policy '(:persistence :default)
                                       event)))))))))

(defun recode-event-persistence-policy
    (agent-session &key agent-message-handler model-change-handler
                     option-change-handler custom-message-handler
                     context-patch-handler agent-end-handler default-handler)
  (let ((state (funcall (session-event-persistence-policy agent-session)
                        :inspect)))
    (setf (session-event-persistence-policy agent-session)
          (make-event-persistence-policy
           :agent-message-handler
           (or agent-message-handler (getf state :agent-message-handler))
           :model-change-handler
           (or model-change-handler (getf state :model-change-handler))
           :option-change-handler
           (or option-change-handler (getf state :option-change-handler))
           :custom-message-handler
           (or custom-message-handler (getf state :custom-message-handler))
           :context-patch-handler
           (or context-patch-handler (getf state :context-patch-handler))
           :agent-end-handler
           (or agent-end-handler (getf state :agent-end-handler))
           :default-handler
           (or default-handler (getf state :default-handler)))))
  agent-session)

(defun default-skill-expander (text) text)
(defun default-template-expander (text) text)

(defun make-prompt-expansion-policy
    (&key (skill-expander    #'default-skill-expander)
          (template-expander #'default-template-expander)
          (input-interceptor nil))
  (pandoriclet ((skill-expander    skill-expander)
                (template-expander template-expander)
                (input-interceptor input-interceptor))
    (lambda (operation &rest arguments)
      (case operation
        (:inspect (list :skill-expander    skill-expander
                        :template-expander template-expander
                        :input-interceptor input-interceptor))
        (:expand
         (destructuring-bind (text images) arguments
           (let ((current-text text)
                 (current-images images)
                 (cancelled-p nil))
             (when input-interceptor
               (multiple-value-bind (t2 i2 c2)
                   (kli/ext:with-extension-fault-barrier
                       (:seam :session-policy
                        :id '(:prompt-expansion :input-interceptor))
                     (funcall input-interceptor current-text current-images))
                 (when t2 (setf current-text t2))
                 (when i2 (setf current-images i2))
                 (when c2 (setf cancelled-p t))))
             (unless cancelled-p
               (setf current-text
                     (kli/ext:with-extension-fault-barrier
                         (:seam :session-policy
                          :id '(:prompt-expansion :skill-expander)
                          :on-fault current-text)
                       (funcall skill-expander current-text)))
               (setf current-text
                     (kli/ext:with-extension-fault-barrier
                         (:seam :session-policy
                          :id '(:prompt-expansion :template-expander)
                          :on-fault current-text)
                       (funcall template-expander current-text))))
             (values current-text current-images cancelled-p))))))))

(defun recode-prompt-expansion-policy
    (agent-session &key skill-expander template-expander input-interceptor)
  (let ((state (funcall (session-prompt-expansion-policy agent-session) :inspect)))
    (setf (session-prompt-expansion-policy agent-session)
          (make-prompt-expansion-policy
           :skill-expander    (or skill-expander    (getf state :skill-expander))
           :template-expander (or template-expander (getf state :template-expander))
           :input-interceptor (or input-interceptor (getf state :input-interceptor)))))
  agent-session)

(defun make-context-transform-policy
    (&key (extra-messages-fn (constantly '()))
          (system-prompt-layers (kli/ext:make-layer-stack)))
  "The session context transform. The system prompt is an ordered set of named
layers, each (label . transform), composed in install order each submission;
retracting a layer by label is order-independent, so an extension that recodes
the shared prompt never strands a sibling's layer when it rolls back."
  (pandoriclet ((extra-messages-fn     extra-messages-fn)
                (system-prompt-layers  system-prompt-layers))
    (lambda (operation &rest arguments)
      (case operation
        (:inspect (list :extra-messages-fn    extra-messages-fn
                        :system-prompt-layers system-prompt-layers))
        ;; Per-submission: rewrite the system prompt in place by folding every
        ;; layer over the base in composition order.
        (:system-prompt
         (let ((base (first arguments)))
           (kli/ext:with-extension-fault-barrier
               (:seam :session-policy
                :id '(:context-transform :system-prompt)
                :on-fault base)
             (kli/ext:compose-layers system-prompt-layers base))))
        ;; Install or replace a named system-prompt layer, latest in order.
        (:add-system-prompt-layer
         (destructuring-bind (label transform &optional (kind :transform)) arguments
           (setf system-prompt-layers
                 (kli/ext:install-layer system-prompt-layers label transform
                                        :kind kind))))
        ;; Drop a named system-prompt layer; surviving layers keep their order.
        (:remove-system-prompt-layer
         (destructuring-bind (label) arguments
           (setf system-prompt-layers
                 (kli/ext:remove-layer-by-label system-prompt-layers label))))
        ;; Per-turn: fresh ephemeral messages, barrier yields NIL on fault.
        (:extra-messages
         (kli/ext:with-extension-fault-barrier
             (:seam :session-policy
              :id '(:context-transform :extra-messages))
           (funcall extra-messages-fn)))))))

(defun recode-context-transform-policy
    (agent-session &key extra-messages-fn)
  (let ((state (funcall (session-context-transform-policy agent-session) :inspect)))
    (setf (session-context-transform-policy agent-session)
          (make-context-transform-policy
           :extra-messages-fn (or extra-messages-fn (getf state :extra-messages-fn))
           :system-prompt-layers (getf state :system-prompt-layers))))
  agent-session)

(defun add-system-prompt-layer (agent-session label transform &key (kind :transform))
  "Install TRANSFORM as a system-prompt layer named LABEL on AGENT-SESSION, latest
in composition order. KIND is :transform (TRANSFORM takes the running prompt and
returns the next) or :append (TRANSFORM is a thunk whose block is concatenated).
Re-installing LABEL replaces it in place."
  (funcall (session-context-transform-policy agent-session)
           :add-system-prompt-layer label transform kind)
  agent-session)

(defun remove-system-prompt-layer (agent-session label)
  "Retract the system-prompt layer named LABEL from AGENT-SESSION. Order
independent: the surviving layers keep their relative composition order."
  (funcall (session-context-transform-policy agent-session)
           :remove-system-prompt-layer label)
  agent-session)

(defparameter +retryable-http-statuses+ '(408 429 500 502 503 504 529)
  "Provider statuses a retry can plausibly outlive: timeouts, rate limits,
and server-side failures. Other client errors are deterministic and
re-signal immediately.")

(defun transient-model-error-p (error)
  "True for failures worth retrying: network-layer transport errors and
provider errors carrying a transient HTTP status. Classification rides the
kli/ext condition protocol, so any producer's conditions participate."
  (case (kli/ext:condition-category error)
    (:network t)
    (:provider (and (member (kli/ext:condition-http-status error)
                            +retryable-http-statuses+)
                    (not (terminal-openai-usage-limit-error-p error))
                    t))
    (t nil)))

(defun make-retry-policy
    (&key (max-attempts 5)
          (base-delay-ms 1000)
          (retryable-p #'transient-model-error-p))
  (pandoriclet ((max-attempts  max-attempts)
                (base-delay-ms base-delay-ms)
                (retryable-p   retryable-p))
    (lambda (operation &rest arguments)
      (case operation
        (:inspect (list :max-attempts  max-attempts
                        :base-delay-ms base-delay-ms
                        :retryable-p   retryable-p))
        (:should-retry
         (destructuring-bind (error-payload attempt) arguments
           (cond
             ((>= attempt max-attempts)             (values nil nil))
             ((not (kli/ext:safely-invoke retryable-p
                                          :session-policy '(:retry :retryable-p)
                                          error-payload))
              (values nil nil))
             (t (values t (* base-delay-ms (expt 2 attempt)))))))))))

(defun recode-retry-policy
    (agent-session &key max-attempts base-delay-ms retryable-p)
  (let ((state (funcall (session-retry-policy agent-session) :inspect)))
    (setf (session-retry-policy agent-session)
          (make-retry-policy
           :max-attempts  (or max-attempts  (getf state :max-attempts))
           :base-delay-ms (or base-delay-ms (getf state :base-delay-ms))
           :retryable-p   (or retryable-p   (getf state :retryable-p)))))
  agent-session)

(defparameter +summarization-system-prompt+
  "You are a context summarization assistant. Your task is to read a conversation between a user and an AI coding assistant, then produce a structured summary following the exact format specified.

Do NOT continue the conversation. Do NOT respond to any questions in the conversation. ONLY output the structured summary.")

(defparameter +summarization-prompt+
  "The messages above are a conversation to summarize. Create a structured context checkpoint summary that another LLM will use to continue the work.

Use this EXACT format:

## Goal
[What is the user trying to accomplish? Can be multiple items if the session covers different tasks.]

## Constraints & Preferences
- [Any constraints, preferences, or requirements mentioned by user]
- [Or \"(none)\" if none were mentioned]

## Progress
### Done
- [x] [Completed tasks/changes]

### In Progress
- [ ] [Current work]

### Blocked
- [Issues preventing progress, if any]

## Key Decisions
- **[Decision]**: [Brief rationale]

## Next Steps
1. [Ordered list of what should happen next]

## Critical Context
- [Any data, examples, or references needed to continue]
- [Or \"(none)\" if not applicable]

Keep each section concise. Preserve exact file paths, function names, and error messages.")

(defparameter +update-summarization-prompt+
  "The messages above are NEW conversation messages to incorporate into the existing summary provided in <previous-summary> tags.

Update the existing structured summary with new information. RULES:
- PRESERVE all existing information from the previous summary
- ADD new progress, decisions, and context from the new messages
- UPDATE the Progress section: move items from \"In Progress\" to \"Done\" when completed
- UPDATE \"Next Steps\" based on what was accomplished
- PRESERVE exact file paths, function names, and error messages
- If something is no longer relevant, you may remove it

Use this EXACT format:

## Goal
[Preserve existing goals, add new ones if the task expanded]

## Constraints & Preferences
- [Preserve existing, add new ones discovered]

## Progress
### Done
- [x] [Include previously done items AND newly completed items]

### In Progress
- [ ] [Current work - update based on progress]

### Blocked
- [Current blockers - remove if resolved]

## Key Decisions
- **[Decision]**: [Brief rationale] (preserve all previous, add new)

## Next Steps
1. [Update based on current state]

## Critical Context
- [Preserve important context, add new if needed]

Keep each section concise. Preserve exact file paths, function names, and error messages.")

(defparameter +turn-prefix-summarization-prompt+
  "This is the PREFIX of a turn that was too large to keep. The SUFFIX (recent work) is retained.

Summarize the prefix to provide context for the retained suffix:

## Original Request
[What did the user ask for in this turn?]

## Early Progress
- [Key decisions and work done in the prefix]

## Context for Suffix
- [Information needed to understand the retained recent work]

Be concise. Focus on what's needed to understand the kept suffix.")

(defparameter +tool-result-max-chars+ 2000
  "Tool results are clipped to this many characters before summarization.")

(defun truncate-for-summary (text max-chars)
  (if (<= (length text) max-chars)
      text
      (format nil "~A~2%[... ~D more characters truncated]"
              (subseq text 0 max-chars)
              (- (length text) max-chars))))

(defun serialize-tool-call (tool-call)
  (format nil "~A(~A)"
          (getf tool-call :name)
          (or (getf tool-call :arguments-json) "")))

(defun serialize-conversation (messages)
  "Render MESSAGES as plain text so the model summarizes the conversation instead
of continuing it. Assistant thinking, text, and tool calls are emitted as
separate lines, and tool results are clipped."
  (let ((parts '()))
    (flet ((emit (line) (push line parts)))
      (dolist (message messages)
        (let ((role (kli/session/log:message-role message))
              (content (kli/session/log:message-content message))
              (metadata (kli/session/log:message-metadata message)))
          (case role
            ((:user :custom)
             (when (and (stringp content) (plusp (length content)))
               (emit (format nil "[User]: ~A" content))))
            (:assistant
             (let ((thinking (getf metadata :thinking-blocks))
                   (tool-calls (getf metadata :tool-calls)))
               (when thinking
                 (let ((text (format nil "~{~A~^~%~}"
                                     (mapcar (lambda (block)
                                               (or (getf block :thinking) ""))
                                             thinking))))
                   (when (plusp (length text))
                     (emit (format nil "[Assistant thinking]: ~A" text)))))
               (when (and (stringp content) (plusp (length content)))
                 (emit (format nil "[Assistant]: ~A" content)))
               (when tool-calls
                 (emit (format nil "[Assistant tool calls]: ~{~A~^; ~}"
                               (mapcar #'serialize-tool-call tool-calls))))))
            (:tool-result
             (when (and (stringp content) (plusp (length content)))
               (emit (format nil "[Tool result]: ~A"
                             (truncate-for-summary content
                                                   +tool-result-max-chars+)))))))))
    (format nil "~{~A~^~%~%~}" (nreverse parts))))

(defun run-summarization (messages base-prompt
                          &key agent-context lens-provider runtime-provider
                            runtime selection context previous-summary
                            on-request)
  "Frame MESSAGES as a serialized conversation under BASE-PROMPT and run one
isolated completion, returning the summary text. The completion is never seen by
the agent loop, so nothing is appended to the session. ON-REQUEST receives the
in-flight model request, giving abort a handle on the call."
  (let* ((serialized (serialize-conversation messages))
         (prompt (with-output-to-string (out)
                   (format out "<conversation>~%~A~%</conversation>~2%" serialized)
                   (when previous-summary
                     (format out "<previous-summary>~%~A~%</previous-summary>~2%"
                             previous-summary))
                   (write-string base-prompt out)))
         (entries-provider (session-entries-provider-of context))
         (user-message (provider-call entries-provider :make-user-message prompt))
         (sealed (provider-call lens-provider :seal-range-projection
                                agent-context (list user-message) context)))
    (provider-call runtime-provider :complete-text
                   runtime selection sealed context
                   :instructions +summarization-system-prompt+
                   :on-request on-request)))

(defun default-summarize (&key agent-context messages turn-prefix-messages
                            previous-summary custom-instructions
                            lens-provider runtime-provider runtime selection
                            context on-request
                          &allow-other-keys)
  "Summarize the to-be-compacted history through one isolated completion the agent
loop never sees, so no turn is appended to the session. A split turn adds a
separate prefix summary merged into the result. Custom instructions append a focus
line to the base directive. ON-REQUEST receives each in-flight model request so an
abort can interrupt the call. Returns the summary string, or NIL when the model
seam is unavailable."
  (when (and lens-provider runtime-provider runtime selection context)
    (let* ((base (if previous-summary
                     +update-summarization-prompt+
                     +summarization-prompt+))
           (base (if custom-instructions
                     (format nil "~A~2%Additional focus: ~A" base custom-instructions)
                     base)))
      (flet ((summarize (msgs prompt &optional prev)
               (run-summarization msgs prompt
                                  :agent-context agent-context
                                  :lens-provider lens-provider
                                  :runtime-provider runtime-provider
                                  :runtime runtime
                                  :selection selection
                                  :context context
                                  :previous-summary prev
                                  :on-request on-request)))
        (if turn-prefix-messages
            (let ((history (if messages
                               (summarize messages base previous-summary)
                               "No prior history."))
                  (prefix (summarize turn-prefix-messages
                                     +turn-prefix-summarization-prompt+)))
              (format nil "~A~2%---~2%**Turn Context (split turn):**~2%~A"
                      history prefix))
            (summarize messages base previous-summary))))))

(defun derived-compaction-keep-recent-tokens (context-window)
  (if (and context-window (plusp context-window))
      (min 50000 (max 20000 (floor (* context-window 0.05))))
      20000))

(defun make-compaction-policy
    (&key (enabled t)
          (threshold-ratio 0.85)
          keep-recent-tokens
          summary-reserve-tokens
          (summarizer #'default-summarize))
  (pandoriclet ((enabled            enabled)
                (threshold-ratio    threshold-ratio)
                (keep-recent-tokens keep-recent-tokens)
                (summary-reserve-tokens summary-reserve-tokens)
                (summarizer         summarizer))
    (lambda (operation &rest arguments)
      (case operation
        (:inspect (list :enabled            enabled
                        :threshold-ratio    threshold-ratio
                        :keep-recent-tokens keep-recent-tokens
                        :summary-reserve-tokens summary-reserve-tokens
                        :summarizer         summarizer))
        (:should-compact
         (let ((ratio (first arguments)))
           (and enabled ratio (>= ratio threshold-ratio))))
        (:summarize
         (apply summarizer arguments))))))

(defun recode-compaction-policy
    (agent-session &key (enabled nil enabled-supplied-p)
                        threshold-ratio
                        (keep-recent-tokens nil keep-recent-tokens-supplied-p)
                        (summary-reserve-tokens nil
                         summary-reserve-tokens-supplied-p)
                        summarizer)
  (let ((state (funcall (session-compaction-policy agent-session) :inspect)))
    (setf (session-compaction-policy agent-session)
          (make-compaction-policy
           :enabled            (if enabled-supplied-p
                                   enabled
                                   (getf state :enabled))
           :threshold-ratio    (or threshold-ratio (getf state :threshold-ratio))
           :keep-recent-tokens (if keep-recent-tokens-supplied-p
                                   keep-recent-tokens
                                   (getf state :keep-recent-tokens))
           :summary-reserve-tokens
           (if summary-reserve-tokens-supplied-p
               summary-reserve-tokens
               (getf state :summary-reserve-tokens))
           :summarizer         (or summarizer (getf state :summarizer)))))
  agent-session)

(defparameter *pending-command-record-limit* 8
  "Most command records held for the next prompt. Recording past the limit
drops the oldest record and the injected block surfaces the dropped count.")

(defparameter *pending-command-text-limit* 2000
  "Largest command output carried into a pending record, in characters. The
session log keeps the full result -- the injected context block gets the
clamped text with a visible truncation marker.")

(defun pending-command-text (content)
  "Joined text of a command result's CONTENT plists, clamped to
*pending-command-text-limit* with a visible marker."
  (let ((text (format nil "~{~A~^~%~}"
                      (loop for part in content
                            for part-text = (and (consp part)
                                                 (getf part :text))
                            when part-text collect part-text))))
    (if (> (length text) *pending-command-text-limit*)
        (format nil "~A~%[output truncated at ~D characters]"
                (subseq text 0 *pending-command-text-limit*)
                *pending-command-text-limit*)
        text)))

(defun record-pending-command (agent-session payload)
  "Queue a :command/result event PAYLOAD for injection into the next prompt.
Visibility is resolved at invoke time and defaults on -- only commands that
declared :model-visible nil (pure display, secret-bearing tails) arrive here
flagged off and stay out of the model's context. A result with no text and
no error flag queues nothing either: an empty result means the feedback
happened in the UI (a selection menu), and a blank record would mislead the
model about output the user never saw. Past *pending-command-record-limit*
the oldest record drops and the drop is counted for the block."
  (when (getf payload :model-visible)
    (let ((text (pending-command-text (getf payload :content))))
      (when (or (plusp (length text)) (getf payload :error-p))
        (sb-thread:with-mutex ((session-pending-command-lock agent-session))
          (let ((records (session-pending-command-records agent-session)))
            (when (>= (length records) *pending-command-record-limit*)
              (setf records (rest records))
              (incf (session-pending-command-dropped agent-session)))
            (setf (session-pending-command-records agent-session)
                  (append records
                          (list (list :command (getf payload :command)
                                      :tail (getf payload :tail)
                                      :error-p (getf payload :error-p)
                                      :text text))))))))))

(defun drain-pending-command-block (agent-session)
  "Pending command records as a caveated context block for the next user
message, or NIL when nothing is pending. Draining clears the queue, so a
prompt retracted after its submit consumed the records does not replay them
into a later turn."
  (sb-thread:with-mutex ((session-pending-command-lock agent-session))
    (let ((records (session-pending-command-records agent-session))
          (dropped (session-pending-command-dropped agent-session)))
      (when records
        (setf (session-pending-command-records agent-session) '()
              (session-pending-command-dropped agent-session) 0)
        (with-output-to-string (stream)
          (format stream "<local-command-output>~%The user ran the following ~
command~P locally in the session UI. The output was already shown to them. ~
Treat it as shared context -- do not respond to it directly unless it is ~
relevant to the user's message.~%" (length records))
          (when (plusp dropped)
            (format stream "[~D earlier command record~:P dropped]~%" dropped))
          (dolist (record records)
            (format stream "~%/~A~@[ ~A~]~:[~; (error)~]:~%~A~%"
                    (getf record :command)
                    (getf record :tail)
                    (getf record :error-p)
                    (getf record :text)))
          (format stream "</local-command-output>~%~%"))))))

(defun clear-pending-command-records (agent-session)
  "Drop every queued model-visible command record without injecting it. A
session switch abandons the departing conversation's between-turn UI context,
so its undrained command output must not ride into the session switched to. The
output was already shown to the user in the old session's UI."
  (sb-thread:with-mutex ((session-pending-command-lock agent-session))
    (setf (session-pending-command-records agent-session) '()
          (session-pending-command-dropped agent-session) 0)))

(defun make-agent-session-service
    (&key (id :agent-session-service)
       event-persistence-policy
       prompt-expansion-policy
       context-transform-policy
       retry-policy
       compaction-policy)
  (make-instance 'agent-session
                 :id id
                 :event-persistence-policy
                 (or event-persistence-policy
                     (make-event-persistence-policy))
                 :prompt-expansion-policy
                 (or prompt-expansion-policy
                     (make-prompt-expansion-policy))
                 :context-transform-policy
                 (or context-transform-policy
                     (make-context-transform-policy))
                 :retry-policy
                 (or retry-policy
                     (make-retry-policy))
                 :compaction-policy
                 (or compaction-policy
                     (make-compaction-policy))))
