(in-package #:kli/tests)

(defun agent-session-provider (protocol)
  (ext:require-capability-provider protocol
                                   :agent/session
                                   :contract :agent/session/v1))

(defun agent-session-service (context)
  (kli:find-live-object (kli:context-registry context)
                        :agent-session-service))

(defun settle-tui-app (app)
  "Join any in-flight agent or command worker and drain its marshaled
   projections, so assertions observe the finished work exactly as a trampoline
   iteration would. A bound-mode submit runs the agent loop off the input thread,
   and a slash command (e.g. /compact) runs its body on the command worker; both
   queue their renders onto the loop thread, so synchronous reads must wait."
  (dolist (thread (list (tui-app:tui-app-agent-worker-thread app)
                        (tui-app:tui-app-command-worker-thread app)))
    (when thread (sb-thread:join-thread thread)))
  (kli/tui/app:run-pending-main-thread-tasks app))

(defun agent-session-test-context (&key tools)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*
                        agents:*agent-loop-extension-manifest*
                        agent-session:*agent-session-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (when tools
      (install-extension context *agent-loop-test-tools-extension-manifest*))
    (values context protocol)))

(defun bind-agent-session-mode
    (context &key (mode-id :default-mode)
                  (session-id :agent-session-test-session)
                  (provider-id "agent-session-provider")
                  (model-id "agent-session-model")
                  (deltas '("hello back"))
                  metadata option-schemas options)
  (let* ((store (session-log-store context))
         (session (sess:create-session store context :id session-id))
         (_selection
           (agent-loop-register-model context provider-id model-id
                                      :metadata
                                      (or metadata
                                          (list :fake-deltas deltas))
                                      :option-schemas option-schemas
                                      :options options))
         (service (agent-session-service context)))
    (declare (ignore _selection))
    (agent-session:switch-agent-session service mode-id (kli:object-id session)
                                        context)
    (values service session)))

(test (agent-session-mode-tables-are-synchronized :fixture interactive-authority)
  "Mode bindings mutate from the TUI loop thread while agent worker threads
resolve modes per emitted event -- both tables are cross-thread."
  (let* ((context (agent-session-test-context))
         (service (bind-agent-session-mode context)))
    (is (sb-ext:hash-table-synchronized-p
         (agent-session:session-mode-bindings service)))
    (is (sb-ext:hash-table-synchronized-p
         (agent-session:session-source->mode service)))))

(test (agent-session-abort-fires-only-while-busy :fixture interactive-authority)
  "Idle, abort is a no-op. Streaming, abort requests cooperative cancellation."
  (let* ((context (agent-session-test-context))
         (service (bind-agent-session-mode context :deltas '("ok")))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding))))
    (is (not (agent-session:agent-session-busy-p service :default-mode context)))
    (is (null (agent-session:abort-agent-session service :default-mode context)))
    (is (null (agents:agent-abort-requested-p agent)))
    (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (is (agent-session:agent-session-busy-p service :default-mode context))
    (is (eq t (agent-session:abort-agent-session service :default-mode context)))
    (is (agents:agent-abort-requested-p agent))))

(test default-agent-system-prompt-includes-tools-guidelines-and-context
  "The minimal system prompt lists the live tools, the guidelines, and the
current date plus working directory."
  (multiple-value-bind (context protocol)
      (agent-session-test-context :tools t)
    (declare (ignore protocol))
    (let ((prompt (agent-session:default-agent-system-prompt context)))
      (is (search "You are kli" prompt))
      (is (search "Your name is kli" prompt))
      (is (search "never identify as ChatGPT" prompt))
      (is (search "Available tools:" prompt))
      (is (search "Echo a message for agent-loop tests." prompt))
      (is (search "Guidelines:" prompt))
      (is (search "Be concise in your responses." prompt))
      (is (search "authoritative" prompt))
      (is (search "Current date:" prompt))
      (is (search "Current working directory:" prompt)))))

(test default-agent-system-prompt-builds-without-tools
  (multiple-value-bind (context protocol)
      (agent-session-test-context)
    (declare (ignore protocol))
    (let ((prompt (agent-session:default-agent-system-prompt context)))
      (is (search "You are kli" prompt))
      (is (search "Available tools:" prompt))
      (is (search "Current working directory:" prompt)))))

(test agent-session-registers-provider-service-and-live-object
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*)
    (signals error
      (install-extension context
                         agent-session:*agent-session-extension-manifest*)))
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (is (typep (agent-session-service context) 'agent-session:agent-session))
    (is (agent-session-provider protocol))))

(test (agent-session-switch-creates-binding-and-emits-event :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((switched '())
           (service (agent-session-service context)))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :listener
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :session-switch)
            (push (list mode-id (event:event-payload event)) switched)))
        :filter (lambda (event)
                  (eq (event:event-type event) :session-switch)))
       context)
      (bind-agent-session-mode context)
      (let ((bindings (agent-session:session-mode-bindings service)))
        (is (= 1 (hash-table-count bindings)))
        (let ((binding (gethash :default-mode bindings)))
          (is (typep binding 'agent-session:mode-binding))
          (is (eq :default-mode
                  (agent-session:mode-binding-mode-id binding)))
          (is (eq :agent-session-test-session
                  (agent-session:session-binding-session-id
                   (agent-session:mode-binding-session-binding binding))))
          (is (not (null
                    (agent-session:mode-binding-agent-id binding))))))
      (is (= 1 (length switched)))
      (is (eq :default-mode (first (first switched))))
      (is (eq :agent-session-test-session
              (getf (second (first switched)) :session-id))))))

(test (agent-session-submit-prompts-agent-and-appends-messages :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :deltas '("greeting back"))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent-id (agent-session:mode-binding-agent-id binding))
           (agent (kli:find-live-object (kli:context-registry context)
                                        agent-id)))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (is (agents:agent-idle-p agent))
      (is (= 1 (length (agents:agent-turns agent))))
      (is (equal '(:user :assistant)
                 (agent-session-message-roles agent)))
      (is (equal '("hello" "greeting back")
                 (agent-session-message-contents agent))))))

(test (agent-session-submit-returns-agent-and-final-response-text :fixture interactive-authority)
  "submit-agent-session-prompt returns the agent and, as a second value, the
final assistant text of the completed run, so a synchronous caller reads the
reply without reconstructing it from streamed deltas."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :deltas '("the final reply"))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (multiple-value-bind (returned-agent final-text)
          (agent-session:submit-agent-session-prompt service :default-mode
                                                     "hello" context)
        (is (eq agent returned-agent)
            "the first value is still the agent")
        (is (equal "the final reply" final-text))))))

(test (agent-session-submit-empty-final-turn-returns-empty-string :fixture interactive-authority)
  "A completed run whose final assistant content is empty returns \"\", not NIL,
so a caller can tell a silent reply from no reply."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :deltas '(""))
    (let ((service (agent-session-service context)))
      (is (equal ""
                 (nth-value 1
                            (agent-session:submit-agent-session-prompt
                             service :default-mode "hello" context)))))))

(test (agent-session-submit-aborted-run-returns-nil-final-text :fixture interactive-authority)
  "An aborted run returns NIL as the final text, never the prior completed
turn's stale reply."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :deltas '("first reply"))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "one" context)
      (rt:register-model-stream-adapter
       (model-runtime-service context) :fake
       (lambda (provider request ctx &key emit)
         (declare (ignore provider request))
         (funcall emit (rt:make-assistant-delta "partial reply"))
         (agents:abort-agent agent ctx)
         (error "stream torn down by abort"))
       context)
      (multiple-value-bind (returned-agent final-text)
          (agent-session:submit-agent-session-prompt service :default-mode
                                                     "two" context)
        (is (eq agent returned-agent))
        (is (null final-text)
            "an aborted run yields no final text")))))

(defvar *captured-model-messages* nil
  "Per-turn model-visible message contents, captured by the fake stream adapter
before the runtime releases the request snapshot. Newest turn first.")

(defun install-capturing-fake-adapter (context)
  "Record each turn's model-visible message contents, then emit the provider's
deltas. Replaces the harness :fake adapter so the contents are observable; the
runtime nulls model-request-model-messages once streaming ends."
  (rt:register-model-stream-adapter
   (model-runtime-service context) :fake
   (lambda (provider request ctx &key emit)
     (declare (ignore ctx))
     (push (mapcar (lambda (m) (getf m :content))
                   (rt:model-request-model-messages request))
           *captured-model-messages*)
     (dolist (text (test-fake-provider-text-deltas provider))
       (funcall emit (rt:make-assistant-delta text))))
   context))

(defun bind-mode-with-sentinel-extras (context)
  "Bind a mode, capture model-visible messages, and recode the context-transform
policy to inject one ephemeral user message each turn. Returns the bound agent."
  (bind-agent-session-mode context :deltas '("ok"))
  (install-capturing-fake-adapter context)
  (let ((service (agent-session-service context)))
    (agent-session:recode-context-transform-policy
     service :extra-messages-fn
     (constantly (list (sess:make-user-message "EPHEMERAL-SENTINEL"))))
    (let ((binding (gethash :default-mode
                            (agent-session:session-mode-bindings service))))
      (kli:find-live-object (kli:context-registry context)
                            (agent-session:mode-binding-agent-id binding)))))

(defun sentinel-count (messages)
  (count "EPHEMERAL-SENTINEL" messages :test #'equal))

(test (agent-session-extra-messages-reach-the-model-turn :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((*captured-model-messages* nil))
      (bind-mode-with-sentinel-extras context)
      (agent-session:submit-agent-session-prompt (agent-session-service context)
                                                 :default-mode "hello" context)
      (is (= 1 (length *captured-model-messages*)))
      (is (= 1 (sentinel-count (first *captured-model-messages*)))
          "the ephemeral extra reaches the model request")
      (is (member "hello" (first *captured-model-messages*) :test #'equal)
          "the real user turn is present too"))))

(test (agent-session-extra-messages-are-not-persisted :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((*captured-model-messages* nil)
           (agent (bind-mode-with-sentinel-extras context)))
      (agent-session:submit-agent-session-prompt (agent-session-service context)
                                                 :default-mode "hello" context)
      (is (equal '(:user :assistant) (agent-session-message-roles agent)))
      (is (equal '("hello" "ok") (agent-session-message-contents agent))
          "the durable session excludes the ephemeral extra"))))

(test (agent-session-extra-messages-do-not-accumulate-across-turns :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((*captured-model-messages* nil)
           (agent (bind-mode-with-sentinel-extras context)))
      (agent-session:submit-agent-session-prompt (agent-session-service context)
                                                 :default-mode "one" context)
      (agent-session:submit-agent-session-prompt (agent-session-service context)
                                                 :default-mode "two" context)
      (is (= 2 (length *captured-model-messages*)))
      (dolist (turn *captured-model-messages*)
        (is (= 1 (sentinel-count turn))
            "each turn carries exactly one ephemeral extra"))
      (is (not (member "EPHEMERAL-SENTINEL"
                       (agent-session-message-contents agent)
                       :test #'equal))
          "no ephemeral extra is ever persisted"))))

(test (agent-session-branch-creates-new-session-and-emits-branch-event :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent-id (agent-session:mode-binding-agent-id binding))
           (agent (kli:find-live-object (kli:context-registry context)
                                        agent-id))
           (events '()))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :branch-listener
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :session-branch)
            (push (list mode-id (event:event-payload event)) events))))
       context)
      (let* ((store (agents:agent-store agent))
             (session (agents:agent-session agent))
             (first-entry
               (sess:append-session-entry
                store session
                (sess:make-message-entry
                 (sess:make-user-message "first message"))
                context))
             (branch-point-id (kli:object-id first-entry)))
        (sess:append-session-entry
         store session
         (sess:make-message-entry
          (sess:make-user-message "after branch point"))
         context)
        (let ((new-session-id
                (agent-session:branch-agent-session service :default-mode
                                                    branch-point-id context)))
          (is (not (eq :agent-session-test-session new-session-id)))
          (let ((new-session (sess:find-session store new-session-id)))
            (is (typep new-session 'sess:session))
            (is (eq :agent-session-test-session
                    (getf (sess:session-metadata new-session) :branched-from)))
            (is (eq branch-point-id
                    (getf (sess:session-metadata new-session) :branched-at)))
            (is (eq branch-point-id (sess:session-leaf-id new-session)))
            (is (= 1 (length (sess:session-entries new-session)))))))
      (is (= 1 (length events)))
      (is (eq :default-mode (first (first events))))
      (is (not (eq :agent-session-test-session
                   (getf (second (first events)) :new-session-id)))))))

(defun mode-session-id (service mode-id)
  (let ((binding (gethash mode-id (agent-session:session-mode-bindings service))))
    (and binding
         (agent-session:session-binding-session-id
          (agent-session:mode-binding-session-binding binding)))))

(test (agent-session-reset-starts-fresh-session-keeping-binding-and-model :fixture interactive-authority)
  "Reset swaps in a brand-new, empty session while the model selection carries
across from the registry current."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (store (session-log-store context))
           (old-session-id (mode-session-id service :default-mode))
           (resets '()))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :reset-listener
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :session-reset)
            (push mode-id resets))))
       context)
      (agent-session:reset-agent-session service :default-mode context)
      (is (= 1 (length resets)))
      (is (eq :default-mode (first resets)))
      (let ((binding (gethash :default-mode
                              (agent-session:session-mode-bindings service))))
        (is (not (null binding)))
        (let ((new-session-id (mode-session-id service :default-mode)))
          (is (not (eq old-session-id new-session-id)))
          (let ((new-session (sess:find-session store new-session-id)))
            (is (typep new-session 'sess:session))
            (is (null (sess:session-entries new-session)))))
        (let ((agent (kli:find-live-object
                      (kli:context-registry context)
                      (agent-session:mode-binding-agent-id binding))))
          (is (not (null (agents:agent-model-selection agent)))))))))

(test (agent-session-rewind-branches-before-the-latest-prompt :fixture interactive-authority)
  "Rewind steps the conversation back one user turn: branch at the entry
before the latest user prompt, switch the mode onto the branch, leave the
original session intact, and report the rewound text for editor restore.
Steer and follow-up user entries belong to the turn they steered, so only
prompt entries are rewind targets."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding)))
           (store (agents:agent-store agent))
           (session (agents:agent-session agent))
           (rewinds '()))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :rewind-listener
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :session-rewind)
            (push (list mode-id (event:event-payload event)) rewinds))))
       context)
      (flet ((add (message &optional metadata)
               (sess:append-session-entry
                store session
                (sess:make-message-entry message :metadata metadata)
                context)))
        (add (sess:make-user-message "first prompt")
             '(:agent-input :prompt))
        (let ((pre-second (add (sess:make-assistant-message "first reply"))))
          (add (sess:make-user-message "second prompt")
               '(:agent-input :prompt))
          (add (sess:make-user-message "mid-turn steer")
               '(:agent-input :steer))
          (add (sess:make-assistant-message "second reply"))
          (is (agent-session:rewind-agent-session-available-p
               service :default-mode context))
          (multiple-value-bind (new-session-id text)
              (agent-session:rewind-agent-session service :default-mode
                                                  context)
            (is (eq new-session-id (mode-session-id service :default-mode)))
            (is (string= "second prompt" text))
            (let ((new-session (sess:find-session store new-session-id)))
              (is (eq (kli:object-id pre-second)
                      (sess:session-leaf-id new-session)))
              (is (= 2 (length (sess:session-entries new-session)))))
            (is (= 5 (length (sess:session-entries session)))
                "the rewound-away session keeps all its entries for /resume")
            (is (= 1 (length rewinds)))
            (let ((payload (second (first rewinds))))
              (is (eq new-session-id (getf payload :new-session-id)))
              (is (string= "second prompt" (getf payload :text))))))))))

(test (agent-session-rewind-first-turn-falls-back-to-fresh-session :fixture interactive-authority)
  "The first prompt has no parent entry to branch at, so rewinding it starts
a fresh empty session -- the same observable state as branching before the
conversation began."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding)))
           (store (agents:agent-store agent))
           (session (agents:agent-session agent))
           (old-session-id (mode-session-id service :default-mode)))
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-user-message "only prompt"))
       context)
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-assistant-message "only reply"))
       context)
      (multiple-value-bind (new-session-id text)
          (agent-session:rewind-agent-session service :default-mode context)
        (is (not (eq old-session-id new-session-id)))
        (is (eq new-session-id (mode-session-id service :default-mode)))
        (is (string= "only prompt" text))
        (is (null (sess:session-entries
                   (sess:find-session store new-session-id))))))))

(test (agent-session-rewind-nothing-to-rewind :fixture interactive-authority)
  "An empty session has no rewind target: available-p is false and rewind
returns NIL without branching."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let ((service (agent-session-service context))
          (old-session-id nil))
      (setf old-session-id (mode-session-id service :default-mode))
      (is (not (agent-session:rewind-agent-session-available-p
                service :default-mode context)))
      (is (null (agent-session:rewind-agent-session service :default-mode
                                                    context)))
      (is (eq old-session-id (mode-session-id service :default-mode))))))

(test (agent-session-rewind-restores-typed-text-over-expanded :fixture interactive-authority)
  "A prompt entry's content is the expanded text (it may carry a prepended
command block). The :typed-text message metadata preserves what the user
submitted, and rewind prefers it for the editor restore."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding)))
           (store (agents:agent-store agent))
           (session (agents:agent-session agent)))
      (sess:append-session-entry
       store session
       (sess:make-message-entry
        (sess:make-user-message "first prompt"))
       context)
      (sess:append-session-entry
       store session
       (sess:make-message-entry
        (sess:make-user-message
         "<local-command-output>...</local-command-output> expanded body"
         :metadata '(:typed-text "typed body")))
       context)
      (is (string= "typed body"
                   (nth-value 1 (agent-session:rewind-agent-session
                                 service :default-mode context)))))))

(test (agent-session-rewind-targets-and-entry-id-selection :fixture interactive-authority)
  "list-rewind-targets lists the branch's user prompts newest first as
(:entry-id :text) rows, and rewind-agent-session :entry-id branches before
the chosen prompt rather than the latest."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding)))
           (store (agents:agent-store agent))
           (session (agents:agent-session agent)))
      (flet ((add (message)
               (sess:append-session-entry
                store session (sess:make-message-entry message) context)))
        (add (sess:make-user-message "first prompt"))
        (let ((pre-second (add (sess:make-assistant-message "first reply")))
              (second-prompt (add (sess:make-user-message "second prompt"))))
          (add (sess:make-assistant-message "second reply"))
          (add (sess:make-user-message "third prompt"))
          (add (sess:make-assistant-message "third reply"))
          (let ((targets (agent-session:list-rewind-targets
                          service :default-mode context)))
            (is (equal '("third prompt" "second prompt" "first prompt")
                       (mapcar (lambda (target) (getf target :text))
                               targets)))
            (is (eq (kli:object-id second-prompt)
                    (getf (second targets) :entry-id))))
          (multiple-value-bind (new-session-id text)
              (agent-session:rewind-agent-session
               service :default-mode context
               :entry-id (kli:object-id second-prompt))
            (is (string= "second prompt" text))
            (is (eq new-session-id (mode-session-id service :default-mode)))
            (let ((new-session (sess:find-session store new-session-id)))
              (is (eq (kli:object-id pre-second)
                      (sess:session-leaf-id new-session))
                  "the branch cuts at the chosen prompt's parent")
              (is (= 2 (length (sess:session-entries new-session)))))))))))

(test (agent-session-clear-drops-binding-and-source-index :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent-id (agent-session:mode-binding-agent-id binding))
           (cb (agent-session:mode-binding-context-binding binding))
           (context-id (agent-session:context-binding-context-id cb))
           (cleared '()))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :clear-listener
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :session-cleared)
            (push mode-id cleared))))
       context)
      (agent-session:clear-agent-session service :default-mode context)
      (is (null (gethash :default-mode
                         (agent-session:session-mode-bindings service))))
      (is (null (gethash agent-id
                         (agent-session:session-source->mode service))))
      (is (null (gethash context-id
                         (agent-session:session-source->mode service))))
      (is (= 1 (length cleared))))))

(test (agent-session-clear-on-missing-binding-is-silent :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (agent-session-service context)))
      (is (null (agent-session:clear-agent-session service :missing-mode
                                                   context))))))

(test agent-session-recode-policies-preserve-service-identity
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((service (agent-session-service context))
           (identity service)
           (event-cell-before
             (agent-session:session-event-persistence-policy service)))
      (agent-session:recode-event-persistence-policy
       service :default-handler (lambda (event)
                                  (declare (ignore event))
                                  :recoded))
      (agent-session:recode-prompt-expansion-policy
       service :skill-expander (lambda (text)
                                 (concatenate 'string ">>" text)))
      (agent-session:recode-context-transform-policy
       service :extra-messages-fn (constantly '()))
      (agent-session:recode-retry-policy service :max-attempts 9)
      (agent-session:recode-compaction-policy service :threshold-ratio 0.5)
      (is (eq identity (agent-session-service context)))
      (is (not (eq event-cell-before
                   (agent-session:session-event-persistence-policy
                    service))))
      (is (eq :recoded
              (funcall (agent-session:session-event-persistence-policy
                        service)
                       :route
                       (event:make-event :unhandled-kind)
                       service
                       context)))
      (is (string= ">>hi"
                   (funcall (agent-session:session-prompt-expansion-policy
                             service)
                            :expand "hi" nil)))
      (is (= 9 (getf (funcall (agent-session:session-retry-policy service)
                              :inspect)
                     :max-attempts))))))

(test (agent-session-listener-register-unregister-is-idempotent :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((service (agent-session-service context))
           (listener (agent-session:make-session-event-listener
                      :probe
                      (lambda (event mode-id ctx)
                        (declare (ignore event mode-id ctx)) nil))))
      (agent-session:register-session-event-listener service listener context)
      (is (= 1 (length (agent-session:session-event-listeners service))))
      (is (typep (agent-session:unregister-session-event-listener
                  service :probe context)
                 'agent-session:session-event-listener))
      (is (null (agent-session:session-event-listeners service)))
      (is (null (agent-session:unregister-session-event-listener
                 service :probe context))))))

(test agent-session-capability-gates-deny-restricted-subject
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (agent-session-service context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (agent-session:switch-agent-session service :default-mode
                                              :nope context))
        (signals ext:capability-denied
          (agent-session:reset-agent-session service :default-mode context))
        (signals ext:capability-denied
          (agent-session:clear-agent-session service :default-mode context))
        (signals ext:capability-denied
          (agent-session:register-session-event-listener
           service
           (agent-session:make-session-event-listener
            :denied
            (lambda (event mode-id ctx)
              (declare (ignore event mode-id ctx)) nil))
           context))))))

(test (agent-session-context-patch-committed-routes-to-listener :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent-id (agent-session:mode-binding-agent-id binding))
           (agent (kli:find-live-object (kli:context-registry context)
                                        agent-id))
           (agent-context (agents:agent-context agent))
           (patch-events '()))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :patch-watcher
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :context/patch-committed)
            (push mode-id patch-events)))
        :filter (lambda (event)
                  (eq (event:event-type event) :context/patch-committed)))
       context)
      (ctx:stage-context-patch
       agent-context
       (ctx:make-append-message-patch
        (sess:make-user-message "patched in")))
      (ctx:commit-context-patches agent-context context)
      (is (= 1 (length patch-events)))
      (is (eq :default-mode (first patch-events)))
      (let* ((cb (agent-session:mode-binding-context-binding binding)))
        (is (null (agent-session:context-binding-rebuilt-at cb)))))))

(test (agent-session-set-model-without-credential-signals-error :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (registry (model-registry context)))
      (models:register-model-provider
       registry
       (models:make-model-provider "needs-auth-provider" :fake)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition "needs-auth-provider" "needs-auth-model"
                                     :fake)
       context)
      (let* ((model (models:find-model-definition registry
                                                  "needs-auth-provider"
                                                  "needs-auth-model"))
             (selection (models:select-model registry model context)))
        (handler-case
            (progn
              (agent-session:set-agent-session-model service :default-mode
                                                     selection context)
              (is nil "Expected agent-session-error"))
          (agent-session:agent-session-error (condition)
            (is (eq :no-credential
                    (agent-session:agent-session-error-reason condition)))
            (is (string= "needs-auth-provider"
                         (agent-session:agent-session-error-provider-id
                          condition)))))))))

(test agent-session-error-reports-remediation-and-classifies-config
  "User-actionable reasons report what to run instead of a raw keyword, and
classify :config so the TUI sheds the \"Internal error\" label. Invariant
reasons keep the generic report and stay :internal."
  (let ((c (make-condition 'agent-session:agent-session-error
                           :reason :no-credential
                           :provider-id "openai-codex")))
    (is (string= "No credential for openai-codex -- run /auth login openai-codex"
                 (princ-to-string c)))
    (is (eq :config (ext:condition-category c))))
  (let ((c (make-condition 'agent-session:agent-session-error
                           :reason :no-model)))
    (is (string= "No model selected -- run /model to choose one"
                 (princ-to-string c)))
    (is (eq :config (ext:condition-category c))))
  (let ((c (make-condition 'agent-session:agent-session-error
                           :reason :no-current-model)))
    (is (string= "No model selected -- run /model to choose one"
                 (princ-to-string c)))
    (is (eq :config (ext:condition-category c))))
  (let ((c (make-condition 'agent-session:agent-session-error
                           :reason :no-agent)))
    (is (search ":NO-AGENT" (princ-to-string c)))
    (is (eq :internal (ext:condition-category c)))))

(test (agent-session-deactivate-removes-service-and-after-fanout :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (bind-agent-session-mode context)
    (let* ((service-before (agent-session-service context))
           (extension (kli:find-live-object (kli:context-registry context)
                                            :agent-session))
           (post-deactivate-fires 0))
      (is (not (null service-before)))
      (agent-session:register-session-event-listener
       service-before
       (agent-session:make-session-event-listener
        :post-deactivate
        (lambda (event mode-id ctx)
          (declare (ignore event mode-id ctx))
          (incf post-deactivate-fires)))
       context)
      (ext:deactivate-extension protocol extension context)
      (is (null (agent-session-service context)))
      (event:emit-event context (event:make-event :probe :source :probe))
      (is (zerop post-deactivate-fires)))))

(test agent-session-declares-event-types-in-manifest
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore context))
    (let ((declared (event:protocol-event-types protocol)))
      (dolist (kind '(:session-switch :session-branch :session-reset
                      :session-cleared :session-focus :listener-registered
                      :listener-unregistered :model-change :option-change
                      :session-compaction-needed))
        (is (typep (gethash kind declared)
                   'event:event-type-contribution)
            "missing event-type declaration: ~S" kind)))))

(test (agent-session-event-types-retract-when-extension-deactivates :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (let* ((declared-before (event:protocol-event-types protocol))
           (extension (kli:find-live-object (kli:context-registry context)
                                            :agent-session)))
      (is (gethash :session-switch declared-before))
      (ext:deactivate-extension protocol extension context)
      (let ((declared-after (event:protocol-event-types protocol)))
        (dolist (kind '(:session-switch :session-branch :session-reset
                        :session-cleared :listener-registered
                        :listener-unregistered :model-change :option-change
                        :session-compaction-needed))
          (is (null (gethash kind declared-after))
              "event-type lingered after retract: ~S" kind))))))

(test (agent-session-option-change-payload-carries-selection :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (service _session)
        (bind-agent-session-mode context
                                 :option-schemas
                                 (list (test-reasoning-effort-schema)))
      (declare (ignore _session))
      (let ((payloads '()))
        (agent-session:register-session-event-listener
         service
         (agent-session:make-session-event-listener
          :option-watcher
          (lambda (event mode-id ctx)
            (declare (ignore mode-id ctx))
            (when (eq (event:event-type event) :option-change)
              (push (event:event-payload event) payloads))))
         context)
        (agent-session:set-agent-session-option service :default-mode
                                                :reasoning-effort :high context)
        (is (= 1 (length payloads)))
        (let ((payload (first payloads)))
          (is (eq :reasoning-effort (getf payload :option-id)))
          (is (eq :high (getf payload :value)))
          (is (typep (getf payload :selection) 'models:model-selection))
          (is (eq :high (test-selection-reasoning-effort
                         (getf payload :selection)))))))))

(test (agent-session-branch-failed-on-bad-entry-signals-error :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let ((service (agent-session-service context)))
      (signals error
        (agent-session:branch-agent-session service :default-mode
                                            :nonexistent-entry context)))))

(test (agent-session-switch-deregisters-prior-agent :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (service _first-session) (bind-agent-session-mode context)
      (declare (ignore _first-session))
      (let* ((registry (kli:context-registry context))
             (binding (gethash :default-mode
                               (agent-session:session-mode-bindings service)))
             (first-agent-id (agent-session:mode-binding-agent-id binding))
             (loop-service (kli:find-live-object registry
                                                 :agent-loop-service))
             (second-session (sess:create-session
                              (session-log-store context) context
                              :id :agent-session-second-session)))
        (is (kli:find-live-object registry first-agent-id))
        (agent-session:switch-agent-session service :default-mode
                                            (kli:object-id second-session)
                                            context)
        (is (null (kli:find-live-object registry first-agent-id)))
        (is (null (gethash first-agent-id
                           (agents:agent-loop-service-agents loop-service))))))))

(test (agent-session-compaction-triggers-when-usage-exceeds-threshold :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode
     context
     :metadata (list :fake-deltas '("done")
                     :fake-usage (list :input-tokens 30 :output-tokens 50
                                       :total-tokens 80)
                     :context-window 100))
    (let* ((service (agent-session-service context))
           (compaction-events '()))
      (agent-session:recode-compaction-policy service
                                              :threshold-ratio 0.0)
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :compaction-watcher
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :session-compaction-needed)
            (push mode-id compaction-events))))
       context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (is (= 1 (length compaction-events)))
      (is (eq :default-mode (first compaction-events))))))

(test (agent-session-compaction-skipped-below-threshold :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode
     context
     :metadata (list :fake-deltas '("done")
                     :fake-usage (list :total-tokens 10)
                     :context-window 100))
    (let* ((service (agent-session-service context))
           (events '()))
      (agent-session:recode-compaction-policy service
                                              :threshold-ratio 2.0)
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :compaction-watcher
        (lambda (event mode-id ctx)
          (declare (ignore mode-id ctx))
          (when (eq (event:event-type event) :session-compaction-needed)
            (push t events))))
       context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hi" context)
      (is (zerop (length events))))))

(test (agent-session-retry-policy-drives-prompt-retry :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let ((service (agent-session-service context)))
      (agent-session:recode-retry-policy service
                                         :max-attempts 5
                                         :base-delay-ms 0
                                         :retryable-p (constantly t))
      (let ((*test-fake-model-transient-errors* 2))
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hi" context)
        (is (zerop *test-fake-model-transient-errors*))))))

(test (agent-session-retry-policy-propagates-when-not-retryable :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let ((service (agent-session-service context)))
      (agent-session:recode-retry-policy service
                                         :max-attempts 5
                                         :base-delay-ms 0
                                         :retryable-p (constantly nil))
      (let ((*test-fake-model-transient-errors* 1))
        (signals error
          (agent-session:submit-agent-session-prompt service :default-mode
                                                     "hi" context))
        (is (zerop *test-fake-model-transient-errors*))))))

(test (agent-session-default-retry-policy-retries-network-errors :fixture interactive-authority)
  "The default classifier retries network-layer failures, so a flaky
transport heals without recoding the policy."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let ((service (agent-session-service context)))
      (agent-session:recode-retry-policy service :base-delay-ms 0)
      (let ((*test-fake-model-transient-errors* 2))
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hi" context)
        (is (zerop *test-fake-model-transient-errors*))))))

(test transient-model-error-p-classifies-by-category-and-status
  (is (eq t (agent-session:transient-model-error-p
             (make-condition 'transports:model-network-error
                             :cause (make-condition 'simple-error)))))
  (is (eq t (agent-session:transient-model-error-p
             (make-condition 'transports:anthropic-api-error :status 529))))
  (is (eq t (agent-session:transient-model-error-p
             (make-condition 'transports:openai-api-error :status 429))))
  (is (not (agent-session:transient-model-error-p
            (make-condition
             'transports:openai-api-error
             :status 429
             :body "{\"error\":{\"code\":\"usage_limit_reached\",\"message\":\"The usage limit has been reached\"}}")))
      "terminal ChatGPT usage-limit errors are not retried")
  (is (not (agent-session:transient-model-error-p
            (make-condition 'transports:anthropic-api-error :status 400)))
      "deterministic client errors re-signal immediately")
  (is (not (agent-session:transient-model-error-p
            (make-condition 'transports:anthropic-api-error)))
      "a provider error without a status is not presumed transient")
  (is (not (agent-session:transient-model-error-p
            (make-condition 'simple-error)))
      "unclassified conditions are internal bugs, never retried"))

(test (agent-session-retry-continues-instead-of-reprompting :fixture interactive-authority)
  "A retry continues the agent loop rather than re-prompting -- prompt-agent
appends the user message, so re-prompting would duplicate it in the context."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (agent-session:recode-retry-policy service :base-delay-ms 0)
      (let ((*test-fake-model-transient-errors* 2))
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hi" context))
      (is (equal '(:user :assistant) (agent-session-message-roles agent))
          "the prompt appears exactly once despite two retries")
      (is (eq :idle (agents:agent-state-value (agents:agent-state agent)))
          "a healed retry settles back to idle, not :retrying"))))

(test (agent-session-abort-during-backoff-stops-retrying :fixture interactive-authority)
  "An abort requested while the session backs off stops the retry loop
without re-signaling and leaves the agent aborted, never stuck :retrying."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (agent-session:recode-retry-policy
       service
       :base-delay-ms 0
       :retryable-p (lambda (condition)
                      (declare (ignore condition))
                      (setf (agents:agent-abort-requested-p agent) t)
                      t))
      (let ((*test-fake-model-transient-errors* 3))
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hi" context)
        (is (= 2 *test-fake-model-transient-errors*)
            "no retry attempt runs after the abort")
        (is (eq :aborted (agents:agent-state-value
                          (agents:agent-state agent))))))))

(test (agent-session-abort-interrupts-backoff-wait :fixture interactive-authority)
  "An abort requested during the backoff wait cuts it short within a poll
slice instead of after the whole exponential delay lapses -- Esc during
attempt-4 backoff must not sit through a 16s sleep."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (agent-session:recode-retry-policy
       service
       :base-delay-ms 30000
       :retryable-p (lambda (condition)
                      (declare (ignore condition))
                      (setf (agents:agent-abort-requested-p agent) t)
                      t))
      (let ((*test-fake-model-transient-errors* 1)
            (started (get-internal-real-time)))
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hi" context)
        (is (< (/ (- (get-internal-real-time) started)
                  internal-time-units-per-second)
               5)
            "the wait notices the abort within a slice, not after 30s")
        (is (eq :aborted (agents:agent-state-value
                          (agents:agent-state agent))))))))

(test (agent-session-retry-emits-progress-events :fixture interactive-authority)
  "Each retry surfaces an :agent/retry event resolving to the mode, so the
TUI can show the backoff instead of a frozen prompt."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let ((service (agent-session-service context))
          (events '()))
      (agent-session:recode-retry-policy service :base-delay-ms 0)
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :retry-watcher
        (lambda (event mode-id ctx)
          (declare (ignore ctx))
          (when (eq (event:event-type event) :agent/retry)
            (push (cons mode-id (event:event-payload event)) events))))
       context)
      (let ((*test-fake-model-transient-errors* 1))
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hi" context))
      (is (= 1 (length events)))
      (destructuring-bind (mode-id . payload) (first events)
        (is (eq :default-mode mode-id))
        (is (= 1 (getf payload :attempt)))
        (is (= 5 (getf payload :max-attempts)))
        (is (eq :network (getf payload :category)))
        (is (search "transient failure" (getf payload :message)))))))

(test (agent-session-supervised-errors-stamp-the-agent-error-payload :fixture interactive-authority)
  "An error escaping a prompt under the session retry loop emits :agent/error
with :supervised t and its condition category -- the supervisor re-signals it
into the visible :tui-error path, so the transcript projector must be able to
skip the supervised twin."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let ((service (agent-session-service context))
          (payloads '()))
      (agent-session:recode-retry-policy service
                                         :max-attempts 5
                                         :base-delay-ms 0
                                         :retryable-p (constantly nil))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :error-watcher
        (lambda (event mode-id ctx)
          (declare (ignore mode-id ctx))
          (when (eq (event:event-type event) :agent/error)
            (push (event:event-payload event) payloads))))
       context)
      (let ((*test-fake-model-transient-errors* 1))
        (signals error
          (agent-session:submit-agent-session-prompt service :default-mode
                                                      "hi" context)))
      (is (= 1 (length payloads)))
      (is (eq t (getf (first payloads) :supervised)))
      (is (eq :network (getf (first payloads) :category))))))

(test (agent-session-switch-restores-model-from-session-log :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (store (session-log-store context)))
      (register-runtime-model context "second-provider" "second-model"
                              :auth-required-p nil
                              :metadata (list :fake-deltas '("other")))
      (let ((second-session (sess:create-session store context
                                                 :id :restore-session)))
        (sess:append-session-entry
         store second-session
         (sess:make-model-change-entry "second-provider" "second-model")
         context)
        (agent-session:switch-agent-session service :default-mode
                                            (kli:object-id second-session)
                                            context)
        (let* ((binding (gethash :default-mode
                                 (agent-session:session-mode-bindings
                                  service)))
               (agent (kli:find-live-object (kli:context-registry context)
                                            (agent-session:mode-binding-agent-id
                                             binding)))
               (current (agents:agent-model-selection agent)))
          (is (string= "second-provider"
                       (models:model-selection-provider-id current)))
          (is (string= "second-model"
                       (models:model-selection-model-id current))))))))

(test (agent-session-steer-injects-and-runs-when-idle :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode
     context :metadata (list :fake-deltas '("steered response")))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object (kli:context-registry context)
                                        (agent-session:mode-binding-agent-id
                                         binding))))
      (is (agents:agent-idle-p agent))
      (agent-session:steer-agent-session
       service :default-mode
       (sess:make-user-message "please redirect") context)
      (is (agents:agent-idle-p agent))
      (let ((contents (agent-session-message-contents agent)))
        (is (find "please redirect" contents :test #'string=))
        (is (find "steered response" contents :test #'string=))))))

(test (agent-session-loop-exit-drains-steer-landing-after-last-boundary :fixture interactive-authority)
  "A steer that lands after the final turn's boundary drain -- here inside
the :agent/end fanout, past the :idle transition -- must still run. The loop
drains its queues once more on exit instead of stranding the message until
the next submit."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode
     context :metadata (list :fake-deltas '("reply")))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding)))
           (injected nil))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :late-steer-injector
        (lambda (event mode-id ctx)
          (declare (ignore mode-id))
          (when (and (eq (event:event-type event) :agent/end)
                     (not injected))
            (setf injected t)
            (agent-session:queue-agent-session-steer
             service :default-mode "late steer" ctx))))
       context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hi" context)
      (is (equal '(:user :assistant :user :assistant)
                 (agent-session-message-roles agent))
          "the exit drain runs the late steer as its own turn")
      (is (find "late steer" (agent-session-message-contents agent)
                :test #'string=))
      (is (eq :idle (agents:agent-state-value (agents:agent-state agent)))))))

(test (agent-end-event-carries-final-text :fixture interactive-authority)
  "The :agent/end event payload carries the run's final assistant text, so a
headless formatter can render the whole reply from that one terminal event."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode
     context :metadata (list :fake-deltas '("the answer")))
    (let ((service (agent-session-service context))
          (captured :unset))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :final-text-probe
        (lambda (event mode-id ctx)
          (declare (ignore mode-id ctx))
          (when (eq (event:event-type event) :agent/end)
            (setf captured (getf (event:event-payload event) :text)))))
       context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hi" context)
      (is (equal "the answer" captured)
          "the :agent/end payload reports the final assistant text"))))

(test (agent-session-follow-up-injects-and-runs-when-idle :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode
     context :metadata (list :fake-deltas '("follow-up reply")))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object (kli:context-registry context)
                                        (agent-session:mode-binding-agent-id
                                         binding))))
      (agent-session:follow-up-agent-session
       service :default-mode
       (sess:make-user-message "any update?") context)
      (is (agents:agent-idle-p agent))
      (let ((contents (agent-session-message-contents agent)))
        (is (find "any update?" contents :test #'string=))
        (is (find "follow-up reply" contents :test #'string=))))))

(test agent-session-steer-without-binding-signals-error
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (agent-session-service context)))
      (signals error
        (agent-session:steer-agent-session
         service :unbound-mode
         (sess:make-user-message "lost") context)))))

(test (agent-session-branch-restores-context-up-to-branch-point :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object (kli:context-registry context)
                                        (agent-session:mode-binding-agent-id
                                         binding))))
      (let* ((store (agents:agent-store agent))
             (session (agents:agent-session agent))
             (m1 (sess:append-session-entry
                  store session
                  (sess:make-message-entry (sess:make-user-message "msg-1"))
                  context))
             (branch-id (kli:object-id m1)))
        (sess:append-session-entry
         store session
         (sess:make-message-entry (sess:make-user-message "msg-2"))
         context)
        (sess:append-session-entry
         store session
         (sess:make-message-entry (sess:make-user-message "msg-3"))
         context)
        (let* ((new-session-id (agent-session:branch-agent-session
                                service :default-mode branch-id context))
               (new-session (sess:find-session store new-session-id))
               (lens-context (agent-session:agent-session-context
                              service :default-mode context))
               (messages (ctx:context-projected-messages lens-context)))
          (is (= 1 (length (sess:session-entries new-session))))
          (is (find "msg-1" messages
                    :key #'sess:message-content :test #'string=))
          (is (null (find "msg-2" messages
                          :key #'sess:message-content :test #'string=)))
          (is (null (find "msg-3" messages
                          :key #'sess:message-content :test #'string=))))))))

(test (agent-session-committed-patch-replays-into-lens :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context)
    (let* ((service (agent-session-service context))
           (agent-context (agent-session:agent-session-context
                           service :default-mode context)))
      (ctx:stage-context-patch
       agent-context
       (ctx:make-append-message-patch
        (sess:make-user-message "patched-in message")))
      (ctx:commit-context-patches agent-context context)
      (let ((messages (ctx:context-projected-messages agent-context)))
        (is (find "patched-in message" messages
                  :key #'sess:message-content :test #'string=))))))

(test (agent-session-listener-fan-out-isolates-failures-and-records-id :fixture interactive-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (bind-agent-session-mode context :deltas '("hi back"))
      (let* ((ext:*extension-fault-policy* nil)
             (service (agent-session-service context))
             (second-saw nil))
        (agent-session:register-session-event-listener
         service
         (agent-session:make-session-event-listener
          :first-throws
          (lambda (event mode-id ctx)
            (declare (ignore event mode-id ctx))
            (error "boom-listener"))
          :filter (lambda (event)
                    (eq (event:event-type event) :agent/delta)))
         context)
        (agent-session:register-session-event-listener
         service
         (agent-session:make-session-event-listener
          :second-records
          (lambda (event mode-id ctx)
            (declare (ignore mode-id ctx))
            (when (eq (event:event-type event) :agent/delta)
              (setf second-saw event)))
          :filter (lambda (event)
                    (eq (event:event-type event) :agent/delta)))
         context)
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hello" context)
        (is (not (null second-saw))
            "second listener must still fire after the first throws")
        (let ((lines (event-callback-log-lines)))
          (is (<= 1 (length lines)))
          (is (some (lambda (l) (search "id=:FIRST-THROWS" l)) lines)
              "diagnostic line must carry the failing listener's id"))))))

(test (agent-session-throwing-listener-filter-skips-listener-and-logs :fixture interactive-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (multiple-value-bind (context protocol) (agent-session-test-context)
        (declare (ignore protocol))
        (let ((seen '())
              (service (agent-session-service context)))
          (agent-session:register-session-event-listener
           service
           (agent-session:make-session-event-listener
            :throwing-filter-listener
            (lambda (event mode-id ctx)
              (declare (ignore mode-id ctx))
              (push event seen))
            :filter (lambda (event)
                      (declare (ignore event))
                      (error "filter boom")))
           context)
          (bind-agent-session-mode context)
          (is (null seen) "a faulting filter skips its listener")
          (let ((lines (fault-log-lines :listener-filter)))
            (is (plusp (length lines)))
            (is (search "THROWING-FILTER-LISTENER" (first lines)))))))))

(test event-persistence-policy-contains-faulting-slot-handler
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let ((policy (agent-session:make-event-persistence-policy
                     :agent-message-handler
                     (lambda (event) (declare (ignore event))
                       (error "slot boom")))))
        (is (null (funcall policy :route
                           (event:make-event :agent-message) nil nil)))
        (let ((lines (fault-log-lines :session-policy)))
          (is (= 1 (length lines)))
          (is (search "(:PERSISTENCE :AGENT-MESSAGE)" (first lines))))))))

(test prompt-expansion-policy-contains-faulting-expander
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let ((policy (agent-session:make-prompt-expansion-policy
                     :skill-expander (lambda (text) (declare (ignore text))
                                       (error "skill boom")))))
        (multiple-value-bind (text images cancelled)
            (funcall policy :expand "raw" nil)
          (is (string= "raw" text) "a faulting expander degrades to identity")
          (is (null images))
          (is (null cancelled)))
        (is (= 1 (length (fault-log-lines :session-policy))))))))

(test context-transform-policy-layers-compose-and-retract-out-of-order
  "System-prompt layers compose in install order, and retracting the earlier
layer leaves the later one intact -- retraction is keyed by label, not order."
  (let ((policy (agent-session:make-context-transform-policy)))
    (funcall policy :add-system-prompt-layer :ctx
             (lambda (running) (concatenate 'string running " <ctx>")))
    (funcall policy :add-system-prompt-layer :ads
             (lambda (running) (concatenate 'string running " <ads>")))
    (is (string= "base <ctx> <ads>" (funcall policy :system-prompt "base"))
        "layers fold over the base in install order")
    (funcall policy :remove-system-prompt-layer :ctx)
    (is (string= "base <ads>" (funcall policy :system-prompt "base"))
        "dropping the earlier layer keeps the later layer")
    (funcall policy :remove-system-prompt-layer :ads)
    (is (string= "base" (funcall policy :system-prompt "base"))
        "with every layer retracted the base passes through unchanged")))

(test context-transform-policy-contains-faulting-prompt-layer
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let ((policy (agent-session:make-context-transform-policy)))
        (funcall policy :add-system-prompt-layer :boom
                 (lambda (running) (declare (ignore running))
                   (error "prompt boom")))
        (is (string= "base prompt"
                     (funcall policy :system-prompt "base prompt"))
            "a faulting layer keeps the base prompt")
        (is (= 1 (length (fault-log-lines :session-policy))))))))

(test context-transform-policy-extra-messages-are-fresh-and-fault-safe
  (let ((policy (agent-session:make-context-transform-policy
                 :extra-messages-fn
                 (lambda () (list (sess:make-user-message "X"))))))
    (let ((a (funcall policy :extra-messages))
          (b (funcall policy :extra-messages)))
      (is (equal '("X") (mapcar #'sess:message-content a)))
      (is (not (eq a b)) "each call rebuilds a fresh list")))
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let ((policy (agent-session:make-context-transform-policy
                     :extra-messages-fn (lambda () (error "extras boom")))))
        (is (null (funcall policy :extra-messages))
            "a faulting extra-messages-fn yields no extras")
        (is (= 1 (length (fault-log-lines :session-policy))))))))

(test retry-policy-contains-faulting-retryable-p
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let ((policy (agent-session:make-retry-policy
                     :retryable-p (lambda (e) (declare (ignore e))
                                    (error "retryable boom")))))
        (multiple-value-bind (retry-p delay)
            (funcall policy :should-retry :payload 0)
          (is (null retry-p) "a faulting retryable-p means not retryable")
          (is (null delay)))
        (is (= 1 (length (fault-log-lines :session-policy))))))))

(test (basic-commands-reset-starts-fresh-session-and-preserves-model :fixture interactive-authority)
  "/reset is a new-conversation primitive. It swaps in an empty session,
dropping model-visible history, while keeping the user's model selection, then
surfaces the reset notice. The current session is seeded with a turn so the
fresh session is a real contrast, not just a no-op rebind."
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-basic-command-stack context)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (bind-agent-session-mode context :deltas '("hi back"))
    (let* ((service (agent-session-service context))
           (store (session-log-store context))
           (app (tui-app:make-tui-app :context context :columns 48))
           (old-session-id (mode-session-id service :default-mode)))
      (setf app:*current-context* context
            app:*current-app* app)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (tui-app:tui-app-add-system-event app "in-flight")
      (is (null (command-result-text
                 context
                 (invoke-test-command context :reset (list :app app))))
          "/reset is silent: the :session-reset event projects the line")
      (let ((events (tui-app:tui-app-transcript-events app)))
        (is (= 1 (length events)))
        (is (eq :notice (tui-transcript:event-kind (first events))))
        (is (string= "Session reset."
                     (tui-transcript:event-text (first events)))))
      (let ((new-session-id (mode-session-id service :default-mode)))
        (is (not (eq old-session-id new-session-id)))
        (is (null (sess:session-entries (sess:find-session store new-session-id)))))
      (let* ((binding (gethash :default-mode
                               (agent-session:session-mode-bindings service)))
             (agent (kli:find-live-object
                     (kli:context-registry context)
                     (agent-session:mode-binding-agent-id binding))))
        (is (equal "agent-session-provider"
                   (models:model-selection-provider-id
                    (agents:agent-model-selection agent))))))))

(test agent-session-pending-records-respect-model-visibility
  (let ((service (agent-session::make-agent-session-service)))
    (agent-session::record-pending-command
     service '(:command "redraw" :content ((:type :text :text "Redrawn."))))
    (is (null (agent-session::session-pending-command-records service)))
    (agent-session::record-pending-command
     service '(:command "eval" :tail "(+ 1 2)" :model-visible t
               :content ((:type :text :text "3"))))
    (is (= 1 (length
              (agent-session::session-pending-command-records service))))))

(test agent-session-pending-records-skip-blank-results
  "A model-visible command whose result carries no text queues nothing: an
empty result means the feedback happened in the UI (a selection menu), so a
blank record would only mislead the model about output it never saw. An
errored result still queues even when terse."
  (let ((service (agent-session::make-agent-session-service)))
    (agent-session::record-pending-command
     service '(:command "branches" :model-visible t :content ()))
    (is (null (agent-session::session-pending-command-records service)))
    (agent-session::record-pending-command
     service '(:command "rewind" :model-visible t :error-p t
               :content ((:type :text :text "Session is busy."))))
    (is (= 1 (length
              (agent-session::session-pending-command-records service))))))

(test agent-session-pending-command-block-renders-invocation-and-drains
  (let ((service (agent-session::make-agent-session-service)))
    (agent-session::record-pending-command
     service '(:command "eval" :tail "(+ 1 2)" :model-visible t
               :content ((:type :text :text "3"))))
    (agent-session::record-pending-command
     service '(:command "bash" :tail "false" :model-visible t :error-p t
               :content ((:type :text :text "exit 1"))))
    (let ((injected (agent-session::drain-pending-command-block service)))
      (is (search "<local-command-output>" injected))
      (is (search "/eval (+ 1 2):" injected))
      (is (search "3" injected))
      (is (search "/bash false (error):" injected))
      (is (search "exit 1" injected))
      (is (null (search "dropped" injected))))
    (is (null (agent-session::drain-pending-command-block service)))))

(test agent-session-pending-records-clamp-and-drop-oldest
  (let ((service (agent-session::make-agent-session-service))
        (agent-session::*pending-command-record-limit* 2)
        (agent-session::*pending-command-text-limit* 8))
    (flet ((record (tail text)
             (agent-session::record-pending-command
              service (list :command "eval" :tail tail :model-visible t
                            :content (list (list :type :text :text text))))))
      (record "first" "one")
      (record "second" "two")
      (record "third" "aaaaaaaaaaaa"))
    (let ((records (agent-session::session-pending-command-records service)))
      (is (= 2 (length records)))
      (is (equal '("second" "third")
                 (mapcar (lambda (record) (getf record :tail)) records)))
      (is (search "[output truncated at 8 characters]"
                  (getf (second records) :text))))
    (let ((injected (agent-session::drain-pending-command-block service)))
      (is (search "[1 earlier command record dropped]" injected))
      (is (null (search "/eval first" injected))))))

(test agent-session-command-result-event-feeds-pending-records
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (agent-session-service context)))
      (event:emit-event
       context
       (event:make-event :command/result
                         :payload '(:command "eval" :tail "(+ 1 2)"
                                    :model-visible t :error-p nil
                                    :content ((:type :text :text "3")))
                         :source :test))
      (is (= 1 (length
                (agent-session::session-pending-command-records service))))
      (event:emit-event
       context
       (event:make-event :command/result
                         :payload '(:command "redraw"
                                    :content ((:type :text :text "Redrawn.")))
                         :source :test))
      (is (= 1 (length
                (agent-session::session-pending-command-records service)))))))

(test (agent-session-submit-injects-pending-command-block-once :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :deltas '("ok"))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (agent-session::record-pending-command
       service '(:command "eval" :tail "(defparameter *x* 42)"
                 :model-visible t :content ((:type :text :text "*X*"))))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "what did i just set?" context)
      (let ((first-user (first (agent-session-message-contents agent))))
        (is (search "<local-command-output>" first-user))
        (is (search "/eval (defparameter *x* 42):" first-user))
        (is (search "*X*" first-user))
        (is (search "what did i just set?" first-user))
        (is (< (search "/eval" first-user)
               (search "what did i just set?" first-user))))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "again" context)
      (is (string= "again"
                   (third (agent-session-message-contents agent)))))))

(test (agent-session-switch-clears-pending-command-records :fixture interactive-authority)
  "A session switch abandons the departing conversation's between-turn UI
context. Command output queued in one session but never drained -- the user
switched sessions before submitting -- must not ride into the session switched
to. Reset, branch, rewind, and resume all route through switch-agent-session,
so clearing here covers every path."
  (let* ((context (agent-session-test-context))
         (service (bind-agent-session-mode context)))
    (agent-session::record-pending-command
     service '(:command "profile" :model-visible t
               :content ((:type :text :text "Switched to interactive-terminal."))))
    (is (= 1 (length
              (agent-session::session-pending-command-records service))))
    (let ((second-session (sess:create-session (session-log-store context)
                                               context
                                               :id :leak-test-session-2)))
      (agent-session:switch-agent-session service :default-mode
                                          (kli:object-id second-session)
                                          context))
    (is (null (agent-session::drain-pending-command-block service))
        "a session switch clears the prior session's undrained command output")))

(test (agent-session-usage-delta-tracks-live-then-clears-at-end
       :fixture interactive-authority)
  "A streaming usage-delta routes to the binding's live usage so the footer can
track the in-flight turn; the committed usage compaction reads stays untouched
mid-stream; agent-end commits the final usage and clears the live slot so the
footer falls back to the committed value between turns."
  (let* ((context (agent-session-test-context))
         (service (bind-agent-session-mode context))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent-id (agent-session:mode-binding-agent-id binding))
         (cb (agent-session:mode-binding-context-binding binding))
         (policy (agent-session:session-event-persistence-policy service)))
    (flet ((route (type payload)
             (funcall policy :route
                      (event:make-event type :payload payload :source agent-id)
                      service context)))
      (route :agent/usage '(:usage (:input-tokens 100 :total-tokens 100)))
      (is (= 100 (agent-session:usage-total-tokens
                  (agent-session:context-binding-live-usage cb)))
          "the usage-delta is recorded as live usage")
      (is (null (agent-session:context-binding-usage cb))
          "the committed usage compaction reads is untouched mid-stream")
      (route :agent/end '(:usage (:input-tokens 100 :output-tokens 50
                                  :total-tokens 150)))
      (is (= 150 (agent-session:usage-total-tokens
                  (agent-session:context-binding-usage cb)))
          "agent-end commits the final usage")
      (is (null (agent-session:context-binding-live-usage cb))
          "agent-end clears the live slot"))))

(test (agent-session-snapshot-round-trips-mode-binding :fixture interactive-authority)
  "The session service declares its durable identity -- per mode, the
{session-id, leaf-id} that names a conversation at a point in its history -- and
restoring that representation rebinds the mode there through the canonical
switch, repointing past any work the live session accrued after the snapshot.
A model's selection is not captured: switch recovers it from the session log."
  (let ((context (agent-session-test-context)))
    (multiple-value-bind (service session) (bind-agent-session-mode context)
      (let ((store (session-log-store context))
            (session-id (kli:object-id session)))
        (sess:append-session-entry
         store session
         (sess:make-message-entry (sess:make-user-message "one") :id :snap-e1)
         context)
        (agent-session:switch-agent-session service :default-mode session-id context)
        (let ((record (snapshot:snapshot-representation service)))
          (sess:append-session-entry
           store session
           (sess:make-message-entry (sess:make-user-message "two") :id :snap-e2)
           context)
          (agent-session:switch-agent-session service :default-mode session-id context)
          (flet ((bound-leaf ()
                   (agent-session:session-binding-leaf-id
                    (agent-session:mode-binding-session-binding
                     (gethash :default-mode
                              (agent-session:session-mode-bindings service))))))
            (is (eql :snap-e2 (bound-leaf))
                "precondition: post-snapshot work advanced the bound leaf")
            (snapshot:restore-representation service record context)
            (let ((restored (agent-session:mode-binding-session-binding
                             (gethash :default-mode
                                      (agent-session:session-mode-bindings service)))))
              (is (eql session-id
                       (agent-session:session-binding-session-id restored))
                  "restore rebinds the mode to the captured session")
              (is (eql :snap-e1
                       (agent-session:session-binding-leaf-id restored))
                  "restore repoints to the captured leaf, not the advanced one"))))))))

;;;; Focus -- the canonical "which mode is on top" pointer. One op writes the
;;;; pointer, re-seeds the registry global to the focused mode's model, and
;;;; announces :session-focus; the pointer round-trips through the snapshot.

(test (agent-session-focus-sets-pointer-reseeds-and-emits :fixture interactive-authority)
  "Focusing a mode points active-mode-id at it, re-seeds the registry global to
that mode's live model, and emits exactly one :session-focus carrying the mode
and its session-id."
  (let ((context (agent-session-test-context)))
    (let ((service (bind-agent-session-mode context :mode-id :alpha :session-id :sa
                                            :provider-id "pa" :model-id "ma")))
      (bind-agent-session-mode context :mode-id :charlie :session-id :sc
                               :provider-id "pc" :model-id "mc")
      (let ((registry (model-registry context))
            (focuses '()))
        (agent-session:register-session-event-listener
         service
         (agent-session:make-session-event-listener
          :focus-watcher
          (lambda (event mode-id ctx)
            (declare (ignore ctx))
            (when (eq (event:event-type event) :session-focus)
              (push (list mode-id (event:event-payload event)) focuses)))
          :filter (lambda (event) (eq (event:event-type event) :session-focus)))
         context)
        (agent-session:focus-agent-session-mode service :alpha context)
        (is (eq :alpha (agent-session:session-active-mode-id service)))
        (is (string= "ma" (models:model-selection-model-id
                           (models:current-model-selection registry)))
            "focus re-seeds the registry global to the focused mode's model")
        (is (= 1 (length focuses)))
        (is (eq :alpha (first (first focuses))))
        (is (eq :alpha (getf (second (first focuses)) :mode)))
        (is (eq :sa (getf (second (first focuses)) :session-id)))))))

(test (agent-session-focus-idempotent-no-op :fixture interactive-authority)
  "Re-focusing the already-active mode changes nothing and emits no event."
  (let ((context (agent-session-test-context)))
    (let ((service (bind-agent-session-mode context :mode-id :alpha :session-id :sa
                                            :provider-id "pa" :model-id "ma"))
          (focuses 0))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :focus-counter
        (lambda (event mode-id ctx)
          (declare (ignore mode-id ctx))
          (when (eq (event:event-type event) :session-focus) (incf focuses))))
       context)
      (agent-session:focus-agent-session-mode service :alpha context)
      (agent-session:focus-agent-session-mode service :alpha context)
      (is (eq :alpha (agent-session:session-active-mode-id service)))
      (is (= 1 focuses) "re-focusing the active mode emits no second event"))))

(test (agent-session-focus-errors-on-unbound-mode :fixture interactive-authority)
  "Focusing a mode with no binding is a programmer error, not a silent no-op."
  (let ((context (agent-session-test-context)))
    (let ((service (agent-session-service context)))
      (signals agent-session:agent-session-error
        (agent-session:focus-agent-session-mode service :ghost context)))))

(test (agent-session-switch-never-moves-the-active-pointer :fixture interactive-authority)
  "focus-agent-session-mode is the sole writer of active-mode-id: a later switch,
on another mode, leaves the pointer untouched."
  (let ((context (agent-session-test-context)))
    (let ((service (bind-agent-session-mode context :mode-id :alpha :session-id :sa
                                            :provider-id "pa" :model-id "ma")))
      (agent-session:focus-agent-session-mode service :alpha context)
      (is (eq :alpha (agent-session:session-active-mode-id service)))
      (let ((other (sess:create-session (session-log-store context) context
                                        :id :sc)))
        (agent-session:switch-agent-session service :charlie
                                            (kli:object-id other) context))
      (is (eq :alpha (agent-session:session-active-mode-id service))
          "switching another mode does not steal focus"))))

(test (agent-session-bind-default-session-focuses-default-mode :fixture interactive-authority)
  "Boot's bind-default-session focuses :default-mode so a single-mode start has a
focused mode -- and a seeded registry -- from the first frame."
  (let ((context (agent-session-test-context)))
    (let ((service (agent-session-service context))
          (app (tui-app:make-tui-app :context context :columns 48)))
      (app::bind-default-session app context :continue-p nil)
      (is (eq :default-mode (agent-session:session-active-mode-id service))))))

(test (agent-session-snapshot-focus-round-trips-and-seeds-active :fixture interactive-authority)
  "The focused mode round-trips through the snapshot, and on restore the registry
global -- which seeds newly constructed agents -- follows the active mode's model
rather than the alphabetically-last mode the restore loop rebinds last."
  (let ((context (agent-session-test-context)))
    (let ((service (bind-agent-session-mode context :mode-id :alpha :session-id :sa
                                            :provider-id "pa" :model-id "ma"))
          (store (session-log-store context)))
      (flet ((log-model (mode-id session-id provider model)
               ;; Persist the mode's model in its log and re-switch, so a restore
               ;; recovers each mode's own model regardless of the live registry.
               (sess:append-session-entry
                store (sess:find-session store session-id)
                (sess:make-model-change-entry provider model) context)
               (agent-session:switch-agent-session service mode-id session-id
                                                   context)))
        (log-model :alpha :sa "pa" "ma")
        (bind-agent-session-mode context :mode-id :charlie :session-id :sc
                                 :provider-id "pc" :model-id "mc")
        (log-model :charlie :sc "pc" "mc")
        (let ((registry (model-registry context)))
          (is (string= "mc" (models:model-selection-model-id
                             (models:current-model-selection registry)))
              "precondition: binding charlie last seeded the registry on its model")
          (agent-session:focus-agent-session-mode service :alpha context)
          (is (string= "ma" (models:model-selection-model-id
                             (models:current-model-selection registry)))
              "focus moves the seed onto the focused mode's model")
          (let ((record (snapshot:snapshot-representation service)))
            (models:select-model
             registry (models:find-model-definition registry "pc" "mc") context)
            (is (string= "mc" (models:model-selection-model-id
                               (models:current-model-selection registry)))
                "precondition: the live seed advanced off the focused mode")
            (snapshot:restore-representation service record context)
            (is (eq :alpha (agent-session:session-active-mode-id service))
                "restore round-trips the focused mode")
            (is (string= "ma" (models:model-selection-model-id
                               (models:current-model-selection registry)))
                "restore re-seeds the registry to the active mode's model")
            (is (null (agent-session:session-restore-unfocused-mode service))
                "a clean restore records no unfocused loss")))))))

(test (agent-session-restore-skips-focus-when-active-mode-cannot-bind
       :fixture interactive-authority)
  "A cross-image restore whose active mode's session file is gone leaves the
pointer unset and focuses nothing, while the modes that do load still bind."
  (let* ((root (temp-session-root))
         (alpha-file nil)
         (record
           (let ((context (agent-session-test-context)))
             (let ((store (install-file-session-store context root))
                   (service (agent-session-service context)))
               (setf alpha-file (sess:session-file-path store :sa))
               (flet ((bind-persisted (mode-id session-id provider model entry-id)
                        (bind-agent-session-mode context :mode-id mode-id
                                                 :session-id session-id
                                                 :provider-id provider
                                                 :model-id model)
                        (sess:append-session-entry
                         store (sess:find-session store session-id)
                         (sess:make-message-entry (sess:make-user-message "hi")
                                                  :id entry-id)
                         context)
                        (agent-session:switch-agent-session service mode-id
                                                            session-id context)))
                 (bind-persisted :alpha :sa "pa" "ma" :ea1)
                 (bind-persisted :charlie :sc "pc" "mc" :ec1))
               (agent-session:focus-agent-session-mode service :alpha context)
               (snapshot:snapshot-representation service)))))
    (delete-file alpha-file)
    (let ((context (agent-session-test-context)))
      (install-file-session-store context root)
      (let ((service (agent-session-service context)))
        (snapshot:restore-representation service record context)
        (is (null (agent-session:session-active-mode-id service))
            "the active mode never bound, so focus is skipped and the pointer stays unset")
        (is (gethash :charlie (agent-session:session-mode-bindings service))
            "a mode whose file survived still binds")
        (is (null (gethash :alpha (agent-session:session-mode-bindings service)))
            "the mode whose file was removed is left unbound")
        (is (eq :alpha (agent-session:session-restore-unfocused-mode service))
            "the lost focused mode is recorded for the restore command to report")))))
