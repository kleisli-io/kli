(in-package #:kli/tests)

(defun model-command-test-context ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        commands:*commands-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*
                        agents:*agent-loop-extension-manifest*
                        agent-session:*agent-session-extension-manifest*
                        model-commands:*model-commands-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (values context protocol)))

(defun bootstrap-model-command-mode (context
                                     &key (mode-id :default-mode)
                                          (session-id
                                           :model-command-test-session))
  (let* ((store (session-log-store context))
         (session (sess:create-session store context :id session-id))
         (service (agent-session-service context)))
    (agent-session:switch-agent-session service mode-id
                                        (kli:object-id session)
                                        context)))

(defun register-command-test-models (context)
  (let ((store (credential-store context))
        (registry (model-registry context)))
    (register-path-auth store context :provider-id "path-provider")
    (models:register-model-provider
     registry
     (models:make-model-provider "path-provider" :fake)
     context)
    (models:register-model-provider
     registry
     (models:make-model-provider "missing-provider" :fake)
     context)
    (models:register-model-provider
     registry
     (models:make-model-provider "local-provider" :fake
                                 :auth-required-p nil)
     context)
    (models:register-model-definition
     registry
     (models:make-model-definition
      "path-provider"
      "selected-model"
      :fake
      :name "Selected"
      :option-schemas (list (test-reasoning-effort-schema)))
     context)
    (models:register-model-definition
     registry
     (models:make-model-definition "missing-provider"
                                   "missing-model"
                                   :fake)
     context)
    (models:register-model-definition
     registry
     (models:make-model-definition "local-provider"
                                   "local-model"
                                   :fake)
     context)))

(test model-commands-register-command-surface
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore context))
    (let ((commands (ext:require-capability-provider protocol
                                                     :commands
                                                     :contract :commands/v1)))
      (dolist (name '(:model :models :providers :thinking :auth))
        (is (ext:provider-call commands :find-command name))))))

(test (model-commands-list-auth-available-models-and-providers :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (let ((models-output
            (command-result-text context
                                 (invoke-test-command context :models)))
          (providers-output
            (command-result-text context
                                 (invoke-test-command context :providers))))
      (is (search "path-provider/selected-model" models-output))
      (is (search "options reasoning-effort" models-output))
      (is (null (search " thinking" models-output))
          "/models shows compact semantic option ids, not the legacy marker")
      (is (search "local-provider/local-model" models-output))
      (is (null (search "missing-provider/missing-model" models-output)))
      (is (search "path-provider auth yes models 1" providers-output))
      (is (search "missing-provider auth no models 1" providers-output))
      (is (search "local-provider auth local models 1" providers-output)))))

(test (model-command-selects-model-and-thinking :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (bootstrap-model-command-mode context)
    (let* ((result (invoke-test-command context
                                        :model
                                        '(:tail "path-provider/selected-model:high")))
           (selection (models:current-model-selection
                       (model-registry context))))
      (is (not (commands:command-result-error-p result)))
      (is (string= "" (command-result-text context result))
          "set-model stays silent -- the model-change event announces the change")
      (let ((details (commands:command-result-details result)))
        (is (string= "path-provider" (getf details :provider)))
        (is (string= "selected-model" (getf details :model)))
        (is (eq :high (getf details :reasoning-effort))))
      (is (string= "path-provider"
                   (models:model-selection-provider-id selection)))
      (is (string= "selected-model"
                   (models:model-selection-model-id selection)))
      (is (eq :high
              (test-selection-reasoning-effort selection))))))

(defun register-colon-slash-models (context)
  "A no-auth provider with model ids that contain ':' and '/', mirroring the
synthetic registry (syn:small:text, hf:openai/gpt-oss-120b)."
  (let ((registry (model-registry context)))
    (models:register-model-provider
     registry
     (models:make-model-provider "synthetic" :fake :auth-required-p nil)
     context)
    (models:register-model-definition
     registry
     (models:make-model-definition
      "synthetic" "syn:small:text" :fake
      :name "syn:small:text"
      :option-schemas (list (test-reasoning-effort-schema)))
     context)
    (models:register-model-definition
     registry
     (models:make-model-definition
      "synthetic" "hf:openai/gpt-oss-120b" :fake
      :name "gpt-oss-120b"
      :option-schemas (list (test-reasoning-effort-schema)))
     context)))

(test (model-command-selects-colon-and-slash-model-ids :fixture interactive-authority)
  "Model ids legitimately contain a colon and a slash. /model must keep the id whole and never misread a model segment as a thinking level. Splitting on the last colon made every synthetic id fail with Unknown thinking level."
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (register-colon-slash-models context)
    (bootstrap-model-command-mode context)
    (let* ((result (invoke-test-command context :model
                                        '(:tail "synthetic/syn:small:text")))
           (selection (models:current-model-selection (model-registry context))))
      (is (not (commands:command-result-error-p result)))
      (is (string= "synthetic"
                   (models:model-selection-provider-id selection)))
      (is (string= "syn:small:text"
                   (models:model-selection-model-id selection)))
      (is (eq :off (test-selection-reasoning-effort selection))))
    (let* ((result (invoke-test-command
                    context :model
                    '(:tail "synthetic/hf:openai/gpt-oss-120b")))
           (selection (models:current-model-selection (model-registry context))))
      (is (not (commands:command-result-error-p result)))
      (is (string= "synthetic"
                   (models:model-selection-provider-id selection)))
      (is (string= "hf:openai/gpt-oss-120b"
                   (models:model-selection-model-id selection))))))

(test (model-command-parses-space-delimited-thinking-on-colon-id :fixture interactive-authority)
  "The thinking level is a trailing token recognised only when it names a level. The colon-laden model id before it stays intact across the space separator."
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (register-colon-slash-models context)
    (bootstrap-model-command-mode context)
    (let* ((result (invoke-test-command
                    context :model
                    '(:tail "synthetic/syn:small:text high")))
           (selection (models:current-model-selection (model-registry context))))
      (is (not (commands:command-result-error-p result)))
      (is (string= "synthetic"
                   (models:model-selection-provider-id selection)))
      (is (string= "syn:small:text"
                   (models:model-selection-model-id selection)))
      (is (eq :high (test-selection-reasoning-effort selection))))))

(test (model-command-parses-space-delimited-thinking-on-synthetic-hf-id :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (register-colon-slash-models context)
    (bootstrap-model-command-mode context)
    (let* ((result (invoke-test-command
                    context :model
                    '(:tail "synthetic/hf:openai/gpt-oss-120b high")))
           (selection (models:current-model-selection (model-registry context))))
      (is (not (commands:command-result-error-p result)))
      (is (string= "synthetic"
                   (models:model-selection-provider-id selection)))
      (is (string= "hf:openai/gpt-oss-120b"
                   (models:model-selection-model-id selection)))
      (is (eq :high (test-selection-reasoning-effort selection))))))

(test (thinking-command-updates-current-selection :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (bootstrap-model-command-mode context)
    (invoke-test-command context
                         :model
                         '(:tail "path-provider/selected-model"))
    (let* ((result (invoke-test-command context
                                        :thinking
                                        '(:tail "low")))
           (selection (models:current-model-selection
                       (model-registry context))))
      (is (not (commands:command-result-error-p result)))
      (is (string= "" (command-result-text context result))
          "typed /thinking is silent: the option-change event projects the line")
      (is (eq :low
              (test-selection-reasoning-effort selection))))))

(test (thinking-command-rejects-unsupported-value :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (bootstrap-model-command-mode context)
    (invoke-test-command context :model '(:tail "path-provider/selected-model"))
    (let ((result (invoke-test-command context :thinking '(:tail "turbo"))))
      (is (commands:command-result-error-p result))
      (is (search "Unknown thinking level" (command-result-text context result))))))

(test (model-command-targets-non-default-mode :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (bootstrap-model-command-mode context :mode-id :default-mode)
    (bootstrap-model-command-mode context :mode-id :other-mode
                                          :session-id :other-mode-session)
    (invoke-test-command context :model
                         '(:tail "local-provider/local-model"
                           :mode-id :default-mode))
    (let ((result (invoke-test-command
                   context :model
                   '(:tail "path-provider/selected-model:high"
                     :mode-id :other-mode))))
      (is (not (commands:command-result-error-p result))))
    (let* ((service (agent-session-service context))
           (bindings (agent-session:session-mode-bindings service))
           (default-agent (kli:find-live-object
                           (kli:context-registry context)
                           (agent-session:mode-binding-agent-id
                            (gethash :default-mode bindings))))
           (other-agent (kli:find-live-object
                         (kli:context-registry context)
                         (agent-session:mode-binding-agent-id
                          (gethash :other-mode bindings))))
           (default-selection (agents:agent-model-selection default-agent))
           (other-selection (agents:agent-model-selection other-agent)))
      (is (string= "path-provider"
                   (models:model-selection-provider-id other-selection)))
      (is (string= "selected-model"
                   (models:model-selection-model-id other-selection)))
      (is (eq :high
              (test-selection-reasoning-effort other-selection)))
      (is (string= "local-provider"
                   (models:model-selection-provider-id default-selection)))
      (is (string= "local-model"
                   (models:model-selection-model-id default-selection))))))

(test (thinking-baselines-on-the-mode-agent-not-the-registry :fixture interactive-authority)
  "A thinking change reads the mode's own model, never the registry default that
another mode last set. default-mode runs a thinking-capable model while other-mode
selected a different one last; /thinking on default-mode keeps default-mode on its
own model and leaves other-mode untouched."
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (bootstrap-model-command-mode context :mode-id :default-mode)
    (bootstrap-model-command-mode context :mode-id :other-mode
                                          :session-id :other-mode-session)
    (invoke-test-command context :model
                         '(:tail "path-provider/selected-model"
                           :mode-id :default-mode))
    (invoke-test-command context :model
                         '(:tail "local-provider/local-model"
                           :mode-id :other-mode))
    (let ((result (invoke-test-command context :thinking
                                       '(:tail "high" :mode-id :default-mode))))
      (is (not (commands:command-result-error-p result))
          "thinking applies to default-mode's own thinking-capable model"))
    (let ((default-selection (agent-session:mode-current-selection
                              (agent-session-service context)
                              :default-mode context))
          (other-selection (agent-session:mode-current-selection
                            (agent-session-service context)
                            :other-mode context)))
      (is (string= "selected-model"
                   (models:model-selection-model-id default-selection))
          "default-mode kept its own model, not the registry default")
      (is (eq :high (test-selection-reasoning-effort default-selection)))
      (is (string= "local-model"
                   (models:model-selection-model-id other-selection))
          "other-mode is untouched"))))

(test (model-display-reads-the-mode-agent-not-the-registry-default :fixture interactive-authority)
  "Bare /model shows what the active mode runs -- its agent's selection -- not the
registry default, which only seeds new agents. When the default drifts to another
model, the mode still displays its own."
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (bootstrap-model-command-mode context)
    (invoke-test-command context :model
                         '(:tail "path-provider/selected-model"
                           :mode-id :default-mode))
    (let* ((registry (model-registry context))
           (local-def (models:find-model-definition registry
                                                     "local-provider" "local-model")))
      (models:select-model registry local-def context))
    (let ((text (command-result-text
                 context
                 (invoke-test-command context :model '(:mode-id :default-mode)))))
      (is (search "Current model: path-provider/selected-model" text)
          "the current line shows the mode's own model")
      (is (null (search "Current model: local-provider/local-model" text))
          "not the drifted registry default"))))

(test (model-menu-accept-preserves-thinking :fixture interactive-authority)
  "Accepting a model from the menu carries the mode's current thinking level
forward when the chosen model supports it -- matching the typed path -- rather than
resetting it to off."
  (let ((context (model-menu-test-context)))
    (register-command-test-models context)
    (let ((registry (model-registry context)))
      (models:register-model-provider
       registry
       (models:make-model-provider "aa-provider" :fake :auth-required-p nil)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition
        "aa-provider" "aa-model" :fake
        :name "AA"
        :option-schemas (list (test-reasoning-effort-schema)))
       context))
    (bootstrap-model-command-mode context)
    (let ((app (tui-app:make-tui-app :context context :columns 48)))
      (invoke-test-command context :model
                           (list :mode-id :default-mode
                                 :tail "aa-provider/aa-model:high"))
      (invoke-test-command context :model (list :mode-id :default-mode :app app))
      (tui-editor::accept-completion (tui-app:tui-app-editor app))
      (let ((selection (agent-session:mode-current-selection
                        (agent-session-service context) :default-mode context)))
        (is (string= "aa-model" (models:model-selection-model-id selection))
            "the menu's first row, the thinking-capable model, was accepted")
        (is (eq :high (test-selection-reasoning-effort selection))
            "the menu accept preserved the thinking level")))))

(test (thinking-command-rejects-unsupported-model-thinking :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (bootstrap-model-command-mode context)
    (invoke-test-command context
                         :model
                         '(:tail "local-provider/local-model"))
    (let ((result (invoke-test-command context
                                       :thinking
                                       '(:tail "high"))))
      (is (commands:command-result-error-p result))
      (is (search "does not support thinking"
                  (command-result-text context result))))))

(test (auth-command-registers-env-backed-auth-reference :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (let ((store (credential-store context))
          (registry (model-registry context)))
      (models:register-model-provider
       registry
       (models:make-model-provider "path-provider" :fake)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition "path-provider"
                                     "env-model"
                                     :fake)
       context)
      (is (null (search "path-provider/env-model"
                        (command-result-text
                         context
                         (invoke-test-command context :models)))))
      (let ((result (invoke-test-command context
                                         :auth
                                         '(:words ("env"
                                                   "path-provider"
                                                   "PATH")))))
        (is (not (commands:command-result-error-p result)))
        (is (auth:credential-available-p store "path-provider"))
        (is (search "path-provider/env-model"
                    (command-result-text
                     context
                     (invoke-test-command context :models))))))))

(test auth-completion-answers-by-position
  (let ((menu (getf (model-commands:auth-completion nil "") :candidates)))
    (is (equal '("env" "key" "login" "code" "logout") (mapcar #'car menu)))
    (is (every (lambda (entry)
                 (and (stringp (cdr entry)) (plusp (length (cdr entry)))))
               menu)))
  (is (equal '("env" "key" "login" "code" "logout")
             (mapcar #'car
                     (getf (model-commands:auth-completion nil "lo")
                           :candidates))))
  (is (string= "<pasted code or redirect URL>"
               (getf (model-commands:auth-completion nil "code ") :hint)))
  (is (string= "<ENV_VAR>"
               (getf (model-commands:auth-completion nil "env path-provider ")
                     :hint)))
  (is (null (model-commands:auth-completion nil "logout gone extra ")))
  (is (null (model-commands:auth-completion nil "frob "))))

(test (auth-completion-offers-registered-provider-ids :fixture interactive-authority)
  (multiple-value-bind (context protocol) (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (let ((result (model-commands:auth-completion context "logout ")))
      (is (member "logout path-provider" (getf result :candidates)
                  :test #'string=))
      (is (string= "<provider>" (getf result :hint))))))

(test (thinking-completion-is-model-schema-driven :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (model-command-test-context)
    (declare (ignore protocol))
    (let ((registry (model-registry context)))
      (register-command-test-models context)
      (models:register-model-provider
       registry
       (models:make-model-provider "narrow-provider" :fake :auth-required-p nil)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition
        "narrow-provider" "narrow-model" :fake
        :option-schemas (list (test-reasoning-effort-schema
                               :values '(:off :high))))
       context)
      (bootstrap-model-command-mode context)
      (invoke-test-command context :model
                           '(:tail "narrow-provider/narrow-model"))
      (is (equal '("off" "high")
                 (getf (model-commands:thinking-completion nil "" context)
                       :candidates))))))

(test (model-completion-offers-auth-available-references :fixture interactive-authority)
  (multiple-value-bind (context protocol) (model-command-test-context)
    (declare (ignore protocol))
    (register-command-test-models context)
    (let ((candidates (getf (model-commands:model-completion context "")
                            :candidates)))
      (is (member "path-provider/selected-model" candidates :test #'string=))
      (is (member "local-provider/local-model" candidates :test #'string=))
      (is (not (member "missing-provider/missing-model" candidates
                       :test #'string=))))))

(test auth-command-help-renders-full-grammar
  (multiple-value-bind (context protocol) (model-command-test-context)
    (declare (ignore context))
    (let* ((commands (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (auth (ext:provider-call commands :find-command :auth))
           (usage (commands:command-usage-text auth)))
      (is (search "/auth env <provider> <ENV_VAR>" usage))
      (is (search "/auth logout <provider>" usage)))))

(defun model-menu-test-context ()
  "Model commands on the full TUI stack, so menu tests can ride a real app."
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-tui-app-stack context)
    (install-extension context
                       model-commands:*model-commands-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    context))

(test (model-command-opens-menu-with-app :fixture interactive-authority)
  "With a TUI app riding on the arguments, bare /model opens a selection
menu over the auth-available models with the current selection marked, and
accepting a row selects that model silently -- the model-change event
announces it. An explicit pattern still acts directly, and with nothing
available the command answers with the text listing instead of opening an
empty menu."
  (let ((context (model-menu-test-context)))
    (bootstrap-model-command-mode context)
    (let ((app (tui-app:make-tui-app :context context :columns 48)))
      (is (search "No available models."
                  (command-result-text
                   context
                   (invoke-test-command context :model
                                        (list :mode-id :default-mode
                                              :app app)))))
      (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app))))
      (register-command-test-models context)
      (let ((result (invoke-test-command context :model
                                         (list :mode-id :default-mode
                                               :app app))))
        (is (null (commands:command-result-content result))
            "the menu is the feedback, the result stays empty"))
      (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
        (is (not (null popup)))
        (is (eq :selection (tui-editor:completion-popup-kind popup)))
        (is (equal '("local-provider/local-model"
                     "path-provider/selected-model")
                   (mapcar #'tui-editor:completion-candidate-insert
                           (tui-editor:completion-popup-candidates popup)))
            "auth-available models sorted by reference")
        (is (equal '("  " "  Selected options reasoning-effort")
                   (mapcar #'tui-editor:completion-candidate-description
                           (tui-editor:completion-popup-candidates popup)))
            "row text carries name and compact option ids beside the insert"))
      (tui-editor::accept-completion (tui-app:tui-app-editor app))
      (let ((selection (models:current-model-selection
                        (model-registry context))))
        (is (string= "local-provider"
                     (models:model-selection-provider-id selection))
            "accepting the selected row selects that model")
        (is (string= "local-model"
                     (models:model-selection-model-id selection))))
      (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
          "the menu closes on accept")
      (invoke-test-command context :model
                           (list :mode-id :default-mode :app app))
      (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
        (is (string= "* "
                     (tui-editor:completion-candidate-description
                      (first (tui-editor:completion-popup-candidates popup))))
            "the current selection is marked"))
      (tui-editor::dismiss-editor-completion (tui-app:tui-app-editor app))
      (invoke-test-command context :model
                           (list :mode-id :default-mode
                                 :app app
                                 :tail "path-provider/selected-model:high"))
      (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
          "an explicit pattern acts directly, no menu opens")
      (is (eq :high
              (test-selection-reasoning-effort
               (models:current-model-selection (model-registry context))))))))

(test (thinking-command-opens-menu-with-app :fixture interactive-authority)
  "With a TUI app riding on the arguments and a thinking-capable model
current, bare /thinking opens a selection menu over the levels with the
current one marked, and accepting a row sets that level. Without a current
model, or on a model without thinking support, it degrades to the text
answer instead of opening a menu."
  (let ((context (model-menu-test-context)))
    (register-command-test-models context)
    (bootstrap-model-command-mode context)
    (let ((app (tui-app:make-tui-app :context context :columns 48)))
      (let ((result (invoke-test-command context :thinking
                                         (list :mode-id :default-mode
                                               :app app))))
        (is (commands:command-result-error-p result))
        (is (search "No current model" (command-result-text context result)))
        (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))))
      (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app))))
      (invoke-test-command context :model
                           (list :mode-id :default-mode
                                 :tail "path-provider/selected-model:high"))
      (let ((result (invoke-test-command context :thinking
                                         (list :mode-id :default-mode
                                               :app app))))
        (is (null (commands:command-result-content result))
            "the menu is the feedback, the result stays empty"))
      (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
        (is (not (null popup)))
        (is (eq :selection (tui-editor:completion-popup-kind popup)))
        (is (equal '("off" "minimal" "low" "medium" "high" "xhigh")
                   (mapcar #'tui-editor:completion-candidate-insert
                           (tui-editor:completion-popup-candidates popup))))
        (is (equal '("  " "  " "  " "  " "* " "  ")
                   (mapcar #'tui-editor:completion-candidate-description
                           (tui-editor:completion-popup-candidates popup)))
            "the current level is marked"))
      (tui-editor::accept-completion (tui-app:tui-app-editor app))
      (is (eq :off
              (test-selection-reasoning-effort
               (models:current-model-selection (model-registry context))))
          "accepting the selected row sets that level")
      (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
          "the menu closes on accept")
      (invoke-test-command context :model
                           (list :mode-id :default-mode
                                 :tail "local-provider/local-model"))
      (let ((result (invoke-test-command context :thinking
                                         (list :mode-id :default-mode
                                               :app app))))
        (is (commands:command-result-error-p result))
        (is (search "does not support thinking"
                    (command-result-text context result)))
        (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app))))))))

(test (model-menu-survives-typed-submit :fixture interactive-authority)
  "Typed bare /model reaches its menu through the real input path: decoder,
route, editor submit, dispatch, and the post-input completion refresh that
must not clobber the menu the command just opened."
  (let ((context (model-menu-test-context)))
    (register-command-test-models context)
    (bootstrap-model-command-mode context)
    (let* ((app (tui-app:make-tui-app :context context :columns 48))
           (editor (tui-app:tui-app-editor app)))
      (tui-app:tui-app-feed app "/model" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (let ((popup (tui-editor:editor-completion editor)))
        (is (not (null popup)) "typed bare /model leaves its menu open")
        (is (eq :selection (tui-editor:completion-popup-kind popup)))))))
