(in-package #:kli/tests)
(in-suite all)

(defun install-config-from (context root settings)
  "Write SETTINGS as the global settings.json under ROOT and install the
config extension against it."
  (let ((global-dir (make-config-test-dir root "global")))
    (write-config-test-file (merge-pathnames "settings.json" global-dir)
                            settings)
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extension context config:*config-extension-manifest*))))

(defun install-settings-wiring (context)
  (install-extension context
                     config-wiring:*settings-wiring-extension-manifest*))

(test settings-wiring-applies-theme
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-config-from context root "{\"theme\": \"light\"}")
    (install-extension context tui-style:*tui-style-extension-manifest*)
    (install-settings-wiring context)
    (is (equal "light"
               (tui-style:theme-name (tui-style:active-theme protocol)))))
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-config-from context root "{\"theme\": \"no-such-theme\"}")
    (install-extension context tui-style:*tui-style-extension-manifest*)
    (handler-bind ((warning #'muffle-warning))
      (install-settings-wiring context))
    (is (equal "dark"
               (tui-style:theme-name (tui-style:active-theme protocol)))
        "an unregistered theme name warns and leaves the default active")))

(test settings-wiring-auto-theme-defers-to-boot
  "theme auto stores the deferred intent without warning or changing the
active theme, leaving boot-time background detection to resolve it."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (warned nil))
    (install-config-from context root "{\"theme\": \"auto\"}")
    (install-extension context tui-style:*tui-style-extension-manifest*)
    (handler-bind ((warning (lambda (w)
                              (declare (ignore w))
                              (setf warned t)
                              (muffle-warning))))
      (install-settings-wiring context))
    (is (not warned))
    (is (eq :auto (ext:protocol-storage protocol tui-style:+theme-mode-key+)))
    (is (equal "dark"
               (tui-style:theme-name (tui-style:active-theme protocol))))))

(test settings-wiring-absent-theme-defaults-to-auto
  "A missing theme key reads as auto, the new default, storing the same
deferred intent."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-config-from context root "{\"frobnicate\": true}")
    (install-extension context tui-style:*tui-style-extension-manifest*)
    (install-settings-wiring context)
    (is (eq :auto (ext:protocol-storage protocol tui-style:+theme-mode-key+)))))

(test settings-wiring-explicit-theme-pins-mode
  "An explicit registered theme applies immediately and marks the mode
explicit so boot detection is skipped."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-config-from context root "{\"theme\": \"light\"}")
    (install-extension context tui-style:*tui-style-extension-manifest*)
    (install-settings-wiring context)
    (is (eq :explicit (ext:protocol-storage protocol tui-style:+theme-mode-key+)))
    (is (equal "light"
               (tui-style:theme-name (tui-style:active-theme protocol))))))

(test settings-wiring-applies-keybindings
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-config-from context root
                         "{\"keybindings\": {\"ctrl+t\": \"undo\"}}")
    (install-extension context tui-keymap:*tui-keymap-extension-manifest*)
    (install-settings-wiring context)
    (is (eq :undo (tui-keymap:keymap-action protocol "ctrl+t")))))

(test settings-wiring-selects-default-model
  "The configured default overrides a selection made by an earlier install, and
defaultOptions ride along as typed semantic model options when the model declares
them."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root))
          (registry (model-registry context)))
      (register-runtime-model context "prior-provider" "prior-model"
                              :auth-required-p nil)
      (models:register-model-provider
       registry
       (models:make-model-provider "settings-provider" :fake
                                   :auth-required-p nil)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition
        "settings-provider" "settings-model"
        :fake
        :option-schemas (list (test-reasoning-effort-schema)))
       context)
      (install-config-from
       context root
       "{\"defaultProvider\": \"settings-provider\",
          \"defaultModel\": \"settings-model\",
          \"defaultOptions\": {\"reasoning-effort\": \"high\"}}")
      (install-settings-wiring context)
      (let ((selection (models:registry-current-selection registry)))
        (is (equal "settings-provider"
                   (models:model-selection-provider-id selection)))
        (is (equal "settings-model"
                   (models:model-selection-model-id selection)))
        (is (eq :high (test-selection-reasoning-effort selection)))))))

(test settings-wiring-unknown-model-keeps-selection
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root))
          (registry (model-registry context)))
      (register-runtime-model context "prior-provider" "prior-model"
                              :auth-required-p nil)
      (let ((previous (models:registry-current-selection registry)))
        (install-config-from
         context root
         "{\"defaultProvider\": \"absent\", \"defaultModel\": \"nope\"}")
        (handler-bind ((warning #'muffle-warning))
          (install-settings-wiring context))
        (is (eq previous (models:registry-current-selection registry)))))))

(test (settings-wiring-construct-agent-reads-delivery-modes :fixture interactive-authority)
  "Delivery modes act at agent construction through the config capability,
with no wiring extension involved."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root)))
      (install-config-from
       context root
       "{\"steeringMode\": \"all\", \"followUpMode\": \"one-at-a-time\"}")
      (let ((service (bind-agent-session-mode context)))
        (let* ((binding (gethash :default-mode
                                 (agent-session:session-mode-bindings
                                  service)))
               (agent (kli:find-live-object
                       (kli:context-registry context)
                       (agent-session:mode-binding-agent-id binding))))
          (is (eq :all (agents:agent-behavior-value
                        agent :steering-delivery-policy)))
          (is (eq :one-at-a-time (agents:agent-behavior-value
                                  agent :follow-up-delivery-policy))))))))

(defun constructed-agent-subject (context)
  "Bind a default-mode agent through construct-agent-for and return its subject."
  (let* ((service (bind-agent-session-mode context))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding))))
    (agents:agent-subject agent)))

(test (construct-agent-absent-capabilities-boots-unrestricted :fixture interactive-authority)
  "The regression guard: a root agent constructed with no capabilities key boots
unrestricted -- an ordinary subject, not the narrow default and not a
system-subject -- so the baseline Cairn/file tools are usable rather than denied."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root)))
      (install-config-from context root "{\"theme\": \"light\"}")
      (let ((subject (constructed-agent-subject context)))
        (is (not (typep subject 'ext:system-subject))
            "the unrestricted root agent is a policy grant, not substrate authority")
        (dolist (cap '(:cairn/read :file/read :process/exec :image/eval))
          (is (ext:check-capability subject cap)
              "the unrestricted root agent confers ~A" cap))))))

(test (construct-agent-present-capabilities-boots-bounded :fixture interactive-authority)
  "A present capabilities key bounds the constructed root agent to exactly its
grant, so enforcement begins once a policy is declared."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root)))
      (install-config-from context root "{\"capabilities\": [\"file/read\"]}")
      (let ((subject (constructed-agent-subject context)))
        (is (ext:check-capability subject :file/read))
        (is (null (ext:check-capability subject :process/exec)))))))

(test settings-wiring-swaps-file-session-store
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (sessions-dir (merge-pathnames "sessions/" root)))
    (switch-to-extension-protocol context)
    (install-extensions context sess:*session-log-extension-manifest*)
    (install-config-from context root
                         (format nil "{\"sessionDir\": ~S}"
                                 (namestring sessions-dir)))
    (install-settings-wiring context)
    (let ((store (session-log-store context)))
      (is (typep store 'sess:file-session-store))
      (is (equal sessions-dir (sess:file-store-root store))))))

(test (settings-wiring-retract-restores :fixture interactive-authority)
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tui-style:*tui-style-extension-manifest*
                        tui-keymap:*tui-keymap-extension-manifest*
                        sess:*session-log-extension-manifest*)
    (let ((memory-store (session-log-store context)))
      (install-config-from
       context root
       (format nil "{\"theme\": \"light\",
                     \"keybindings\": {\"ctrl+t\": \"undo\"},
                     \"sessionDir\": ~S}"
               (namestring (merge-pathnames "sessions/" root))))
      (let ((handle (install-settings-wiring context)))
        (is (equal "light"
                   (tui-style:theme-name (tui-style:active-theme protocol))))
        (is (eq :undo (tui-keymap:keymap-action protocol "ctrl+t")))
        (is (typep (session-log-store context) 'sess:file-session-store))
        (ext:retract-manifest handle protocol context)
        (is (equal "dark"
                   (tui-style:theme-name (tui-style:active-theme protocol))))
        (is (null (tui-keymap:keymap-action protocol "ctrl+t")))
        (is (eq memory-store (session-log-store context)))))))

(test settings-overlay-in-place-before-wiring-applies
  "A settings overlay recorded on the protocol before the config extension
installs is live when settings-wiring applies, so wiring reads the overlayed
view rather than the bare file merge."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (config:record-settings-overlay
     protocol (com.inuoe.jzon:parse "{\"theme\": \"light\"}"))
    (install-config-from context root "{\"theme\": \"dark\"}")
    (install-extension context tui-style:*tui-style-extension-manifest*)
    (install-settings-wiring context)
    (is (equal "light"
               (tui-style:theme-name (tui-style:active-theme protocol)))
        "wiring applies the overlay theme, not the file theme")
    (is (eq :explicit
            (ext:protocol-storage protocol tui-style:+theme-mode-key+)))))

(test swap-settings-overlay-reapplies-wiring
  "Swapping the overlay reverts the standing settings application and
re-applies it over the new merge, the discipline live switching rides."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-config-from context root "{\"theme\": \"light\"}")
    (install-extension context tui-style:*tui-style-extension-manifest*)
    (install-settings-wiring context)
    (is (equal "light" (tui-style:theme-name (tui-style:active-theme protocol))))
    (let ((overlay (com.inuoe.jzon:parse "{\"theme\": \"dark\"}")))
      (is (null (config-wiring:swap-settings-overlay context overlay))
          "no overlay stood before the swap")
      (is (equal "dark" (tui-style:theme-name (tui-style:active-theme protocol)))
          "the swapped-in overlay theme applies")
      (is (eq overlay (config-wiring:swap-settings-overlay context nil))
          "swapping away returns the standing overlay")
      (is (equal "light" (tui-style:theme-name (tui-style:active-theme protocol)))
          "clearing the overlay re-applies the file theme"))))

(test swap-settings-overlay-without-wiring-swaps-the-layer
  "Without the wiring extension the swap still installs the overlay layer on
the config service."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-config-from context root "{\"theme\": \"dark\"}")
    (let ((overlay (com.inuoe.jzon:parse "{\"theme\": \"light\"}")))
      (config-wiring:swap-settings-overlay context overlay)
      (is (equal "light"
                 (config:settings-value
                  (config:config-service-settings
                   (config:find-config-service context))
                  "theme"))))))

(test settings-wiring-unknown-keys-inert
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-config-from context root "{\"frobnicate\": true}")
    (finishes (install-settings-wiring context))
    (is (ext:extension-loaded-p protocol :settings-wiring))))

(defun register-capabilities-agent (context &key subject granted-capabilities)
  "Register a fresh live agent on CONTEXT's agent-loop service with the given
authority and return it."
  (let ((service (kli:find-live-object (kli:context-registry context)
                                       :agent-loop-service))
        (agent (agents:make-agent nil nil nil nil nil
                                  :subject subject
                                  :granted-capabilities granted-capabilities)))
    (agents:register-agent service agent context)
    agent))

(test capabilities-setting-narrows-top-agent
  "A present capabilities key meets each live agent's subject; an agent at the
lattice top narrows to exactly the key's grant rather than staying top."
  (let* ((root (temp-config-root))
         (context (agent-session-test-context)))
    (install-config-from context root "{\"capabilities\": [\"file/read\"]}")
    (let ((agent (register-capabilities-agent
                  context :subject (ext:make-system-subject))))
      (config-wiring::apply-capabilities-setting
       (config-wiring::config-provider-of context) context)
      (is (not (typep (agents:agent-subject agent) 'ext:system-subject))
          "the meet replaces top with a bounded subject")
      (is (ext:check-capability (agents:agent-subject agent) :file/read))
      (is (null (ext:check-capability (agents:agent-subject agent)
                                      :process/exec))
          "the narrowed subject confers only the key's capability"))))

(test capabilities-setting-absent-key-is-a-no-op
  "An absent capabilities key leaves every live agent's subject untouched: an
overlay can only narrow, never restore or widen authority."
  (let* ((root (temp-config-root))
         (context (agent-session-test-context)))
    (install-config-from context root "{\"theme\": \"light\"}")
    (let* ((agent (register-capabilities-agent
                   context :granted-capabilities '(:file/read)))
           (before (agents:agent-subject agent)))
      (config-wiring::apply-capabilities-setting
       (config-wiring::config-provider-of context) context)
      (is (eq before (agents:agent-subject agent))
          "no capabilities key, so the subject is left as-is"))))

(test capabilities-setting-cannot-widen
  "A capabilities key broader than the agent already holds yields the meet, so
the present-key branch can only attenuate."
  (let* ((root (temp-config-root))
         (context (agent-session-test-context)))
    (install-config-from context root
                         "{\"capabilities\": [\"file/read\", \"process/exec\"]}")
    (let ((agent (register-capabilities-agent
                  context :granted-capabilities '(:file/read))))
      (config-wiring::apply-capabilities-setting
       (config-wiring::config-provider-of context) context)
      (is (ext:check-capability (agents:agent-subject agent) :file/read))
      (is (null (ext:check-capability (agents:agent-subject agent)
                                      :process/exec))
          "the key cannot grant a capability the agent did not already hold"))))

(test capabilities-setting-all-invalid-narrows-to-bottom
  "A present-but-all-invalid capabilities value narrows every live agent to
nothing rather than being a no-op: presence opts into enforcement, so a bad
value denies the gated tools."
  (let* ((root (temp-config-root))
         (context (agent-session-test-context)))
    (install-config-from context root "{\"capabilities\": [7]}")
    (let ((agent (register-capabilities-agent
                  context :granted-capabilities '(:file/read :process/exec))))
      (handler-bind ((warning #'muffle-warning))
        (config-wiring::apply-capabilities-setting
         (config-wiring::config-provider-of context) context))
      (is (null (ext:check-capability (agents:agent-subject agent) :file/read))
          "an all-invalid present key narrows the agent to bottom")
      (is (null (ext:check-capability (agents:agent-subject agent)
                                      :process/exec))))))

(test capabilities-setting-mixed-invalid-narrows-to-valid-subset
  "A capabilities value mixing valid and invalid entries narrows to the valid
subset, dropping only the invalid entries."
  (let* ((root (temp-config-root))
         (context (agent-session-test-context)))
    (install-config-from context root "{\"capabilities\": [\"file/read\", 7]}")
    (let ((agent (register-capabilities-agent
                  context :granted-capabilities '(:file/read :process/exec))))
      (handler-bind ((warning #'muffle-warning))
        (config-wiring::apply-capabilities-setting
         (config-wiring::config-provider-of context) context))
      (is (ext:check-capability (agents:agent-subject agent) :file/read)
          "the valid entry still applies")
      (is (null (ext:check-capability (agents:agent-subject agent)
                                      :process/exec))
          "the invalid entry is dropped, not treated as granting everything"))))

(test capabilities-setting-revert-restores-prior
  "Revert via the returned snapshot restores each agent's exact prior subject."
  (let* ((root (temp-config-root))
         (context (agent-session-test-context)))
    (install-config-from context root "{\"capabilities\": [\"file/read\"]}")
    (let* ((agent (register-capabilities-agent
                   context :granted-capabilities '(:file/read :process/exec)))
           (before (agents:agent-subject agent))
           (state (config-wiring::apply-capabilities-setting
                   (config-wiring::config-provider-of context) context)))
      (is (null (ext:check-capability (agents:agent-subject agent)
                                      :process/exec))
          "the overlay attenuated the agent")
      (config-wiring::revert-capabilities-setting state)
      (is (eq before (agents:agent-subject agent))
          "revert restores the exact prior subject")
      (is (ext:check-capability (agents:agent-subject agent) :process/exec)
          "the restored subject holds process/exec again"))))

(test settings-wiring-applies-bash-policy
  "A \"bash\" settings object seeds the session policy -- cwd, envAllowlist, and
defaultTimeout land in the policy plist -- and seeds the exec backend."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (install-config-from
     context root
     "{\"bash\": {\"cwd\": \"/tmp/work\",
                  \"envAllowlist\": [\"PATH\", \"HOME\"],
                  \"defaultTimeout\": 90,
                  \"backend\": \"persistent\"}}")
    (install-settings-wiring context)
    (let ((policy (tools-bash:bash-policy protocol)))
      (is (equal "/tmp/work" (getf policy :cwd)))
      (is (equal '("PATH" "HOME") (getf policy :env-allowlist)))
      (is (eql 90 (getf policy :default-timeout))))
    (is (eql tools-bash:+persistent-shell-bash-exec-provider-id+
             (tools-bash:active-bash-exec-provider-id protocol))
        "the backend is seeded from settings")))

(test settings-wiring-bash-backend-is-seed-only
  "Backend is seed-only: settings seeds it when unset, but a standing explicit
pick survives a settings apply."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (setf (tools-bash:active-bash-exec-provider-id protocol)
          tools-bash:+local-bash-exec-provider-id+)
    (install-config-from context root "{\"bash\": {\"backend\": \"persistent\"}}")
    (install-settings-wiring context)
    (is (eql tools-bash:+local-bash-exec-provider-id+
             (tools-bash:active-bash-exec-provider-id protocol))
        "the settings backend does not clobber the standing pick")))

(test (settings-wiring-bash-policy-reverts :fixture interactive-authority)
  "Retract restores the bash policy snapshotted at apply; the seeded backend is
sticky so it is not yanked from under the session."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (install-config-from
     context root
     "{\"bash\": {\"cwd\": \"/tmp/work\", \"backend\": \"persistent\"}}")
    (let ((handle (install-settings-wiring context)))
      (is (equal "/tmp/work" (getf (tools-bash:bash-policy protocol) :cwd)))
      (is (eql tools-bash:+persistent-shell-bash-exec-provider-id+
               (tools-bash:active-bash-exec-provider-id protocol)))
      (ext:retract-manifest handle protocol context)
      (is (null (tools-bash:bash-policy protocol))
          "the prior empty policy is restored")
      (is (eql tools-bash:+persistent-shell-bash-exec-provider-id+
               (tools-bash:active-bash-exec-provider-id protocol))
          "the seeded backend selection is sticky across revert"))))

(test settings-wiring-absent-bash-key-is-a-no-op
  "With no \"bash\" key the policy and backend are left untouched."
  (let* ((root (temp-config-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (install-config-from context root "{\"frobnicate\": true}")
    (install-settings-wiring context)
    (is (null (tools-bash:bash-policy protocol)))
    (is (eql tools-bash:+local-bash-exec-provider-id+
             (tools-bash:active-bash-exec-provider-id protocol)))))
(test settings-wiring-applies-openai-family-default-options
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root))
          (registry (model-registry context)))
      (models:register-model-provider
       registry
       (models:make-model-provider "settings-provider" :fake
                                   :auth-required-p nil)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition
        "settings-provider" "settings-model"
        :fake
        :option-schemas (list (test-reasoning-effort-schema)
                              (test-text-verbosity-schema)
                              (test-service-tier-schema)))
       context)
      (install-config-from
       context root
       "{\"defaultProvider\": \"settings-provider\",
          \"defaultModel\": \"settings-model\",
          \"defaultOptions\": {\"reasoning-effort\": \"low\",
                              \"text-verbosity\": \"high\",
                              \"service-tier\": \"flex\"}}")
      (install-settings-wiring context)
      (let ((selection (models:registry-current-selection registry)))
        (is (eq :low (models:model-selection-option-value
                      selection "reasoning-effort")))
        (is (eq :high (models:model-selection-option-value
                       selection "text-verbosity")))
        (is (eq :flex (models:model-selection-option-value
                       selection "service-tier")))))))
(test settings-wiring-rejects-non-semantic-default-options
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root))
          (registry (model-registry context)))
      (models:register-model-provider
       registry
       (models:make-model-provider "settings-provider" :fake
                                   :auth-required-p nil)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition
        "settings-provider" "settings-model"
        :fake
        :option-schemas (list (test-reasoning-effort-schema)))
       context)
      (install-config-from
       context root
       "{\"defaultProvider\": \"settings-provider\",
          \"defaultModel\": \"settings-model\",
          \"defaultOptions\": {\"developer-role\": \"system\",
                              \"reasoning-effort\": \"high\"}}")
      (handler-bind ((warning #'muffle-warning))
        (install-settings-wiring context))
      (let ((selection (models:registry-current-selection registry)))
        (is (eq :high (test-selection-reasoning-effort selection)))
        (is (null (models:model-selection-option-value
                   selection "developer-role")))))))
(test (settings-wiring-compaction-enabled-false-skips-auto-but-not-manual :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root)))
      (install-config-from context root "{\"compaction\": {\"enabled\": false}}")
      (install-settings-wiring context)
      (bind-agent-session-mode
       context
       :metadata (list :fake-deltas '("STRUCTURED SUMMARY")
                       :fake-usage (list :input-tokens 30 :output-tokens 50
                                         :total-tokens 80)
                       :context-window 100))
      (let ((service (agent-session-service context))
            (events '()))
        (agent-session:register-session-event-listener
         service
         (agent-session:make-session-event-listener
          :compaction-watcher
          (lambda (event mode-id ctx)
            (declare (ignore mode-id ctx))
            (when (eq (event:event-type event) :session-compaction-needed)
              (push event events))))
         context)
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "hello" context)
        (is (null events))
        (let* ((store (session-log-store context))
               (session (sess:find-session store :agent-session-test-session)))
          (flet ((append! (entry)
                   (sess:append-session-entry store session entry context)))
            (append! (sized-message-entry :user 400 :cw-u1))
            (append! (sized-message-entry :assistant 400 :cw-a1))
            (append! (sized-message-entry :user 40 :cw-u2)))
          (agent-session:recode-compaction-policy service :keep-recent-tokens 5)
          (let ((entry (agent-session:compact-agent-session
                        service :default-mode context)))
            (is (typep entry 'sess:compaction-entry))))))))
(test settings-wiring-compaction-recodes-valid-fields
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root)))
      (install-config-from
       context root
       "{\"compaction\": {\"enabled\": false,
                         \"thresholdRatio\": 0.5,
                         \"reserveTokens\": 12345}}")
      (install-settings-wiring context)
      (let* ((service (agent-session-service context))
             (state (funcall (agent-session:session-compaction-policy service)
                             :inspect)))
        (is (eq nil (getf state :enabled)))
        (is (= 0.5 (getf state :threshold-ratio)))
        (is (= 12345 (getf state :keep-recent-tokens)))))))

(test (settings-wiring-compaction-retract-restores-null-reserve :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (let ((root (temp-config-root)))
      (install-config-from
       context root
       "{\"compaction\": {\"enabled\": false,
                         \"thresholdRatio\": 0.5,
                         \"reserveTokens\": 12345}}")
      (let* ((service (agent-session-service context))
             (handle (install-settings-wiring context)))
        (is (= 12345
               (getf (funcall (agent-session:session-compaction-policy service)
                              :inspect)
                     :keep-recent-tokens)))
        (ext:retract-manifest handle protocol context)
        (let ((state (funcall (agent-session:session-compaction-policy service)
                              :inspect)))
          (is (eq t (getf state :enabled)))
          (is (= 0.85 (getf state :threshold-ratio)))
          (is (null (getf state :keep-recent-tokens))))))))

(test settings-wiring-malformed-compaction-settings-are-ignored
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((root (temp-config-root)))
      (install-config-from
       context root
       "{\"compaction\": {\"enabled\": \"no\",
                         \"thresholdRatio\": -1,
                         \"reserveTokens\": \"many\"}}")
      (handler-bind ((warning #'muffle-warning))
        (install-settings-wiring context))
      (let* ((service (agent-session-service context))
             (state (funcall (agent-session:session-compaction-policy service)
                             :inspect)))
        (is (eq t (getf state :enabled)))
        (is (= 0.85 (getf state :threshold-ratio)))
        (is (null (getf state :keep-recent-tokens)))))))
