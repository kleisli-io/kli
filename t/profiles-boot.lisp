(in-package #:kli/tests)
(in-suite all)

(test select-profile-name-prefers-arg-then-env-then-default
  "Boot selects a profile by name -- a --profile argument, then the KLI_PROFILE environment fallback, then the default."
  (is (eq :headless
          (app:select-profile-name :argv '("--profile" "headless")
                                   :env "autonomous"))
      "an explicit --profile wins over the environment")
  (is (eq :autonomous
          (app:select-profile-name :argv '() :env "autonomous"))
      "KLI_PROFILE applies when --profile is absent")
  (is (eq app:*default-profile*
          (app:select-profile-name :argv '() :env nil))
      "neither source selecting falls back to the default")
  (is (eq app:*default-profile*
          (app:select-profile-name :argv '() :env ""))
      "an empty KLI_PROFILE is treated as unset")
  (is (eq :headless
          (app:select-profile-name :argv '("--profile" "Headless") :env nil))
      "a selected name is normalized to a keyword"))

(test find-profile-manifest-resolves-every-known-profile
  (is (eq profiles:*headless-extension-manifest*
          (profiles:find-profile-manifest :headless)))
  (is (eq profiles:*interactive-terminal-extension-manifest*
          (profiles:find-profile-manifest "interactive-terminal"))
      "resolution accepts a string name")
  (is (eq profiles:*human-in-loop-extension-manifest*
          (profiles:find-profile-manifest :human-in-loop)))
  (is (eq profiles:*autonomous-extension-manifest*
          (profiles:find-profile-manifest :autonomous))))

(test find-profile-manifest-errors-helpfully-on-unknown-name
  (handler-case
      (progn
        (profiles:find-profile-manifest :no-such-profile)
        (fail "expected unknown-profile"))
    (profiles:unknown-profile (condition)
      (is (eq :no-such-profile (profiles:unknown-profile-name condition)))
      (is (subsetp '(:headless :interactive-terminal :human-in-loop :autonomous)
                   (profiles:unknown-profile-known condition))
          "the error enumerates the known profiles")
      (is (search "INTERACTIVE-TERMINAL" (princ-to-string condition))
          "the report names an alternative"))))

(test main-installs-the-selected-headless-profile-without-tui
  (let* ((context (app:main :profile :headless))
         (protocol (kli:active-protocol context)))
    (is (null app:*current-app*))
    (let ((record (profiles:protocol-profile-activation protocol :headless)))
      (is (not (null record)))
      (is (eq :headless (profiles:profile-activation-id record))))
    (is (ext:extension-loaded-p protocol :agent-session))
    (is (ext:extension-loaded-p protocol :observability)
        "headless sinks the event stream for automated runs")
    (is (not (ext:extension-loaded-p protocol :tui-app))
        "a headless selection installs no terminal UI")
    (is (not (app:profile-interactive-p context))
        "the boot path treats headless as non-interactive")))

(test main-installs-the-selected-interactive-profile-with-tui
  (let* ((context (app:main :profile :interactive-terminal))
         (protocol (kli:active-protocol context)))
    (let ((record (profiles:protocol-profile-activation
                   protocol :interactive-terminal)))
      (is (not (null record)))
      (is (eq :interactive-terminal (profiles:profile-activation-id record))))
    (is (ext:extension-loaded-p protocol :agent-session))
    (is (ext:extension-loaded-p protocol :tui-app)
        "an interactive selection installs the terminal UI")
    (is (app:profile-interactive-p context)
        "the boot path treats interactive-terminal as interactive")))

(test main-installs-the-print-profile-with-settings-wiring-no-tui
  "The print profile carries the cross-cutting non-UI extensions a one-shot run
needs -- settings-wiring selects the configured default model, skills and
context files shape the prompt, observability sinks events -- yet stays
headless. Without settings-wiring a one-shot run boots with no model and refuses
to start."
  (let* ((context (app:main :profile :print))
         (protocol (kli:active-protocol context)))
    (is (ext:extension-loaded-p protocol :settings-wiring)
        "print must apply settings so the configured default model is selected")
    (is (ext:extension-loaded-p protocol :skills)
        "print advertises skills to the model")
    (is (ext:extension-loaded-p protocol :context-files)
        "print loads project context files such as AGENTS.md")
    (is (ext:extension-loaded-p protocol :observability)
        "print sinks the event stream for automated runs")
    (is (ext:extension-loaded-p protocol :agent-session))
    (is (not (ext:extension-loaded-p protocol :tui-app))
        "print stays headless -- no terminal UI")
    (is (not (app:profile-interactive-p context)))))

(test main-installs-the-autonomous-profile-with-the-full-agent-set
  "A deployed agent carries the full non-UI agent set yet stays headless."
  (let* ((context (app:main :profile :autonomous))
         (protocol (kli:active-protocol context)))
    (is (ext:extension-loaded-p protocol :settings-wiring)
        "a deployed agent must select the configured default model")
    (is (ext:extension-loaded-p protocol :skills))
    (is (ext:extension-loaded-p protocol :context-files))
    (is (ext:extension-loaded-p protocol :observability))
    (is (ext:extension-loaded-p protocol :agent-session))
    (is (not (ext:extension-loaded-p protocol :tui-app))
        "an autonomous selection installs no terminal UI")
    (is (not (app:profile-interactive-p context)))))

(test build-tui-app-autobinds-a-default-session
  "Boot binds a fresh agent session so the user can chat immediately, without needing an explicit binding command."
  (let* ((context (app:main :profile :interactive-terminal))
         (app (app:build-tui-app context))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service)))
    (is (not (null (gethash (tui-app:tui-app-mode-id app)
                            (agent-session:session-mode-bindings service)))))))

(test select-profile-name-consults-settings
  (let ((settings (com.inuoe.jzon:parse "{\"profile\": \"writing\"}")))
    (is (eq :writing (app:select-profile-name :argv '() :env nil
                                              :settings settings))
        "the profile settings key applies when --profile and KLI_PROFILE are absent")
    (is (eq :headless (app:select-profile-name
                       :argv '("--profile" "headless") :env nil
                       :settings settings))
        "--profile wins over the settings key")
    (is (eq :autonomous (app:select-profile-name
                         :argv '() :env "autonomous"
                         :settings settings))
        "KLI_PROFILE wins over the settings key"))
  (is (eq app:*default-profile*
          (app:select-profile-name
           :argv '() :env nil
           :settings (com.inuoe.jzon:parse "{\"profile\": 7}")))
      "a non-string profile value is ignored")
  (is (eq app:*default-profile*
          (app:select-profile-name
           :argv '() :env nil
           :settings (com.inuoe.jzon:parse "{\"profile\": \"\"}")))
      "an empty profile value is ignored")
  (is (eq app:*default-profile*
          (app:select-profile-name
           :argv '() :env nil
           :settings (make-hash-table :test #'equal)))))

(test record-active-profile-round-trips
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (resolved (profiles:make-resolved-profile
                    :name :x :base :headless :enable '() :disable '()
                    :settings (make-hash-table :test #'equal))))
    (is (null (profiles:protocol-active-profile protocol))
        "no active profile before any was recorded")
    (profiles:record-active-profile protocol resolved)
    (is (eq resolved (profiles:protocol-active-profile protocol)))))

(test main-boots-a-data-profile-through-its-base
  (let* ((settings (com.inuoe.jzon:parse "{
      \"profiles\": {
        \"quiet\": {\"extends\": \"headless\",
                    \"disable\": [\"my-ext\"],
                    \"settings\": {\"theme\": \"dark\"}}}}"))
         (context (app:main :profile :quiet :settings settings))
         (protocol (kli:active-protocol context)))
    (is (ext:extension-loaded-p protocol :agent-session)
        "the base builtin's manifest group installs")
    (is (not (ext:extension-loaded-p protocol :tui-app))
        "a headless-based data profile installs no terminal UI")
    (let ((record (profiles:protocol-profile-activation protocol :headless)))
      (is (not (null record))
          "the activation record is the base builtin's"))
    (let ((active (profiles:protocol-active-profile protocol)))
      (is (profiles:resolved-profile-p active))
      (is (eq :quiet (profiles:resolved-profile-name active)))
      (is (eq :headless (profiles:resolved-profile-base active)))
      (is (equal '(:my-ext) (profiles:resolved-profile-disable active)))
      (is (equal "dark" (gethash "theme"
                                 (profiles:resolved-profile-settings active)))
          "the resolved settings overlay rides on the active record"))))

(test main-falls-back-to-default-on-unknown-profile
  (let* ((context (app:main :profile :no-such-profile
                            :settings (make-hash-table :test #'equal)))
         (protocol (kli:active-protocol context)))
    (is (ext:extension-loaded-p protocol :tui-app)
        "the default interactive profile installs")
    (let ((active (profiles:protocol-active-profile protocol)))
      (is (profiles:resolved-profile-p active))
      (is (eq app:*default-profile* (profiles:resolved-profile-name active))))
    (let ((boot-texts (loop for entry in (app:extension-diagnostics protocol)
                            when (eq :boot (car entry))
                              collect (cdr entry))))
      (is (some (lambda (text) (search "profile" text :test #'char-equal))
                boot-texts)
          "the unknown-profile warning is captured as a :boot diagnostic"))))

(test main-applies-a-data-profile-settings-overlay-at-boot
  "A data profile's settings ride the boot as the third merge layer, so
settings-wiring themes the session before the TUI takes over."
  (let* ((root (temp-config-root))
         (settings (com.inuoe.jzon:parse "{
      \"profiles\": {
        \"sunny\": {\"settings\": {\"theme\": \"light\"}}}}"))
         (config:*global-config-dir* (make-config-test-dir root "global"))
         (config:*project-start-directory* (make-config-test-dir root "proj"))
         (context (app:main :profile :sunny :settings settings))
         (protocol (kli:active-protocol context)))
    (is (equal "light"
               (tui-style:theme-name (tui-style:active-theme protocol)))
        "the profile overlay is live before settings-wiring applies")
    (is (equal "light"
               (config:settings-value
                (config:config-service-settings
                 (config:find-config-service context))
                "theme"))
        "the config service carries the overlay as its third layer")))

(test reuse-boot-snapshot-refreshes-compatible-provider-config
  "A baked boot snapshot can carry compatible-provider loaded with no providers.json;
reuse must reread the runtime file so configured compatible models appear."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (providers-dir (make-config-test-dir root "providers"))
         (providers-path (merge-pathnames "providers.json" providers-dir))
         (settings (make-hash-table :test #'equal))
         (old-groq-key (uiop:getenv "KLI_TEST_GROQ_KEY"))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir)
          (compatible:*providers-path* providers-path))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv app::+profile-env-var+)
               (sb-posix:unsetenv "KLI_TEST_GROQ_KEY")
               (let* ((snapshot (app:main :profile :interactive-terminal
                                          :settings settings))
                      (protocol (kli:active-protocol snapshot))
                      (registry (model-registry snapshot)))
                 (is (ext:extension-loaded-p protocol :compatible-provider))
                 (is (null (models:find-model-provider registry "groq")))
                 (write-config-test-file providers-path *compatible-canned-json*)
                 (setf (uiop:getenv "KLI_TEST_GROQ_KEY") "sk-groq")
                 (setf app::*boot-snapshot-context* snapshot)
                 (let ((reused (app::reuse-boot-snapshot settings)))
                   (is (eq snapshot reused)
                       "snapshot reuse keeps the baked context")
                   (is (not (null (models:find-model-provider registry "groq"))))
                   (is (not (null (models:find-model-definition
                                   registry "groq" "llama-3.3-70b"))))
                   (is (= 1 (count-provider-available
                             "groq" registry (credential-store snapshot) snapshot))))))
          (if old-groq-key
              (setf (uiop:getenv "KLI_TEST_GROQ_KEY") old-groq-key)
              (sb-posix:unsetenv "KLI_TEST_GROQ_KEY"))
          (if old-profile
              (setf (uiop:getenv app::+profile-env-var+) old-profile)
              (sb-posix:unsetenv app::+profile-env-var+)))))))

(test extension-enabled-p-layers-profile-deltas-over-config
  (let ((entry (app::make-user-extension-entry :id :tool :metadata '()))
        (autoload-off (app::make-user-extension-entry
                       :id :quiet :metadata '(:autoload nil)))
        (profile (profiles:make-resolved-profile
                  :name :p :base :headless
                  :enable '(:quiet) :disable '(:tool)
                  :settings (make-hash-table :test #'equal))))
    (is (eq t (app::extension-enabled-p entry '() nil))
        "no profile and no config keeps the default-on behavior")
    (is (null (app::extension-enabled-p entry '() profile))
        "a profile disable wins over default-on")
    (is (null (app::extension-enabled-p entry '(:enabled (:tool)) profile))
        "a profile disable wins over a config enable")
    (is (eq t (app::extension-enabled-p autoload-off '() profile))
        "a profile enable wins over :autoload nil")
    (is (eq t (app::extension-enabled-p autoload-off '(:disabled (:quiet))
                                        profile))
        "a profile enable wins over a config disable")
    (is (null (app::extension-enabled-p autoload-off '() nil))
        ":autoload nil still applies without a profile")))
