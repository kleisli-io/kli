(in-package #:kli/tests)
(in-suite all)

(defun boot-write-prompt-file (dir name string)
  (write-config-test-file (merge-pathnames name dir) string))

(defun boot-skill-md (name description &key body extra)
  (format nil "---~%name: ~A~%description: ~A~%~@[~A~%~]---~%~A"
          name description extra (or body "Skill body.")))

(defun boot-write-skill-file (dir string)
  (write-config-test-file (merge-pathnames "SKILL.md" dir) string))

(defun boot-system-prompt (context &optional (base "You are kli base."))
  (funcall (agent-session:session-context-transform-policy
            (agent-session-service context))
           :system-prompt base))

(defun boot-observability-sink (context)
  (kli:find-live-object (kli:context-registry context) :observability-sink))

(defun boot-warning-containing-p (needle warnings)
  (some (lambda (warning)
          (search needle warning :test #'char-equal))
        warnings))

(defun boot-static-credential-value (store provider-id)
  (let ((reference
          (find-if (lambda (reference)
                     (typep reference 'auth:static-credential-reference))
                   (auth:find-credential-references store provider-id))))
    (and reference (auth:credential-reference-value reference))))

(defun boot-oauth-credential-value (store provider-id)
  (let ((reference
          (find-if (lambda (reference)
                     (typep reference 'auth:oauth-credential-reference))
                   (auth:find-credential-references store provider-id))))
    (values (and reference (auth:credential-reference-value reference))
            (and reference
                 (auth:oauth-credential-reference-account-id reference)))))

(defun boot-live-credential-reference (context provider-id kind)
  (kli:find-live-object (kli:context-registry context)
                        (list :credential-reference provider-id kind)))

(defun boot-live-static-credential-value (context provider-id)
  (let ((reference (boot-live-credential-reference context provider-id :static)))
    (and reference (auth:credential-reference-value reference))))

(defun boot-live-oauth-credential-value (context provider-id)
  (let ((reference (boot-live-credential-reference context provider-id :oauth)))
    (values (and reference (auth:credential-reference-value reference))
            (and reference
                 (auth:oauth-credential-reference-account-id reference)))))

(defun boot-write-provider-credentials (path static-key access refresh account-id)
  (auth:write-credential-record
   "openai" (auth:static-credential-record static-key) path)
  (auth:write-credential-record
   "openai-codex"
   (auth:oauth-credential-record
    :access access :refresh refresh
    :expires (+ (get-universal-time) 3600)
    :account-id account-id)
   path))

(defun boot-assert-store-provider-credentials (context static-key access-token account-id)
  (let ((store (credential-store context)))
    (is (equal static-key (boot-static-credential-value store "openai"))
        "auth store static credential matches")
    (multiple-value-bind (access account)
        (boot-oauth-credential-value store "openai-codex")
      (is (equal access-token access)
          "auth store OAuth access credential matches")
      (is (equal account-id account)
          "auth store OAuth account id matches"))))

(defun boot-assert-live-provider-credentials (context static-key access-token account-id)
  (is (equal static-key (boot-live-static-credential-value context "openai"))
      "live registry static credential matches")
  (multiple-value-bind (access account)
      (boot-live-oauth-credential-value context "openai-codex")
    (is (equal access-token access)
        "live registry OAuth access credential matches")
    (is (equal account-id account)
        "live registry OAuth account id matches")))

(defun boot-assert-persisted-runtime-credentials (context)
  (let ((store (credential-store context)))
    (is (equal "sk-openai-persist"
               (boot-static-credential-value store "openai"))
        "OpenAI static credential restored from runtime credentials.json")
    (is (equal "sk-openai-persist"
               (boot-live-static-credential-value context "openai"))
        "OpenAI static credential live object restored from runtime credentials.json")
    (multiple-value-bind (access account-id)
        (boot-oauth-credential-value store "openai-codex")
      (is (equal "AT-CODEX-PERSIST" access)
          "OpenAI-Codex OAuth access credential restored from runtime credentials.json")
      (is (equal "acct-boot" account-id)
          "OpenAI-Codex OAuth account id restored from runtime credentials.json"))
    (multiple-value-bind (access account-id)
        (boot-live-oauth-credential-value context "openai-codex")
      (is (equal "AT-CODEX-PERSIST" access)
          "OpenAI-Codex OAuth live object restored from runtime credentials.json")
      (is (equal "acct-boot" account-id)
          "OpenAI-Codex OAuth live account id restored from runtime credentials.json"))))

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

(test reuse-boot-snapshot-refreshes-compatible-provider-default-selection-without-warning
  "When runtime providers.json and default model settings arrive after snapshot
capture, reuse must match a full install: select the configured model without
emitting an early unknown-model warning."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (providers-dir (make-config-test-dir root "providers"))
         (providers-path (merge-pathnames "providers.json" providers-dir))
         (empty-settings (make-hash-table :test #'equal))
         (settings-json
           "{\"defaultProvider\": \"groq\", \"defaultModel\": \"llama-3.3-70b\"}")
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
                                          :settings empty-settings))
                      (registry (model-registry snapshot)))
                 (is (null (models:find-model-provider registry "groq")))
                 (write-config-test-file providers-path *compatible-canned-json*)
                 (write-config-test-file (merge-pathnames "settings.json" global-dir)
                                         settings-json)
                 (setf (uiop:getenv "KLI_TEST_GROQ_KEY") "sk-groq")
                 (let* ((full-install
                          (app:main :profile :interactive-terminal
                                    :settings empty-settings))
                        (full-selection
                          (models:current-model-selection
                           (model-registry full-install))))
                   (is (not (null full-selection))
                       "full install control selects a configured model")
                   (when full-selection
                     (is (equal "groq"
                                (models:model-selection-provider-id full-selection))
                         "full install control selects the configured provider")
                     (is (equal "llama-3.3-70b"
                                (models:model-selection-model-id full-selection))
                         "full install control selects the configured model")))
                 (setf app::*boot-snapshot-context* snapshot)
                 (let ((warnings '()))
                   (handler-bind ((warning
                                    (lambda (condition)
                                      (push (princ-to-string condition) warnings)
                                      (muffle-warning condition))))
                     (let ((reused (app::reuse-boot-snapshot empty-settings)))
                       (is (eq snapshot reused))))
                   (setf warnings (nreverse warnings))
                   (is (not (boot-warning-containing-p "Unknown model" warnings))
                       "snapshot reuse must not apply settings before runtime providers refresh")
                   (let ((selection (models:current-model-selection registry)))
                     (is (not (null selection))
                         "snapshot reuse selects a default model after provider refresh")
                     (when selection
                       (is (equal "groq"
                                  (models:model-selection-provider-id selection)))
                       (is (equal "llama-3.3-70b"
                                  (models:model-selection-model-id selection))))))))
          (if old-groq-key
              (setf (uiop:getenv "KLI_TEST_GROQ_KEY") old-groq-key)
              (sb-posix:unsetenv "KLI_TEST_GROQ_KEY"))
          (if old-profile
              (setf (uiop:getenv app::+profile-env-var+) old-profile)
              (sb-posix:unsetenv app::+profile-env-var+)))))))

(test (reuse-boot-snapshot-restores-runtime-persisted-provider-credentials
       :fixture interactive-authority)
  "Provider-owned credential restoration must be equivalent under snapshot reuse
and full install when runtime credentials.json appears after snapshot capture."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv app::+profile-env-var+)
               (let ((snapshot (app:main :profile :interactive-terminal
                                         :settings settings)))
                 (is (null (boot-static-credential-value
                            (credential-store snapshot) "openai"))
                     "precondition: baked snapshot has no persisted OpenAI static ref")
                 (boot-write-provider-credentials
                  cpath "sk-openai-persist" "AT-CODEX-PERSIST" "RT-CODEX-PERSIST"
                  "acct-boot")
                 (let ((full-install
                         (app:main :profile :interactive-terminal
                                   :settings settings)))
                   (boot-assert-persisted-runtime-credentials full-install))
                 (setf app::*boot-snapshot-context* snapshot)
                 (let ((reused (app::reuse-boot-snapshot settings)))
                   (is (eq snapshot reused)
                       "snapshot reuse keeps the baked context")
                   (boot-assert-persisted-runtime-credentials reused))))
          (if old-profile
              (setf (uiop:getenv app::+profile-env-var+) old-profile)
              (sb-posix:unsetenv app::+profile-env-var+)))))))

(test (reuse-boot-snapshot-replaces-changed-runtime-persisted-provider-credentials
       :fixture interactive-authority)
  "A second in-process snapshot reuse must replace stale persisted credential
references in both the auth store and the live registry."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv app::+profile-env-var+)
               (let ((snapshot (app:main :profile :interactive-terminal
                                         :settings settings)))
                 (setf app::*boot-snapshot-context* snapshot)
                 (boot-write-provider-credentials
                  cpath "sk-openai-A" "AT-CODEX-A" "RT-CODEX-A" "acct-A")
                 (let ((reused-a (app::reuse-boot-snapshot settings)))
                   (is (eq snapshot reused-a))
                   (boot-assert-store-provider-credentials
                    reused-a "sk-openai-A" "AT-CODEX-A" "acct-A")
                   (boot-assert-live-provider-credentials
                    reused-a "sk-openai-A" "AT-CODEX-A" "acct-A"))
                 (boot-write-provider-credentials
                  cpath "sk-openai-B" "AT-CODEX-B" "RT-CODEX-B" "acct-B")
                 (let ((reused-b (app::reuse-boot-snapshot settings)))
                   (is (eq snapshot reused-b))
                   (boot-assert-store-provider-credentials
                    reused-b "sk-openai-B" "AT-CODEX-B" "acct-B")
                   (boot-assert-live-provider-credentials
                    reused-b "sk-openai-B" "AT-CODEX-B" "acct-B"))))
          (if old-profile
              (setf (uiop:getenv app::+profile-env-var+) old-profile)
              (sb-posix:unsetenv app::+profile-env-var+)))))))

(test reuse-boot-snapshot-refreshes-runtime-context-files
  "Context files added under runtime config/project roots after snapshot capture
must affect SYSTEM.md, APPEND_SYSTEM.md, and AGENTS.md prompt layers like a full
install, without duplicate append/context sections."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (project-config-dir (make-config-test-dir project-dir ".kli"))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv app::+profile-env-var+)
               (let ((snapshot (app:main :profile :interactive-terminal
                                         :settings settings)))
                 (is (not (search "Runtime system" (boot-system-prompt snapshot))))
                 (make-config-test-dir project-dir ".git")
                 (write-config-test-file (merge-pathnames "SYSTEM.md" project-config-dir)
                                         "Runtime system.")
                 (write-config-test-file (merge-pathnames "APPEND_SYSTEM.md" project-config-dir)
                                         "Runtime append.")
                 (write-config-test-file (merge-pathnames "AGENTS.md" project-dir)
                                         "Runtime agents.")
                 (let* ((full-install (app:main :profile :interactive-terminal
                                                :settings settings))
                        (full-prompt (boot-system-prompt full-install)))
                   (is (eql 0 (search "Runtime system." full-prompt))
                       "full install control applies SYSTEM.md replacement")
                   (is (not (search "You are kli base." full-prompt))
                       "full install control drops the base prompt")
                   (is (search "Runtime append." full-prompt)
                       "full install control applies APPEND_SYSTEM.md")
                   (is (search "Runtime agents." full-prompt)
                       "full install control applies AGENTS.md project context")
                   (setf app::*boot-snapshot-context* snapshot)
                   (let* ((reused (app::reuse-boot-snapshot settings))
                          (prompt (boot-system-prompt reused)))
                     (is (eq snapshot reused))
                     (is (equal full-prompt prompt)
                         "snapshot reuse context-files prompt matches full install")
                     (is (= 1 (count-substring "Runtime append." prompt))
                         "APPEND_SYSTEM.md layer is not duplicated")
                     (is (= 1 (count-substring "# Project Context" prompt))
                         "project context layer is not duplicated")))))
          (if old-profile
              (setf (uiop:getenv app::+profile-env-var+) old-profile)
              (sb-posix:unsetenv app::+profile-env-var+)))))))

(test reuse-boot-snapshot-refreshes-runtime-observability-settings
  "Observability path and event-filter settings loaded after snapshot capture
must update the existing sink on reuse, matching a full install."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (sink-path (merge-pathnames "runtime-events.jsonl" root))
         (settings-json
           (format nil "{\"observability\":{\"path\":~S,\"events\":[\"agent/\",\"tool/\"]}}"
                   (namestring sink-path)))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv app::+profile-env-var+)
               (let* ((snapshot (app:main :profile :interactive-terminal
                                          :settings settings))
                      (snapshot-sink (boot-observability-sink snapshot)))
                 (is (not (null snapshot-sink))
                     "precondition: interactive profile installs observability")
                 (is (null (observability:sink-path snapshot-sink))
                     "precondition: baked snapshot has no observability path")
                 (is (null (observability::sink-filter snapshot-sink))
                     "precondition: baked snapshot has no observability filter")
                 (write-config-test-file (merge-pathnames "settings.json" global-dir)
                                         settings-json)
                 (let* ((full-install
                          (app:main :profile :interactive-terminal
                                    :settings settings))
                        (full-sink (boot-observability-sink full-install)))
                   (is (equal (namestring sink-path)
                              (namestring (observability:sink-path full-sink)))
                       "full install control applies observability.path")
                   (is (equal '("agent/" "tool/")
                              (observability::sink-filter full-sink))
                       "full install control applies observability.events")
                   (setf app::*boot-snapshot-context* snapshot)
                   (let* ((reused (app::reuse-boot-snapshot settings))
                          (reused-sink (boot-observability-sink reused)))
                     (is (eq snapshot reused)
                         "snapshot reuse keeps the baked context")
                     (is (eq snapshot-sink reused-sink)
                         "snapshot reuse mutates the existing sink")
                     (is (equal (namestring (observability:sink-path full-sink))
                                (and (observability:sink-path reused-sink)
                                     (namestring (observability:sink-path reused-sink))))
                         "snapshot reuse observability.path matches full install")
                     (is (equal (observability::sink-filter full-sink)
                                (observability::sink-filter reused-sink))
                         "snapshot reuse observability.events matches full install")))))
          (if old-profile
              (setf (uiop:getenv app::+profile-env-var+) old-profile)
              (sb-posix:unsetenv app::+profile-env-var+)))))))

(test reuse-boot-snapshot-refreshes-output-spill-startup-sweep
  "Output-spill's crash-backstop startup sweep must also run when a boot
snapshot is reused, without reaping the current protocol run or clearing live handles."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (full-spill-root (merge-pathnames "full-spill/" root))
         (reuse-spill-root (merge-pathnames "reuse-spill/" root))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv app::+profile-env-var+)
               (let ((spill:*output-spill-directory* (namestring full-spill-root))
                     (spill:*output-spill-sweep-ttl-seconds* -1))
                 (let* ((base (spill:output-spill-base-directory))
                        (stale (merge-pathnames "stale-full-install/" base)))
                   (spill:ensure-hardened-directory base)
                   (spill:ensure-hardened-directory stale)
                   (write-config-test-file (merge-pathnames "payload" stale) "old")
                   (is (probe-file stale)
                       "precondition: full-install stale spill directory exists")
                   (app:main :profile :interactive-terminal :settings settings)
                   (is (not (probe-file stale))
                       "full install control sweeps stale spill directories")))
               (let ((spill:*output-spill-directory* (namestring reuse-spill-root))
                     (spill:*output-spill-sweep-ttl-seconds* -1))
                 (let* ((snapshot (app:main :profile :interactive-terminal
                                            :settings settings))
                        (protocol (kli:active-protocol snapshot))
                        (live-entry (spill:write-string-spill protocol "live"))
                        (live-token (spill:spill-entry-token live-entry))
                        (live-run (spill:output-spill-run-directory protocol))
                        (base (spill:output-spill-base-directory))
                        (stale (merge-pathnames "stale-reuse/" base)))
                   (spill:ensure-hardened-directory stale)
                   (write-config-test-file (merge-pathnames "payload" stale) "old")
                   (is (probe-file stale)
                       "precondition: snapshot-reuse stale spill directory exists")
                   (is (probe-file live-run)
                       "precondition: current protocol run directory exists")
                   (setf app::*boot-snapshot-context* snapshot)
                   (let ((reused (app::reuse-boot-snapshot settings)))
                     (is (eq snapshot reused)
                         "snapshot reuse keeps the baked context")
                     (is (not (probe-file stale))
                         "snapshot reuse runs the stale spill startup sweep")
                     (is (probe-file live-run)
                         "snapshot reuse does not reap the current protocol run")
                     (is (eq live-entry (spill:find-spill-entry protocol live-token))
                         "snapshot reuse does not clear live spill handles")))))
          (if old-profile
              (setf (uiop:getenv app::+profile-env-var+) old-profile)
              (sb-posix:unsetenv app::+profile-env-var+)))))))

(test reuse-boot-snapshot-refreshes-settings-declaration-diagnostics
  "Settings declaration diagnostics should be recomputed against runtime settings
when a boot snapshot is reused, matching a full install and not accumulating stale diagnostics."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (labels ((setting-diagnostics (context)
               (loop for entry in (app:extension-diagnostics
                                   (kli:active-protocol context))
                     for text = (cdr entry)
                     when (and (eq :boot (car entry))
                               (search "kli settings" text :test #'char-equal))
                       collect text))
             (diagnostic-containing-p (needle diagnostics)
               (some (lambda (text) (search needle text :test #'char-equal))
                     diagnostics)))
      (let ((app::*boot-snapshot-context* nil)
            (app:*current-context* nil)
            (app:*current-app* nil)
            (app:*default-profile* :interactive-terminal)
            (config:*global-config-dir* global-dir)
            (config:*project-start-directory* project-dir))
        (with-temp-credentials (cpath)
          (unwind-protect
               (progn
                 (sb-posix:unsetenv app::+profile-env-var+)
                 (let ((snapshot (app:main :profile :interactive-terminal
                                           :settings settings)))
                   (app::call-with-diagnostics-capture
                    (kli:active-protocol snapshot) :boot
                    (lambda ()
                      (install-extension
                       snapshot *settings-declaration-fixture-extension-manifest*)))
                   (is (null (setting-diagnostics snapshot))
                       "precondition: baked snapshot has no declaration diagnostics")
                   (write-config-test-file
                    (merge-pathnames "settings.json" global-dir)
                    "{\"extensions\":{\"skarlike\":{\"toolResultCap\":0}}}")
                   (let ((full-install (app:main :profile :interactive-terminal
                                                 :settings settings)))
                     (app::call-with-diagnostics-capture
                      (kli:active-protocol full-install) :boot
                      (lambda ()
                        (install-extension
                         full-install *settings-declaration-fixture-extension-manifest*)))
                     (let ((full-diagnostics (setting-diagnostics full-install)))
                       (is (= 1 (length full-diagnostics))
                           "full install control records one settings declaration diagnostic")
                       (is (diagnostic-containing-p "toolResultCap" full-diagnostics))))
                   (setf app::*boot-snapshot-context* snapshot)
                   (let* ((reused (app::boot-context settings))
                          (diagnostics (setting-diagnostics reused)))
                     (is (eq snapshot reused)
                         "snapshot reuse keeps the baked context")
                     (is (= 1 (length diagnostics))
                         "snapshot reuse records one current settings declaration diagnostic")
                     (is (diagnostic-containing-p "toolResultCap" diagnostics))
                     (multiple-value-bind (value source)
                         (config:declared-settings-value reused "skarlike" "toolResultCap")
                       (is (= 0 value)
                           "runtime invalid value is still readable as configured data")
                       (is (eq :settings source)))
                     (multiple-value-bind (value source)
                         (config:declared-settings-value reused "skarlike" "cairn" "granularity")
                       (is (equal "stage" value))
                       (is (eq :default source)
                           "declared default reads still work after refresh"))
                     (let ((again-diagnostics
                             (setting-diagnostics (app::boot-context settings))))
                       (is (= 1 (length again-diagnostics))
                           "a later reuse replaces, rather than duplicates, boot diagnostics")))))
            (if old-profile
                (setf (uiop:getenv app::+profile-env-var+) old-profile)
                (sb-posix:unsetenv app::+profile-env-var+))))))))

(test reuse-boot-snapshot-refreshes-runtime-prompt-commands
  "Prompt templates added under the runtime config roots after snapshot capture
surface when the boot snapshot is reused."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir))
      (unwind-protect
           (progn
             (sb-posix:unsetenv app::+profile-env-var+)
             (let* ((snapshot (app:main :profile :interactive-terminal
                                        :settings settings))
                    (provider (command-provider snapshot)))
               (is (null (ext:provider-call provider :find-command :snap)))
               (boot-write-prompt-file (make-config-test-dir global-dir "prompts")
                                       "snap.md"
                                       (format nil "---~%description: Runtime snap~%---~%Hello $1"))
               (setf app::*boot-snapshot-context* snapshot)
               (let ((reused (app::reuse-boot-snapshot settings)))
                 (is (eq snapshot reused))
                 (let ((command (ext:provider-call provider :find-command :snap)))
                   (is (not (null command)))
                   (is (equal "Runtime snap"
                              (commands:command-description command)))))))
        (if old-profile
            (setf (uiop:getenv app::+profile-env-var+) old-profile)
            (sb-posix:unsetenv app::+profile-env-var+))))))

(test reuse-boot-snapshot-refreshes-runtime-skills
  "Skills added under runtime config roots after snapshot capture register their
commands, advertisement, and sigil expansion when the boot snapshot is reused."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir)
          (skills:*user-agents-skills-directory*
            (make-config-test-dir root "user-agents-absent")))
      (unwind-protect
           (progn
             (sb-posix:unsetenv app::+profile-env-var+)
             (let* ((snapshot (app:main :profile :interactive-terminal
                                        :settings settings))
                    (provider (command-provider snapshot))
                    (service (agent-session-service snapshot)))
               (is (null (ext:provider-call provider :find-command
                                            "skill:grilling")))
               (boot-write-skill-file
                (make-config-test-dir global-dir "skills" "grilling")
                (boot-skill-md "grilling" "Stress-test a plan."
                               :body "Ask hard questions."))
               (boot-write-skill-file
                (make-config-test-dir global-dir "skills" "writing-great-skills")
                (boot-skill-md "writing-great-skills"
                               "Hidden skill."
                               :body "Hidden body."
                               :extra "disable-model-invocation: true"))
               (setf app::*boot-snapshot-context* snapshot)
               (let ((reused (app::reuse-boot-snapshot settings)))
                 (is (eq snapshot reused))
                 (is (not (null (ext:provider-call provider :find-command
                                                   "skill:grilling"))))
                 (multiple-value-bind (expanded images cancelled-p)
                     (funcall (agent-session:session-prompt-expansion-policy
                               service)
                              :expand "$grilling" nil)
                   (declare (ignore images cancelled-p))
                   (is (search "<skill name=\"grilling\"" expanded))
                   (is (search "Ask hard questions." expanded)))
                 (let ((prompt
                         (funcall (agent-session:session-context-transform-policy
                                   service)
                                  :system-prompt "")))
                   (is (search "<name>grilling</name>" prompt))
                   (is (not (search "writing-great-skills" prompt))
                       "disable-model-invocation skills stay out of the model advertisement")))))
        (if old-profile
            (setf (uiop:getenv app::+profile-env-var+) old-profile)
            (sb-posix:unsetenv app::+profile-env-var+))))))

(test boot-context-falls-back-when-runtime-refresh-fails
  "A refresh failure abandons snapshot reuse for this boot and records a
diagnostic on the full profile install."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj"))
         (settings (make-hash-table :test #'equal))
         (old-profile (uiop:getenv app::+profile-env-var+)))
    (let ((app::*boot-snapshot-context* nil)
          (app:*current-context* nil)
          (app:*current-app* nil)
          (app:*default-profile* :interactive-terminal)
          (config:*global-config-dir* global-dir)
          (config:*project-start-directory* project-dir))
      (unwind-protect
           (progn
             (sb-posix:unsetenv app::+profile-env-var+)
             (let* ((snapshot (app:main :profile :interactive-terminal
                                        :settings settings))
                    (protocol (kli:active-protocol snapshot))
                    (boom
                      (ext:make-effect-contribution
                       :name :refresh-boom
                       :installer (lambda (protocol contribution context)
                                    (declare (ignore protocol contribution context))
                                    nil)
                       :retractor (lambda (protocol contribution context)
                                    (declare (ignore protocol contribution context))
                                    nil)
                       :refresh (lambda (protocol contribution context)
                                  (declare (ignore protocol contribution context))
                                  (error "refresh boom")))))
               (ext:install-contribution protocol boom snapshot)
               (setf app::*boot-snapshot-context* snapshot)
               (let* ((booted (app::boot-context settings))
                      (booted-protocol (kli:active-protocol booted)))
                 (is (not (eq snapshot booted))
                     "fallback uses a freshly installed context")
                 (is (eq booted app:*current-context*))
                 (is (ext:extension-loaded-p booted-protocol :tui-app))
                 (is (some (lambda (entry)
                             (and (eq (car entry) :snapshot-reuse)
                                  (search "refresh boom" (cdr entry))))
                           (app:extension-diagnostics booted-protocol)))
                 (is (null app::*boot-snapshot-context*)
                     "a failed refresh quarantines the poisoned snapshot")
                 (let* ((second (app::boot-context settings))
                        (second-protocol (kli:active-protocol second)))
                   (is (not (eq snapshot second))
                       "a later boot does not retry the poisoned snapshot")
                   (is (not (some (lambda (entry)
                                    (and (eq (car entry) :snapshot-reuse)
                                         (search "refresh boom" (cdr entry))))
                                  (app:extension-diagnostics second-protocol)))
                       "quarantine avoids repeating the same refresh failure")))))
        (if old-profile
            (setf (uiop:getenv app::+profile-env-var+) old-profile)
            (sb-posix:unsetenv app::+profile-env-var+))))))

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
