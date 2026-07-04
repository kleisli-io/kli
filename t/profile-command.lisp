(in-package #:kli/tests)
(in-suite all)

(defparameter +profile-command-settings+
  "{\"profiles\": {
      \"quiet\": {\"extends\": \"interactive-terminal\",
                  \"disable\": [\"test-prof-x\"],
                  \"enable\": [\"test-prof-y\"],
                  \"settings\": {\"theme\": \"light\",
                                 \"capabilities\": [\"file/read\"]}},
      \"loud\": {\"extends\": \"quiet\"},
      \"calm\": {\"extends\": \"headless\"}}}")

(ext:defextension test-prof-x)
(ext:defextension test-prof-y)

(defun boot-profile-command-context (&key (profile :interactive-terminal))
  "Boot main against the profile-command fixture settings, install the
/profile command, and seed the user-extension pool: x installed as the boot
pass would, y indexed but autoload-off."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global")))
    (write-config-test-file (merge-pathnames "settings.json" global-dir)
                            +profile-command-settings+)
    (let* ((config:*global-config-dir* global-dir)
           (config:*project-start-directory* (make-config-test-dir root "proj"))
           (context (app:main :profile profile
                              :settings (com.inuoe.jzon:parse
                                         +profile-command-settings+)))
           (protocol (kli:active-protocol context)))
      (install-extension context app::*profile-commands-extension-manifest*)
      (let ((x (app::make-user-extension-entry
                :id :test-prof-x :metadata '()
                :manifest *test-prof-x-extension-manifest*))
            (y (app::make-user-extension-entry
                :id :test-prof-y :metadata '(:autoload nil)
                :manifest *test-prof-y-extension-manifest*)))
        (setf (gethash :test-prof-x (app::available-extensions protocol)) x)
        (setf (gethash :test-prof-y (app::available-extensions protocol)) y)
        (app::install-user-extension protocol x context))
      (values context protocol))))

(test (profile-command-lists-profiles-with-active-marked :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (declare (ignore protocol))
    (let ((text (command-result-text
                 context (invoke-test-command context :profile))))
      (is (search "* interactive-terminal" text)
          "the active builtin carries the marker")
      (is (search "quiet" text))
      (is (search "loud" text))
      (is (search "headless" text))
      (is (not (search "* quiet" text))
          "inactive profiles carry no marker"))))

(test (profile-command-live-switch-applies-delta-overlay-and-record :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (is (ext:extension-loaded-p protocol :test-prof-x))
    (is (not (ext:extension-loaded-p protocol :test-prof-y)))
    (is (equal "dark" (tui-style:theme-name (tui-style:active-theme protocol))))
    (let ((text (command-result-text
                 context (invoke-test-command context :profile
                                              '(:words ("quiet"))))))
      (is (search "Switched to quiet" text)))
    (is (not (ext:extension-loaded-p protocol :test-prof-x))
        "the target profile's disable retracts the installed extension")
    (is (ext:extension-loaded-p protocol :test-prof-y)
        "the target profile's enable installs over :autoload nil")
    (is (equal "light" (tui-style:theme-name (tui-style:active-theme protocol)))
        "the settings overlay swaps and re-applies wiring")
    (is (eq :quiet (profiles:resolved-profile-name
                    (profiles:protocol-active-profile protocol)))
        "the activation record follows the switch")))

(test (profile-command-switch-away-reverts :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (invoke-test-command context :profile '(:words ("quiet")))
    (let ((text (command-result-text
                 context
                 (invoke-test-command context :profile
                                      '(:words ("interactive-terminal"))))))
      (is (search "Switched to interactive-terminal" text)))
    (is (ext:extension-loaded-p protocol :test-prof-x)
        "the default-on extension reinstalls on switch-away")
    (is (not (ext:extension-loaded-p protocol :test-prof-y))
        ":autoload nil retracts again without the profile enable")
    (is (equal "dark" (tui-style:theme-name (tui-style:active-theme protocol)))
        "the overlay clears and the file view re-applies")
    (is (eq :interactive-terminal
            (profiles:resolved-profile-name
             (profiles:protocol-active-profile protocol))))))

(test (profile-command-rejects-base-change :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (let ((text (command-result-text
                 context (invoke-test-command context :profile
                                              '(:words ("calm"))))))
      (is (search "base" text)
          "the rejection names the base boundary")
      (is (search "--profile calm" text)
          "the rejection points at a restart"))
    (is (eq :interactive-terminal
            (profiles:resolved-profile-name
             (profiles:protocol-active-profile protocol)))
        "the activation record is untouched")
    (is (ext:extension-loaded-p protocol :test-prof-x)
        "the extension set is untouched")))

(test (profile-command-completes-profile-names :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (declare (ignore protocol))
    (let* ((provider (command-provider context))
           (command (ext:provider-call provider :find-command "profile"))
           (completion (let ((completer (commands:command-completer command)))
                         (and completer (funcall completer command ""))))
           (names (mapcar (lambda (entry) (if (consp entry) (car entry) entry))
                          (getf completion :candidates))))
      (dolist (name '("interactive-terminal" "headless" "quiet" "loud" "calm"))
        (is (member name names :test #'string=)
            "~A appears in the /profile completion menu" name)))))

(test (profile-capabilities-flow-to-configured-agent-subject :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (declare (ignore protocol))
    (is (null (agent-session::configured-agent-subject context))
        "no capabilities key in the file view, so the key is absent")
    (invoke-test-command context :profile '(:words ("quiet")))
    (let ((subject (agent-session::configured-agent-subject context)))
      (is (not (null subject))
          "the quiet profile's settings overlay carries capabilities")
      (is (ext:check-capability subject :file/read))
      (is (null (ext:check-capability subject :process/exec))))
    (invoke-test-command context :profile '(:words ("interactive-terminal")))
    (is (null (agent-session::configured-agent-subject context))
        "switching away clears the overlay and the key with it")))

(test (root-agent-subject-is-unrestricted-when-capabilities-absent :fixture interactive-authority)
  "With no capabilities key the root-agent fallback is unrestricted -- an ordinary
subject over the lattice top, not the narrow default and not a system-subject --
so a normally booted root agent can use the baseline user-facing tools. Once a
profile supplies capabilities, the root agent boots bounded by them."
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (declare (ignore protocol))
    (let ((subject (agent-session::root-agent-subject context)))
      (is (not (typep subject 'ext:system-subject))
          "the unrestricted fallback is a policy grant, not substrate authority")
      (dolist (cap '(:file/read :cairn/read :process/exec :image/eval))
        (is (ext:check-capability subject cap)
            "the unrestricted root agent confers ~A" cap)))
    (invoke-test-command context :profile '(:words ("quiet")))
    (let ((subject (agent-session::root-agent-subject context)))
      (is (ext:check-capability subject :file/read)
          "a present capabilities key bounds the root agent to its grant")
      (is (null (ext:check-capability subject :process/exec))
          "the bounded root agent withholds capabilities the key omits"))))

(test (profile-switch-restricts-live-agents :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (declare (ignore protocol))
    (let* ((service (kli:find-live-object (kli:context-registry context)
                                          :agent-loop-service))
           (agent (agents:make-agent nil nil nil nil nil :id :test-live-caps
                                     :granted-capabilities
                                     '(:file/read :process/exec))))
      (agents:register-agent service agent context)
      (is (not (typep (agents:agent-subject agent) 'ext:system-subject))
          "a live agent runs bounded, never the lattice top")
      (is (ext:check-capability (agents:agent-subject agent) :process/exec)
          "the live agent starts holding process/exec")
      (invoke-test-command context :profile '(:words ("quiet")))
      (is (ext:check-capability (agents:agent-subject agent) :file/read)
          "a file/read profile meets the live agent, keeping the shared cap")
      (is (null (ext:check-capability (agents:agent-subject agent) :process/exec))
          "the meet drops process/exec the active profile does not grant")
      (invoke-test-command context :profile '(:words ("interactive-terminal")))
      (is (ext:check-capability (agents:agent-subject agent) :process/exec)
          "switching to a profile without a capabilities key reverts the overlay,
restoring the recorded prior authority"))))

(test (profile-command-replies-on-unknown-and-same :fixture interactive-authority)
  (multiple-value-bind (context protocol) (boot-profile-command-context)
    (declare (ignore protocol))
    (is (search "Unknown profile"
                (command-result-text
                 context (invoke-test-command context :profile
                                              '(:words ("nope"))))))
    (is (search "already"
                (command-result-text
                 context
                 (invoke-test-command context :profile
                                      '(:words ("interactive-terminal"))))))))
