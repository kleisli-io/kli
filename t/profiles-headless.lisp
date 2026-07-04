(in-package #:kli/tests)
(in-suite all)

(defun headless-profile-test-context ()
  "Fresh protocol booted via the headless profile, plus the deterministic
fake model provider the agent-session tests drive."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context profiles:*headless-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (values context protocol)))

(test headless-profile-installs-baseline-without-tui
  "The headless profile boots the full agent, session, model, and tool baseline carrying no TUI extensions."
  (multiple-value-bind (context protocol) (headless-profile-test-context)
    (declare (ignore context))
    (let ((record (profiles:protocol-profile-activation protocol :headless)))
      (is (not (null record))
          "installing the headless profile records its activation")
      (is (eq :headless (profiles:profile-activation-id record))))
    (is (ext:extension-loaded-p protocol :standard-object))
    (is (ext:extension-loaded-p protocol :agent-session))
    (is (ext:extension-loaded-p protocol :model-runtime))
    (is (not (ext:extension-loaded-p protocol :tui-app)))))

(defun print-profile-test-context ()
  "Fresh protocol booted via the print profile."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context profiles:*print-extension-manifest*)
    (values context protocol)))

(test (print-profile-installs-providers-without-tui :fixture interactive-authority)
  "The print profile boots the headless baseline plus the model providers but no
TUI, distinguishing it from :headless (no providers) and :interactive-terminal
(full TUI)."
  (multiple-value-bind (context protocol) (print-profile-test-context)
    (declare (ignore context))
    (let ((record (profiles:protocol-profile-activation protocol :print)))
      (is (not (null record))
          "installing the print profile records its activation")
      (is (eq :print (profiles:profile-activation-id record))))
    (is (ext:extension-loaded-p protocol :agent-session))
    (is (ext:extension-loaded-p protocol :model-runtime))
    (is (ext:extension-loaded-p protocol :anthropic-provider)
        "the print profile loads model providers, unlike :headless")
    (is (not (ext:extension-loaded-p protocol :tui-app))
        "the print profile installs no TUI, unlike :interactive-terminal")))

(test (headless-profile-runs-fake-provider-agent-turn :fixture interactive-authority)
  "A deterministic fake-provider turn runs end-to-end under the headless profile, reusing the same agent-session path the interactive app drives."
  (multiple-value-bind (context protocol) (headless-profile-test-context)
    (declare (ignore protocol))
    (bind-agent-session-mode context :deltas '("headless reply"))
    (let* ((service (agent-session-service context))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent-id (agent-session:mode-binding-agent-id binding))
           (agent (kli:find-live-object (kli:context-registry context)
                                        agent-id)))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "headless hello" context)
      (is (agents:agent-idle-p agent))
      (is (equal '(:user :assistant)
                 (agent-session-message-roles agent)))
      (is (equal '("headless hello" "headless reply")
                 (agent-session-message-contents agent))))))
