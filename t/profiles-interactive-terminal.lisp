(in-package #:kli/tests)
(in-suite all)

(defun interactive-terminal-profile-test-context (&key (deltas '("ack")))
  "Fresh protocol booted via the interactive-terminal profile, plus the
deterministic fake model provider and a bound default mode the TUI submit
path drives."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context profiles:*interactive-terminal-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (bind-agent-session-mode context :deltas deltas)
    (values context protocol)))

(test (interactive-terminal-profile-installs-baseline-and-tui :fixture interactive-authority)
  "The interactive-terminal profile boots the full baseline substrate plus the terminal UI as one composition point, carrying both under a single activation record."
  (multiple-value-bind (context protocol)
      (interactive-terminal-profile-test-context)
    (declare (ignore context))
    (let ((record (profiles:protocol-profile-activation
                   protocol :interactive-terminal)))
      (is (not (null record))
          "installing the interactive-terminal profile records its activation")
      (is (eq :interactive-terminal (profiles:profile-activation-id record))))
    (is (ext:extension-loaded-p protocol :standard-object))
    (is (ext:extension-loaded-p protocol :agent-session))
    (is (ext:extension-loaded-p protocol :model-runtime))
    (is (ext:extension-loaded-p protocol :tui-app))))

(test (interactive-terminal-profile-runs-tui-app-submit-end-to-end :fixture interactive-authority)
  "The end-to-end TUI submit path runs over the same agent-session extension the headless profile drives."
  (multiple-value-bind (context protocol)
      (interactive-terminal-profile-test-context :deltas '("ack"))
    (declare (ignore protocol))
    (let* ((app (tui-app:make-tui-app :context context :columns 32))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (tui-app:tui-app-feed app "hello" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (settle-tui-app app)
      (is (equal '(:user :assistant)
                 (agent-session-message-roles agent)))
      (is (equal '("hello" "ack")
                 (agent-session-message-contents agent))))))
