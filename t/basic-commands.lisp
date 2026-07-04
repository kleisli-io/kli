(in-package #:kli/tests)

(defun load-basic-command-stack (context)
  (load-tui-app-stack context)
  (install-extensions context basic-commands:*basic-commands-extension-manifest*))

(defun command-provider (context)
  (ext:require-capability-provider (kli:active-protocol context)
                                   :commands
                                   :contract :commands/v1))

(defun invoke-test-command (context name &optional arguments)
  (let ((provider (command-provider context)))
    (ext:provider-call provider
                       :invoke-command
                       name
                       arguments
                       context)))

(defun command-result-text (context result)
  (declare (ignore context))
  (getf (first (commands:command-result-content result))
        :text))

(test basic-commands-registers-default-interactive-commands
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-basic-command-stack context)
    (let* ((provider (command-provider context))
           (names (mapcar #'commands:command-name
                          (ext:provider-call provider :list-commands))))
      (dolist (name '("clear" "commands" "help" "quit" "redraw" "reset"))
        (is (member name names :test #'string=))))))

(test basic-commands-list-and-help-use-command-metadata
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-basic-command-stack context)
    (is (search "/help"
                (command-result-text
                 context
                 (invoke-test-command context :commands))))
    (let ((help (command-result-text
                 context
                 (invoke-test-command context
                                      :help
                                      '(:words ("clear"))))))
      (is (search "/clear - Clear" help))
      (is (search "Clear the terminal display." help)))))

(test basic-commands-help-accepts-displayed-slash-name
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-basic-command-stack context)
    (let ((help (command-result-text
                 context
                 (invoke-test-command context
                                      :help
                                      '(:words ("/clear"))))))
      (is (search "/clear - Clear" help))
      (is (search "Clear the terminal display." help)))))

(test (basic-commands-clear-reset-and-quit-target-current-app :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (app nil))
    (switch-to-extension-protocol context)
    (load-basic-command-stack context)
    (setf app (tui-app:make-tui-app :context context :columns 48)
          app:*current-context* context
          app:*current-app* app)
    (tui-app:tui-app-add-system-event app "ready")
    (is (search "Screen cleared."
                (command-result-text
                 context
                 (invoke-test-command context :clear (list :app app)))))
      (is (null (command-result-text
                 context
                 (invoke-test-command context :reset (list :app app))))
          "/reset is silent: the :session-reset event projects the line")
    (let ((events (tui-app:tui-app-transcript-events app)))
      (is (= 1 (length events)))
      (is (eq :notice (tui-transcript:event-kind (first events))))
      (is (string= "Session reset."
                   (tui-transcript:event-text (first events)))))
    (is (tui-app:tui-app-running-p app))
    (tui-app:tui-app-add-system-event app "visible")
    (tui-terminal:terminal-clear (tui-app:tui-app-terminal app))
    (is (search "Redrawn."
                (command-result-text
                 context
                 (invoke-test-command context :redraw (list :app app)))))
    (let ((output (tui-terminal:terminal-output
                   (tui-app:tui-app-terminal app))))
      (is (search (format nil "~C[2J" #\Esc) output))
      (is (not (search (format nil "~C[3J" #\Esc) output))
          "/redraw preserves the shell's native scrollback")
      (is (search "· visible" output)))
    (is (search "Exiting."
                (command-result-text
                 context
                 (invoke-test-command context :quit (list :app app)))))
    (is (not (tui-app:tui-app-running-p app)))))


(test (basic-commands-unregisters-on-extension-deactivation :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (load-basic-command-stack context)
    (let ((provider (command-provider context))
          (extension (kli:find-live-object (kli:context-registry context)
                                           :basic-commands)))
      (is (ext:provider-call provider :find-command :help))
      (ext:deactivate-extension protocol extension context)
      (is (null (ext:provider-call provider :find-command :help))))))

(test basic-commands-help-renders-usage-from-spec
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-basic-command-stack context)
    (let ((help (command-result-text
                 context
                 (invoke-test-command context
                                      :help
                                      '(:words ("help"))))))
      (is (search "Usage: /help <command>" help)))
    (let ((help (command-result-text
                 context
                 (invoke-test-command context
                                      :help
                                      '(:words ("clear"))))))
      (is (null (search "Usage:" help))))))

(test basic-commands-display-commands-opt-out-of-model-visibility
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-basic-command-stack context)
    (let ((provider (command-provider context)))
      (is (getf (commands:command-metadata
                 (ext:provider-call provider :find-command :reset))
                :model-visible t))
      (is (null (getf (commands:command-metadata
                       (ext:provider-call provider :find-command :redraw))
                      :model-visible t)))
      (is (null (getf (commands:command-metadata
                       (ext:provider-call provider :find-command :clear))
                      :model-visible t))))))
