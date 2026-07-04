(in-package #:kli/interaction/basic-commands)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun tui-app-provider (context)
  (require-capability-provider (active-protocol context)
                               :tui/app
                               :contract :tui/app/v1))

(defun agent-session-service (context)
  (or (find-live-object (context-registry context) :agent-session-service)
      (error "No agent-session service is loaded.")))

(defun command-result (text)
  (make-command-result
   :content (list (make-command-text-content text))))

(defun command-list-names (commands)
  "Listing names: bare for a singly-owned command, every qualified
SOURCE:NAME form when more than one source contests the bare name."
  (let ((contested (loop for entry in (provider-call commands :command-collisions)
                         collect (cons (getf entry :name) (getf entry :sources)))))
    (loop for command in (provider-call commands :list-commands)
          for sources = (cdr (assoc (command-name command) contested
                                    :test #'string=))
          if sources append sources
            else collect (command-name command))))

(defun command-list-text (commands)
  (let ((names (command-list-names commands)))
    (if names
        (format nil "~{/~A~^, ~}" names)
        "No commands registered.")))

(defun find-help-target (commands arguments)
  (let ((words (getf arguments :words)))
    (and words
         (provider-call commands
                        :find-command
                        (first words)))))

(defun help-text-for-command (command)
  (format nil "/~A~@[ - ~A~]~@[~%~A~]~@[~%~A~]"
          (command-name command)
          (command-label command)
          (command-description command)
          (command-usage-text command)))

(defun run-commands-command (command arguments context &key call-id on-update)
  (declare (ignore command arguments call-id on-update))
  (command-result (command-list-text (commands-provider context))))

(defun run-help-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((commands (commands-provider context))
         (target (find-help-target commands arguments)))
    (command-result
     (if target
         (help-text-for-command target)
         (format nil "Commands: ~A~%Use /help <command> for details."
                 (command-list-text commands))))))

(defun app-command-target (arguments)
  (or (getf arguments :app)
      (error "No current TUI app is available.")))

(defun run-clear-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (provider-call (tui-app-provider context)
                 :clear-tui-app-screen
                 (app-command-target arguments))
  (command-result "Screen cleared."))

(defun run-reset-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let ((app (app-command-target arguments)))
    (reset-agent-session (agent-session-service context)
                         (kli/tui/app:tui-app-mode-id app)
                         context)
    ;; The :session-reset event projects the "Session reset." line; echoing it
    ;; here would double the confirmation.
    (make-command-result)))

(defun run-redraw-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (provider-call (tui-app-provider context)
                 :redraw-tui-app
                 (app-command-target arguments))
  (command-result "Redrawn."))

(defun run-quit-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (provider-call (tui-app-provider context)
                 :stop-tui-app
                 (app-command-target arguments))
  (command-result "Exiting."))

(defun make-basic-command (name label description runner
                           &optional arguments metadata)
  (make-command :name name
                :label label
                :description description
                :arguments arguments
                :runner runner
                :metadata metadata))

(defun register-basic-command (commands context source name label description
                               runner &optional arguments metadata)
  (provider-call commands
                 :register-command
                 context
                 name
                 (make-basic-command name label description runner
                                     arguments metadata)
                 :source source
                 :tier :core))

(defun register-basic-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context))
        (source (contribution-extension contribution)))
    (list
     (register-basic-command commands
                             context
                             source
                             :commands
                             "Commands"
                             "List registered commands."
                             #'run-commands-command)
     (register-basic-command commands
                             context
                             source
                             :help
                             "Help"
                             "Show command help."
                             #'run-help-command
                             '(:tail :command))
     (register-basic-command commands
                             context
                             source
                             :clear
                             "Clear"
                             "Clear the terminal display."
                             #'run-clear-command
                             nil
                             '(:model-visible nil))
     (register-basic-command commands
                             context
                             source
                             :reset
                             "Reset"
                             "Start a new conversation."
                             #'run-reset-command)
     (register-basic-command commands
                             context
                             source
                             :redraw
                             "Redraw"
                             "Repaint the terminal display."
                             #'run-redraw-command
                             nil
                             '(:model-visible nil))
     (register-basic-command commands
                             context
                             source
                             :quit
                             "Quit"
                             "Stop the current TUI app."
                             #'run-quit-command))))

(defun unregister-basic-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context)))
    (dolist (registration (contribution-state contribution))
      (provider-call commands
                     :unregister-command
                     context
                     registration))))

(defextension basic-commands
  (:requires
   (capability commands :contract commands/v1)
   (capability tui/app :contract tui/app/v1)
   (capability agent/session :contract agent/session/v1))
  (:provides
   (effect basic-commands
     #'register-basic-commands
     #'unregister-basic-commands)))
