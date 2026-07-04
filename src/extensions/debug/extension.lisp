(in-package #:kli/debug)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun fault-injection-target (arguments)
  (or (getf arguments :app)
      (error "No current TUI app is available.")))

(defun force-trampoline-escape (app)
  "Recode the TUI runtime cell with a function that signals on the next
   tick. Replacements do not inherit the default step's handler-case, so
   the signal escapes the trampoline and bubbles to the binary boundary's
   with-fatal-error-handler."
  (recode-tui-app-runtime
   app
   :function (lambda (a s)
               (declare (ignore a s))
               (error 'simple-error
                      :format-control "force-trampoline-escape"))))

(defun run-force-trampoline-escape (command arguments context
                                    &key call-id on-update)
  (declare (ignore command context call-id on-update))
  (force-trampoline-escape (fault-injection-target arguments))
  (make-command-result
   :content (list (make-command-text-content
                   "Trampoline runtime recoded to signal on next tick."))))

(defun register-fault-injection-commands (protocol contribution context)
  "Debug-only commands gated by binary-boundary opt-in. Active only when
KLI_DEBUG_FAULT_INJECTION is set, otherwise the extension is never installed
and its commands are not registered."
  (declare (ignore protocol))
  (let* ((commands (commands-provider context))
         (source (contribution-extension contribution))
         (command (make-command
                   :name :force-trampoline-escape
                   :label "Force trampoline escape"
                   :description
                   "Recode the TUI runtime to signal on the next tick. Lets the binary boundary's fatal-handler be witnessed end-to-end."
                   :runner #'run-force-trampoline-escape)))
    (list
     (provider-call commands
                    :register-command
                    context
                    :force-trampoline-escape
                    command
                    :source source
                    :tier :core))))

(defun unregister-fault-injection-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context)))
    (dolist (registration (contribution-state contribution))
      (provider-call commands
                     :unregister-command
                     context
                     registration))))

(defextension fault-injection
  (:requires
   (capability commands :contract commands/v1)
   (capability tui/app :contract tui/app/v1))
  (:provides
   (capability debug/force-fault)
   (effect fault-injection-commands
     #'register-fault-injection-commands
     #'unregister-fault-injection-commands)))
