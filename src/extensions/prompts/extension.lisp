(in-package #:kli/prompts)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun config-provider (context)
  (require-capability-provider (active-protocol context)
                               :config
                               :contract :config/v1))

(defun expand-prompt-template (template tail)
  "TEMPLATE's body with TAIL parsed and substituted as command arguments."
  (substitute-arguments (prompt-template-body template)
                        (parse-command-arguments tail)
                        :raw tail))

(defun undelivered-prompt-result (events)
  (make-command-result
   :content (mapcar (lambda (event)
                      (make-command-text-content
                       (kli/tui/transcript:event-text event)))
                    events)
   :error-p t))

(defun make-template-runner (template)
  "Runner expanding TEMPLATE and routing the expansion as user input.
With an app the expansion goes through the app's submit path and reaches
the bound agent, so the transcript keeps the typed command line while
the model sees the expanded prompt. Events returned by a submit that
could not reach an agent surface as an error result. Without an app the
expansion itself is the result."
  (lambda (command arguments context &key call-id on-update)
    (declare (ignore command context call-id on-update))
    (let ((expanded (expand-prompt-template template
                                            (or (getf arguments :tail) "")))
          (app (getf arguments :app)))
      (if app
          (let ((events (funcall (kli/tui/app:tui-app-on-submit app)
                                 app expanded)))
            (if events
                (undelivered-prompt-result events)
                (make-command-result)))
          (make-command-result
           :content (list (make-command-text-content expanded)))))))

(defun template-command (template)
  (make-command
   :name (prompt-template-name template)
   :description (prompt-template-description template)
   :arguments (prompt-template-argument-hint template)
   :runner (make-template-runner template)
   ;; Hidden from the model: the expansion already enters the conversation
   ;; as the user message, so a command record would prepend a duplicate
   ;; onto the very submit the runner performs.
   :metadata (list :template-path (prompt-template-path template)
                   :model-visible nil)))

(defvar *builtin-prompt-roots* nil
  "Override for the in-image prompt directories. NIL resolves to the
declared roots of loaded extensions.")

(defun prompt-source-groups (protocol config context)
  "Discovery groups (source tier . directories): each extension's root under
that extension at :prompt, so its prompt and command share a source and resolve
by precedence instead of contesting; the user's global and project dirs under
:user at the tighter :user-prompt, so a user template beats an extension's by
name yet yields to any command; an override root under :builtin. An unresolved
extension root contributes nothing."
  (append
   (if *builtin-prompt-roots*
       (list (list* :builtin :prompt *builtin-prompt-roots*))
       (loop for (id . key) in (installed-extension-resource-roots protocol
                                                                   :prompts)
             for root = (ignore-errors (resource-root key))
             when root collect (list* id :prompt (list root))))
   (let ((user-dirs (provider-call config :resource-paths context :prompts)))
     (when user-dirs
       (list (list* :user :user-prompt user-dirs))))))

(defun register-prompt-template-commands (protocol contribution context)
  (declare (ignore contribution))
  (let ((commands (commands-provider context))
        (config (config-provider context)))
    (multiple-value-bind (prior present)
        (gethash :prompts (config-service-resource-kinds
                           (find-config-service context)))
      (provider-call config :register-resource-kind context :prompts "prompts/")
      (list :registrations
            (loop for (source tier . directories)
                    in (prompt-source-groups protocol config context)
                  nconc (loop for template in (discover-prompt-templates
                                               directories)
                              collect (provider-call commands :register-command
                                                     context
                                                     (prompt-template-name
                                                      template)
                                                     (template-command template)
                                                     :source source
                                                     :tier tier)))
            :prior-resource-kind (if present prior :absent)))))

(defun unregister-prompt-template-commands (protocol contribution context)
  (declare (ignore protocol))
  (let* ((commands (commands-provider context))
         (config (config-provider context))
         (state (contribution-state contribution))
         (prior (getf state :prior-resource-kind)))
    (dolist (registration (getf state :registrations))
      (provider-call commands :unregister-command context registration))
    (if (eq prior :absent)
        (provider-call config :unregister-resource-kind context :prompts)
        (provider-call config :register-resource-kind context :prompts prior))))

(defun refresh-prompt-template-commands (context)
  "Re-run the prompt-templates effect against the current extension set, so a
prompt root declared by an extension loaded or retracted after the effect's
install surfaces or withdraws its commands. No-op when the effect is not
installed (headless profiles)."
  (let* ((protocol (active-protocol context))
         (contribution
           (find-if (lambda (candidate)
                      (and (typep candidate 'effect-contribution)
                           (eq (contribution-name candidate)
                               (normalize-extension-id 'prompt-templates))))
                    (protocol-installed-contributions protocol))))
    (when contribution
      (unregister-prompt-template-commands protocol contribution context)
      (setf (contribution-state contribution)
            (register-prompt-template-commands protocol contribution context)))))

(defextension prompt-templates
  (:requires
   (capability commands :contract commands/v1)
   (capability config :contract config/v1))
  (:provides
   (effect prompt-templates
     #'register-prompt-template-commands
     #'unregister-prompt-template-commands)))
