(in-package #:kli/skills)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun config-provider (context)
  (require-capability-provider (active-protocol context)
                               :config
                               :contract :config/v1))

(defun skill-invocation-text (skill tail)
  "Pi's expansion of a skill command: the SKILL.md body read fresh, wrapped
in a skill block with the base directory called out, TAIL appended after."
  (let ((arguments (string-trim '(#\Space #\Tab) (or tail ""))))
    (skill-block skill :tail (when (plusp (length arguments)) arguments))))

(defun undelivered-skill-result (events)
  (make-command-result
   :content (mapcar (lambda (event)
                      (make-command-text-content
                       (kli/tui/transcript:event-text event)))
                    events)
   :error-p t))

(defun make-skill-runner (skill)
  "Runner expanding SKILL and routing the expansion as user input. With an
app the block goes through the app's submit path so the transcript keeps
the typed command line while the model sees the skill content. Without an
app the block itself is the result. A read failure is an error result."
  (lambda (command arguments context &key call-id on-update)
    (declare (ignore command call-id on-update))
    (handler-case
        (let ((expanded (skill-invocation-text skill
                                               (getf arguments :tail)))
              (app (getf arguments :app)))
          (if app
              (let ((events (provider-call
                             (find-capability-provider (active-protocol context)
                                                       :tui/app
                                                       :contract :tui/app/v1)
                             :submit app expanded)))
                (if events
                    (undelivered-skill-result events)
                    (make-command-result)))
              (make-command-result
               :content (list (make-command-text-content expanded)))))
      (error (condition)
        (make-command-result
         :content (list (make-command-text-content
                         (princ-to-string condition)))
         :error-p t)))))

(defun skill-command-name (skill)
  (format nil "skill:~A" (skill-name skill)))

(defun skill-command (skill)
  (make-command
   :name (skill-command-name skill)
   :description (skill-description skill)
   :runner (make-skill-runner skill)
   :metadata (list :skill-path (skill-path skill))))

(defun builtin-skill-entries (protocol)
  "Builtin skill directories: the core kli skills shipped with the image,
then each loaded extension's declared skill root. Error-tolerant: an
unresolved root contributes nothing rather than signaling."
  (loop for key in (cons "kli/skills"
                         (installed-extension-resource-root-keys protocol :skills))
        for root = (ignore-errors (resource-root key))
        when root collect (list :directory root :root-files-p nil)))

(defun skill-location-entries (protocol config context)
  "Skill directories in pi's precedence order: the project kli skills
directory, agents directories walking up to the repo root, the global kli
skills directory, the user agents directory, then the builtin skills
shipped with the image. Root markdown files load only from the kli-owned
directories. Last position makes every builtin skill shadowable by name
from any user-owned directory."
  (let* ((paths (provider-call config :resource-paths context :skills
                               :existing-only nil))
         (global (first paths))
         (project (second paths)))
    (append
     (when project
       (list (list :directory project :root-files-p t)))
     (loop for dir in (project-ancestors)
           collect (list :directory (merge-pathnames ".agents/skills/" dir)
                         :root-files-p nil))
     (list (list :directory global :root-files-p t))
     (list (list :directory (user-agents-skills-directory)
                 :root-files-p nil))
     (builtin-skill-entries protocol))))

(defun install-skills-advertisement (context skills label)
  "Install the skills advertisement as a system-prompt layer named LABEL.
Returns restoration state, or NIL when no session service is registered."
  (let ((service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (when service
      (let ((advertisement (format-skills-for-prompt skills)))
        (add-system-prompt-layer
         service label
         (lambda () advertisement)
         :kind :append)
        (list :service service :label label)))))

(defun install-skill-expander (context skills)
  "Compose dollar-sigil expansion onto the agent-session prompt-expansion
policy. Returns restoration state, or NIL when no session service is
registered."
  (let ((service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (when service
      (let ((previous (getf (funcall (session-prompt-expansion-policy
                                      service)
                                     :inspect)
                            :skill-expander)))
        (recode-prompt-expansion-policy
         service
         :skill-expander
         (lambda (text)
           (expand-skill-sigils (funcall previous text) skills)))
        (list :service service :previous-fn previous)))))

(defun rebuild-skill-registrations (protocol contribution context)
  "Re-discover skills and (re)install the skill commands, advertisement, and
expander. Returns the :registrations/:advertisement/:expander handles, the same
shape register-skill-commands builds for those keys."
  (let ((commands (commands-provider context))
        (config (config-provider context))
        (source (contribution-extension contribution)))
    (let ((skills (discover-skills
                   (skill-location-entries protocol config context))))
      (list :registrations
            (loop for skill in skills
                  collect (provider-call commands :register-command context
                                         (skill-command-name skill)
                                         (skill-command skill)
                                         :source source))
            :advertisement (install-skills-advertisement
                            context skills (contribution-name contribution))
            :expander (install-skill-expander context skills)))))

(defun drain-skill-registrations (context state)
  "Drain the skill-command, advertisement, and expander handles in STATE. Leaves
the resource-kind and the refresh command registration untouched."
  (let ((commands (commands-provider context)))
    (dolist (registration (getf state :registrations))
      (provider-call commands :unregister-command context registration)))
  (let ((advertisement (getf state :advertisement)))
    (when advertisement
      (remove-system-prompt-layer (getf advertisement :service)
                                  (getf advertisement :label))))
  (let ((expander (getf state :expander)))
    (when expander
      (recode-prompt-expansion-policy
       (getf expander :service)
       :skill-expander (getf expander :previous-fn)))))

(defun make-skills-refresh-runner (protocol contribution)
  "Runner for /skills refresh: drain the current skill registrations, rebuild
from fresh discovery under the same contribution, and recode contribution-state
in place so it stays reversible. The refresh registration and prior-resource-kind
are preserved."
  (lambda (command arguments context &key call-id on-update)
    (declare (ignore command arguments call-id on-update))
    (let ((state (contribution-state contribution)))
      (drain-skill-registrations context state)
      (let ((fresh (rebuild-skill-registrations protocol contribution context)))
        (setf (contribution-state contribution)
              (list :registrations (getf fresh :registrations)
                    :advertisement (getf fresh :advertisement)
                    :expander (getf fresh :expander)
                    :refresh-registration (getf state :refresh-registration)
                    :prior-resource-kind (getf state :prior-resource-kind))))
      (make-command-result
       :content (list (make-command-text-content "Skills refreshed."))))))

(defun refresh-skill-commands (protocol contribution context)
  "Re-discover skills for an already-loaded skills effect."
  (let ((state (contribution-state contribution)))
    (drain-skill-registrations context state)
    (let ((fresh (rebuild-skill-registrations protocol contribution context)))
      (setf (contribution-state contribution)
            (list :registrations (getf fresh :registrations)
                  :advertisement (getf fresh :advertisement)
                  :expander (getf fresh :expander)
                  :refresh-registration (getf state :refresh-registration)
                  :prior-resource-kind (getf state :prior-resource-kind)))))
  contribution)

(defun register-skill-commands (protocol contribution context)
  (let ((commands (commands-provider context))
        (config (config-provider context))
        (source (contribution-extension contribution)))
    (multiple-value-bind (prior present)
        (gethash :skills (config-service-resource-kinds
                          (find-config-service context)))
      (provider-call config :register-resource-kind context :skills "skills/")
      (let* ((skills (discover-skills
                      (skill-location-entries protocol config context)))
             (refresh-registration
               (provider-call commands :register-command context
                              "skills"
                              (make-command
                               :name "skills"
                               :description "Re-discover and re-register skills."
                               :runner (make-skills-refresh-runner protocol
                                                                   contribution))
                              :source source)))
        (list :registrations
              (loop for skill in skills
                    collect (provider-call commands :register-command context
                                           (skill-command-name skill)
                                           (skill-command skill)
                                           :source source))
              :advertisement (install-skills-advertisement
                              context skills (contribution-name contribution))
              :expander (install-skill-expander context skills)
              :refresh-registration refresh-registration
              :prior-resource-kind (if present prior :absent))))))

(defun unregister-skill-commands (protocol contribution context)
  (declare (ignore protocol))
  (let* ((commands (commands-provider context))
         (config (config-provider context))
         (state (contribution-state contribution))
         (prior (getf state :prior-resource-kind)))
    (drain-skill-registrations context state)
    (let ((refresh (getf state :refresh-registration)))
      (when refresh
        (provider-call commands :unregister-command context refresh)))
    (if (eq prior :absent)
        (provider-call config :unregister-resource-kind context :skills)
        (provider-call config :register-resource-kind context :skills prior))))

(defextension skills
  (:requires
   (capability commands :contract commands/v1)
   (capability config :contract config/v1))
  (:provides
   (effect skills
     #'register-skill-commands
     #'unregister-skill-commands
     :refresh #'refresh-skill-commands)))
