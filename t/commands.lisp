(in-package #:kli/tests)

(defun run-test-command (command arguments context &key call-id on-update)
  (declare (ignore command context call-id on-update))
  (commands:make-command-result
   :content (list (commands:make-command-text-content
                   (or (getf arguments :message) "")))))

(defun register-provider-command (protocol contribution context)
  (let* ((provider (ext:require-capability-provider protocol
                                                    :commands
                                                    :contract :commands/v1))
         (command (commands:make-command
                   :name :provider-echo
                   :label "Provider Echo"
                   :description "Echo a message."
                   :runner #'run-test-command)))
    (ext:provider-call provider
                       :register-command
                       context
                       :provider-echo
                       command
                       :source (ext:contribution-extension contribution))))

(defun unregister-provider-command (protocol contribution context)
  (declare (ignore protocol))
  (let ((provider (ext:require-capability-provider (kli:active-protocol context)
                                                  :commands
                                                  :contract :commands/v1)))
    (ext:provider-call provider
                       :unregister-command
                       context
                       (ext:contribution-state contribution))))

(ext:defextension provider-command-extension
  (:requires
   (capability commands :contract commands/v1))
  (:provides
   (effect provider-command
     #'register-provider-command
     #'unregister-provider-command)))

(test commands-extension-provides-command-registrar
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context commands:*commands-extension-manifest*)
    (let* ((provider (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (command (commands:make-command :name :echo
                                           :runner #'run-test-command))
           (registration (ext:provider-call provider
                                            :register-command
                                            context
                                            :echo
                                            command
                                            :source :test))
           (result (ext:provider-call provider
                                      :invoke-command
                                      :echo
                                      '(:message "hello")
                                      context)))
      (is (typep registration 'commands:command-registration))
      (is (eq command
              (ext:provider-call provider :find-command :echo)))
      (is (eq command
              (ext:provider-call provider :find-command "/echo")))
      (is (not (commands:command-result-error-p result)))
      (is (string= "hello"
                   (getf (first (commands:command-result-content result))
                         :text))))))

(test command-unregister-restores-previous-provider
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context commands:*commands-extension-manifest*)
    (let* ((provider (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (first-command (commands:make-command
                           :name :echo
                           :runner
                           (lambda (command arguments context
                                    &key call-id on-update)
                             (declare (ignore command arguments context
                                              call-id on-update))
                             "first")))
           (second-command (commands:make-command
                            :name :echo
                            :runner
                            (lambda (command arguments context
                                     &key call-id on-update)
                              (declare (ignore command arguments context
                                               call-id on-update))
                              "second")))
           (first-registration (ext:provider-call provider
                                                  :register-command
                                                  context
                                                  :echo
                                                  first-command))
           (second-registration (ext:provider-call provider
                                                   :register-command
                                                   context
                                                   :echo
                                                   second-command)))
      (declare (ignore first-registration))
      (is (eq second-command
              (ext:provider-call provider :find-command :echo)))
      (ext:provider-call provider
                         :unregister-command
                         context
                         second-registration)
      (is (eq first-command
              (ext:provider-call provider :find-command :echo))))))

(test (command-registration-can-be-owned-by-an-extension-effect :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        *provider-command-extension-extension-manifest*)
    (let* ((provider (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (result (ext:provider-call provider
                                      :invoke-command
                                      :provider-echo
                                      '(:message "effect")
                                      context)))
      (is (string= "effect"
                   (getf (first (commands:command-result-content result))
                         :text)))
      (ext:deactivate-extension
       protocol
       (kli:find-live-object (kli:context-registry context)
                             :provider-command-extension)
       context)
      (is (null (ext:provider-call provider
                                   :find-command
                                   :provider-echo))))))

(test command-result-event-payload-carries-invocation-and-visibility
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        commands:*commands-extension-manifest*)
    (let ((provider (ext:require-capability-provider protocol
                                                     :commands
                                                     :contract :commands/v1))
          (payloads '()))
      (ext:install-contribution
       protocol
       (event:make-event-handler-contribution
        :name :capture-command-results
        :event-type :command/result
        :handler (lambda (event context)
                   (declare (ignore context))
                   (push (event:event-payload event) payloads))
        :source :test)
       context)
      (ext:provider-call provider
                         :register-command
                         context
                         :echo
                         (commands:make-command :name :echo
                                                :runner #'run-test-command)
                         :source :test)
      (ext:provider-call provider
                         :register-command
                         context
                         :mute-echo
                         (commands:make-command :name :mute-echo
                                                :runner #'run-test-command
                                                :metadata '(:model-visible nil))
                         :source :test)
      (ext:provider-call provider
                         :invoke-command
                         :echo
                         '(:message "hi" :tail "hi")
                         context)
      (ext:provider-call provider
                         :invoke-command
                         :mute-echo
                         '(:message "quiet")
                         context)
      (let ((visible (second payloads))
            (muted (first payloads)))
        (is (string= "hi" (getf visible :tail)))
        (is (getf visible :model-visible))
        (is (string= "hi" (getf (first (getf visible :content)) :text)))
        (is (null (getf visible :error-p)))
        (is (null (getf muted :model-visible)))
        (is (null (getf muted :tail)))))))

(ext:defextension visibility-clause-probe
  (:requires
   (capability commands :contract commands/v1))
  (:provides
   (command "vis-probe"
     :description "Metadata clause probe."
     :metadata '(:model-visible t)
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                "probed"))))

(test command-clause-carries-metadata-to-registered-command
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        *visibility-clause-probe-extension-manifest*)
    (let* ((provider (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (command (ext:provider-call provider :find-command "vis-probe")))
      (is (getf (commands:command-metadata command) :model-visible)))))

(test built-in-mutating-commands-stay-model-visible
  (dolist (command (list (tools-eval::make-eval-command)
                         (tools-bash::make-bash-command)
                         (model-commands::make-thinking-command)
                         (model-commands::make-model-command
                          (kli:make-kernel-host))))
    (is (getf (commands:command-metadata command) :model-visible t)
        "Command ~A must not opt out of model visibility"
        (commands:command-name command))))

(test command-usage-text-tier-chain
  (is (string= "full grammar"
               (commands:command-usage-text
                (commands:make-command :name :meta
                                       :metadata '(:usage "full grammar")))))
  (is (string= "Usage: /comp <thing>"
               (commands:command-usage-text
                (commands:make-command
                 :name :comp
                 :completer (lambda (command tail)
                              (declare (ignore command tail))
                              '(:hint "<thing>"))))))
  (is (string= "Usage: /spec <alpha> <beta>"
               (commands:command-usage-text
                (commands:make-command :name :spec
                                       :arguments '(:tail :alpha :beta)))))
  (is (string= "Usage: /str <path> [notes...]"
               (commands:command-usage-text
                (commands:make-command :name :str
                                       :arguments "<path> [notes...]"))))
  (is (null (commands:command-usage-text
             (commands:make-command :name :bare)))))

(test command-resolution-precedence-tiers-and-collisions
  "One owner's command outranks its own same-named template without contesting;
   a user template overrides an extension's by precedence; peer extensions tie."
  (let ((service (commands:make-command-service)))
    (flet ((reg (name source tier desc)
             (commands:register-command
              service name
              (commands:make-command :name name :description desc
                                     :runner #'run-test-command)
              :source source :tier tier)))
      (reg :resume :cairn :prompt "prompt resume")
      (reg :resume :cairn :extension "command resume")
      (is (equal "command resume"
                 (commands:command-description
                  (commands:find-command service "resume")))
          "the command outranks its owner's same-named template")
      (is (equal "command resume"
                 (commands:command-description
                  (commands:find-command service "cairn:resume")))
          "a qualified name selects the owner's strongest entry, not the last")
      (is (null (commands:command-collisions service))
          "one owner across two tiers does not contest")
      (reg :plan :docs :prompt "extension plan")
      (reg :plan :user :user-prompt "user plan")
      (is (equal "user plan"
                 (commands:command-description
                  (commands:find-command service "plan")))
          "a user template overrides an extension's by precedence")
      (reg :deploy :alpha :extension "alpha deploy")
      (reg :deploy :beta :extension "beta deploy")
      (is (null (commands:find-command service "deploy"))
          "peer extensions tie with no winner")
      (is (eq :ambiguous
              (nth-value 1 (commands:resolve-command service "deploy")))))))

(test command-alias-binds-tightest-and-reports-shadowed-sources
  "A clone registered at the :alias tier outranks every other source, carries
   the alias name, and still reports shadowed qualified sources."
  (let ((service (commands:make-command-service)))
    (flet ((reg (name source tier desc)
             (commands:register-command
              service name
              (commands:make-command :name name :description desc
                                     :runner #'run-test-command)
              :source source :tier tier))
           (alias (name target)
             (commands:register-command
              service name
              (commands:clone-command (commands:find-command service target)
                                      :name name)
              :source :user :tier :alias)))
      (reg :resume :cairn :extension "command resume")
      (alias "r" "resume")
      (is (equal "command resume"
                 (commands:command-description
                  (commands:find-command service "r")))
          "the alias resolves to the cloned target")
      (is (equal "r" (commands:command-name (commands:find-command service "r")))
          "the clone carries the alias name, not the target's")
      (is (null (commands:command-collisions service))
          "a fresh alias name introduces no contest")
      (reg :deploy :ops :extension "ops deploy")
      (alias "deploy" "deploy")
      (is (equal "ops deploy"
                 (commands:command-description
                  (commands:find-command service "deploy")))
          "the alias at the tightest tier wins over the extension")
      (let ((collision (first (commands:command-collisions service))))
        (is (equal "deploy" (getf collision :name)))
        (is (eq :shadowed (getf collision :status)))
        (is (equal "user:deploy" (getf collision :winner)))
        (is (equal '("user:deploy" "ops:deploy")
                   (getf collision :sources))))
      (is (null (commands:find-command service "no-such-target"))
          "an unresolved target yields nothing to clone"))))

(test argument-words-is-the-shared-command-argument-splitter
  "Both command extensions resolve argument-words to the single
kli/interaction/commands definition; :words wins over :tail, otherwise :tail is
split on whitespace."
  (is (eq 'commands:argument-words
          (find-symbol "ARGUMENT-WORDS" '#:kli/context/commands)))
  (is (eq 'commands:argument-words
          (find-symbol "ARGUMENT-WORDS" '#:kli/model/commands)))
  (is (equal '("a" "b") (commands:argument-words (list :words '("a" "b")))))
  (is (equal '("a" "b" "c")
             (commands:argument-words (list :tail (format nil "a~Cb c" #\Tab)))))
  (is (null (commands:argument-words (list)))))

(test list-commands-yields-precedence-winner-not-push-order-head
  "LIST-COMMANDS gives one command per bare name: its precedence winner, agreeing
with FIND-COMMAND. The registry pushes registrations, so the most recent is the
list head; when a later-loaded source loses on precedence (cairn :extension
after native :core), the winner must still be the listed entry -- it is what
completion shows and shadows against."
  (let ((service (commands:make-command-service)))
    (flet ((reg (source tier description)
             (commands:register-command
              service :resume
              (commands:make-command :name :resume :description description
                                     :runner #'run-test-command)
              :source source :tier tier)))
      (reg :session-commands :core "native resume")
      (reg :cairn :extension "cairn resume")
      (let ((listed (commands:list-commands service)))
        (is (= 1 (length listed)))
        (is (equal "native resume"
                   (commands:command-description (first listed)))
            "the listed command is the precedence winner, not the push-order head")
        (is (equal (commands:command-description
                    (commands:find-command service "resume"))
                   (commands:command-description (first listed)))
            "LIST-COMMANDS and FIND-COMMAND agree on the winner")))))
