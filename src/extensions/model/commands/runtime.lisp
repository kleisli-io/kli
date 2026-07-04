(in-package #:kli/model/commands)

(defun option-value-name (value)
  (etypecase value
    (keyword (string-downcase (symbol-name value)))
    (symbol (string-downcase (symbol-name value)))
    (string value)
    (integer (princ-to-string value))
    (real (princ-to-string value))))

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun auth-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :auth
                               :contract :auth/v1))

(defun registry-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :model/registry
                               :contract :model/registry/v1))

(defun command-credential-store (context)
  (or (find-live-object (context-registry context) :credential-store)
      (error "No credential store is loaded.")))

(defun command-model-registry (context)
  (or (find-live-object (context-registry context) :model-registry-service)
      (error "No model registry is loaded.")))

(defun command-result (text &key details error-p)
  (make-command-result
   :content (list (make-command-text-content text))
   :details details
   :error-p error-p))

(defun tui-app-menu-provider (context arguments)
  "The :tui/app provider when this invocation can host a selection menu: a
dispatching app rode in on the arguments and the capability is loaded. NIL
headless, where menu commands degrade to text output."
  (and (getf arguments :app)
       (find-capability-provider (active-protocol context)
                                 :tui/app
                                 :contract :tui/app/v1)))

(defun command-error (text &key details)
  (command-result text :details details :error-p t))

(defun join-lines (lines)
  (format nil "~{~A~^~%~}" lines))

(defun yes-no (value)
  (if value "yes" "no"))

(defun reasoning-effort-name (level)
  (option-value-name (or level :off)))

(defun reasoning-effort-schema (model)
  (model-option-schema-for model "reasoning-effort"))

(defun model-supports-reasoning-effort-p (model)
  (not (null (reasoning-effort-schema model))))

(defun selection-reasoning-effort (selection)
  (and selection
       (model-selection-option-value selection "reasoning-effort")))

(defun reasoning-effort-options (level)
  (when level
    (list :reasoning-effort level)))

(defun reasoning-effort-for-word (word)
  "Return the globally named reasoning-effort keyword WORD denotes, or NIL."
  (and (stringp word)
       (let* ((definition (find-model-option-definition "reasoning-effort"))
              (level (intern (string-upcase word) :keyword)))
         (and (member level (model-option-definition-enum-values definition))
              level))))

(defun parse-reasoning-effort-for-model (model value)
  (let ((schema (reasoning-effort-schema model)))
    (unless schema
      (error "Model ~A does not support thinking." (model-reference model)))
    (parse-model-option-value schema value)))

(defun thinking-values-for-model (model)
  (mapcar #'option-value-name
          (model-option-schema-values (reasoning-effort-schema model))))

(defun normalize-reasoning-effort (model value)
  (let ((schema (reasoning-effort-schema model)))
    (unless schema
      (error "Model ~A does not support thinking." (model-reference model)))
    (handler-case
        (parse-model-option-value schema value)
      (error ()
        (error "Unknown thinking level ~S. Expected one of: ~{~A~^, ~}."
               value
               (mapcar #'option-value-name
                       (model-option-schema-values schema)))))))

(defun current-thinking-model (context &optional arguments)
  (let* ((registry-provider (registry-provider-of context))
         (registry (command-model-registry context))
         (selection (current-display-selection context (or arguments '())))
         (model (selection-model-definition registry registry-provider selection)))
    (values selection model)))

(defun thinking-completion (command tail &optional context)
  "Completion for /thinking from the current model's reasoning-effort schema."
  (declare (ignore command tail))
  (when context
    (multiple-value-bind (selection model)
        (current-thinking-model context)
      (declare (ignore selection))
      (when (and model (model-supports-reasoning-effort-p model))
        (list :candidates (thinking-values-for-model model))))))

(defun parse-model-pattern (tail)
  "Return TAIL as a model pattern. Thinking suffixes are parsed only after a
candidate model is resolved, so colon/slash-laden model ids stay whole."
  (values (trim-whitespace tail) nil))

(defun resolve-model-and-thinking (registry store context tail registry-provider)
  "Resolve TAIL to a model and optional reasoning-effort value. A full candidate
wins first; only if it cannot resolve do a final whitespace token or colon suffix
become a thinking value for the resolved model prefix."
  (let ((text (trim-whitespace tail)))
    (handler-case
        (values (resolve-selectable-model registry store context text registry-provider)
                nil)
      (error (whole-error)
        (labels ((resolved-model (pattern)
                   (handler-case
                       (resolve-selectable-model registry store context pattern
                                                 registry-provider)
                     (error () nil)))
                 (try-split (pattern token)
                   (when (and pattern token (not (blank-string-p pattern)))
                     (let ((model (resolved-model pattern)))
                       (when model
                         (values model
                                 (normalize-reasoning-effort model token)
                                 t)))))
                 (split-result (pattern token)
                   (multiple-value-bind (model reasoning-effort ok)
                       (try-split pattern token)
                     (when ok
                       (list model reasoning-effort)))))
          (let* ((words (split-on-whitespace text))
                 (space-result
                   (and (rest words)
                        (split-result (format nil "~{~A~^ ~}" (butlast words))
                                      (car (last words)))))
                 (colon (position #\: text :from-end t))
                 (colon-result
                   (and (not space-result)
                        colon
                        (split-result (trim-whitespace (subseq text 0 colon))
                                      (trim-whitespace (subseq text (1+ colon)))))))
            (cond
              (space-result
               (values (first space-result) (second space-result)))
              (colon-result
               (values (first colon-result) (second colon-result)))
              (t (error whole-error)))))))))

(defun model-completion (context tail)
  "Completion for /model: available model references."
  (declare (ignore tail))
  (let* ((registry-provider (registry-provider-of context))
         (registry (command-model-registry context))
         (store (command-credential-store context)))
    (list :candidates
          (mapcar #'model-reference
                  (sorted-models
                   (provider-call registry-provider
                                  :available-models
                                  registry
                                  store
                                  context))))))

(defun split-model-reference (pattern)
  (let ((slash (position #\/ pattern)))
    (if slash
        (let ((provider-id (trim-whitespace (subseq pattern 0 slash)))
              (model-id (trim-whitespace (subseq pattern (1+ slash)))))
          (when (or (blank-string-p provider-id)
                    (blank-string-p model-id))
            (error "Model reference must be provider/model."))
          (values provider-id model-id))
        (values nil (trim-whitespace pattern)))))

(defun all-providers (registry)
  (loop for provider being the hash-values
          of (registry-providers registry)
        collect provider))

(defun model-reference (model)
  (format nil "~A/~A"
          (model-definition-provider-id model)
          (model-definition-model-id model)))

(defun sorted-models (models)
  (sort (copy-list models)
        #'string<
        :key #'model-reference))

(defun sorted-providers (providers)
  (sort (copy-list providers)
        #'string<
        :key (lambda (provider)
               (model-provider-provider-id provider))))

(defun current-selection-matches-model-p (selection model)
  (and selection
       (string= (model-selection-provider-id selection)
                (model-definition-provider-id model))
       (string= (model-selection-model-id selection)
                (model-definition-model-id model))))

(defun string-contains-p (needle haystack)
  (search needle haystack :test #'char-equal))

(defun model-matches-search-p (model search)
  (or (blank-string-p search)
      (string-contains-p search (model-reference model))
      (string-contains-p
       search
       (model-definition-provider-id model))
      (string-contains-p
       search
       (model-definition-model-id model))
      (let ((name (model-definition-name model)))
        (and name (string-contains-p search name)))))

(defun filter-models (models search)
  (remove-if-not (lambda (model)
                   (model-matches-search-p model search))
                 models))

(defun format-model-option-presence (model)
  (let ((ids (mapcar #'model-option-schema-option-id
                     (model-definition-option-schemas model))))
    (when ids
      (format nil " options ~{~A~^,~}" ids))))

(defun format-model-line (model selection)
  (format nil "  ~A ~A~@[ - ~A~]~@[~A~]"
          (if (current-selection-matches-model-p selection model)
              "*"
              " ")
          (model-reference model)
          (model-definition-name model)
          (format-model-option-presence model)))

(defun format-model-list (registry store context registry-provider selection
                          &optional search)
  (let* ((models (sorted-models
                  (filter-models (provider-call registry-provider
                                                :available-models
                                                registry
                                                store
                                                context)
                                 (or search "")))))
    (if models
        (join-lines
         (cons "Available models:"
               (mapcar (lambda (model)
                         (format-model-line model selection))
                       models)))
        (if (blank-string-p search)
            "No available models."
            (format nil "No available models match ~S." search)))))

(defun provider-model-count (registry provider-id)
  (loop for model being the hash-values
          of (registry-models registry)
        count (string= provider-id
                       (model-definition-provider-id model))))

(defun provider-auth-status (provider store auth-provider)
  (cond
    ((not (model-provider-auth-required-p provider))
     "local")
    ((provider-call auth-provider
                    :credential-available-p
                    store
                    (model-provider-credential-provider-id provider))
     "yes")
    (t "no")))

(defun format-provider-line (registry store provider auth-provider)
  (let ((provider-id (model-provider-provider-id provider)))
    (format nil "  ~A auth ~A models ~D"
            provider-id
            (provider-auth-status provider store auth-provider)
            (provider-model-count registry provider-id))))

(defun format-provider-list (registry store auth-provider)
  (let ((providers (sorted-providers (all-providers registry))))
    (if providers
        (join-lines
         (cons "Providers:"
               (mapcar (lambda (provider)
                         (format-provider-line registry store provider
                                               auth-provider))
                       providers)))
        "No model providers registered.")))

(defun matching-exact-models (models provider-id model-id)
  (remove-if-not
   (lambda (model)
     (and (or (null provider-id)
              (string= provider-id
                       (model-definition-provider-id model)))
          (or (string= model-id
                       (model-definition-model-id model))
              (string= model-id (model-reference model)))))
   models))

(defun matching-search-models (models provider-id model-id)
  (remove-if-not
   (lambda (model)
     (and (or (null provider-id)
              (string= provider-id
                       (model-definition-provider-id model)))
          (model-matches-search-p model model-id)))
   models))

(defun resolve-selectable-model (registry store context pattern registry-provider)
  (multiple-value-bind (provider-id model-id)
      (split-model-reference pattern)
    (when (blank-string-p model-id)
      (error "Model selection requires a model."))
    (let* ((models (provider-call registry-provider
                                  :available-models
                                  registry
                                  store
                                  context))
           (exact (matching-exact-models models provider-id model-id))
           (matches (if exact
                        exact
                        (matching-search-models models provider-id model-id))))
      (cond
        ((null matches)
         (error "No available model matches ~S." pattern))
        ((rest matches)
         (error "Ambiguous model ~S: ~{~A~^, ~}."
                pattern
                (mapcar #'model-reference (sorted-models matches))))
        (t (first matches))))))

(defun ensure-reasoning-effort-supported (model reasoning-effort)
  (when (and reasoning-effort
             (not (eq reasoning-effort :off))
             (not (model-supports-reasoning-effort-p model)))
    (error "Model ~A does not support thinking."
           (model-reference model)))
  model)

(defun format-selection (selection)
  (if selection
      (format nil "~A/~A thinking ~A"
              (model-selection-provider-id selection)
              (model-selection-model-id selection)
              (reasoning-effort-name
               (selection-reasoning-effort selection)))
      "none"))

(defun selection-model-definition (registry registry-provider selection)
  "The model definition named by SELECTION, or NIL."
  (and selection
       (provider-call registry-provider
                      :find-model-definition
                      registry
                      (model-selection-provider-id selection)
                      (model-selection-model-id selection))))

(defun run-models-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((registry-provider (registry-provider-of context))
         (registry (command-model-registry context))
         (store (command-credential-store context))
         (selection (current-display-selection context arguments))
         (search (trim-whitespace (or (getf arguments :tail) ""))))
    (command-result (format-model-list registry store context
                                       registry-provider selection search))))

(defun run-providers-command (command arguments context &key call-id on-update)
  (declare (ignore command arguments call-id on-update))
  (let* ((auth-provider (auth-provider-of context))
         (registry (command-model-registry context))
         (store (command-credential-store context)))
    (command-result (format-provider-list registry store auth-provider))))

(defun resolve-agent-session-target (context arguments)
  (let ((service (or (find-live-object (context-registry context)
                                       :agent-session-service)
                     (error "No agent-session service is loaded.")))
        (mode-id (or (getf arguments :mode-id) :default-mode)))
    (values service mode-id)))

(defun current-display-selection (context arguments)
  "The selection shown as current and used as the thinking baseline: the active
mode's live agent selection when a session is bound, else the registry default a
new agent would inherit. Reads the agent so /model, /thinking, and the menus
reflect what the mode actually runs, never another mode's last selection."
  (let ((service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (or (and service
             (mode-current-selection service
                                     (or (getf arguments :mode-id) :default-mode)
                                     context))
        (provider-call (registry-provider-of context)
                       :current-model-selection
                       (command-model-registry context)))))

(defun model-menu-rows (registry store context registry-provider selection)
  "Selection-menu rows over the auth-available models, sorted by reference,
with the current selection marked. Row text carries the display name and compact
semantic option presence beside the reference the insert column shows."
  (loop for model in (sorted-models
                      (provider-call registry-provider
                                     :available-models
                                     registry
                                     store
                                     context))
        collect (list :insert (model-reference model)
                      :description
                      (format nil "~A~@[~A~]~@[~A~]"
                              (if (current-selection-matches-model-p
                                   selection model)
                                  "* "
                                  "  ")
                              (model-definition-name model)
                              (format-model-option-presence model))
                      :value model)))

(defun select-model-from-menu (context arguments model)
  "Menu-accept path of /model: select MODEL, carrying the mode's current thinking
level forward when MODEL supports it -- matching the typed path -- and resetting
to off otherwise. Silent like the typed path: the model-change event announces it."
  (multiple-value-bind (service mode-id)
      (resolve-agent-session-target context arguments)
    (let* ((current (mode-current-selection service mode-id context))
           (options (preserve-valid-model-options
                     model
                     (and current (model-selection-options current)))))
      (set-agent-session-model service mode-id
                               (make-model-selection
                                model
                                :options options
                                :metadata '(:source :command))
                               context))))

(defun run-model-command (command arguments context &key call-id on-update)
  "Select the current model. Bare with a TUI app at hand, open a selection
menu over the available models and report nothing -- the menu is the
feedback, and accepting a row selects that model. Bare headless, show the
current selection and the text listing. On a successful change the command
stays silent because the model-change event projects its own \"Model: …\"
system line, so a result message here would double the confirmation."
  (declare (ignore command call-id on-update))
  (let* ((registry-provider (registry-provider-of context))
         (registry (command-model-registry context))
         (store (command-credential-store context))
         (selection (current-display-selection context arguments))
         (tail (trim-whitespace (or (getf arguments :tail) "")))
         (tui (tui-app-menu-provider context arguments)))
    (if (blank-string-p tail)
        (if (and tui
                 (provider-call tui :open-tui-app-menu
                                (getf arguments :app)
                                (model-menu-rows registry store context
                                                 registry-provider selection)
                                (lambda (model)
                                  (select-model-from-menu context arguments
                                                          model))))
            (make-command-result)
            (command-result
                        (join-lines
                         (list (format nil "Current model: ~A"
                                       (format-selection selection))
                               (format-model-list registry store context
                                                  registry-provider selection)))))
        (multiple-value-bind (model reasoning-effort)
            (resolve-model-and-thinking registry store context tail registry-provider)
          (let* ((candidate (make-model-selection
                             model
                             :options (preserve-valid-model-options
                                       model
                                       (and selection
                                            (model-selection-options selection))
                                       (reasoning-effort-options reasoning-effort))
                             :metadata '(:source :command))))
            (multiple-value-bind (service mode-id)
                (resolve-agent-session-target context arguments)
              (let ((selection (set-agent-session-model service mode-id
                                                        candidate context)))
                (command-result
                 ""
                 :details
                 (list :provider
                       (model-selection-provider-id selection)
                       :model
                       (model-selection-model-id selection)
                       :reasoning-effort
                       (selection-reasoning-effort selection))))))))))

(defun thinking-menu-rows (registry registry-provider selection)
  "Selection-menu rows over the selected model's reasoning-effort schema."
  (let* ((model (selection-model-definition registry registry-provider selection))
         (current (or (selection-reasoning-effort selection)
                      :off)))
    (when (and model (model-supports-reasoning-effort-p model))
      (loop for level in (model-option-schema-values (reasoning-effort-schema model))
            collect (list :insert (reasoning-effort-name level)
                          :description (if (eq level current) "* " "  ")
                          :value level)))))

(defun run-thinking-command (command arguments context &key call-id on-update)
  "Set the thinking level for the selected model. Bare with a TUI app at
hand and a thinking-capable model current, open a selection menu over the
levels and report nothing -- the menu is the feedback, and accepting a row
sets that level via the option-change event's system line. Bare
otherwise, show the current level."
  (declare (ignore command call-id on-update))
  (let* ((registry-provider (registry-provider-of context))
         (registry (command-model-registry context))
         (selection (current-display-selection context arguments))
         (tail (trim-whitespace (or (getf arguments :tail) "")))
         (tui (tui-app-menu-provider context arguments)))
    (let ((model (selection-model-definition registry registry-provider selection)))
      (unless model
        (return-from run-thinking-command
          (command-error "No current model is selected.")))
      (unless (model-supports-reasoning-effort-p model)
        (return-from run-thinking-command
          (command-error (format nil "Model ~A does not support thinking."
                                 (model-reference model)))))
      (if (blank-string-p tail)
          (if (and tui
                   (provider-call tui :open-tui-app-menu
                                  (getf arguments :app)
                                  (thinking-menu-rows registry registry-provider
                                                      selection)
                                  (lambda (level)
                                    (multiple-value-bind (service mode-id)
                                        (resolve-agent-session-target context
                                                                      arguments)
                                      (set-agent-session-option service mode-id
                                                                :reasoning-effort level context)))))
              (make-command-result)
              (command-result
               (format nil "Thinking: ~A"
                       (reasoning-effort-name (selection-reasoning-effort selection)))))
          (let ((reasoning-effort (normalize-reasoning-effort model tail)))
            (multiple-value-bind (service mode-id)
                (resolve-agent-session-target context arguments)
              (let ((selection (set-agent-session-option service mode-id
                                                        :reasoning-effort reasoning-effort context)))
                ;; Silent like /model: the option-change event projects its own
                ;; "Thinking: …" line, so echoing here would double the confirmation.
                (command-result
                 ""
                 :details
                 (list :provider
                       (model-selection-provider-id selection)
                       :model
                       (model-selection-model-id selection)
                       :reasoning-effort
                       (selection-reasoning-effort selection))))))))))

(defun format-auth-reference-line (reference)
  (format nil "  ~A ~A~@[ ~A~] available ~A"
          (getf reference :provider-id)
          (string-downcase (symbol-name (getf reference :kind)))
          (getf reference :variable)
          (yes-no (getf reference :available))))

(defun format-auth-state (store auth-provider)
  (let* ((inspection (provider-call auth-provider
                                    :inspect-auth-store
                                    store))
         (providers (getf inspection :providers))
         (references (getf inspection :references)))
    (cond
      ((and (null providers) (null references))
       "Auth: no providers registered.")
      (t
       (join-lines
        (append
         (list "Auth providers:")
         (if providers
             (mapcar (lambda (provider)
                       (format nil "  ~A~@[ - ~A~]"
                               (getf provider :provider-id)
                               (getf provider :display-name)))
                     providers)
             (list "  none"))
         (list "Auth references:")
         (if references
             (mapcar #'format-auth-reference-line references)
             (list "  none"))))))))

(defun register-env-auth (store context provider-id variable auth-provider)
  (unless (provider-call auth-provider :find-auth-provider store provider-id)
    (provider-call auth-provider
                   :register-auth-provider
                   store
                   (provider-call auth-provider
                                  :make-auth-provider
                                  provider-id
                                  :display-name provider-id)
                   context))
  (provider-call auth-provider
                 :register-credential-reference
                 store
                 (provider-call auth-provider
                                :make-env-credential-reference
                                provider-id
                                variable)
                 context))

(defun auth-availability (auth-provider store provider-id)
  (provider-call auth-provider :credential-available-p store provider-id))

(defun auth-usage ()
  (join-lines
   '("Usage:"
     "  /auth env <provider> <ENV_VAR>"
     "  /auth key <provider> <KEY>"
     "  /auth login <provider>"
     "  /auth code <pasted code or redirect URL>"
     "  /auth logout <provider>")))

(defparameter *auth-grammar*
  '(("env" "credential from an environment variable"
     "<provider>" "<ENV_VAR>")
    ("key" "store a static API key" "<provider>" "<KEY>")
    ("login" "start an OAuth login" "<provider>")
    ("code" "complete the pending login" "<pasted code or redirect URL>")
    ("logout" "forget a provider credential" "<provider>"))
  "Per subcommand: name, description, then positional placeholders.")

(defun terminated-word-count (tail)
  "Words in TAIL already sealed by trailing whitespace — the position
being typed."
  (let ((words (split-on-whitespace tail)))
    (cond
      ((null words) 0)
      ((find (char tail (1- (length tail)))
             '(#\Space #\Tab #\Newline #\Return))
       (length words))
      (t (1- (length words))))))

(defun auth-provider-ids (context)
  (let* ((auth-provider (auth-provider-of context))
         (store (command-credential-store context)))
    (loop for provider in (getf (provider-call auth-provider
                                               :inspect-auth-store
                                               store)
                                :providers)
          collect (getf provider :provider-id))))

(defun auth-completion (context tail)
  "Completion for /auth: the subcommand menu at the first word, then each
subcommand's positional placeholders, with registered provider ids
offered where a provider is expected."
  (let* ((words (split-on-whitespace tail))
         (position (terminated-word-count tail))
         (subcommand (and (plusp position) (string-downcase (first words))))
         (grammar (and subcommand
                       (assoc subcommand *auth-grammar* :test #'string=))))
    (cond
      ((zerop position)
       (list :candidates (loop for (name description) in *auth-grammar*
                               collect (cons name description))
             :hint (format nil "~{~A~^ | ~}" (mapcar #'first *auth-grammar*))))
      ((or (null grammar) (>= position (1- (length grammar)))) nil)
      (t
       (let ((candidates
               (and (= position 1)
                    (string/= subcommand "code")
                    (loop for id in (auth-provider-ids context)
                          collect (format nil "~A ~A" subcommand id)))))
         (append (and candidates (list :candidates candidates))
                 (list :hint (nth (1+ position) grammar))))))))

(defun run-auth-env (store context auth-provider words)
  (if (= (length words) 3)
      (destructuring-bind (provider-id variable) (rest words)
        (if (or (blank-string-p provider-id) (blank-string-p variable))
            (command-error "Usage: /auth env <provider> <ENV_VAR>.")
            (progn
              (register-env-auth store context provider-id variable auth-provider)
              (command-result
               (format nil "Auth: ~A env ~A available ~A"
                       provider-id variable
                       (yes-no (auth-availability auth-provider store provider-id)))
               :details (list :provider provider-id
                              :kind :env
                              :variable variable
                              :available (auth-availability auth-provider store
                                                            provider-id))))))
      (command-error "Usage: /auth env <provider> <ENV_VAR>.")))

(defun run-auth-key (store context auth-provider words)
  "Register a static credential. The key is never echoed back into the result."
  (if (= (length words) 3)
      (destructuring-bind (provider-id key) (rest words)
        (if (or (blank-string-p provider-id) (blank-string-p key))
            (command-error "Usage: /auth key <provider> <KEY>.")
            (progn
              (provider-call auth-provider :set-static-credential
                             store provider-id key context)
              (command-result
               (format nil "Auth: ~A key set available ~A"
                       provider-id
                       (yes-no (auth-availability auth-provider store provider-id)))
               :details (list :provider provider-id
                              :kind :static
                              :available (auth-availability auth-provider store
                                                            provider-id))))))
      (command-error "Usage: /auth key <provider> <KEY>.")))

(defun run-auth-login (store context auth-provider words)
  (if (= (length words) 2)
      (let ((provider-id (second words)))
        (cond
          ((blank-string-p provider-id)
           (command-error "Usage: /auth login <provider>."))
          ((provider-call auth-provider :restore-oauth-credential
                          store provider-id context)
           (command-result
            (format nil "Auth: ~A login restored available ~A"
                    provider-id
                    (yes-no (auth-availability auth-provider store provider-id)))
            :details (list :provider provider-id :kind :oauth)))
          (t
           (let* ((verifier (pkce-verifier))
                  (state (oauth-state))
                  (url (build-authorize-url (pkce-challenge verifier) state)))
             (setf (pending-login (active-protocol context) provider-id)
                   (list :verifier verifier :state state))
             (command-result
              (join-lines
               (list (format nil "Open this URL to authorize ~A:" provider-id)
                     url
                     "Then run /auth code <pasted code or redirect URL>."))
              :details (list :provider provider-id :kind :oauth))))))
      (command-error "Usage: /auth login <provider>.")))

(defun run-auth-code (store context auth-provider words)
  "Complete an oauth login. The pasted input and parsed code are never echoed
back."
  (let* ((protocol (active-protocol context))
         (table (pending-logins protocol))
         (n (hash-table-count table)))
    (cond
      ((zerop n)
       (command-error "Run /auth login <provider> first."))
      ((> n 1)
       (command-error "More than one /auth login is in flight; finish them one at a time."))
      ((/= (length words) 2)
       (command-error "Usage: /auth code <pasted code or redirect URL>."))
      (t
       (let* ((provider-id (loop for k being the hash-keys of table return k))
              (pending (gethash provider-id table))
              (verifier (getf pending :verifier))
              (expected-state (getf pending :state)))
         (multiple-value-bind (code state)
             (parse-authorization-input (second words))
           (cond
             ((blank-string-p code)
              (command-error "No authorization code found in the pasted input."))
             ((or (null state) (not (string= state expected-state)))
              (command-error "Authorization state mismatch; restart /auth login."))
             (t
              (handler-case
                  (multiple-value-bind (access refresh expires)
                      (token-exchange code verifier)
                    (provider-call auth-provider :store-oauth-credential
                                   store provider-id context
                                   :access access :refresh refresh
                                   :expires expires
                                   :account-id (jwt-account-id access))
                    (clear-pending-login protocol provider-id)
                    (command-result
                     (format nil "Auth: ~A login complete available ~A"
                             provider-id
                             (yes-no (auth-availability auth-provider store
                                                        provider-id)))
                     :details (list :provider provider-id :kind :oauth)))
                ((or token-endpoint-error token-network-error) (e)
                  (command-error
                   (format nil "Login for ~A failed: ~A" provider-id e))))))))))))

(defun run-auth-logout (store context auth-provider words)
  (if (= (length words) 2)
      (let ((provider-id (second words)))
        (if (blank-string-p provider-id)
            (command-error "Usage: /auth logout <provider>.")
            (progn
              (provider-call auth-provider :forget-credential
                             store provider-id context)
              (command-result (format nil "Auth: ~A logged out" provider-id)
                              :details (list :provider provider-id)))))
      (command-error "Usage: /auth logout <provider>.")))

(defun run-auth-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((auth-provider (auth-provider-of context))
         (store (command-credential-store context))
         (words (argument-words arguments))
         (subcommand (and words (string-downcase (first words)))))
    (cond
      ((null words) (command-result (format-auth-state store auth-provider)))
      ((string= subcommand "env") (run-auth-env store context auth-provider words))
      ((string= subcommand "key") (run-auth-key store context auth-provider words))
      ((string= subcommand "login")
       (run-auth-login store context auth-provider words))
      ((string= subcommand "code")
       (run-auth-code store context auth-provider words))
      ((string= subcommand "logout")
       (run-auth-logout store context auth-provider words))
      (t (command-error (auth-usage))))))

(defun make-models-command ()
  (make-command :name :models
                :label "Models"
                :description "List auth-available models."
                :arguments '(:tail :search)
                :runner #'run-models-command
                :metadata '(:model-command models)))

(defun make-providers-command ()
  (make-command :name :providers
                :label "Providers"
                :description "List model providers and auth status."
                :arguments '()
                :runner #'run-providers-command
                :metadata '(:model-command providers)))

(defun make-model-command (context)
  (make-command :name :model
                :label "Model"
                :description "Select the current model."
                :arguments '(:tail :model)
                :runner #'run-model-command
                :completer (lambda (command tail)
                             (declare (ignore command))
                             (model-completion context tail))
                :metadata '(:model-command model)))

(defun make-thinking-command (&optional context)
  (make-command :name :thinking
                :label "Thinking"
                :description "Set thinking level for the selected model."
                :arguments '(:tail :level)
                :runner #'run-thinking-command
                :completer (lambda (command tail)
                             (thinking-completion command tail context))
                :metadata '(:model-command thinking)))

(defun make-auth-command (context)
  (make-command :name :auth
                :label "Auth"
                :description "Inspect or register provider credentials."
                :arguments '(:tail :words)
                :runner #'run-auth-command
                :completer (lambda (command tail)
                             (declare (ignore command))
                             (auth-completion context tail))
                ;; Hidden from the model: the tail can carry raw API keys.
                :metadata (list :model-command 'auth :model-visible nil
                                :usage (auth-usage))))

(defun register-model-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context)))
    (loop for (name command) in
          (list (list :model (make-model-command context))
                (list :models (make-models-command))
                (list :providers (make-providers-command))
                (list :thinking (make-thinking-command context))
                (list :auth (make-auth-command context)))
          collect
          (provider-call commands
                         :register-command
                         context
                         name
                         command
                         :source (contribution-extension contribution)
                         :tier :core))))

(defun unregister-model-commands (protocol contribution context)
  (declare (ignore protocol))
  (dolist (registration (contribution-state contribution))
    (provider-call (commands-provider context)
                   :unregister-command
                   context
                   registration))
  (contribution-state contribution))
