(in-package #:kli/config/wiring)

(defun config-provider-of (context)
  (require-capability-provider (active-protocol context)
                               :config
                               :contract :config/v1))

(defun settings-string (config context key)
  "Value of KEY when it is a non-empty string, otherwise NIL."
  (let ((value (provider-call config :settings-value context key)))
    (and (stringp value) (plusp (length value)) value)))

(defun copy-override-table (table)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun restore-override-table (table snapshot)
  (clrhash table)
  (maphash (lambda (key value) (setf (gethash key table) value)) snapshot))

(defun apply-theme-setting (protocol config context)
  (let ((name (settings-string config context "theme"))
        (previous-mode (protocol-storage protocol +theme-mode-key+)))
    (cond
      ((or (null name) (string= name "auto"))
       (setf (protocol-storage protocol +theme-mode-key+) :auto)
       (list :previous-mode previous-mode :mode-touched t))
      ((find-theme protocol name)
       (let ((previous (theme-name (active-theme protocol))))
         (set-active-theme protocol name)
         (setf (protocol-storage protocol +theme-mode-key+) :explicit)
         (list :previous previous :previous-mode previous-mode :mode-touched t)))
      (t
       (warn "Unknown theme ~S in settings." name)
       nil))))

(defun apply-keybindings-setting (protocol config context)
  (let ((binds (provider-call config :settings-value context "keybindings")))
    (cond ((hash-table-p binds)
           (let ((snapshot (copy-override-table
                            (protocol-keymap-overrides protocol))))
             (load-keymap protocol binds)
             (list :snapshot snapshot)))
          (binds
           (warn "Settings keybindings must be an object.")
           nil))))

(defun default-model-options (config context model)
  (let ((object (provider-call config :settings-value context "defaultOptions"))
        (options '()))
    (cond ((null object) nil)
          ((hash-table-p object)
           (maphash (lambda (key value)
                      (let ((schema (model-option-schema-for model key)))
                        (cond
                          ((null schema)
                           (warn "Model does not support configured option ~S." key))
                          (t
                           (handler-case
                               (let ((canonical (canonicalize-model-option-value
                                                 schema value)))
                                 (setf options
                                       (append options
                                               (list (model-option-keyword key)
                                                     canonical))))
                             (error (condition)
                               (warn "Invalid value for defaultOptions.~A: ~A"
                                     key condition)))))))
                    object))
          (t
           (warn "Settings defaultOptions must be an object.")))
    options))

(defun apply-default-model-setting (config context)
  (let ((provider-id (settings-string config context "defaultProvider"))
        (model-id (settings-string config context "defaultModel")))
    (when (and provider-id model-id)
      (let ((registry-provider (find-capability-provider
                                (active-protocol context)
                                :model/registry
                                :contract :model/registry/v1))
            (registry (find-live-object (context-registry context)
                                        :model-registry-service)))
        (when (and registry-provider registry)
          (let ((model (provider-call registry-provider
                                      :find-model-definition
                                      registry provider-id model-id)))
            (cond
              (model
               (let ((previous (provider-call registry-provider
                                              :current-model-selection
                                              registry)))
                  (provider-call registry-provider :select-model
                                 registry model context
                                 :options (default-model-options config context model))
                 (list :registry registry :previous previous)))
              (t
               (warn "Unknown model ~A/~A in settings." provider-id model-id)
               nil))))))))

(defun apply-session-dir-setting (config context)
  (let ((value (settings-string config context "sessionDir")))
    (when value
      (let* ((registry (context-registry context))
             (previous (find-live-object registry :session-store)))
        (when previous
          (remove-live-object registry :session-store)
          (register-live-object registry
                                (make-file-session-store
                                 (expand-config-path value :directory t)))
          (list :previous previous))))))

(defun apply-capabilities-setting (config context)
  "Narrow every registered agent's subject by the \"capabilities\" settings
key, snapshotting the prior subjects for revert. A present key meets each
agent's current subject with the key's closed capability set, so it can only
attenuate: an empty array denies every gated tool, and an array broader than
the agent already holds leaves the narrower grant intact. A present-but-malformed
key narrows to its valid subset (to nothing when no entry is valid) -- presence
opts into enforcement, so a bad value tightens rather than silently widening.
Only an absent key is a no-op, leaving every agent's authority intact. The key
confers nothing; it only restricts what is already met against it."
  (let ((service (find-live-object (context-registry context)
                                   :agent-loop-service)))
    (when service
      (multiple-value-bind (subject present)
          (capabilities-subject
           (provider-call config :settings-value context "capabilities"))
        (let ((snapshot '()))
          (when present
            (maphash (lambda (id agent)
                       (declare (ignore id))
                       (push (cons agent (agent-subject agent)) snapshot)
                       (setf (agent-subject agent)
                             (subject-meet (agent-subject agent) subject)))
                     (agent-loop-service-agents service)))
          (list :snapshot snapshot))))))

(defun revert-capabilities-setting (state)
  (dolist (entry (getf state :snapshot))
    (setf (agent-subject (car entry)) (cdr entry))))

(defun settings-bash-env-allowlist (object)
  "The :env-allowlist list from a \"bash\" settings OBJECT's envAllowlist key: a
vector of strings becomes a list of strings. A present-but-malformed value warns
and is dropped, so the credential scrub (the default with no allowlist) applies."
  (multiple-value-bind (value present) (gethash "envAllowlist" object)
    (cond ((not present) nil)
          ((and (vectorp value) (not (stringp value)) (every #'stringp value))
           (coerce value 'list))
          (t (warn "Settings bash.envAllowlist must be an array of strings.")
             nil))))

(defun settings-bash-default-timeout (object)
  "The :default-timeout from a \"bash\" settings OBJECT, a positive integer, or
NIL when absent or malformed (warned)."
  (multiple-value-bind (value present) (gethash "defaultTimeout" object)
    (cond ((not present) nil)
          ((and (integerp value) (plusp value)) value)
          (t (warn "Settings bash.defaultTimeout must be a positive integer.")
             nil))))

(defun bash-policy-from-settings (object)
  "A bash policy plist (:cwd :env-allowlist :default-timeout) mirroring the
\"bash\" settings OBJECT: each field present when well-formed, absent otherwise,
so the policy declares exactly what settings carry."
  (let ((cwd (gethash "cwd" object))
        (allowlist (settings-bash-env-allowlist object))
        (timeout (settings-bash-default-timeout object))
        (policy '()))
    (when timeout (setf policy (list* :default-timeout timeout policy)))
    (when allowlist (setf policy (list* :env-allowlist allowlist policy)))
    (when (and (stringp cwd) (plusp (length cwd)))
      (setf policy (list* :cwd cwd policy)))
    policy))

(defun settings-boolean-field (object key)
  (multiple-value-bind (value present) (gethash key object)
    (cond ((not present) (values nil nil nil))
          ((typep value 'boolean) (values value t nil))
          (t (values nil t (format nil "Settings compaction.~A must be a boolean." key))))))

(defun settings-ratio-field (object key)
  (multiple-value-bind (value present) (gethash key object)
    (cond ((not present) (values nil nil nil))
          ((and (numberp value) (plusp value)) (values value t nil))
          (t (values nil t (format nil "Settings compaction.~A must be a positive number." key))))))

(defun settings-token-field (object key)
  (multiple-value-bind (value present) (gethash key object)
    (cond ((not present) (values nil nil nil))
          ((and (integerp value) (plusp value)) (values value t nil))
          (t (values nil t (format nil "Settings compaction.~A must be a positive integer." key))))))

(defun apply-compaction-setting (config context)
  (let ((object (provider-call config :settings-value context "compaction"))
        (service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (cond ((null object) nil)
          ((not (hash-table-p object))
           (warn "Settings compaction must be an object.")
           nil)
          (service
           (let* ((previous (funcall (session-compaction-policy service) :inspect))
                  (args '()))
             (flet ((add (key value present warning)
                      (when warning (warn "~A" warning))
                      (when (and present (null warning))
                        (setf args (append args (list key value))))))
               (multiple-value-bind (value present warning)
                   (settings-boolean-field object "enabled")
                 (add :enabled value present warning))
               (multiple-value-bind (value present warning)
                   (settings-ratio-field object "thresholdRatio")
                 (add :threshold-ratio value present warning))
               (multiple-value-bind (value present warning)
                   (settings-token-field object "reserveTokens")
                 (add :keep-recent-tokens value present warning)))
             (when args
               (apply #'recode-compaction-policy service args))
             (list :service service :previous previous))))))

(defun revert-compaction-setting (state)
  (when state
    (let ((service (getf state :service))
          (previous (getf state :previous)))
      (recode-compaction-policy
       service
       :enabled (getf previous :enabled)
       :threshold-ratio (getf previous :threshold-ratio)
       :keep-recent-tokens (getf previous :keep-recent-tokens)
       :summarizer (getf previous :summarizer)))))

(defun apply-bash-policy-setting (protocol config context)
  "Seed the session bash execution context from the \"bash\" settings object
{cwd, envAllowlist, defaultTimeout, backend}. The policy mirrors the object;
backend is seed-only -- it selects the exec provider only when none is yet
chosen, so a live /bash-shell pick is never clobbered. Snapshots the prior
policy for revert. An absent or non-object key is a no-op."
  (let ((object (provider-call config :settings-value context "bash")))
    (when (hash-table-p object)
      (let ((previous (bash-policy protocol)))
        (setf (bash-policy protocol) (bash-policy-from-settings object))
        (let ((backend (gethash "backend" object)))
          (when (stringp backend)
            (let ((id (resolve-bash-exec-provider-id backend)))
              (if id
                  (seed-bash-exec-provider-id protocol id)
                  (warn "Unknown bash backend ~S in settings." backend)))))
        (list :previous previous :touched t)))))

(defun revert-bash-policy-setting (protocol state)
  "Restore the bash policy snapshotted at apply. The seeded backend is sticky:
settings cannot tell its own seed from a later live /bash-shell pick, so revert
never yanks the backend selection."
  (when (getf state :touched)
    (setf (bash-policy protocol) (getf state :previous))))

(defun installed-settings-wiring (protocol)
  (find-if (lambda (contribution)
             (and (eq (contribution-kind contribution) :effect)
                  (eq (contribution-name contribution) :settings-wiring)))
           (protocol-installed-contributions protocol)))

(defun swap-settings-overlay (context overlay)
  "Swap the settings overlay on the config service, reverting the standing
settings application first and re-applying it over the new merge, so
theme/keybindings/model/session-dir all follow the overlay. Without the
wiring extension installed the overlay layer still swaps. Returns the
previous overlay."
  (let* ((protocol (active-protocol context))
         (service (find-config-service context))
         (contribution (installed-settings-wiring protocol)))
    (when contribution
      (revert-settings protocol contribution context))
    (let ((previous (set-settings-overlay service overlay)))
      (when contribution
        (setf (contribution-state contribution)
              (apply-settings protocol contribution context)))
      previous)))

(defun refresh-settings (protocol contribution context)
  "Refresh settings-wiring after boot snapshot reuse.

The boot path has already rebound runtime config files and installed the profile
overlay as data. This hook is the ordered settings consumer: it reverts the old
settings effects and reapplies them over the fresh merged settings, after earlier
provider/resource refresh hooks have rebuilt their runtime-derived state."
  (revert-settings protocol contribution context)
  (setf (contribution-state contribution)
        (apply-settings protocol contribution context))
  contribution)

(defun apply-settings (protocol contribution context)
  (declare (ignore contribution))
  (let ((config (config-provider-of context)))
    (list :theme (apply-theme-setting protocol config context)
          :keybindings (apply-keybindings-setting protocol config context)
          :model (apply-default-model-setting config context)
          :session-dir (apply-session-dir-setting config context)
          :capabilities (apply-capabilities-setting config context)
          :bash (apply-bash-policy-setting protocol config context)
          :compaction (apply-compaction-setting config context))))

(defun revert-settings (protocol contribution context)
  (let ((state (contribution-state contribution)))
    (let ((theme (getf state :theme)))
      (when theme
        (when (getf theme :previous)
          (set-active-theme protocol (getf theme :previous)))
        (when (getf theme :mode-touched)
          (setf (protocol-storage protocol +theme-mode-key+)
                (getf theme :previous-mode)))))
    (let ((keybindings (getf state :keybindings)))
      (when keybindings
        (restore-override-table (protocol-keymap-overrides protocol)
                                (getf keybindings :snapshot))))
    (let ((model (getf state :model)))
      (when model
        (setf (registry-current-selection (getf model :registry))
              (getf model :previous))))
    (let ((session-dir (getf state :session-dir)))
      (when session-dir
        (let ((registry (context-registry context)))
          (remove-live-object registry :session-store)
          (register-live-object registry (getf session-dir :previous)))))
    (revert-capabilities-setting (getf state :capabilities))
    (revert-bash-policy-setting protocol (getf state :bash))
    (revert-compaction-setting (getf state :compaction))))
