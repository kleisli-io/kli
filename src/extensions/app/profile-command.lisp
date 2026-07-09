(in-package #:kli/app)

(defun current-profile-specs (context)
  "Profile specs parsed from the settings FILE merge, not the overlayed view,
so a profile's own settings overlay cannot mutate the profile registry. Falls
back to loading the settings files when no config service is present. Parse
warnings are muffled here because boot already surfaced them."
  (let ((service (find-config-service context)))
    (handler-bind ((warning #'muffle-warning))
      (parse-profile-specs (if service
                               (config-service-file-settings service)
                               (load-settings))))))

(defun profile-list-text (context)
  "One profile per line, builtins then data profiles, the active one marked."
  (let* ((protocol (active-protocol context))
         (active (protocol-active-profile protocol))
         (active-name (and active (resolved-profile-name active)))
         (data-names (sort (loop for name being the hash-keys
                                   of (current-profile-specs context)
                                 collect name)
                           #'string<)))
    (with-output-to-string (out)
      (format out "Profiles:")
      (dolist (name (append (known-profile-names) data-names))
        (format out "~%~:[ ~;*~] ~(~A~)" (eq name active-name) name)))))

(defun profile-extension-delta (protocol target)
  "User-extension ids to install and retract for TARGET."
  (let ((config (read-user-config))
        (installed '())
        (retracted '()))
    (loop for entry being the hash-values of (available-extensions protocol)
          for id = (user-extension-entry-id entry)
          for wanted = (extension-enabled-p entry config target)
          for present = (and (gethash id (installed-user-handles protocol)) t)
          do (cond ((and wanted (not present))
                    (push id installed))
                   ((and present (not wanted))
                    (push id retracted))))
    (values (sort installed #'string< :key #'princ-to-string)
            (sort retracted #'string< :key #'princ-to-string))))

(defun apply-profile-extension-delta (protocol context target)
  "Re-base the user-extension pool onto TARGET's effective set: install
available extensions TARGET wants that are absent, retract installed ones it
does not. The delta is computed against the installed handles, so manual
/enable and /disable drift re-bases too. Returns (values installed retracted)."
  (multiple-value-bind (installed retracted)
      (profile-extension-delta protocol target)
    (dolist (id installed)
      (install-user-extension protocol
                              (gethash id (available-extensions protocol))
                              context))
    (dolist (id retracted)
      (retract-user-extension protocol id context))
    (values installed retracted)))

(defun switch-profile (context name)
  "Live-switch the session to profile NAME. Returns (values status detail):
:unknown with the normalized name, :same with the active name, :base-change
with the target resolved profile (the builtin base only applies at boot), or
:switched with a plist (:name :installed :retracted) describing the applied
delta. Resolution warnings are muffled so the outcome reads from the status."
  (with-extension-lifecycle-lock
    (let* ((protocol (active-protocol context))
           (current (protocol-active-profile protocol))
           (target (handler-bind ((warning #'muffle-warning))
                     (resolve-profile-spec name (current-profile-specs context)))))
      (cond
        ((null target)
         (values :unknown (normalize-extension-id name)))
        ((and current (eq (resolved-profile-name target)
                          (resolved-profile-name current)))
         (values :same (resolved-profile-name target)))
        ((and current (not (eq (resolved-profile-base target)
                               (resolved-profile-base current))))
         (values :base-change target))
        (t
         (let ((snapshot (snapshot-user-extension-state protocol))
               (previous-profile current)
               (previous-overlay nil)
               (overlay-touched nil))
           (handler-case
               (multiple-value-bind (installed retracted)
                   (apply-profile-extension-delta protocol context target)
                 (let ((settings (resolved-profile-settings target)))
                   (setf previous-overlay
                         (swap-settings-overlay
                          context
                          (and (hash-table-p settings)
                               (plusp (hash-table-count settings))
                               settings))
                         overlay-touched t))
                 (record-active-profile protocol target)
                 (values :switched (list :name (resolved-profile-name target)
                                         :installed installed
                                         :retracted retracted)))
             (error (condition)
               (when overlay-touched
                 (ignore-errors
                   (swap-settings-overlay context previous-overlay)))
               (record-active-profile protocol previous-profile)
               (restore-user-extension-state protocol context snapshot)
               (error condition)))))))))

(defun profile-reply-text (context name)
  "Reply for the /profile command: a NIL NAME lists profiles, otherwise the
switch outcome."
  (if name
      (multiple-value-bind (status detail) (switch-profile context name)
        (ecase status
          (:unknown
           (format nil "Unknown profile: ~(~A~)." detail))
          (:same
           (format nil "~(~A~) is already the active profile." detail))
          (:base-change
           (let ((target-name (resolved-profile-name detail)))
             (format nil "~(~A~) changes the builtin base, which only applies at boot. Restart with --profile ~(~A~)."
                     target-name target-name)))
          (:switched
           (format nil "Switched to ~(~A~).~@[ Installed: ~{~(~A~)~^, ~}.~]~@[ Retracted: ~{~(~A~)~^, ~}.~]"
                   (getf detail :name)
                   (getf detail :installed)
                   (getf detail :retracted)))))
      (profile-list-text context)))

(defun profile-name-argument (arguments)
  "First word of the command tail, from :words when the parser provided them,
else the raw :tail."
  (or (first (getf arguments :words))
      (rest-arg arguments)))

(defun profile-completion (context)
  "Completion candidates for /profile: builtin then data profile names."
  (list :candidates
        (mapcar (lambda (name) (format nil "~(~A~)" name))
                (append (known-profile-names)
                        (sort (loop for name being the hash-keys
                                      of (current-profile-specs context)
                                    collect name)
                              #'string<)))
        :hint "<profile>"))

(defextension profile-commands
  (:requires
   (capability commands :contract commands/v1)
   (capability config :contract config/v1))
  (:provides
   (command "profile"
     :description "List profiles, or live-switch to a named profile."
     :arguments '(:tail :profile)
     :metadata '(:run-as :extension-load)
     :completer (lambda (command tail context)
                  (declare (ignore command tail))
                  (profile-completion context))
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command call-id on-update))
                (reply (profile-reply-text
                        context (profile-name-argument arguments)))))))
