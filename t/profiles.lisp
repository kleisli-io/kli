(in-package #:kli/tests)

(ext:defextension profile-probe-extension
  (:provides
   (profile :probe
     (list 'obj:*standard-object-extension-manifest*
           'event:*events-extension-manifest*))))

(test profile-installs-manifest-group-in-order
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (profile (install-extension
                   context
                   *profile-probe-extension-extension-manifest*)))
    (is (eq :profile-probe-extension (kli:object-id profile)))
    (is (ext:extension-loaded-p protocol :standard-object))
    (is (ext:extension-loaded-p protocol :events))
    (let ((record (profiles:protocol-profile-activation protocol :probe)))
      (is (not (null record))
          "installing a profile records its activation")
      (is (eq :probe (profiles:profile-activation-id record)))
      (is (equal '(:standard-object :events)
                 (mapcar #'kli:object-id
                         (profiles:profile-activation-extensions record)))
          "activation record holds the group handles in install order"))))

(test (profile-retract-reverses-install :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (profile (install-extension
                   context
                   *profile-probe-extension-extension-manifest*)))
    (is (ext:extension-loaded-p protocol :standard-object))
    (ext:deactivate-extension protocol profile context)
    (is (not (ext:extension-loaded-p protocol :standard-object)))
    (is (not (ext:extension-loaded-p protocol :events)))
    (is (not (ext:extension-loaded-p protocol :profile-probe-extension)))
    (is (null (profiles:protocol-profile-activation protocol :probe))
        "retracting the profile clears its activation record")
    (is (null (ext:protocol-installed-contributions protocol))
        "retracting the profile drains every contribution it installed")))

(test (snapshot-restores-profile-built-protocol :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (protocol (switch-to-extension-protocol context))
         (profile (install-extension
                   context
                   *profile-probe-extension-extension-manifest*)))
    (let ((snapshot (snapshot:snapshot-context context)))
      (is (equal '(:profile-probe-extension)
                 (mapcar (lambda (record) (getf record :id))
                         (getf snapshot :extensions)))
          "profile members are not roots, only the profile itself is")
      (is (null (getf snapshot :unrestorable-extensions)))
      (kli:switch-protocol boot :boot-protocol context)
      (ext:deactivate-extension protocol profile context)
      (kli:remove-live-object (kli:context-registry context)
                              (kli:object-id protocol))
      (let ((restored (snapshot:restore-active-protocol context snapshot)))
        (is (not (eq protocol restored)))
        (is (eq restored (kli:active-protocol context)))
        (is (ext:extension-loaded-p restored :profile-probe-extension))
        (is (ext:extension-loaded-p restored :standard-object)
            "reinstalling the profile manifest reconstructs its members")
        (is (ext:extension-loaded-p restored :events))
        (is (not (null (profiles:protocol-profile-activation restored :probe)))
            "the profile activation record is rebuilt by reinstallation")))))

(test nix-declared-hooks-empty-in-core
  "Plain core ships the nix-declared baking hooks empty -- only a nix-configured
image's baked shim populates them, so core itself names no extension."
  (is (null profiles:*nix-declared-extension-manifests*))
  (is (null profiles:*nix-declared-baseline-ids*)))

(test profiles-splice-nix-declared-manifests
  "Every baseline profile splices *nix-declared-extension-manifests* into its
manifest group right after the baseline, read dynamically when the manifest is
constructed. This is the seam a configured image's baked shim writes to: a baked
symbol boots as a baseline child of whichever profile is selected."
  (let ((profiles:*nix-declared-extension-manifests* '(nix-declared-probe-marker)))
    (dolist (factory (list profiles:*headless-extension-manifest*
                           profiles:*interactive-terminal-extension-manifest*
                           profiles:*human-in-loop-extension-manifest*
                           profiles:*autonomous-extension-manifest*))
      (let* ((extension (funcall factory))
             (contribution (first (ext:extension-contribution-list extension)))
             (group (profiles:contribution-manifest-group contribution)))
        (is (member 'nix-declared-probe-marker group)
            "profile ~S splices the nix-declared hook into its group"
            (kli:object-id extension))))))

(test profile-activation-isolates-across-protocols
  (let* ((ctx-a (kli:make-kernel-host))
         (proto-a (switch-to-extension-protocol ctx-a))
         (ctx-b (kli:make-kernel-host))
         (proto-b (switch-to-extension-protocol ctx-b)))
    (install-extension ctx-a *profile-probe-extension-extension-manifest*)
    (is (not (null (profiles:protocol-profile-activation proto-a :probe))))
    (is (null (profiles:protocol-profile-activation proto-b :probe))
        "a profile installed into proto-a must not appear in proto-b")
    (is (ext:extension-loaded-p proto-a :events))
    (is (not (ext:extension-loaded-p proto-b :events)))
    (is (not (eq (profiles:protocol-profile-activations proto-a)
                 (profiles:protocol-profile-activations proto-b)))
        "activation tables must be per-protocol")))
