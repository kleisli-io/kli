(in-package #:kli/profiles)

(defextension autonomous
  (:provides
   (profile :autonomous
       ;; settings-wiring stays last so it selects the configured default model
       ;; after the providers register.
       (append *baseline-extension-manifests*
               *nix-declared-extension-manifests*
               *model-provider-extension-manifests*
               (list 'kli/skills:*skills-extension-manifest*
                     'kli/context/files:*context-files-extension-manifest*
                     'kli/observability:*observability-extension-manifest*
                     'kli/config/wiring:*settings-wiring-extension-manifest*))
     :seam (:planner :scheduler :watchdog :recovery))))
