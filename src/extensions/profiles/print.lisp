(in-package #:kli/profiles)

(defextension print
  (:provides
   (profile :print
     ;; Cross-cutting non-UI extensions a one-shot run needs: skills and context
     ;; files shape the prompt, observability sinks events, settings-wiring (last,
     ;; over the model providers) selects the configured default model.
     (append *baseline-extension-manifests*
             *nix-declared-extension-manifests*
             *model-provider-extension-manifests*
             (list 'kli/skills:*skills-extension-manifest*
                   'kli/context/files:*context-files-extension-manifest*
                   'kli/observability:*observability-extension-manifest*
                   'kli/config/wiring:*settings-wiring-extension-manifest*)))))
