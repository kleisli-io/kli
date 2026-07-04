(in-package #:kli/profiles)

(defextension headless
  (:provides
   (profile :headless
     ;; Automated runs still need the event sink, even with no agent loop.
     (append *baseline-extension-manifests*
             *nix-declared-extension-manifests*
             (list 'kli/observability:*observability-extension-manifest*)))))
