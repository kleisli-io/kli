(in-package #:kli/profiles)

(defextension interactive-terminal
  (:provides
   (profile :interactive-terminal
     (append *baseline-extension-manifests*
             *nix-declared-extension-manifests*
             *model-provider-extension-manifests*
             *tui-app-extension-manifests*))))
