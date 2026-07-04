(in-package #:kli/profiles)

(defextension human-in-loop
  (:provides
   (profile :human-in-loop
       (append *baseline-extension-manifests*
               *nix-declared-extension-manifests*
               *model-provider-extension-manifests*
               *tui-app-extension-manifests*)
     :seam (:approval))))
