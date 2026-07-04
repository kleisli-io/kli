(in-package #:kli/context/files)

(defextension context-files
  (:requires
   (capability config :contract config/v1))
  (:provides
   (effect context-files
     #'install-context-files
     #'uninstall-context-files)))
