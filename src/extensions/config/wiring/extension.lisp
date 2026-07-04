(in-package #:kli/config/wiring)

(defextension settings-wiring
  (:requires
   (capability config :contract config/v1))
  (:provides
   (effect settings-wiring
     #'apply-settings
     #'revert-settings)))
