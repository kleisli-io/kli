(in-package #:kli/model/providers/anthropic)

(defextension anthropic-provider
  (:requires
   (capability auth :contract auth/v1)
   (capability model/registry :contract model/registry/v1)
   (capability model/runtime :contract model/runtime/v1))
  (:provides
   (effect anthropic-provider #'install-anthropic-provider #'retract-anthropic-provider
     :refresh #'refresh-anthropic-provider)))
