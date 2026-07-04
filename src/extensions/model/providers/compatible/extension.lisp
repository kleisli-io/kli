(in-package #:kli/model/providers/compatible)

(defextension compatible-provider
  (:requires
   (capability auth :contract auth/v1)
   (capability model/registry :contract model/registry/v1)
   (capability model/runtime :contract model/runtime/v1))
  (:provides
   (effect compatible-provider #'install-compatible-providers #'retract-compatible-providers)))
