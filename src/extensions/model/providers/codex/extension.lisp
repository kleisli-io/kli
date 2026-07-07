(in-package #:kli/model/providers/codex)

(defextension codex-provider
  (:requires
   (capability auth :contract auth/v1)
   (capability model/registry :contract model/registry/v1)
   (capability model/runtime :contract model/runtime/v1))
  (:provides
   (effect codex-provider #'install-codex-provider #'retract-codex-provider
     :refresh #'refresh-codex-provider)))
