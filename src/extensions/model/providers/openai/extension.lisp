(in-package #:kli/model/providers/openai)

(defextension openai-provider
  (:requires
   (capability auth :contract auth/v1)
   (capability model/registry :contract model/registry/v1)
   (capability model/runtime :contract model/runtime/v1))
  (:provides
   (effect openai-provider #'install-openai-provider #'retract-openai-provider)))
