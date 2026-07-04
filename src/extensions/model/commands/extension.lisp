(in-package #:kli/model/commands)

(defextension model-commands
  (:requires
   (capability commands :contract commands/v1)
   (capability auth :contract auth/v1)
   (capability model/registry :contract model/registry/v1)
   (capability agent/session :contract agent/session/v1))
  (:provides
   (effect model-commands
     #'register-model-commands
     #'unregister-model-commands)))
