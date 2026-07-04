(in-package #:kli/context/commands)

(defextension context-commands
  (:requires
   (capability commands :contract commands/v1)
   (capability context/lens :contract context/lens/v1)
   (capability agent/session :contract agent/session/v1))
  (:provides
   (effect context-commands
     #'register-context-commands
     #'unregister-context-commands)))
