(defpackage #:kli/debug
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:contribution-extension
                #:contribution-state
                #:defextension
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content)
  (:import-from #:kli/tui/app
                #:recode-tui-app-runtime)
  (:export
   #:register-fault-injection-commands
   #:unregister-fault-injection-commands
   #:*fault-injection-extension-manifest*))

(in-package #:kli/debug)
