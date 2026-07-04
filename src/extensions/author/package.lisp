(defpackage #:kli/author
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:defextension
                #:kli-extension
                #:builder-add-contribution
                #:extension-builder-id
                #:find-capability-provider
                #:normalize-extension-id
                #:provider-call)
  (:import-from #:kli/interaction/commands
                #:make-command-contribution
                #:reply
                #:rest-arg)
  (:import-from #:kli/event
                #:make-event-handler-contribution
                #:on-contribution-name)
  (:export
   #:defextension
   #:kli-extension
   #:command
   #:on
   #:reply
   #:rest-arg
   #:notify))

(in-package #:kli/author)
