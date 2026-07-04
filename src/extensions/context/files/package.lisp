(defpackage #:kli/context/files
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object)
  (:import-from #:kli/ext
                #:contribution-name
                #:contribution-state
                #:defextension
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/text
                #:blank-string-p)
  (:import-from #:kli/config
                #:project-ancestors)
  (:import-from #:kli/agent/session
                #:add-system-prompt-layer
                #:remove-system-prompt-layer)
  (:export
   #:+context-file-candidates+
   #:context-file-in-directory
   #:collect-context-files
   #:render-project-context
   #:discover-system-prompt
   #:discover-append-system-prompt
   #:install-context-files
   #:uninstall-context-files
   #:*context-files-extension-manifest*))

(in-package #:kli/context/files)
