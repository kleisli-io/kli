(defpackage #:kli/tools/trace
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:defextension
                #:install-contribution
                #:retract-contribution
                #:contribution-tool
                #:contribution-extension
                #:contribution-state
                #:contribution-kind
                #:list-tool-contributions
                #:protocol-installed-contributions
                #:make-trace-contribution
                #:resolve-trace-symbol
                #:protocol-trace-buffer
                #:protocol-trace-evicted
                #:make-tool-result
                #:make-tool-text-content
                #:tool-parameter)
  (:export
   #:run-trace-tool
   #:run-untrace-tool
   #:run-trace-read-tool
   #:*trace-tool-extension-manifest*
   #:*untrace-tool-extension-manifest*
   #:*trace-read-tool-extension-manifest*))
