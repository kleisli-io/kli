(defpackage #:kli/tools/introspect
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry)
  (:import-from #:kli/ext
                #:defextension
                #:make-tool-result
                #:make-tool-text-content
                #:provider-call
                #:require-capability-provider
                #:tool-parameter)
  (:import-from #:kli/text
                #:render-bounded-lines)
  (:import-from #:kli/output-spill
                #:register-sequence-spill
                #:spill-entry-token
                #:spill-entry-element-count
                #:format-spill-marker)
  (:export
   #:run-list-objects-tool
   #:run-context-summary-tool
   #:run-inspect-tool
   #:*introspect-print-length*
   #:*introspect-print-level*
   #:*introspect-object-limit*
   #:*list-objects-tool-extension-manifest*
   #:*context-summary-tool-extension-manifest*
   #:*inspect-tool-extension-manifest*))
