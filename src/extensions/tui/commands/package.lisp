(defpackage #:kli/tui/commands
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/text
                #:trim-whitespace
                #:whitespace-char-p)
  (:import-from #:kli/interaction/commands
                #:command-result-content
                #:command-result-error-p
                #:registration-qualified-name)
  (:import-from #:kli/tui/transcript
                #:make-transcript-event)
  (:export
   #:slash-command-input-p
   #:parse-slash-command
   #:dispatch-slash-command
   #:render-command-result))
