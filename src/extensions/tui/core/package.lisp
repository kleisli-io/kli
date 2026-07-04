(defpackage #:kli/tui/core
  (:use #:cl)
  (:import-from #:kli
                #:live-object
                #:object-id)
  (:import-from #:kli/ext
                #:with-extension-fault-barrier)
  (:import-from #:let-over-lambda
                #:get-pandoric
                #:pandoric-hotpatch
                #:pandoriclet)
  (:export
   #:behavior-cell
   #:make-behavior-cell
   #:make-pandoric-behavior
   #:behavior-function
   #:behavior-pandoric-p
   #:behavior-state
   #:behavior-metadata
   #:behavior-capabilities
   #:behavior-version
   #:behavior-fault-policy
   #:behavior-fault-fallback
   #:behavior-pandoric-value
   #:call-behavior
   #:recode-behavior

   #:render-lines
   #:render-transcript-event
   #:cursor-position
   #:handle-input
   #:handle-paste
   #:dismiss-overlay
   #:invalidate
   #:set-focused
   #:children
   #:add-child
   #:remove-child
   #:clear-children
   #:submit-editor
   #:recode-tui-behavior
   #:write-terminal
   #:terminal-size
   #:render-frame
   #:decode-input))
