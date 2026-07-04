(defpackage #:kli/tui/input
  (:use #:cl)
  (:import-from #:kli
                #:next-keyword-id
                #:make-id-counter
                #:live-object
                #:object-protocol)
  (:import-from #:kli/ext
                #:defextension)
  (:import-from #:kli/tui/keymap
                #:keymap-action)
  (:import-from #:kli/tui/core
                #:make-behavior-cell
                #:call-behavior
                #:decode-input
                #:dismiss-overlay
                #:handle-input
                #:handle-paste
                #:recode-tui-behavior)
  (:import-from #:kli/text
                #:printable-string-p
                #:string-suffix-p
                #:parse-positive-integer)
  (:export
   #:+bracketed-paste-start+
   #:+bracketed-paste-end+
   #:input-event
   #:make-text-input-event
   #:make-paste-input-event
   #:make-key-input-event
   #:make-interrupt-input-event
   #:make-terminal-response-input-event
   #:input-event-kind
   #:input-event-key
   #:input-event-key-id
   #:input-event-text
   #:input-event-modifiers
   #:input-event-event-type
   #:input-event-raw
   #:input-decoder
   #:make-input-decoder
   #:input-decoder-buffer
   #:input-decoder-paste-mode
   #:input-decoder-paste-buffer
   #:input-decoder-behavior
   #:input-decoder-feed
   #:input-decoder-flush
   #:input-decoder-clear
   #:decode-input-sequence
   #:recode-input-decoder
   #:route-input-event
   #:input-route-context
   #:make-input-route-context
   #:route-interrupt-handler
   #:route-abort-handler
   #:route-clear-screen-handler
   #:route-next-surface-handler
   #:insertable-input-string
   #:normalize-insertable-text
   #:submit-input-p
   #:newline-input-p
   #:backspace-input-p
   #:printable-character-p
   #:input-data-string
   #:terminal-response-sequence-p
   #:extract-complete-input-sequences
   #:parse-terminal-key
   #:parse-key-id
   #:key-name-keyword
   #:key-id-with-modifiers
   #:modifier-mask-to-list
   #:*tui-input-extension-manifest*))
