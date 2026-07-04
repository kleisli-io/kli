(defpackage #:kli/tui/editor
  (:use #:cl)
  (:import-from #:kli
                #:next-keyword-id
                #:make-id-counter
                #:live-object
                #:object-protocol)
  (:import-from #:kli/ext
                #:defextension
                #:find-capability-provider
                #:provider-call)
  (:import-from #:kli/tui/style
                #:style-span)
  (:import-from #:kli/tui/keymap
                #:keymap-action
                #:keymap-recognized-p)
  (:import-from #:kli/tui/input
                #:insertable-input-string
                #:submit-input-p
                #:newline-input-p
                #:backspace-input-p)
  (:import-from #:kli/tui/core
                #:make-behavior-cell
                #:call-behavior
                #:render-lines
                #:cursor-position
                #:handle-input
                #:handle-paste
                #:dismiss-overlay
                #:invalidate
                #:set-focused
                #:submit-editor
                #:recode-tui-behavior)
  (:import-from #:kli/text
                #:character-width
                #:visible-width
                #:split-lines
                #:whitespace-char-p
                #:truncate-to-width
                #:wrap-text
                #:pad-right
                #:string-prefix-p
                #:string-suffix-p)
  (:export
   #:paste-block
   #:make-paste-block
   #:paste-block-number
   #:paste-block-marker
   #:paste-block-text
   #:editor-view
   #:make-editor
   #:editor-value
   #:editor-expanded-value
   #:set-editor-value
   #:editor-cursor
   #:editor-desired-column
   #:editor-prompt
   #:editor-on-submit
   #:editor-focused
   #:editor-paste-blocks
   #:editor-paste-counter
   #:editor-behavior
   #:recode-editor-behavior
   #:default-editor-behavior
   #:editor-completion
   #:editor-row-lines
   #:editor-rows-and-cursor
   #:render-completion-lines
   #:completion-candidate
   #:make-completion-candidate
   #:completion-candidate-insert
   #:completion-candidate-match
   #:completion-candidate-hint
   #:completion-candidate-description
   #:completion-candidate-value
   #:completion-popup-kind
   #:completion-popup-candidates
   #:completion-popup-selected
   #:open-editor-selection
   #:completion-trigger
   #:fuzzy-score
   #:large-paste-p
   #:paste-line-count
   #:paste-marker-for
   #:editor-paste-marker-ranges
   #:*tui-editor-extension-manifest*))
