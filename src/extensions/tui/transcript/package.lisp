(defpackage #:kli/tui/transcript
  (:use #:cl)
  (:import-from #:kli
                #:next-keyword-id
                #:make-id-counter
                #:live-object
                #:object-protocol)
  (:import-from #:kli/ext
                #:defextension
                #:protocol-storage
                #:presentation-kind
                #:default-call-term
                #:present-default-call-text)
  (:import-from #:kli/tui/input
                #:submit-input-p)
  (:import-from #:kli/tui/editor
                #:make-editor
                #:editor-value
                #:set-editor-value
                #:editor-expanded-value
                #:editor-completion
                #:editor-rows-and-cursor
                #:render-completion-lines)
  (:import-from #:kli/tui/terminal
                #:terminal-columns
                #:terminal-rows
                #:terminal-clear-screen
                #:terminal-clear-line
                #:terminal-clear-scrollback
                #:terminal-move-cursor
                #:terminal-hide-cursor
                #:terminal-show-cursor
                #:terminal-begin-synchronized-update
                #:terminal-end-synchronized-update
                #:terminal-begin-frame
                #:terminal-end-frame)
  (:import-from #:kli/tui/core
                #:make-behavior-cell
                #:call-behavior
                #:write-terminal
                #:render-transcript-event
                #:handle-input
                #:handle-paste
                #:dismiss-overlay
                #:invalidate
                #:set-focused
                #:render-frame
                #:recode-tui-behavior)
  (:import-from #:kli/tui/style
                #:active-theme
                #:style
                #:style-span
                #:theme-token
                #:+bg-reset+)
  (:import-from #:kli/tui/status
                #:render-footer
                #:render-widgets)
  (:import-from #:kli/tui/markdown
                #:markdown->lines
                #:*md-stream*
                #:make-mdstream
                #:*hl-stream*
                #:make-hl-stream
                #:*highlight-memoize-p*
                #:render-diff
                #:render-diff-hunk-blocks
                #:highlight-code-lines
                #:prefix-wrap-segs
                #:make-seg
                #:seg-text
                #:seg-fg
                #:seg-attrs
                #:render-seg-line
                #:seg-line-width
                #:char-wrap-segs)
  (:import-from #:kli/tui/ansi
                #:ansi->segs)
  (:import-from #:kli/text
                #:wrap-text
                #:pad-right
                #:visible-width
                #:split-lines
                #:normalize-text
                #:blank-string-p)
  (:export
   #:project-event-to-transcript
   #:session-entry-transcript-events
   #:make-projection-buffer
   #:transcript-event
   #:make-transcript-event
   #:event-kind
   #:event-role
   #:event-text
   #:event-name
   #:event-status
   #:event-details
   #:event-presentation
   #:transcript
   #:make-transcript
   #:transcript-events
   #:transcript-add-event
   #:transcript-clear
   #:transcript-remove-last-user-event
   #:format-event
   #:render-event
   #:*tool-output-expanded*
   #:tool-output-expanded-p
   #:toggle-tool-output-expansion
   #:transcript-view
   #:make-transcript-view
   #:transcript-view-transcript
   #:transcript-view-editor
   #:transcript-view-history
   #:transcript-view-history-position
   #:transcript-view-history-draft
   #:transcript-view-on-submit
   #:submit-transcript-input
   #:scrollback-renderer
   #:make-scrollback-renderer
   #:scrollback-renderer-transcript
   #:scrollback-renderer-view
   #:scrollback-renderer-terminal
   #:scrollback-renderer-printed-events
   #:scrollback-renderer-frozen-stream-lines
   #:scrollback-renderer-streaming-event
   #:scrollback-renderer-notice
   #:scrollback-renderer-behavior
   #:scrollback-clear-screen
   #:scrollback-anchor-bottom
   #:scrollback-reset
   #:begin-scrollback-stream
   #:finalize-scrollback-stream
   #:recode-scrollback-renderer
   #:*tui-transcript-extension-manifest*
   #:scrollback-renderer-region-lines
   #:render-streaming-event-cached
   #:scrollback-renderer-region-cursor-row
   #:indent-lines))
