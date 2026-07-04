(defpackage #:kli/tui/terminal
  (:use #:cl)
  (:import-from #:kli
                #:next-keyword-id
                #:make-id-counter
                #:live-object)
  (:import-from #:kli/ext
                #:defextension)
  (:import-from #:kli/tui/views
                #:make-tui-container)
  (:import-from #:kli/tui/core
                #:make-behavior-cell
                #:call-behavior
                #:write-terminal
                #:terminal-size
                #:render-frame
                #:render-lines
                #:cursor-position
                #:handle-input
                #:set-focused
                #:invalidate
                #:recode-tui-behavior)
  (:import-from #:kli/text
                #:parse-positive-integer)
  (:export
   #:terminal
   #:memory-terminal
   #:process-terminal
   #:make-memory-terminal
   #:make-process-terminal
   #:terminal-columns
   #:terminal-rows
   #:terminal-output
   #:terminal-clear
   #:terminal-clear-screen
   #:terminal-clear-line
   #:terminal-clear-scrollback
   #:terminal-move-cursor
   #:terminal-hide-cursor
   #:terminal-show-cursor
   #:terminal-begin-synchronized-update
   #:terminal-end-synchronized-update
   #:terminal-begin-frame
   #:terminal-end-frame
   #:terminal-enable-bracketed-paste
   #:terminal-disable-bracketed-paste
   #:call-with-terminal-character-input
   #:call-with-raw-terminal
   #:terminal-input-tty-p
   #:write-terminal-control
   #:terminal-window-size-from-fd
   #:current-terminal-window-size
   #:process-terminal-size
   #:screen-frame
   #:make-screen-frame
   #:frame-root
   #:frame-focused
   #:frame-previous-lines
   #:frame-render-behavior
   #:set-frame-focus
   #:frame-render-lines
   #:write-diff
   #:recode-frame-renderer
   #:parse-osc-color-report
   #:relative-luminance
   #:classify-background
   #:parse-colorfgbg
   #:da1-reply-position
   #:resolve-background-mode
   #:query-terminal-background
   #:*tui-terminal-extension-manifest*))
