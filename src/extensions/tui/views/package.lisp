(defpackage #:kli/tui/views
  (:use #:cl)
  (:import-from #:kli
                #:next-keyword-id
                #:make-id-counter
                #:live-object)
  (:import-from #:kli/ext
                #:defextension)
  (:import-from #:kli/tui/core
                #:render-lines
                #:cursor-position
                #:handle-input
                #:handle-paste
                #:invalidate
                #:set-focused
                #:children
                #:add-child
                #:remove-child
                #:clear-children)
  (:import-from #:kli/text
                #:blank-string-p
                #:wrap-text
                #:normalize-text
                #:pad-right)
  (:export
   #:tui-view
   #:tui-container-view
   #:make-tui-container
   #:tui-text-view
   #:make-tui-text
   #:tui-box-view
   #:make-tui-box
   #:view-children
   #:view-text
   #:view-padding-x
   #:view-padding-y
   #:*tui-views-extension-manifest*))
