(defpackage #:kli/tui/status
  (:use #:cl)
  (:import-from #:kli/ext
                #:defextension #:defcontribution-kind
                #:contribution
                #:install-contribution #:retract-contribution
                #:extension-protocol #:protocol-installed-contributions
                #:protocol-storage #:ensure-protocol-storage
                #:normalize-extension-id #:make-method-contribution
                #:builder-add-contribution #:extension-builder-id
                #:with-extension-fault-barrier #:note-fault)
  (:import-from #:kli/tui/core
                #:render-transcript-event)
  (:import-from #:kli/text
                #:pad-right)
  (:import-from #:kli/session/log
                #:estimate-message-tokens #:message-role
                #:message-entry-p #:entry-message #:compaction-entry)
  (:import-from #:kli/model/registry
                #:current-model-selection #:selection-context-window
                #:model-selection-provider-id #:model-selection-model-id
                #:model-selection-option-value)
  (:import-from #:kli/agent/session
                #:usage-input-tokens #:usage-output-tokens
                #:usage-cache-read-tokens #:usage-cache-write-tokens
                #:usage-total-tokens)
  (:export
   #:register-status-slot #:unregister-status-slot
   #:set-status #:status-text #:list-status-slots
   #:register-widget #:unregister-widget #:list-widgets
   #:render-status-line #:render-widgets #:render-footer
   #:message-renderer-contribution
   #:status-slot-contribution #:make-status-slot-contribution
   #:contribution-slot-id #:contribution-initial
   #:widget-contribution #:make-widget-contribution
   #:contribution-widget-id #:contribution-widget
   #:message-renderer #:status-slot #:widget
   #:spinner-glyph #:render-spinner-line
   #:compute-context-usage #:session-context-usage-readout
   #:format-context-usage-readout
   #:current-context-window #:trailing-token-estimate
   #:usage-unknown-after-compaction-p
   #:context-usage-readout #:context-usage-readout-p
   #:readout-tokens #:readout-context-window #:readout-percent
   #:readout-model-id #:readout-provider #:readout-thinking
   #:readout-input #:readout-output #:readout-cache-read
   #:readout-cache-write #:readout-total #:readout-cost
   #:*tui-status-extension-manifest*))

(in-package #:kli/tui/status)
