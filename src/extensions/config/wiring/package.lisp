(defpackage #:kli/config/wiring
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object
                #:register-live-object
                #:remove-live-object)
  (:import-from #:kli/ext
                #:capabilities-subject
                #:contribution-kind
                #:contribution-name
                #:contribution-state
                #:defextension
                #:find-capability-provider
                #:protocol-installed-contributions
                #:protocol-storage
                #:provider-call
                #:require-capability-provider
                #:subject-meet)
  (:import-from #:kli/agent/loop
                #:agent-subject
                #:agent-loop-service-agents)
  (:import-from #:kli/agent/session
                #:recode-compaction-policy
                #:session-compaction-policy)
  (:import-from #:kli/tui/style
                #:active-theme
                #:find-theme
                #:set-active-theme
                #:theme-name
                #:+theme-mode-key+)
  (:import-from #:kli/tui/keymap
                #:load-keymap
                #:protocol-keymap-overrides)
  (:import-from #:kli/model/registry
                #:model-option-keyword
                #:model-option-schema-for
                #:canonicalize-model-option-value
                #:model-supports-option-p
                #:registry-current-selection)
  (:import-from #:kli/session/log
                #:make-file-session-store)
  (:import-from #:kli/config
                #:expand-config-path
                #:find-config-service
                #:set-settings-overlay)
  (:import-from #:kli/tools/bash
                #:bash-policy
                #:seed-bash-exec-provider-id
                #:resolve-bash-exec-provider-id)
  (:export
   #:*settings-wiring-extension-manifest*
   #:swap-settings-overlay))

(in-package #:kli/config/wiring)
