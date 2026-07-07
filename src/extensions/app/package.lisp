(defpackage #:kli/app
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:install-protocol
                #:switch-protocol
                #:object-id
                #:context-registry
                #:find-live-object
                #:make-kernel-host)
  (:import-from #:kli/ext
                #:call-with-manifest-capture
                #:define-capability-binding
                #:defextension
                #:ensure-protocol-storage
                #:extension-loaded-p
                #:extension-metadata
                #:find-capability-provider
                #:install-manifest
                #:make-extension-protocol
                #:normalize-extension-id
                #:protocol-storage
                #:refresh-runtime-contributions
                #:provider-call
                #:require-capability-provider
                #:retract-manifest
                #:with-boot-stage
                #:boot-marker
                #:with-system-authority
                #:subject-grant
                #:make-subject
                #:make-default-subject
                #:make-grant
                #:grant-meet
                #:grant-without-atoms
                #:grant-capabilities
                #:grant-report
                #:*call-subject*
                #:*ui-subject*
                #:*install-subject*
                #:*image-dump-in-progress*)
  (:import-from #:kli/interaction/commands
                #:clone-command
                #:format-command-collisions
                #:reply
                #:rest-arg)
  (:import-from #:kli/config
                #:config-service-file-settings
                #:config-service-settings
                #:find-config-service
                #:load-settings
                #:rebind-config-dirs
                #:record-settings-overlay
                #:set-settings-overlay
                #:settings-value)
  (:import-from #:kli/config/wiring
                #:swap-settings-overlay)
  (:import-from #:kli/profiles
                #:find-profile-manifest
                #:known-profile-names
                #:parse-profile-specs
                #:resolve-profile-spec
                #:resolved-profile-base
                #:resolved-profile-enable
                #:resolved-profile-disable
                #:resolved-profile-name
                #:resolved-profile-settings
                #:record-active-profile
                #:protocol-active-profile)
  (:import-from #:kli/agent/session
                #:reset-agent-session
                #:configured-agent-subject
                #:root-agent-subject
                #:resume-agent-session
                #:focus-agent-session-mode
                #:submit-agent-session-prompt
                #:set-agent-session-option
                #:mode-binding-agent-id
                #:session-mode-bindings
                #:mode-binding-context-binding
                #:context-binding-usage
                #:context-binding-live-usage
                #:mode-current-selection
                #:make-session-event-listener
                #:register-session-event-listener)
  (:import-from #:kli/model/registry
                #:registered-model-option-definitions
                #:model-option-definition-id
                #:model-option-definition-type
                #:model-option-definition-enum-values)
  (:import-from #:kli/event
                #:event-type
                #:event-payload)
  (:import-from #:kli/tui/status
                #:register-widget
                #:render-spinner-line
                #:session-context-usage-readout
                #:format-context-usage-readout)
  (:import-from #:kli/tui/app
                #:tui-app-mode-id
                #:tui-app-spinner-active-p
                #:tui-app-spinner-phase
                #:tui-app-tool-update-text)
  (:export
   #:*current-context*
   #:*current-app*
   #:*default-profile*
   #:tui-app-provider
   #:tui-app-call
   #:main
   #:+kli-version+
   #:current-version
   #:build-id
   #:compute-build-id
   #:select-profile-name
   #:profile-interactive-p
   #:build-boot-snapshot
   #:*boot-snapshot-context*
   #:run-tui-main
   #:dispatch-main
   #:relocation-probe-main
   #:release-signer-main
   #:write-fatal-log
   #:fatal-exit
   #:with-fatal-error-handler
   #:build-tui-app
   #:+fault-injection-env-var+
   #:fault-injection-manifests
   #:boot-user-extensions
   #:load-user-extensions
   #:reload-user-extensions
   #:enable-user-extension
   #:disable-user-extension
   #:uninstall-remote-extension
   #:user-extension-status
   #:user-extension-units
   #:user-extension-dirs
   #:discover-units
   #:extension-diagnostics
   #:+remote-install-pins-key+
   #:+restore-version-gaps-key+
   #:remote-install-pins
   #:validate-pin
   #:invalid-pin
   #:record-remote-install-pin
   #:restore-version-gaps
   #:reinstall-remote-pin
   #:install-isolated-extension
   #:*remote-install-resolver*
   #:host-triple
   #:continue-last-requested-p))

(in-package #:kli/app)
