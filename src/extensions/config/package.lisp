(defpackage #:kli/config
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object
                #:live-object)
  (:import-from #:kli/ext
                #:check-contribution-precondition
                #:contribution
                #:contribution-extension
                #:contribution-state
                #:defcontribution-kind
                #:defextension
                #:ensure-protocol-storage
                #:extension-loaded-p
                #:extension-protocol
                #:install-contribution
                #:make-provider
                #:make-provider-contract
                #:normalize-extension-id
                #:protocol-installed-contributions
                #:protocol-storage
                #:retract-contribution)
  (:import-from #:kli/interaction/commands
                #:reply)
  (:export
   #:*global-config-dir*
   #:*project-start-directory*
   #:global-config-dir
   #:directory-ancestors
   #:project-ancestors
   #:locate-dominating
   #:project-config-dir
   #:expand-config-path

   #:global-settings-path
   #:project-settings-path
   #:read-settings-file
   #:merge-settings
   #:load-settings
   #:settings-value

   #:config-service
   #:make-config-service
   #:find-config-service
   #:config-service-settings
   #:config-service-file-settings
   #:config-service-global-dir
   #:config-service-project-dir
   #:config-service-resource-kinds
   #:reload-settings
   #:rebind-config-dirs
   #:set-settings-overlay
   #:record-settings-overlay
   #:register-resource-kind
   #:unregister-resource-kind
   #:resource-paths
   #:*extension-resource-roots*
   #:register-extension-resource-roots
   #:installed-extension-resource-root-keys
   #:installed-extension-resource-roots
   #:config-summary
   #:format-config-summary

   #:settings-declaration
   #:make-settings-declaration
   #:settings-declaration-key
   #:settings-declaration-schema
   #:settings-declaration-contribution
   #:make-settings-declaration-contribution
   #:contribution-settings-declaration
   #:find-settings-declaration
   #:list-settings-declarations
   #:validate-settings-schema
   #:settings-schema-diagnostics
   #:settings-declaration-diagnostics
   #:settings-declaration-value
   #:declared-settings-value
   #:extension-settings-report
   #:format-extension-settings-report

   #:*config-extension-manifest*
   #:*config-commands-extension-manifest*))

(in-package #:kli/config)
