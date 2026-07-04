(in-package #:kli/config)

(defun make-config-contract ()
  (make-provider-contract
   :id :config/v1
   :capability :config
   :required-entries
   '(:settings
     :settings-value
     :reload-settings
     :register-resource-kind
     :unregister-resource-kind
     :resource-paths
     :config-summary)))

(defun make-config-provider-object ()
  (make-provider
   :id :config-provider
   :capability :config
   :contracts '(:config/v1)
   :entries
   (list :settings
         (lambda (context)
           (config-service-settings (find-config-service context)))
         :settings-value
         (lambda (context &rest keys)
           (apply #'settings-value
                  (config-service-settings (find-config-service context))
                  keys))
         :reload-settings
         (lambda (context)
           (reload-settings (find-config-service context)))
         :register-resource-kind
         (lambda (context kind subdir)
           (register-resource-kind (find-config-service context) kind subdir))
         :unregister-resource-kind
         (lambda (context kind)
           (unregister-resource-kind (find-config-service context) kind))
         :resource-paths
         (lambda (context kind &rest args)
           (apply #'resource-paths (find-config-service context) kind args))
         :config-summary
         (lambda (context)
           (config-summary (find-config-service context)))
         ;; Extra entries beyond the :config/v1 contract: reads and
         ;; introspection over declared :settings contributions.
         :declared-settings-value
         (lambda (context key &rest keys)
           (apply #'declared-settings-value context key keys))
         :extension-settings-report
         (lambda (context)
           (extension-settings-report
            (active-protocol context)
            (config-service-settings (find-config-service context)))))))

(defextension config
  (:provides
   (contract config/v1
     (make-config-contract))
   (capability config (make-config-provider-object))
   (live-object config-service
     (make-config-service))
   (effect settings-overlay
     #'install-recorded-overlay
     #'revert-recorded-overlay)))

(defextension config-commands
  (:requires
   (capability commands :contract commands/v1)
   (capability config :contract config/v1))
  (:provides
   (command "settings"
     :description "Show config paths and the merged settings."
     :handler #'run-settings-command)))
