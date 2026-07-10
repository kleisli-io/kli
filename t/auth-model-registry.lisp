(in-package #:kli/tests)

(defun auth-provider (protocol)
  (ext:require-capability-provider protocol
                                   :auth
                                   :contract :auth/v1))

(defun model-registry-provider (protocol)
  (ext:require-capability-provider protocol
                                   :model/registry
                                   :contract :model/registry/v1))

(defun credential-store (context)
  (kli:find-live-object (kli:context-registry context)
                        :credential-store))

(defun model-registry (context)
  (kli:find-live-object (kli:context-registry context)
                        :model-registry-service))

(defun test-reasoning-effort-schema (&key (values '(:off :minimal :low :medium :high :xhigh :max))
                                          (default :off))
  (models:make-model-option-schema "reasoning-effort"
                                   :values values
                                   :default default))

(defun test-reasoning-summary-schema (&key (values '(:auto :concise :detailed :none))
                                           (default :auto))
  (models:make-model-option-schema "reasoning-summary"
                                   :values values
                                   :default default))

(defun test-text-verbosity-schema (&key (values '(:low :medium :high))
                                      (default :medium))
  (models:make-model-option-schema "text-verbosity"
                                    :values values
                                    :default default))

(defun test-service-tier-schema (&key (values '(:auto :default :flex :priority))
                                  (default :auto))
  (models:make-model-option-schema "service-tier"
                                    :values values
                                    :default default))

(defun test-transport-schema (&key (values '(:auto :sse :websocket :websocket-cached))
                                (default :auto))
  (models:make-model-option-schema "transport"
                                    :values values
                                    :default default))

(defun test-reasoning-options (level)
  (and level (list :reasoning-effort level)))

(defun test-selection-reasoning-effort (selection)
  (models:model-selection-option-value selection "reasoning-effort"))

(defun auth-model-test-context ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        sess:*session-log-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*)
    (values context protocol)))

(defun register-path-auth (store context &key (provider-id "path-provider"))
  (auth:register-auth-provider
   store
   (auth:make-auth-provider provider-id :display-name "PATH provider")
   context)
  (auth:register-credential-reference
   store
   (auth:make-env-credential-reference provider-id "PATH")
   context))

(test auth-registers-store-and-provider
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context auth:*auth-extension-manifest*)
    (is (typep (credential-store context) 'auth:credential-store))
    (is (auth-provider protocol))))

(test (auth-resolves-env-reference-with-capability-and-redacts-inspection :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (let* ((context (kli:make-kernel-host))
             (protocol (switch-to-extension-protocol context)))
        (install-extensions context auth:*auth-extension-manifest*)
        (values context protocol))
    (declare (ignore protocol))
    (let* ((store (credential-store context))
           (path-value (sb-ext:posix-getenv "PATH")))
      (register-path-auth store context)
      (is (auth:credential-available-p store "path-provider"))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (auth:resolve-credential store
                                   "path-provider"
                                   context)))
      (let* ((credential (auth:resolve-credential store
                                                  "path-provider"
                                                  context))
             (store-inspection (auth:inspect-auth-store store))
             (credential-inspection
               (auth:inspect-resolved-credential credential)))
        (is (equal path-value
                   (auth:resolved-credential-value credential)))
        (is (not (search path-value (prin1-to-string store-inspection)
                         :test #'char=)))
        (is (not (search path-value (prin1-to-string credential-inspection)
                         :test #'char=)))
        (is (getf credential-inspection :secret-present))))))

(test model-registry-registers-provider-and-requires-dependencies
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (signals error
      (install-extension context models:*model-registry-extension-manifest*)))
  (multiple-value-bind (context protocol)
      (auth-model-test-context)
    (is (typep (credential-store context) 'auth:credential-store))
    (is (typep (model-registry context) 'models:model-registry))
    (is (model-registry-provider protocol))))

(test (model-registry-filters-available-models-by-auth :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (auth-model-test-context)
    (declare (ignore protocol))
    (let* ((store (credential-store context))
           (registry (model-registry context)))
      (register-path-auth store context)
      (models:register-model-provider
       registry
       (models:make-model-provider "path-provider" :fake)
       context)
      (models:register-model-provider
       registry
       (models:make-model-provider "missing-provider" :fake)
       context)
      (models:register-model-provider
       registry
       (models:make-model-provider "local-provider" :fake
                                   :auth-required-p nil)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition "path-provider" "available-model" :fake)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition "missing-provider" "missing-model" :fake)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition "local-provider" "local-model" :fake)
       context)
      (is (equal '("available-model" "local-model")
                 (sort (mapcar #'models:model-definition-model-id
                               (models:available-models registry
                                                        store
                                                        context))
                       #'string<))))))

(test (model-registry-selects-model-and-records-session-entries :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (auth-model-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (sess:create-session store context :id :model-session))
           (registry (model-registry context))
           (auth-store (credential-store context)))
      (register-path-auth auth-store context)
      (models:register-model-provider
       registry
       (models:make-model-provider "path-provider" :fake)
       context)
      (let ((model (models:register-model-definition
                    registry
                    (models:make-model-definition
                     "path-provider"
                     "selected-model"
                     :fake
                     :option-schemas (list (test-reasoning-effort-schema)))
                    context)))
        (let ((selection (models:select-model registry
                                              model
                                              context
                                              :id :selected-model-choice
                                              :options (test-reasoning-options :high)
                                              :store store
                                              :session session)))
          (is (eq selection (models:current-model-selection registry)))
          (is (eq selection
                  (kli:find-live-object (kli:context-registry context)
                                        :selected-model-choice)))
          (let ((built (sess:build-session-context store session)))
            (is (equal '(:provider "path-provider" :model-id "selected-model"
                         :options (:reasoning-effort :high))
                       (sess:session-context-model built)))
            (is (equal '(:reasoning-effort :high)
                       (sess:session-context-options built)))))))))

(test model-registry-snapshot-round-trips-current-selection
  "The registry declares its durable identity -- the current model selection as
{provider-id, model-id, options} -- and restoring that representation re-applies
the selection through the canonical select-model path, even after the live
selection has moved on."
  (multiple-value-bind (context protocol) (auth-model-test-context)
    (declare (ignore protocol))
    (let ((registry (model-registry context)))
      (models:register-model-provider
       registry
       (models:make-model-provider "local-provider" :fake :auth-required-p nil)
       context)
      (models:register-model-definition
       registry
       (models:make-model-definition
        "local-provider" "beta" :fake
        :option-schemas (list (test-reasoning-effort-schema)))
       context)
      (let ((alpha (models:register-model-definition
                    registry
                    (models:make-model-definition
                     "local-provider" "alpha" :fake
                     :option-schemas (list (test-reasoning-effort-schema)))
                    context)))
        (models:select-model registry alpha context
                             :options (test-reasoning-options :high))
        (let ((record (snapshot:snapshot-representation registry)))
          (models:select-model
           registry
           (models:find-model-definition registry "local-provider" "beta")
           context
           :options (test-reasoning-options :low))
          (is (equal "beta"
                     (models:model-selection-model-id
                      (models:current-model-selection registry)))
              "precondition: the live selection advanced past the snapshot")
          (snapshot:restore-representation registry record context)
          (let ((restored (models:current-model-selection registry)))
            (is (equal "local-provider"
                       (models:model-selection-provider-id restored)))
            (is (equal "alpha"
                       (models:model-selection-model-id restored))
                "restore re-applies the captured selection, not the live one")
            (is (equal '(:reasoning-effort :high)
                       (models:model-selection-options restored))
                "restore round-trips the captured options")
            (is (eq :high (test-selection-reasoning-effort restored)))))))))
