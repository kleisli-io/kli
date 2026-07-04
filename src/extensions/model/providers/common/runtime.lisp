(in-package #:kli/model/providers/common)

(defun %shallow-merge-plists (&rest plists)
  "Merge plist facts shallowly, with later plists overriding earlier ones."
  (let ((out '()))
    (dolist (plist plists)
      (loop for (key value) on plist by #'cddr
            do (setf (getf out key) value)))
    out))

(defun %metadata-with-transport-profile (metadata transport-profile)
  (if transport-profile
      (list* :transport-profile transport-profile metadata)
      metadata))

(defun %validate-model-option-lowering (api model-id schemas profile)
  (dolist (schema schemas)
    (let ((option-id (model-option-schema-option-id schema)))
      (unless (transport-supports-option-p api profile option-id)
        (error "Model ~A declares option ~A, but ~A has no transport lowering for it."
               model-id option-id api)))))

(defun install-provider-catalogue (context &key provider-id display-name api
                                           config credential models
                                           transport-profile)
  "Register the auth-provider, credential-reference, model provider, model
catalogue, and stream adapter for one provider, returning the
contribution-state retract-provider-catalogue drains. CREDENTIAL is (:env \"VAR\") for an env
credential-reference plus restore-credential, :oauth for
restore-oauth-credential, or NIL for restore-credential alone. MODELS are
plists (:id :name :context-window :option-schemas :metadata :transport-profile).
The auth-provider is captured only when this install created it. The adapter is
referenced through ensure-stream-adapter so its per-api refcount stays correct
when providers share an api."
  (let ((provider-profile (getf transport-profile :provider)))
    (dolist (m models)
      (%validate-model-option-lowering
       api (getf m :id) (getf m :option-schemas)
       (%shallow-merge-plists provider-profile (getf m :transport-profile))))
    (let ((auth (require-capability-provider (active-protocol context)
                                             :auth :contract :auth/v1))
          (reg  (require-capability-provider (active-protocol context)
                                             :model/registry :contract :model/registry/v1))
          (rt-p (require-capability-provider (active-protocol context)
                                             :model/runtime :contract :model/runtime/v1))
          (store    (find-live-object (context-registry context) :credential-store))
          (registry (find-live-object (context-registry context) :model-registry-service))
          (runtime  (find-live-object (context-registry context) :model-runtime-service))
          (auth-provider nil)
          (credential-reference nil)
          (definitions '()))
      (unless (provider-call auth :find-auth-provider store provider-id)
        (setf auth-provider
              (provider-call auth :register-auth-provider store
                             (provider-call auth :make-auth-provider provider-id
                                            :display-name display-name)
                             context)))
      (when (and (consp credential) (eq (first credential) :env))
        (setf credential-reference
              (provider-call auth :register-credential-reference store
                             (provider-call auth :make-env-credential-reference
                                            provider-id (second credential))
                             context)))
      (if (eq credential :oauth)
          (provider-call auth :restore-oauth-credential store provider-id context)
          (provider-call auth :restore-credential store provider-id context))
      (let ((provider
             (provider-call reg :register-model-provider registry
                            (make-model-provider provider-id api
                                                 :auth-required-p t
                                                 :credential-provider-id provider-id
                                                 :config config
                                                 :metadata (%metadata-with-transport-profile nil provider-profile))
                            context)))
        (dolist (m models)
          (let* ((model-profile (%shallow-merge-plists provider-profile
                                                       (getf m :transport-profile)))
                 (option-schemas (getf m :option-schemas)))
            (push (provider-call reg :register-model-definition registry
                                 (make-model-definition provider-id (getf m :id) api
                                                        :name (getf m :name)
                                                        :context-window (getf m :context-window)
                                                        :option-schemas option-schemas
                                                        :metadata (%metadata-with-transport-profile
                                                                   (getf m :metadata)
                                                                   model-profile))
                                 context)
                  definitions)))
        (ensure-stream-adapter rt-p runtime api context)
        (list :auth-provider auth-provider
              :credential-reference credential-reference
              :provider provider
              :models definitions
              :adapter-api api)))))

(defun retract-provider-catalogue (state context)
  "Drain the contribution-state install-provider-catalogue returned, reversing
every registration. The shared adapter is released via the refcounted
unregister, so providers sharing the api keep theirs."
  (let ((auth (require-capability-provider (active-protocol context)
                                           :auth :contract :auth/v1))
        (reg  (require-capability-provider (active-protocol context)
                                           :model/registry :contract :model/registry/v1))
        (rt-p (require-capability-provider (active-protocol context)
                                           :model/runtime :contract :model/runtime/v1))
        (store    (find-live-object (context-registry context) :credential-store))
        (registry (find-live-object (context-registry context) :model-registry-service))
        (runtime  (find-live-object (context-registry context) :model-runtime-service)))
    (dolist (m (getf state :models))
      (provider-call reg :unregister-model-definition registry m context))
    (when (getf state :provider)
      (provider-call reg :unregister-model-provider registry (getf state :provider) context))
    (when (getf state :adapter-api)
      (provider-call rt-p :unregister-model-stream-adapter runtime (getf state :adapter-api) context))
    (when (getf state :credential-reference)
      (provider-call auth :unregister-credential-reference store (getf state :credential-reference) context))
    (when (getf state :auth-provider)
      (provider-call auth :unregister-auth-provider store (getf state :auth-provider) context))
    nil))
