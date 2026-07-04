(in-package #:kli/model/registry)

(defgeneric register-model-provider (registry provider context))
(defgeneric register-model-definition (registry model context))
(defgeneric unregister-model-provider (registry provider context))
(defgeneric unregister-model-definition (registry model context))
(defgeneric find-model-provider (registry provider-id))
(defgeneric find-model-definition (registry provider-id model-id))
(defgeneric available-models (registry auth-store context))
(defgeneric select-model (registry model-definition context
                           &key id options store session actor
                             metadata))
(defgeneric current-model-selection (registry))
(defgeneric inspect-model-provider (provider))
(defgeneric inspect-model-definition (model))
(defgeneric inspect-model-selection (selection))
(defgeneric inspect-model-registry (registry))

(defun model-provider-auth-available-p (provider auth-store auth-provider)
  (or (not (model-provider-auth-required-p provider))
      (and auth-store
           (provider-call auth-provider
                          :credential-available-p
                          auth-store
                          (model-provider-credential-provider-id provider)))))

(defun model-available-p (registry model auth-store auth-provider)
  (let ((provider (find-model-provider registry
                                       (model-definition-provider-id model))))
    (and provider
         (model-provider-auth-available-p provider auth-store auth-provider))))

(defmethod register-model-provider ((registry model-registry)
                                    (provider model-provider)
                                    context)
  (setf (gethash (model-provider-provider-id provider)
                 (registry-providers registry))
        provider)
  (when (and context
             (not (find-live-object (context-registry context)
                                    (object-id provider))))
    (register-live-object (context-registry context) provider))
  provider)

(defmethod register-model-definition ((registry model-registry)
                                      (model model-definition)
                                      context)
  (unless (find-model-provider registry (model-definition-provider-id model))
    (error "Model provider is not registered: ~S"
           (model-definition-provider-id model)))
  (setf (gethash (model-key (model-definition-provider-id model)
                            (model-definition-model-id model))
                 (registry-models registry))
        model)
  (when (and context
             (not (find-live-object (context-registry context)
                                    (object-id model))))
    (register-live-object (context-registry context) model))
  model)

(defmethod unregister-model-provider ((registry model-registry)
                                      (provider model-provider)
                                      context)
  "Inverse of register-model-provider. Object-in: remhash by the provider's own
id (the key register stored) and drop the live-object."
  (remhash (model-provider-provider-id provider) (registry-providers registry))
  (when context
    (remove-live-object (context-registry context) (object-id provider)))
  provider)

(defmethod unregister-model-definition ((registry model-registry)
                                        (model model-definition)
                                        context)
  "Inverse of register-model-definition."
  (remhash (model-key (model-definition-provider-id model)
                      (model-definition-model-id model))
           (registry-models registry))
  (when context
    (remove-live-object (context-registry context) (object-id model)))
  model)

(defmethod find-model-provider ((registry model-registry) provider-id)
  (gethash (normalize-provider-id provider-id)
           (registry-providers registry)))

(defmethod find-model-definition ((registry model-registry)
                                  provider-id
                                  model-id)
  (gethash (model-key provider-id model-id)
           (registry-models registry)))
(defun selection-context-window (registry selection)
  "Context window declared by SELECTION's model definition, or NIL."
  (and selection
       (let ((def (find-model-definition registry
                                         (model-selection-provider-id selection)
                                         (model-selection-model-id selection))))
         (and def (model-definition-context-window def)))))

(defmethod available-models ((registry model-registry) auth-store context)
  (let ((auth-provider (require-capability-provider (active-protocol context)
                                                    :auth
                                                    :contract :auth/v1)))
    (loop for model being the hash-values of (registry-models registry)
          when (model-available-p registry model auth-store auth-provider)
            collect model)))

(defun maybe-register-selection (selection context)
  (when (and context
             (not (find-live-object (context-registry context)
                                    (object-id selection))))
    (register-live-object (context-registry context) selection))
  selection)

(defun maybe-record-model-selection (selection store session context)
  (when (and store session)
    (let* ((protocol (active-protocol context))
           (log-provider (require-capability-provider protocol
                                                      :session/log
                                                      :contract :session/log/v1))
           (entries-provider (require-capability-provider
                              protocol
                              :session/entries
                              :contract :session/entries/v1)))
      (provider-call log-provider
                     :append-session-entry
                     store
                     session
                     (provider-call entries-provider
                                    :make-model-change-entry
                                    (model-selection-provider-id selection)
                                    (model-selection-model-id selection)
                                    :options (model-selection-options selection)
                                    :source (object-id selection))
                     context)))
  selection)

(defmethod select-model ((registry model-registry)
                         (model model-definition)
                         context
                         &key id options
                           (store nil store-supplied-p)
                           (session nil session-supplied-p)
                           actor metadata)
  (declare (ignore actor))
  (unless (eq model
              (find-model-definition registry
                                     (model-definition-provider-id model)
                                     (model-definition-model-id model)))
    (error "Model is not registered: ~S/~S"
           (model-definition-provider-id model)
           (model-definition-model-id model)))
  (let ((selection (make-model-selection model
                                         :id id
                                         :options options
                                         :metadata metadata)))
    (setf (registry-current-selection registry) selection)
    (maybe-register-selection selection context)
    (maybe-record-model-selection selection
                                  (and store-supplied-p store)
                                  (and session-supplied-p session)
                                  context)
    selection))

(defmethod current-model-selection ((registry model-registry))
  (registry-current-selection registry))

(defmethod inspect-model-provider ((provider model-provider))
  (list :id (object-id provider)
        :provider-id (model-provider-provider-id provider)
        :api (model-provider-api provider)
        :auth-required-p (model-provider-auth-required-p provider)
        :credential-provider-id (model-provider-credential-provider-id provider)
        :metadata (copy-list (model-provider-metadata provider))))

(defmethod inspect-model-definition ((model model-definition))
  (list :id (object-id model)
        :provider-id (model-definition-provider-id model)
        :model-id (model-definition-model-id model)
        :api (model-definition-api model)
        :name (model-definition-name model)
        :context-window (model-definition-context-window model)
        :option-schemas
        (loop for schema in (model-definition-option-schemas model)
              collect (list :option-id (model-option-schema-option-id schema)
                            :type (model-option-schema-type schema)
                            :values (copy-list (model-option-schema-values schema))
                            :default (and (model-option-schema-default-supplied-p schema)
                                          (model-option-schema-default schema))
                            :default-supplied-p
                            (model-option-schema-default-supplied-p schema)
                            :min (model-option-schema-min schema)
                            :max (model-option-schema-max schema)))
        :metadata (copy-list (model-definition-metadata model))))

(defmethod inspect-model-selection ((selection model-selection))
  (list :id (object-id selection)
        :provider-id (model-selection-provider-id selection)
        :model-id (model-selection-model-id selection)
        :options (copy-list (model-selection-options selection))
        :timestamp (model-selection-timestamp selection)
        :metadata (copy-list (model-selection-metadata selection))))

(defmethod inspect-model-registry ((registry model-registry))
  (list
   :providers
   (loop for provider being the hash-values of (registry-providers registry)
         collect (inspect-model-provider provider))
   :models
   (loop for model being the hash-values of (registry-models registry)
         collect (inspect-model-definition model))
   :current-selection
   (and (registry-current-selection registry)
        (inspect-model-selection (registry-current-selection registry)))))
