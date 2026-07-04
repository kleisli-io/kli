(in-package #:kli/model/registry)

(defun make-model-registry-contract ()
  (make-provider-contract
   :id :model/registry/v1
   :capability :model/registry
   :required-entries
   '(:register-model-provider
     :register-model-definition
     :unregister-model-provider
     :unregister-model-definition
     :find-model-provider
     :find-model-definition
     :available-models
     :select-model
     :current-model-selection
     :inspect-model-registry
     :make-model-provider
     :make-model-definition
     :make-provider-config)))

(defun make-model-registry-provider ()
  (make-provider
   :id :model-registry-provider
   :capability :model/registry
   :contracts '(:model/registry/v1)
   :entries
   (list :register-model-provider #'register-model-provider
         :register-model-definition #'register-model-definition
         :unregister-model-provider #'unregister-model-provider
         :unregister-model-definition #'unregister-model-definition
         :find-model-provider #'find-model-provider
         :find-model-definition #'find-model-definition
         :available-models #'available-models
         :select-model #'select-model
         :current-model-selection #'current-model-selection
         :inspect-model-registry #'inspect-model-registry
         :make-model-provider #'make-model-provider
         :make-model-definition #'make-model-definition
         :make-provider-config #'make-provider-config)))

(defextension model-registry
  (:requires
   (capability auth :contract auth/v1)
   (capability session/log :contract session/log/v1)
   (capability session/entries :contract session/entries/v1))
  (:provides
   (contract model/registry/v1
     (make-model-registry-contract))
   (capability model/registry (make-model-registry-provider))
   (live-object model-registry
     (make-model-registry))))

;;;; Durable identity: the current model selection as {provider-id, model-id,
;;;; options}. Providers and definitions are reconstructed by manifest /
;;;; config reinstallation, not carried. The default method would slot-scrape the
;;;; provider/model hash tables and the selection object by accident -- and skip
;;;; all three as unserializable, silently dropping the selection.

(defmethod kli/runtime/snapshot:snapshot-representation ((registry model-registry))
  (let ((selection (registry-current-selection registry)))
    (list :selection
          (and selection
               (list :provider-id
                     (kli/runtime/snapshot:serialize-snapshot-value
                      (model-selection-provider-id selection))
                     :model-id
                     (kli/runtime/snapshot:serialize-snapshot-value
                      (model-selection-model-id selection))
                     :options
                     (kli/runtime/snapshot:serialize-snapshot-value
                      (model-selection-options selection))))
          :omitted '("providers" "models"))))

(defmethod kli/runtime/snapshot:restore-representation
    ((registry model-registry) datum context)
  ;; Re-apply through the canonical select-model. No store/session -- this sets
  ;; the registry's current selection without re-journaling it (the binding's own
  ;; log is the source of truth for a mode's model); nothing here is gated.
  ;; A definition the target image no longer registers is skipped, not fatal.
  (let ((selection (getf datum :selection)))
    (when selection
      (let* ((provider-id (kli/runtime/snapshot:deserialize-snapshot-value
                           (getf selection :provider-id)))
             (model-id (kli/runtime/snapshot:deserialize-snapshot-value
                        (getf selection :model-id)))
             (options (kli/runtime/snapshot:deserialize-snapshot-value
                       (getf selection :options)))
             (model (find-model-definition registry provider-id model-id)))
        (when model
          (select-model registry model context
                        :options options))))))
