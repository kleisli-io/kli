(in-package #:kli/runtime/introspection)

(defun registry-object-ids (registry)
  (let ((ids '()))
    (map-live-objects (lambda (id object)
                        (declare (ignore object))
                        (push id ids))
                      registry)
    (sort ids #'string< :key #'princ-to-string)))

(defun describe-live-object (object)
  (flet ((->leaf (value)
           "Coerce non-keyword symbols to strings so the description holds
only durable-log leaves (keyword/string/integer/nil). Class names and
extension sources are non-keyword symbols that would otherwise fail
session-log serialization when the inspect tool result is persisted."
           (if (and (symbolp value) (not (keywordp value)) (not (null value)))
               (princ-to-string value)
               value)))
    (let ((description (list :id (object-id object)
                             :class (->leaf (class-name (class-of object))))))
      (if (standard-live-object-p object)
          (append description
                  (list :kind (->leaf (object-kind object))
                        :source (->leaf (object-source object))
                        :version (object-version object)))
          description))))

(defun object-id-string (id)
  "Canonical string projection of a live-object id; round-trips through
`describe-by-id`."
  (princ-to-string id))

(defun describe-by-id (registry id-string)
  "Describe the REGISTRY object whose id projects to ID-STRING, or NIL."
  (let ((match nil))
    (map-live-objects (lambda (id object)
                        (when (string= (object-id-string id) id-string)
                          (setf match object)))
                      registry)
    (and match (describe-live-object match))))

(defun context-summary (context)
  (let ((control (find-live-object (context-registry context)
                                   :control-plane)))
    (list :active-protocol (and (active-protocol context)
                                (object-id (active-protocol context)))
          :control-plane (and control
                              (object-id control))
          :objects (registry-object-ids (context-registry context)))))

(defun make-introspection-contract ()
  (make-provider-contract
   :id :runtime/introspection/v1
   :capability :runtime/introspection
   :required-entries
   '(:registry-object-ids
     :describe-live-object
     :context-summary
     :object-id-string
     :describe-by-id)))

(defun make-introspection-provider ()
  (make-provider
   :id :runtime-introspection-provider
   :capability :runtime/introspection
   :contracts '(:runtime/introspection/v1)
   :entries
   (list :registry-object-ids #'registry-object-ids
         :describe-live-object #'describe-live-object
         :context-summary #'context-summary
         :object-id-string #'object-id-string
         :describe-by-id #'describe-by-id)))

(defextension introspection
  (:provides
   (contract runtime/introspection/v1
     (make-introspection-contract))
   (capability runtime/introspection (make-introspection-provider))))
