(in-package #:kli/auth/core)

(defgeneric register-auth-provider (store provider context))
(defgeneric register-credential-reference (store reference context))
(defgeneric unregister-auth-provider (store provider context))
(defgeneric unregister-credential-reference (store reference context))
(defgeneric find-auth-provider (store provider-id))
(defgeneric find-credential-references (store provider-id &key scope))
(defgeneric credential-available-p (store provider-id &key scope))
(defgeneric resolve-credential (store provider-id context &key scope))
(defgeneric inspect-auth-store (store))
(defgeneric inspect-credential-reference (reference))
(defgeneric inspect-resolved-credential (credential))

(defmethod register-auth-provider ((store credential-store)
                                   (provider auth-provider)
                                   context)
  (kli/ext:require-capability :auth/register-reference)
  (let ((provider-id (auth-provider-provider-id provider)))
    (setf (gethash provider-id (auth-store-providers store)) provider)
    (when (and context
               (not (find-live-object (context-registry context)
                                      (object-id provider))))
      (register-live-object (context-registry context) provider))
    provider))

(defmethod register-credential-reference ((store credential-store)
                                          (reference credential-reference)
                                          context)
  (kli/ext:require-capability :auth/register-reference)
  (let* ((provider-id (credential-reference-provider-id reference))
         (references (gethash provider-id (auth-store-references store))))
    (setf (gethash provider-id (auth-store-references store))
          (cons reference (remove (object-id reference)
                                  references
                                  :key #'object-id
                                  :test #'equal)))
    (when (and context
               (not (find-live-object (context-registry context)
                                      (object-id reference))))
      (register-live-object (context-registry context) reference))
    reference))

(defmethod unregister-auth-provider ((store credential-store)
                                     (provider auth-provider)
                                     context)
  "Inverse of register-auth-provider: drop the contributed provider registration
and its live-object. User-restored secrets are untouched."
  (kli/ext:require-capability :auth/register-reference)
  (remhash (auth-provider-provider-id provider) (auth-store-providers store))
  (when context
    (remove-live-object (context-registry context) (object-id provider)))
  provider)

(defmethod unregister-credential-reference ((store credential-store)
                                            (reference credential-reference)
                                            context)
  "Inverse of register-credential-reference: drop this reference, leaving any
sibling references for the same provider in place."
  (kli/ext:require-capability :auth/register-reference)
  (let* ((provider-id (credential-reference-provider-id reference))
         (remaining (remove (object-id reference)
                            (gethash provider-id (auth-store-references store))
                            :key #'object-id :test #'equal)))
    (if remaining
        (setf (gethash provider-id (auth-store-references store)) remaining)
        (remhash provider-id (auth-store-references store))))
  (when context
    (remove-live-object (context-registry context) (object-id reference)))
  reference)

(defmethod find-auth-provider ((store credential-store) provider-id)
  (gethash (normalize-provider-id provider-id) (auth-store-providers store)))

(defmethod find-credential-references ((store credential-store)
                                       provider-id
                                       &key scope)
  (let ((scope (normalize-scope scope)))
    (remove-if-not
     (lambda (reference)
       (or (null scope)
           (null (credential-reference-scope reference))
           (eq scope (credential-reference-scope reference))))
     (copy-list (gethash (normalize-provider-id provider-id)
                         (auth-store-references store))))))

(defgeneric reference-available-p (reference))

(defmethod reference-available-p ((reference credential-reference))
  (let ((value (credential-reference-value reference)))
    (and value (> (length value) 0))))

(defmethod credential-available-p ((store credential-store)
                                   provider-id
                                   &key scope)
  (some #'reference-available-p
        (find-credential-references store provider-id :scope scope)))

(defmethod resolve-credential ((store credential-store)
                               provider-id
                               context
                               &key scope)
  (declare (ignore context))
  (kli/ext:require-capability :auth/resolve-secret)
  (let ((reference
          (find-if #'reference-available-p
                   (find-credential-references store
                                               provider-id
                                               :scope scope))))
    (unless reference
      (error "No available credential for provider ~S." provider-id))
    (make-resolved-credential reference
                              (credential-reference-value reference))))

(defmethod inspect-credential-reference ((reference credential-reference))
  (list :id (object-id reference)
        :provider-id (credential-reference-provider-id reference)
        :kind (credential-reference-kind reference)
        :scope (credential-reference-scope reference)
        :metadata (copy-list (credential-reference-metadata reference))
        :available (reference-available-p reference)))

(defmethod inspect-credential-reference ((reference env-credential-reference))
  (append (call-next-method)
          (list :variable (env-credential-reference-variable reference))))

(defmethod inspect-resolved-credential ((credential resolved-credential))
  (list :id (object-id credential)
        :provider-id (resolved-credential-provider-id credential)
        :reference-id (resolved-credential-reference-id credential)
        :scope (resolved-credential-scope credential)
        :source-kind (resolved-credential-source-kind credential)
        :timestamp (resolved-credential-timestamp credential)
        :secret-present t))

(defmethod inspect-auth-store ((store credential-store))
  (kli/ext:require-capability :auth/read-metadata)
  (list
   :providers
   (loop for provider being the hash-values of (auth-store-providers store)
         collect (list :id (object-id provider)
                       :provider-id (auth-provider-provider-id provider)
                       :display-name (auth-provider-display-name provider)
                       :metadata (copy-list (auth-provider-metadata provider))))
   :references
   (loop for references being the hash-values of (auth-store-references store)
         append (mapcar #'inspect-credential-reference references))))
