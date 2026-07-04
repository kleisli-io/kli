(in-package #:kli/auth/core)

(defvar *resolved-credential-counter* (make-id-counter))

(defun next-resolved-credential-id ()
  (next-keyword-id "RESOLVED-CREDENTIAL" '*resolved-credential-counter*))

(defun normalize-provider-id (provider-id)
  (etypecase provider-id
    (string (string-downcase provider-id))
    (symbol (string-downcase (symbol-name provider-id)))))

(defun normalize-scope (scope)
  (etypecase scope
    (null nil)
    (keyword scope)
    (symbol (intern (string-upcase (symbol-name scope)) :keyword))
    (string (intern (string-upcase scope) :keyword))))

(defclass credential-store (live-object)
  ((providers
    :initform (make-hash-table :test #'equal)
    :accessor auth-store-providers)
   (references
    :initform (make-hash-table :test #'equal)
    :accessor auth-store-references)))

(defclass auth-provider (live-object)
  ((provider-id
    :initarg :provider-id
    :reader auth-provider-provider-id)
   (display-name
    :initarg :display-name
    :initform nil
    :reader auth-provider-display-name)
   (metadata
    :initarg :metadata
    :initform '()
    :reader auth-provider-metadata)))

(defclass credential-reference (live-object)
  ((provider-id
    :initarg :provider-id
    :reader credential-reference-provider-id)
   (kind
    :initarg :kind
    :reader credential-reference-kind)
   (scope
    :initarg :scope
    :initform nil
    :reader credential-reference-scope)
   (metadata
    :initarg :metadata
    :initform '()
    :reader credential-reference-metadata)))

(defclass env-credential-reference (credential-reference)
  ((variable
    :initarg :variable
    :reader env-credential-reference-variable)))

(defclass static-credential-reference (credential-reference)
  ((key-string
    :initarg :key-string
    :reader static-credential-reference-key-string)))

(defclass oauth-credential-reference (credential-reference)
  ((access
    :initarg :access
    :initform nil
    :accessor oauth-credential-reference-access)
   (refresh
    :initarg :refresh
    :initform nil
    :accessor oauth-credential-reference-refresh)
   (expires
    :initarg :expires
    :initform 0
    :accessor oauth-credential-reference-expires)
   (account-id
    :initarg :account-id
    :initform nil
    :accessor oauth-credential-reference-account-id)
   (store-path
    :initarg :store-path
    :initform nil
    :reader oauth-credential-reference-store-path)))

(defclass resolved-credential (live-object)
  ((provider-id
    :initarg :provider-id
    :reader resolved-credential-provider-id)
   (reference-id
    :initarg :reference-id
    :reader resolved-credential-reference-id)
   (scope
    :initarg :scope
    :initform nil
    :reader resolved-credential-scope)
   (value
    :initarg :value
    :reader resolved-credential-value)
   (source-kind
    :initarg :source-kind
    :reader resolved-credential-source-kind)
   (timestamp
    :initarg :timestamp
    :reader resolved-credential-timestamp)))

(defun make-credential-store (&key (id :credential-store))
  (make-instance 'credential-store :id id))

(defun make-auth-provider (provider-id &key id display-name metadata)
  (let ((provider-id (normalize-provider-id provider-id)))
    (make-instance 'auth-provider
                   :id (or id (list :auth-provider provider-id))
                   :provider-id provider-id
                   :display-name display-name
                   :metadata metadata)))

(defun make-env-credential-reference (provider-id variable &key id scope
                                                    metadata)
  (let ((provider-id (normalize-provider-id provider-id))
        (scope (normalize-scope scope)))
    (make-instance 'env-credential-reference
                   :id (or id (remove nil
                                      (list :credential-reference
                                            provider-id
                                            :env
                                            variable
                                            scope)))
                   :provider-id provider-id
                   :kind :env
                   :scope scope
                   :metadata metadata
                   :variable variable)))

(defun make-static-credential-reference (provider-id key-string
                                         &key id scope metadata)
  "The key never enters the object id, which would leak it into registry keys
and inspection output."
  (let ((provider-id (normalize-provider-id provider-id))
        (scope (normalize-scope scope)))
    (make-instance 'static-credential-reference
                   :id (or id (remove nil
                                      (list :credential-reference
                                            provider-id
                                            :static
                                            scope)))
                   :provider-id provider-id
                   :kind :static
                   :scope scope
                   :metadata metadata
                   :key-string key-string)))

(defun make-oauth-credential-reference (provider-id
                                        &key access refresh expires
                                             account-id store-path
                                             id scope metadata)
  (let ((provider-id (normalize-provider-id provider-id))
        (scope (normalize-scope scope)))
    (make-instance 'oauth-credential-reference
                   :id (or id (remove nil
                                      (list :credential-reference
                                            provider-id
                                            :oauth
                                            scope)))
                   :provider-id provider-id
                   :kind :oauth
                   :scope scope
                   :metadata metadata
                   :access access
                   :refresh refresh
                   :expires (or expires 0)
                   :account-id account-id
                   :store-path store-path)))

(defun make-resolved-credential (reference value)
  (make-instance 'resolved-credential
                 :id (next-resolved-credential-id)
                 :provider-id (credential-reference-provider-id reference)
                 :reference-id (object-id reference)
                 :scope (credential-reference-scope reference)
                 :value value
                 :source-kind (credential-reference-kind reference)
                 :timestamp (get-universal-time)))
