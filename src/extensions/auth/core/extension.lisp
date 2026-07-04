(in-package #:kli/auth/core)

(defun make-auth-contract ()
  "Resolved-credential accessors live as CLOS readers on the value class.
Callers import them directly from kli/auth/core."
  (make-provider-contract
   :id :auth/v1
   :capability :auth
   :required-entries
   '(:register-auth-provider
     :register-credential-reference
     :unregister-auth-provider
     :unregister-credential-reference
     :find-auth-provider
     :find-credential-references
     :credential-available-p
     :resolve-credential
     :inspect-auth-store
     :inspect-credential-reference
     :inspect-resolved-credential
     :make-auth-provider
     :make-env-credential-reference
     :make-static-credential-reference
     :make-oauth-credential-reference
     :set-static-credential
     :store-oauth-credential
     :restore-oauth-credential
     :restore-credential
     :forget-credential)))

(defun make-auth-provider-object ()
  (make-provider
   :id :auth-provider
   :capability :auth
   :contracts '(:auth/v1)
   :entries
   (list :register-auth-provider #'register-auth-provider
         :register-credential-reference #'register-credential-reference
         :unregister-auth-provider #'unregister-auth-provider
         :unregister-credential-reference #'unregister-credential-reference
         :find-auth-provider #'find-auth-provider
         :find-credential-references #'find-credential-references
         :credential-available-p #'credential-available-p
         :resolve-credential #'resolve-credential
         :inspect-auth-store #'inspect-auth-store
         :inspect-credential-reference #'inspect-credential-reference
         :inspect-resolved-credential #'inspect-resolved-credential
         :make-auth-provider #'make-auth-provider
         :make-env-credential-reference #'make-env-credential-reference
         :make-static-credential-reference #'make-static-credential-reference
         :make-oauth-credential-reference #'make-oauth-credential-reference
         :set-static-credential #'set-static-credential
         :store-oauth-credential #'store-oauth-credential
         :restore-oauth-credential #'restore-oauth-credential
         :restore-credential #'restore-credential
         :forget-credential #'forget-credential)))

(defextension auth
  (:provides
   (contract auth/v1
     (make-auth-contract))
   (capability auth (make-auth-provider-object))
   (live-object credential-store
     (make-credential-store))
   (effect auth-registrations
     #'install-auth-registrations
     #'drain-auth-registrations)))
