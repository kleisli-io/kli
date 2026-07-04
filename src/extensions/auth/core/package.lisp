(defpackage #:kli/auth/core
  (:use #:cl)
  (:import-from #:kli
                #:next-keyword-id
                #:make-id-counter
                #:context-registry
                #:find-live-object
                #:live-object
                #:object-id
                #:register-live-object
                #:remove-live-object)
  (:import-from #:kli/ext
                #:defextension
                #:make-provider
                #:make-provider-contract)
  (:export
   #:credential-store
   #:credential-reference
   #:env-credential-reference
   #:static-credential-reference
   #:oauth-credential-reference
   #:resolved-credential
   #:auth-provider
   #:auth-scope

   #:make-credential-store
   #:make-auth-provider
   #:make-env-credential-reference
   #:make-static-credential-reference
   #:make-oauth-credential-reference
   #:auth-store-providers
   #:auth-store-references
   #:auth-provider-provider-id
   #:auth-provider-display-name
   #:auth-provider-metadata
   #:credential-reference-provider-id
   #:credential-reference-kind
   #:credential-reference-scope
   #:credential-reference-metadata
   #:credential-reference-value
   #:env-credential-reference-variable
   ;; access/refresh slot readers stay internal: the gated credential-reference-value
   ;; is the only interface read of secret material.
   #:oauth-credential-reference-expires
   #:oauth-credential-reference-account-id
   #:oauth-credential-reference-store-path
   #:oauth-credential-expired-p
   #:refresh-oauth-credential
   #:do-refresh-oauth-credential
   #:resolved-credential-provider-id
   #:resolved-credential-reference-id
   #:resolved-credential-scope
   #:resolved-credential-value
   #:resolved-credential-source-kind
   #:resolved-credential-timestamp

   #:register-auth-provider
   #:register-credential-reference
   #:unregister-auth-provider
   #:unregister-credential-reference
   #:find-auth-provider
   #:find-credential-references
   #:credential-available-p
   #:resolve-credential
   #:inspect-auth-store
   #:inspect-credential-reference
   #:inspect-resolved-credential

   #:*credentials-path*
   #:credentials-path
   #:load-credentials
   #:save-credentials
   #:read-credential-record
   #:write-credential-record
   #:delete-credential-record
   #:static-credential-record
   #:oauth-credential-record
   #:credential-record-kind
   #:credential-reference-from-record

   #:set-static-credential
   #:store-oauth-credential
   #:restore-oauth-credential
   #:restore-credential
   #:forget-credential
   #:*auth-extension-manifest*))

(in-package #:kli/auth/core)
