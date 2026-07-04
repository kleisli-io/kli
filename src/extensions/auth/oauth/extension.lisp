(in-package #:kli/auth/oauth)

(defextension oauth
  (:requires
   (capability auth :contract auth/v1))
  (:provides
   (method refresh-oauth-credential () (oauth-credential-reference) (reference)
     (do-refresh-oauth-credential reference #'fetch-refreshed-oauth-tokens))))
