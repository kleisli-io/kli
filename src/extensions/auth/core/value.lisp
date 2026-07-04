(in-package #:kli/auth/core)

(defgeneric refresh-oauth-credential (reference)
  (:documentation
   "Obtain a fresh access token for an oauth reference, persisting the result."))

(defun do-refresh-oauth-credential (reference refresh-fn)
  "Refresh REFERENCE using REFRESH-FN, the provider's token-endpoint call:
called as (refresh-fn reference current-refresh-token), it returns
\(values access refresh expires account-id). The live reference is the
session authority, so its slots are updated before the durable record is
written -- a reader landing between the two steps sees the rotated token,
never stale memory against a fresh file. A failed write still leaves the
session holding the valid token for the next attempt. Slot access stays in
this package; the provider only handles its own HTTP."
  (multiple-value-bind (access refresh expires account-id)
      (funcall refresh-fn reference (oauth-credential-reference-refresh reference))
    (let ((provider-id (credential-reference-provider-id reference))
          (store-path (or (oauth-credential-reference-store-path reference)
                          (credentials-path))))
      (setf (oauth-credential-reference-access reference) access
            (oauth-credential-reference-refresh reference) refresh
            (oauth-credential-reference-expires reference) expires
            (oauth-credential-reference-account-id reference) account-id)
      (write-credential-record
       provider-id
       (oauth-credential-record :access access :refresh refresh
                                :expires expires :account-id account-id)
       store-path)
      access)))

(defmethod credential-reference-value ((reference static-credential-reference))
  (static-credential-reference-key-string reference))

(defun oauth-credential-expired-p (reference)
  (>= (get-universal-time) (oauth-credential-reference-expires reference)))

(defmethod credential-reference-value ((reference oauth-credential-reference))
  (if (oauth-credential-expired-p reference)
      (refresh-oauth-credential reference)
      (oauth-credential-reference-access reference)))

(defun token-present-p (token)
  (and (stringp token) (plusp (length token))))

(defmethod credential-reference-value :before ((reference credential-reference))
  (kli/ext:require-capability :auth/resolve-secret))

(defmethod reference-available-p ((reference static-credential-reference))
  "Availability is presence, not a secret read — stays ungated like the oauth override."
  (token-present-p (static-credential-reference-key-string reference)))

(defmethod reference-available-p ((reference env-credential-reference))
  (token-present-p (sb-ext:posix-getenv (env-credential-reference-variable reference))))

(defmethod reference-available-p ((reference oauth-credential-reference))
  "Availability must not trigger a refresh. An expired access token with a
refresh token is still usable, and provider listing probes this directly."
  (or (token-present-p (oauth-credential-reference-refresh reference))
      (token-present-p (oauth-credential-reference-access reference))))
