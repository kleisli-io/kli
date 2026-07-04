(in-package #:kli/auth/core)

(defun ensure-auth-provider-registered (store provider-id context)
  (unless (find-auth-provider store provider-id)
    (register-auth-provider store
                            (make-auth-provider provider-id
                                                :display-name provider-id)
                            context))
  provider-id)

(defun set-static-credential (store provider-id key-string context
                              &optional (path (credentials-path)))
  (kli/ext:require-capability :auth/register-reference)
  (ensure-auth-provider-registered store provider-id context)
  (write-credential-record provider-id (static-credential-record key-string) path)
  (register-credential-reference store
                                 (make-static-credential-reference provider-id
                                                                   key-string)
                                 context))

(defun store-oauth-credential (store provider-id context
                               &key access refresh expires account-id
                                    (path (credentials-path)))
  (kli/ext:require-capability :auth/register-reference)
  (ensure-auth-provider-registered store provider-id context)
  (write-credential-record
   provider-id
   (oauth-credential-record :access access :refresh refresh
                            :expires expires :account-id account-id)
   path)
  (register-credential-reference
   store
   (make-oauth-credential-reference provider-id
                                    :access access :refresh refresh
                                    :expires expires :account-id account-id
                                    :store-path path)
   context))

(defun restore-oauth-credential (store provider-id context
                                 &optional (path (credentials-path)))
  "Register an oauth reference from a persisted record, or NIL when none exists."
  (let ((record (read-credential-record provider-id path)))
    (when (and record (eq (credential-record-kind record) :oauth))
      (ensure-auth-provider-registered store provider-id context)
      (register-credential-reference
       store
       (credential-reference-from-record provider-id record :store-path path)
       context))))

(defun restore-credential (store provider-id context
                           &optional (path (credentials-path)))
  "Register a reference from a persisted record of any kind, or NIL when none."
  (let ((record (read-credential-record provider-id path)))
    (when record
      (ensure-auth-provider-registered store provider-id context)
      (register-credential-reference
       store
       (credential-reference-from-record provider-id record :store-path path)
       context))))

(defun forget-credential (store provider-id context
                          &optional (path (credentials-path)))
  "Inverse of registration: drop the durable record AND the live in-memory
references, so a logged-out provider stops resolving without a restart."
  (kli/ext:require-capability :auth/forget-secret)
  (delete-credential-record provider-id path)
  (dolist (reference (find-credential-references store provider-id))
    (unregister-credential-reference store reference context))
  provider-id)

(defun install-auth-registrations (protocol contribution context)
  "Install half of the auth-registrations effect: user /auth registrations land
at command time, not here."
  (declare (ignore protocol contribution context))
  nil)

(defun drain-auth-registrations (protocol contribution context)
  "Drop every in-memory reference and provider the store accumulated from /auth
commands, so a rolled-back auth subsystem leaves no orphan live-objects. Placed
last in the auth :provides, so it retracts first, while the credential-store
live-object is still registered."
  (declare (ignore protocol contribution))
  (let ((store (find-live-object (context-registry context) :credential-store)))
    (when store
      (loop for refs being the hash-values of (auth-store-references store)
            do (dolist (ref (copy-list refs))
                 (unregister-credential-reference store ref context)))
      (dolist (provider (loop for p being the hash-values of
                                (auth-store-providers store)
                              collect p))
        (unregister-auth-provider store provider context)))))
