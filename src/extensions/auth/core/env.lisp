(in-package #:kli/auth/core)

(defgeneric credential-reference-value (reference))

(defmethod credential-reference-value ((reference env-credential-reference))
  (sb-ext:posix-getenv (env-credential-reference-variable reference)))
