(defpackage #:kli
  (:use #:cl)
  (:export
   #:live-object
   #:object-id
   #:object-protocol
   #:next-keyword-id
   #:make-id-counter
   #:next-counter-value
   #:advance-counter
   #:counter-value

   #:live-registry
   #:make-registry
   #:register-live-object
   #:find-live-object
   #:remove-live-object
   #:map-live-objects

   #:kernel-context
   #:make-kernel-host
   #:context-registry
   #:active-protocol

   #:protocol
   #:boot-protocol
   #:boot-last-protocol-transaction
   #:install-protocol
   #:switch-protocol
   #:rollback-protocol
   #:recover-protocol
   #:validate-protocol
   #:smoke-test-protocol))

(in-package #:kli)
