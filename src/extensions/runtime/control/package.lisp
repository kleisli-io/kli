(defpackage #:kli/runtime/control
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:boot-last-protocol-transaction
                #:context-registry
                #:find-live-object
                #:install-protocol
                #:live-object
                #:protocol
                #:recover-protocol
                #:rollback-protocol
                #:smoke-test-protocol
                #:switch-protocol
                #:validate-protocol)
  (:import-from #:kli/agent/loop
                #:agent
                #:run-agent-loop)
  (:import-from #:kli/ext
                #:defextension
                #:make-provider
                #:make-provider-contract)
  (:import-from #:let-over-lambda
                #:defmacro!)
  (:export
   #:control-plane
   #:make-control-plane
   #:kernel-boot-protocol
   #:kernel-control-plane
   #:control-install-protocol
   #:control-switch-protocol
   #:control-rollback-protocol
   #:control-recover-protocol
   #:make-control-provider
   #:make-control-contract
   #:with-kernel-recovery
   #:recover-to-boot
   #:*control-extension-manifest*))
