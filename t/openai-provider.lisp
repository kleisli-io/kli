(in-package #:kli/tests)

(in-suite all)

(defun provider-baseline-context ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*)
    (values context protocol)))

(defun count-provider-available (provider-id registry store context)
  (count provider-id
         (models:available-models registry store context)
         :key #'models:model-definition-provider-id
         :test #'string=))

(test (openai-provider-registers-responses-models-and-gates-on-key :fixture interactive-authority)
  "With no env key and no persisted static credential, the openai catalogue stays hidden until a static credential is set."
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (install-extension context openai:*openai-provider-extension-manifest*)
      (let* ((registry (model-registry context))
             (store (credential-store context))
             (provider (models:find-model-provider registry "openai")))
        (is (not (null provider)))
        (is (eq :openai-responses (models:model-provider-api provider)))
        (is (string= "https://api.openai.com/v1/responses"
                     (transports:%responses-endpoint
                      (models:model-provider-config provider)
                      (getf (models:model-provider-metadata provider)
                            :transport-profile))))
	  (let ((gpt (models:find-model-definition registry "openai" "gpt-5.1")))
	    (is (not (null gpt)))
	    (is (models:model-supports-option-p gpt "reasoning-effort"))
	    (is (models:model-supports-option-p gpt "reasoning-summary"))
	    (is (eq :auto (getf (models:materialize-model-options gpt)
	                        :reasoning-summary)))
	    (let ((profile (getf (models:model-definition-metadata gpt)
	                         :transport-profile)))
            (is (eq t (getf profile :developer-role))
                "gpt-5.x carries the developer-role transport capability")
            (is (= 128000 (getf profile :max-output))))
          (is (null (getf (models:model-definition-metadata gpt) :developer-role))))
        (is (not (null (models:find-model-definition registry "openai" "gpt-5.5"))))
        (is (not (null (rt:find-model-stream-adapter
                        (model-runtime-service context) :openai-responses))))
        (unless (sb-ext:posix-getenv "OPENAI_API_KEY")
          (is (zerop (count-provider-available "openai" registry store context))))
        (auth:set-static-credential store "openai" "sk-direct-key" context path)
        (is (= 4 (count-provider-available "openai" registry store context)))))))
