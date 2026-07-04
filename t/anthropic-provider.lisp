(in-package #:kli/tests)

(in-suite all)

(test (anthropic-provider-registers-messages-models-and-gates-on-key :fixture interactive-authority)
  "With no env key and no persisted static credential, the anthropic catalogue stays hidden until a static credential is set. Model facts (thinking mode, xhigh wire effort, max output) live in definition transport profiles."
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (install-extension context anthropic:*anthropic-provider-extension-manifest*)
      (let* ((registry (model-registry context))
             (store (credential-store context))
             (provider (models:find-model-provider registry "anthropic")))
        (is (not (null provider)))
        (is (eq :anthropic-messages (models:model-provider-api provider)))
        (is (string= "https://api.anthropic.com/v1/messages"
                     (transports:anthropic-url
                      (models:provider-config-base-url
                       (models:model-provider-config provider)))))
        (let ((opus (models:find-model-definition registry "anthropic"
                                                  "claude-opus-4-8")))
          (is (not (null opus)))
          (is (models:model-supports-option-p opus "reasoning-effort"))
          (let* ((meta (models:model-definition-metadata opus))
                 (profile (getf meta :transport-profile)))
            (is (eq :adaptive (getf profile :thinking-mode)))
            (is (string= "xhigh" (getf profile :xhigh-effort)))
            (is (= 128000 (getf profile :max-output)))
            (is (eq t (getf profile :midstream-system))
                "opus carries the midstream-system capability")
            (is (null (getf meta :thinking-mode)))
            (is (null (getf meta :xhigh-effort)))))
        (let ((sonnet (models:find-model-definition registry "anthropic"
                                                    "claude-sonnet-4-6")))
          (is (not (null sonnet)))
          (is (string= "max" (getf (getf (models:model-definition-metadata sonnet)
                                         :transport-profile)
                                    :xhigh-effort))))
        (let ((haiku (models:find-model-definition registry "anthropic"
                                                   "claude-haiku-4-5")))
          (is (not (null haiku)))
          (let* ((meta (models:model-definition-metadata haiku))
                 (profile (getf meta :transport-profile)))
            (is (eq :budget (getf profile :thinking-mode)))
            (is (null (getf profile :xhigh-effort)))
            (is (null (getf profile :midstream-system))
                "non-opus models do not get midstream-system")
            (is (null (getf meta :thinking-mode)))))
        (is (not (null (rt:find-model-stream-adapter
                        (model-runtime-service context) :anthropic-messages))))
        (unless (sb-ext:posix-getenv "ANTHROPIC_API_KEY")
          (is (zerop (count-provider-available "anthropic" registry store context))))
        (auth:set-static-credential store "anthropic" "sk-ant-direct" context path)
        (is (= 6 (count-provider-available "anthropic" registry store context)))))))
