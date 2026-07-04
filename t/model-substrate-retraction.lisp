(in-package #:kli/tests)

(in-suite all)

(defun runtime-adapter-refcount (runtime api)
  "Per-api install refcount on RUNTIME, NIL when no adapter is held for API."
  (gethash api (rt::runtime-stream-adapter-refcounts runtime)))

(defun registered-models-for (registry provider-id)
  (loop for model being the hash-values of (models:registry-models registry)
        when (string= provider-id (models:model-definition-provider-id model))
          collect model))

(test model-registry-unregister-removes-provider-and-definition
  "unregister-model-provider/-definition are the structural inverse of register:
find-* returns NIL and the live-objects are dropped from the context registry."
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (declare (ignore protocol))
    (let* ((registry (model-registry context))
           (live (kli:context-registry context))
           (provider (models:register-model-provider
                      registry
                      (models:make-model-provider "probe" :openai-responses
                                                  :auth-required-p nil)
                      context))
           (model (models:register-model-definition
                   registry
                   (models:make-model-definition "probe" "probe-model" :openai-responses
                                                 :name "probe-model" :context-window 1000)
                   context)))
      (is (not (null (models:find-model-provider registry "probe"))))
      (is (not (null (models:find-model-definition registry "probe" "probe-model"))))
      (is (not (null (kli:find-live-object live (kli:object-id provider)))))
      (models:unregister-model-definition registry model context)
      (models:unregister-model-provider registry provider context)
      (is (null (models:find-model-definition registry "probe" "probe-model")))
      (is (null (models:find-model-provider registry "probe")))
      (is (null (kli:find-live-object live (kli:object-id model))))
      (is (null (kli:find-live-object live (kli:object-id provider)))))))

(test provider-catalogue-rejects-option-without-transport-lowering
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (declare (ignore protocol))
    (let ((registry (model-registry context)))
      (signals error
        (kli/model/providers/common:install-provider-catalogue
         context
         :provider-id "bad-lowering"
         :display-name "bad-lowering"
         :api :anthropic-messages
         :config (models:make-provider-config :base-url "https://example.invalid")
         :models (list (list :id "m"
                             :option-schemas
                             (list (models:make-model-option-schema
                                    "text-verbosity"
                                    :values '(:low :medium :high)))))))
      (is (null (models:find-model-provider registry "bad-lowering")))
      (is (null (models:find-model-definition registry "bad-lowering" "m"))))))

(test (stream-adapter-refcount-keeps-shared-api-alive-across-retract :fixture interactive-authority)
  "openai and codex both reference the :openai-responses adapter. A per-api
refcount lets codex retract without stranding openai's adapter; the adapter fn is
removed only when the last referencing provider retracts."
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (with-temp-credentials (path)
      (let* ((runtime (model-runtime-service context))
             (registry (model-registry context))
             (openai-handle (install-extension
                             context openai:*openai-provider-extension-manifest*))
             (codex-handle (install-extension
                            context codex:*codex-provider-extension-manifest*)))
        (is (not (null (rt:find-model-stream-adapter runtime :openai-responses))))
        (is (= 2 (runtime-adapter-refcount runtime :openai-responses)))
        (ext:deactivate-extension protocol codex-handle context)
        (is (not (null (rt:find-model-stream-adapter runtime :openai-responses)))
            "openai's shared adapter survives codex retract")
        (is (= 1 (runtime-adapter-refcount runtime :openai-responses)))
        (is (not (null (models:find-model-provider registry "openai"))))
        (is (not (null (models:find-model-definition registry "openai" "gpt-5.1"))))
        (is (null (models:find-model-provider registry "openai-codex")))
        (ext:deactivate-extension protocol openai-handle context)
        (is (null (rt:find-model-stream-adapter runtime :openai-responses)))
        (is (null (runtime-adapter-refcount runtime :openai-responses)))))))

(test (provider-deactivate-drains-registry-runtime-and-auth :fixture interactive-authority)
  "Deactivating a provider reverses its entire install: provider + models leave
the registry, the stream adapter is released, the auth-provider and
credential-reference leave the store, and every contribution drains from the
protocol."
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (with-temp-credentials (path)
      (let* ((registry (model-registry context))
             (store (credential-store context))
             (runtime (model-runtime-service context))
             (base-count (length (ext:protocol-installed-contributions protocol)))
             (handle (install-extension
                      context anthropic:*anthropic-provider-extension-manifest*)))
        (is (not (null (models:find-model-provider registry "anthropic"))))
        (is (= 6 (length (registered-models-for registry "anthropic"))))
        (is (not (null (rt:find-model-stream-adapter runtime :anthropic-messages))))
        (is (not (null (auth:find-auth-provider store "anthropic"))))
        (is (not (null (auth:find-credential-references store "anthropic"))))
        (is (> (length (ext:protocol-installed-contributions protocol)) base-count))
        (ext:deactivate-extension protocol handle context)
        (is (null (models:find-model-provider registry "anthropic")))
        (is (null (models:find-model-definition registry "anthropic" "claude-opus-4-8")))
        (is (null (registered-models-for registry "anthropic")))
        (is (null (rt:find-model-stream-adapter runtime :anthropic-messages)))
        (is (null (auth:find-auth-provider store "anthropic")))
        (is (null (auth:find-credential-references store "anthropic")))
        (is (= base-count (length (ext:protocol-installed-contributions protocol))))))))

(test (codex-retract-keeps-openai-stream-resolvable :fixture interactive-authority)
  "End-to-end: after codex retracts, an openai selection still resolves its
stream adapter by the provider's api."
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (with-temp-credentials (path)
      (let* ((runtime (model-runtime-service context))
             (registry (model-registry context))
             (openai-handle (install-extension
                             context openai:*openai-provider-extension-manifest*))
             (codex-handle (install-extension
                            context codex:*codex-provider-extension-manifest*)))
        (declare (ignore openai-handle))
        (ext:deactivate-extension protocol codex-handle context)
        (let ((openai-provider (models:find-model-provider registry "openai")))
          (is (not (null openai-provider)))
          (is (not (null (rt:find-model-stream-adapter
                          runtime (models:model-provider-api openai-provider))))))))))

(test contracts-advertise-unregister-primitives
  "The registry and auth providers advertise the unregister entries, so an
agent-reachable retract has the same surface as register."
  (multiple-value-bind (context protocol) (provider-baseline-context)
    (declare (ignore context))
    (let ((reg (ext:require-capability-provider
                protocol :model/registry :contract :model/registry/v1))
          (auth (ext:require-capability-provider
                 protocol :auth :contract :auth/v1)))
      (is (not (null (getf (ext:provider-entries reg) :unregister-model-provider))))
      (is (not (null (getf (ext:provider-entries reg) :unregister-model-definition))))
      (is (not (null (getf (ext:provider-entries auth) :unregister-auth-provider))))
      (is (not (null (getf (ext:provider-entries auth) :unregister-credential-reference)))))))
