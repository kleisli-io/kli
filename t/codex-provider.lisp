(in-package #:kli/tests)

(in-suite all)

(defun codex-provider-test-context ()
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

(defun count-codex-available (registry store context)
  (count "openai-codex"
         (models:available-models registry store context)
         :key #'models:model-definition-provider-id
         :test #'string=))

(test (codex-provider-registers-models-adapter-and-gates-on-credential :fixture interactive-authority)
  "Codex transport facts live in provider/model transport profiles. The catalogue
stays hidden until the codex login resolves."
  (multiple-value-bind (context protocol) (codex-provider-test-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (install-extension context codex:*codex-provider-extension-manifest*)
      (let* ((registry (model-registry context))
             (store (credential-store context))
             (provider (models:find-model-provider registry "openai-codex")))
        (is (not (null provider)))
        (is (eq :openai-responses (models:model-provider-api provider)))
        (let ((profile (getf (models:model-provider-metadata provider)
                             :transport-profile)))
          (is (string= "/codex/responses" (getf profile :url-path)))
          (is (eq t (getf profile :session-identity)))
          (is (eq t (getf profile :developer-role)))
          (is (string= "session-id" (getf profile :session-header)))
          (is (eq t (getf profile :websocket-continuation)))
          (is (eq :session-id (getf profile :prompt-cache-key)))
          (is (eq :session-id (getf profile :client-request-id)))
          (is (string= "chatgpt-account-id" (getf profile :account-id-header)))
          (is (string= "low" (getf profile :text-verbosity)))
          (is (eq t (getf profile :user-agent))))
        (dolist (model-id '("gpt-5.3-codex-spark" "gpt-5.4" "gpt-5.4-mini" "gpt-5.5"))
          (let* ((definition (models:find-model-definition registry "openai-codex" model-id))
                 (meta (and definition (models:model-definition-metadata definition)))
                 (profile (getf meta :transport-profile))
                 (defaults (and definition
                                (models:materialize-model-options definition))))
	            (is (not (null definition)))
	            (is (models:model-supports-option-p definition "reasoning-effort"))
	            (is (models:model-supports-option-p definition "reasoning-summary"))
	            (is (models:model-supports-option-p definition "transport"))
	            (is (eq :auto (getf defaults :reasoning-summary)))
	            (is (eq :auto (getf defaults :transport)))
            (is (eq t (getf profile :developer-role)))
            (is (= 128000 (getf profile :max-output)))
            (is (null (getf meta :developer-role)))))
        (is (null (models:find-model-definition registry "openai-codex" "gpt-5.2-codex")))
        (is (not (null (rt:find-model-stream-adapter
                        (model-runtime-service context) :openai-responses))))
        (is (zerop (count-codex-available registry store context)))
        (auth:store-oauth-credential store "openai-codex" context
                                     :access "AT" :refresh "RT"
                                     :expires (+ (get-universal-time) 3600)
                                     :account-id "acct" :path path)
        (is (= 4 (count-codex-available registry store context)))))))
