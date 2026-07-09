(in-package #:kli/model/providers/codex)

(defparameter +provider-id+ "openai-codex")

(defparameter +base-url+ "https://chatgpt.com/backend-api")

(defparameter +codex-models+
  ;; ChatGPT-account allowlist. The endpoint 400s any other id with
  ;; "model is not supported when using Codex with a ChatGPT account".
  '((:id "gpt-5.3-codex-spark" :ctx 128000 :max-out 128000)
    (:id "gpt-5.4"             :ctx 272000 :max-out 128000)
    (:id "gpt-5.4-mini"        :ctx 272000 :max-out 128000)
    (:id "gpt-5.5"             :ctx 272000 :max-out 128000)))

(defun install-codex-provider (protocol contribution context)
  "Register the Codex catalogue -- oauth-keyed, Responses api -- via the shared
installer and return the contribution-state the retractor drains. Codex
authenticates via restore-oauth-credential, so there is no env
credential-reference."
  (declare (ignore protocol contribution))
  (install-provider-catalogue
   context
   :provider-id +provider-id+
   :display-name "ChatGPT (Codex)"
   :api :openai-responses
   :config (make-provider-config
            :base-url +base-url+
            :headers '(("openai-beta" . "responses=experimental")
                       ("originator" . "kli")))
   :credential :oauth
   :transport-profile '(:provider (:url-path "/codex/responses"
                                             :developer-role t
                                             :session-identity t
                                             :session-header "session-id"
                                             :websocket-continuation t
                                             :prompt-cache-key :session-id
                                             :client-request-id :session-id
                                             :account-id-header "chatgpt-account-id"
                                             :text-verbosity "low"
                                             :user-agent t))
   :models (loop for m in +codex-models+
                 collect (list :id (getf m :id)
                               :name (getf m :id)
                               :context-window (getf m :ctx)
	                               :option-schemas (list (make-model-option-schema "reasoning-effort"
	                                                                               :values '(:off :minimal :low :medium :high :xhigh)
	                                                                               :default :off)
	                                                     (make-model-option-schema "reasoning-summary"
	                                                                               :values '(:auto :concise :detailed :none)
	                                                                               :default :auto)
	                                                     (make-model-option-schema "transport"
                                                                               :values '(:auto :sse :websocket :websocket-cached)
                                                                               :default :auto))
                               :transport-profile (list :max-output (getf m :max-out)
                                                        :developer-role t)))))

(defun retract-codex-provider (protocol contribution context)
  "Drain the contribution-state install returned, reversing every registration."
  (declare (ignore protocol))
  (retract-provider-catalogue (contribution-state contribution) context))

(defun refresh-codex-provider (protocol contribution context)
  "Refresh Codex's runtime-owned provider catalogue after boot snapshot reuse.

This drains and reinstalls only the provider catalogue state owned by this
contribution, including provider/model registrations, adapter references, and
provider-owned persisted OAuth credential restore."
  (retract-provider-catalogue (contribution-state contribution) context)
  (setf (contribution-state contribution)
        (install-codex-provider protocol contribution context))
  contribution)
