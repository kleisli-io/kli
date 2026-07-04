(in-package #:kli/model/providers/openai)

(defparameter +provider-id+ "openai")
(defparameter +base-url+ "https://api.openai.com/v1")

(defparameter +openai-models+
  '((:id "gpt-5.1" :ctx 272000 :max-out 128000 :developer-role t)
    (:id "gpt-5.2" :ctx 272000 :max-out 128000 :developer-role t)
    (:id "gpt-5.4" :ctx 272000 :max-out 128000 :developer-role t)
    (:id "gpt-5.5" :ctx 272000 :max-out 128000 :developer-role t)))

(defun %model-transport-profile (m)
  (append (list :max-output (getf m :max-out))
          (when (getf m :developer-role) (list :developer-role t))))

(defun install-openai-provider (protocol contribution context)
  "Register the OpenAI catalogue -- env-keyed, Responses api -- via the shared
installer and return the contribution-state the retractor drains."
  (declare (ignore protocol contribution))
  (install-provider-catalogue
   context
   :provider-id +provider-id+
   :display-name "OpenAI"
   :api :openai-responses
   :config (make-provider-config :base-url +base-url+)
   :credential '(:env "OPENAI_API_KEY")
   :transport-profile '(:provider (:url-path "/responses"))
   :models (loop for m in +openai-models+
                 collect (list :id (getf m :id)
                               :name (getf m :id)
                               :context-window (getf m :ctx)
                               :option-schemas (list (make-model-option-schema "reasoning-effort"
                                                                               :values '(:off :minimal :low :medium :high :xhigh)
                                                                               :default :off))
                               :transport-profile (%model-transport-profile m)))))

(defun retract-openai-provider (protocol contribution context)
  "Drain the contribution-state install returned, reversing every registration."
  (declare (ignore protocol))
  (retract-provider-catalogue (contribution-state contribution) context))
