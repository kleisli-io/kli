(in-package #:kli/model/providers/anthropic)

(defparameter +provider-id+ "anthropic")
(defparameter +base-url+ "https://api.anthropic.com")

(defparameter +anthropic-models+
  '((:id "claude-opus-4-8" :ctx 1000000 :max-out 128000
     :thinking-mode :adaptive :xhigh-effort "xhigh" :midstream-system t)
    (:id "claude-opus-4-7" :ctx 1000000 :max-out 128000
     :thinking-mode :adaptive :xhigh-effort "xhigh")
    (:id "claude-opus-4-6" :ctx 1000000 :max-out 128000
     :thinking-mode :adaptive :xhigh-effort "max")
    (:id "claude-sonnet-4-6" :ctx 1000000 :max-out 64000
     :thinking-mode :adaptive :xhigh-effort "max")
    (:id "claude-sonnet-4-5" :ctx 200000 :max-out 64000
     :thinking-mode :budget)
    (:id "claude-haiku-4-5" :ctx 200000 :max-out 64000
     :thinking-mode :budget)))

(defun %model-transport-profile (m)
  (append (list :max-output (getf m :max-out)
                :thinking-mode (getf m :thinking-mode))
          (let ((effort (getf m :xhigh-effort)))
            (when effort (list :xhigh-effort effort)))
          (when (getf m :midstream-system) (list :midstream-system t))))

(defun install-anthropic-provider (protocol contribution context)
  "Register the Anthropic catalogue -- env-keyed, Messages api -- via the
shared installer and return the contribution-state the retractor drains."
  (declare (ignore protocol contribution))
  (install-provider-catalogue
   context
   :provider-id +provider-id+
   :display-name "Anthropic"
   :api :anthropic-messages
   :config (make-provider-config :base-url +base-url+)
   :credential '(:env "ANTHROPIC_API_KEY")
   :models (loop for m in +anthropic-models+
                 collect (list :id (getf m :id)
                               :name (getf m :id)
                               :context-window (getf m :ctx)
                               :option-schemas (list (make-model-option-schema "reasoning-effort"
                                                                               :values '(:off :minimal :low :medium :high :xhigh)
                                                                               :default :off))
                               :transport-profile (%model-transport-profile m)))))

(defun retract-anthropic-provider (protocol contribution context)
  "Drain the contribution-state install returned, reversing every registration."
  (declare (ignore protocol))
  (retract-provider-catalogue (contribution-state contribution) context))

(defun refresh-anthropic-provider (protocol contribution context)
  "Refresh Anthropic's runtime-owned provider catalogue after boot snapshot reuse.

This drains and reinstalls only the provider catalogue state owned by this
contribution, which includes provider/model registrations, adapter references,
the env credential reference, and provider-owned persisted credential restore."
  (retract-provider-catalogue (contribution-state contribution) context)
  (setf (contribution-state contribution)
        (install-anthropic-provider protocol contribution context))
  contribution)
