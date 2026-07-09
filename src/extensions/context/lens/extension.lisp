(in-package #:kli/context/lens)

(defun make-context-lens-contract ()
  (make-provider-contract
   :id :context/lens/v1
   :capability :context/lens
   :required-entries
   '(:make-agent-context
     :rebuild-context-projection
     :inspect-agent-context
     :stage-context-patch
     :commit-context-patches
     :abort-context-patches
     :seal-context-projection
     :seal-range-projection
     :transcript-display-items
     :summarizer-input-items
     :provider-replay-items
     :provider-replay-messages
     :context-epoch
     :make-append-message-patch
     :make-remove-message-patch
     :make-replace-message-patch
     :recode-context-capsule)))

(defun make-context-lens-provider ()
  (make-provider
   :id :context-lens-provider
   :capability :context/lens
   :contracts '(:context/lens/v1)
   :entries
   (list :make-agent-context #'make-agent-context
         :rebuild-context-projection #'rebuild-context-projection
         :inspect-agent-context #'inspect-agent-context
         :stage-context-patch #'stage-context-patch
         :commit-context-patches #'commit-context-patches
         :abort-context-patches #'abort-context-patches
         :seal-context-projection #'seal-context-projection
         :seal-range-projection #'seal-range-projection
         :transcript-display-items #'transcript-display-items
         :summarizer-input-items #'summarizer-input-items
         :provider-replay-items #'provider-replay-items
         :provider-replay-messages #'provider-replay-messages
         :context-epoch #'context-epoch
         :make-append-message-patch #'make-append-message-patch
         :make-remove-message-patch #'make-remove-message-patch
         :make-replace-message-patch #'make-replace-message-patch
         :recode-context-capsule #'recode-context-capsule)))

(defextension context-lens
  (:requires
   (capability events :contract events/v1)
   (capability session/log :contract session/log/v1)
   (capability session/entries :contract session/entries/v1))
  (:provides
   (contract context/lens/v1
     (make-context-lens-contract))
   (capability context/lens (make-context-lens-provider))
   (event-type :context/patch-committed)
   (method serialize-record () (context-patch) (patch)
     (make-record :context-patch
                  :id (object-id patch)
                  :kind (context-patch-kind patch)
                  :payload (serialize-value (context-patch-payload patch))
                  :actor (serialize-value (context-patch-actor patch))
                  :timestamp (context-patch-timestamp patch)
                  :metadata (serialize-value (context-patch-metadata patch))))
   (method serialize-value () (context-patch-set) (value)
     (serialize-record value))
   (method serialize-record () (context-patch-set) (set)
     (make-record :context-patch-set
                  :patches (serialize-value (context-patch-set-patches set))
                  :actor (serialize-value (context-patch-actor set))
                  :timestamp (context-patch-timestamp set)
                  :base-epoch (context-patch-set-base-epoch set)
                  :result-epoch (context-patch-set-result-epoch set)))
   (event-type :context/transcript-repair)
   (method serialize-value () (kli/ext:subject) (value)
     (serialize-record value))
   (method serialize-record () (kli/ext:subject) (subject)
     (make-record :subject
                  :grant (serialize-value
                          (kli/ext:grant->datum (kli/ext:subject-grant subject)))))))
