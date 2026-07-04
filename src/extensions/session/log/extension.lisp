(in-package #:kli/session/log)

(defun make-session-log-contract ()
  (make-provider-contract
   :id :session/log/v1
   :capability :session/log
   :required-entries
   '(:create-session
     :find-session
     :append-session-entry
     :session-leaf-entry
     :session-entry-by-id
     :session-branch
     :branch-session-at-entry
     :latest-entries-of-types
     :build-session-context)))

(defun make-session-log-provider ()
  (make-provider
   :id :session-log-provider
   :capability :session/log
   :contracts '(:session/log/v1)
   :entries
   (list :create-session #'create-session
         :find-session #'find-session
         :append-session-entry #'append-session-entry
         :session-leaf-entry #'session-leaf-entry
         :session-entry-by-id #'session-entry-by-id
         :session-branch #'session-branch
         :branch-session-at-entry #'branch-session-at-entry
         :latest-entries-of-types #'latest-entries-of-types
         :build-session-context #'build-session-context
         :repoint-session-leaf #'repoint-session-leaf)))

(defun make-session-entries-contract ()
  (make-provider-contract
   :id :session/entries/v1
   :capability :session/entries
   :required-entries
   '(:session-p
     :session-entry-p
     :message-entry-p
     :entry-parent-id
     :entry-timestamp
     :make-user-message
     :make-assistant-message
     :make-tool-result-message
     :make-message-entry
     :make-model-change-entry
     :make-option-change-entry
     :make-custom-entry
     :make-custom-message-entry)))

(defun make-session-entries-provider ()
  (make-provider
   :id :session-entries-provider
   :capability :session/entries
   :contracts '(:session/entries/v1)
   :entries
   (list :session-p #'session-p
         :session-entry-p #'session-entry-p
         :message-entry-p #'message-entry-p
         :entry-parent-id #'entry-parent-id
         :entry-timestamp #'entry-timestamp
         :make-user-message #'make-user-message
         :make-assistant-message #'make-assistant-message
         :make-tool-result-message #'make-tool-result-message
         :make-message-entry #'make-message-entry
         :make-model-change-entry #'make-model-change-entry
         :make-option-change-entry #'make-option-change-entry
         :make-custom-entry #'make-custom-entry
         :make-custom-message-entry #'make-custom-message-entry
         :make-compaction-entry #'make-compaction-entry
         :make-branch-summary-entry #'make-branch-summary-entry
         :make-custom-agent-message #'make-custom-agent-message
         :custom-entry-p #'custom-entry-p)))

(defextension session-log
  (:provides
   (contract session/log/v1
     (make-session-log-contract))
   (capability session/log (make-session-log-provider))
   (contract session/entries/v1
     (make-session-entries-contract))
   (capability session/entries (make-session-entries-provider))
   (live-object session-store
     (make-session-store))))
