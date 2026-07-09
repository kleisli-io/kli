(in-package #:kli/session/log)

(defun maybe-context-message (entry policy)
  (when (entry-enters-model-context-p entry policy)
    (entry-agent-message entry policy)))

(defun latest-compaction-entry (entries)
  (let ((latest nil))
    (dolist (entry entries latest)
      (when (typep entry 'compaction-entry)
        (setf latest entry)))))

(defun compaction-summary-message (compaction entries)
  (let ((summary (entry-summary compaction)))
    (when summary
      (make-user-message
       summary
       :id (list :compaction-summary (object-id compaction))
       :metadata (list :context-summary t
                       :source-summary-entry-id (object-id compaction)
                       :first-kept-entry-id
                       (entry-first-kept-entry-id compaction)
                       :covered-entry-ids
                       (loop for entry in entries
                             until (eql (object-id entry)
                                        (entry-first-kept-entry-id compaction))
                             collect (object-id entry))
                       :summary-kind :compaction)))))

(defun compacted-context-messages (entries compaction policy)
  "Build context messages from a compacted branch. The summary stands in for
everything dropped before the cut and enters as a user message, since a :custom
role would be discarded downstream."
  (let ((messages '())
        (first-kept (entry-first-kept-entry-id compaction))
        (keeping nil)
        (skip-tool-result-ids '())
        (completed-tool-result-ids
          (loop for entry in entries
                for message = (when (typep entry 'message-entry)
                                (entry-message entry))
                for call-id = (tool-result-call-id message)
                when call-id collect call-id)))
    (let ((summary (compaction-summary-message compaction entries)))
      (when summary
        (push summary messages)))
    (dolist (entry entries)
      (when (and (not keeping) (eql (object-id entry) first-kept))
        (setf keeping t))
      (when keeping
        (let ((message (maybe-context-message entry policy)))
          (when message
            (let ((call-ids (assistant-tool-call-ids message))
                  (result-id (tool-result-call-id message)))
              (cond
                ((and call-ids
                      (eql (object-id entry) first-kept)
                      (every (lambda (id)
                               (member id completed-tool-result-ids
                                       :test #'equal))
                             call-ids))
                 (setf skip-tool-result-ids
                       (append call-ids skip-tool-result-ids)))
                ((and result-id
                      (member result-id skip-tool-result-ids :test #'equal))
                 (setf skip-tool-result-ids
                       (remove result-id skip-tool-result-ids
                               :test #'equal
                               :count 1)))
                (t
                 (push message messages))))))))
    (nreverse messages)))

(defun uncompacted-context-messages (entries policy)
  (let ((messages '()))
    (dolist (entry entries)
      (let ((message (maybe-context-message entry policy)))
        (when message
          (push message messages))))
    (nreverse messages)))

(defmethod build-session-context ((store session-store)
                                  (session session)
                                  &key leaf-id policy)
  (let ((entries (session-branch store session leaf-id))
        (model nil)
        (options '()))
    (dolist (entry entries)
      (typecase entry
        (model-change-entry
         (setf options (copy-list (entry-options entry))
               model (list :provider (entry-provider entry)
                           :model-id (entry-model-id entry)
                           :options options)))
        (option-change-entry
         (setf (getf options (entry-option-id entry))
               (entry-option-value entry))
         (when model
           (setf (getf model :options) options)))))
    (let* ((compaction (latest-compaction-entry entries))
           (messages (if compaction
                         (compacted-context-messages entries compaction policy)
                         (uncompacted-context-messages entries policy))))
      (make-instance 'session-context
                     :entries entries
                     :messages messages
                     :model model
                     :options options
                     :leaf-id (or leaf-id (session-leaf-id session))))))
