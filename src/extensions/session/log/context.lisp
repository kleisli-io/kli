(in-package #:kli/session/log)

(defun maybe-context-message (entry policy)
  (when (entry-enters-model-context-p entry policy)
    (entry-agent-message entry policy)))

(defun latest-compaction-entry (entries)
  (let ((latest nil))
    (dolist (entry entries latest)
      (when (typep entry 'compaction-entry)
        (setf latest entry)))))

(defun compacted-context-messages (entries compaction policy)
  "Build context messages from a compacted branch. The summary stands in for
everything dropped before the cut and enters as a user message, since a :custom
role would be discarded downstream."
  (let ((messages '())
        (first-kept (entry-first-kept-entry-id compaction))
        (keeping nil))
    (let ((summary (entry-summary compaction)))
      (when summary
        (push (make-user-message summary) messages)))
    (dolist (entry entries)
      (when (and (not keeping) (eql (object-id entry) first-kept))
        (setf keeping t))
      (when keeping
        (let ((message (maybe-context-message entry policy)))
          (when message
            (push message messages)))))
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
