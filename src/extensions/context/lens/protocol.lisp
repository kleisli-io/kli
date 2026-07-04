(in-package #:kli/context/lens)

(defgeneric make-agent-context (session store context &key id leaf-id
                                  metadata))
(defgeneric rebuild-context-projection (agent-context &key leaf-id))
(defgeneric inspect-agent-context (agent-context))
(defgeneric stage-context-patch (agent-context patch))
(defgeneric commit-context-patches (agent-context context))
(defgeneric abort-context-patches (agent-context))
(defgeneric seal-context-projection (agent-context context &key extra-messages))
(defgeneric seal-range-projection (agent-context messages context))
(defgeneric context-model-messages (sealed-context))
(defgeneric context-epoch (agent-context))

(defun make-projection (&key entries messages leaf-id epoch)
  (make-instance 'context-projection
                 :entries entries
                 :messages messages
                 :leaf-id leaf-id
                 :epoch epoch))

(defun context-patch-entry-p (entry)
  (and (custom-entry-p entry)
       (eq (entry-custom-type entry) :context-patch-set)
       (typep (entry-data entry) 'context-patch-set)))

(defun branch-context-patch-sets (entries)
  (loop for entry in entries
        when (context-patch-entry-p entry)
          collect (entry-data entry)))

(defun committed-sets-epoch (patch-sets)
  "The context epoch: the highest result-epoch among committed PATCH-SETS, 0 when
none. A pure function of the committed sequence, so a rebuilt context can never
report a stale count -- the epoch is never stored or advanced on its own."
  (reduce #'max patch-sets
          :key #'context-patch-set-result-epoch
          :initial-value 0))

(defun message-position (messages payload)
  (let ((index (getf payload :index)))
    (cond
      (index index)
      ((getf payload :message-id)
       (position (getf payload :message-id)
                 messages
                 :key #'object-id
                 :test #'equal))
      (t nil))))

(defun replace-nth (list index value)
  (loop for item in list
        for position from 0
        collect (if (= position index) value item)))

(defun remove-nth (list index)
  (loop for item in list
        for position from 0
        unless (= position index)
          collect item))

(defun apply-context-patch (messages patch)
  (let ((payload (context-patch-payload patch)))
    (case (context-patch-kind patch)
      (:append-message
       (append messages (list (getf payload :message))))
      (:remove-message
       (let ((position (message-position messages payload)))
         (unless position
           (error "Context patch cannot find message to remove: ~S" patch))
         (remove-nth messages position)))
      (:replace-message
       (let ((position (message-position messages payload)))
         (unless position
           (error "Context patch cannot find message to replace: ~S" patch))
         (replace-nth messages position (getf payload :message))))
      (otherwise
       (error "Unsupported context patch kind: ~S"
              (context-patch-kind patch))))))

(defun apply-context-patches (messages patches)
  (reduce #'apply-context-patch patches :initial-value messages))

(defun all-committed-patches (patch-sets)
  (loop for patch-set in patch-sets
        append (context-patch-set-patches patch-set)))

(defun rebuild-context-from-session (agent-context leaf-id)
  (let* ((session (agent-context-session agent-context))
         (store (agent-context-store agent-context))
         (log-provider (agent-context-log-provider agent-context))
         (built (provider-call log-provider :build-session-context
                               store session :leaf-id leaf-id))
         (entries (session-context-entries built))
         (patch-sets (branch-context-patch-sets entries))
         (patches (all-committed-patches patch-sets))
         (messages (apply-context-patches
                    (session-context-messages built)
                    patches)))
    (setf (agent-context-leaf-id agent-context)
          (or leaf-id (session-leaf-id session))
          (context-committed-patches agent-context)
          patches
          (agent-context-projection agent-context)
          (make-projection :entries entries
                           :messages messages
                           :leaf-id (agent-context-leaf-id agent-context)
                           :epoch (committed-sets-epoch patch-sets)))
    (set-context-projected-messages agent-context messages)
    agent-context))

(defmethod make-agent-context (session store context
                               &key id leaf-id metadata)
  (let* ((protocol (active-protocol context))
         (log-provider (require-capability-provider protocol
                                                    :session/log
                                                    :contract :session/log/v1))
         (entries-provider (require-capability-provider protocol
                                                        :session/entries
                                                        :contract :session/entries/v1))
         (agent-context
           (make-instance 'agent-context
                          :id (or id (next-agent-context-id))
                          :session session
                          :store store
                          :log-provider log-provider
                          :entries-provider entries-provider
                          :leaf-id (or leaf-id (session-leaf-id session))
                          :projection (make-projection :epoch 0)
                          :capsule (make-context-capsule)
                          :metadata metadata)))
    (register-live-object (context-registry context) agent-context)
    (rebuild-context-projection agent-context
                                :leaf-id (agent-context-leaf-id agent-context))
    agent-context))

(defmethod rebuild-context-projection ((agent-context agent-context)
                                       &key leaf-id)
  (rebuild-context-from-session agent-context leaf-id))

(defmethod inspect-agent-context ((agent-context agent-context))
  (kli/ext:require-capability :context/read)
  (list :id (object-id agent-context)
        :session-id (object-id (agent-context-session agent-context))
        :leaf-id (agent-context-leaf-id agent-context)
        :epoch (context-epoch agent-context)
        :projection (inspect-context agent-context)))

(defmethod stage-context-patch ((agent-context agent-context)
                                (patch context-patch))
  (kli/ext:require-capability :context/stage-edit)
  (funcall (agent-context-capsule agent-context) :stage patch)
  patch)

(defmethod abort-context-patches ((agent-context agent-context))
  (kli/ext:require-capability :context/stage-edit)
  (let ((patches (context-staged-patches agent-context)))
    (funcall (agent-context-capsule agent-context) :clear-staged)
    patches))

(defmethod commit-context-patches ((agent-context agent-context) context)
  (kli/ext:require-capability :context/commit-edit)
  (let ((patches (context-staged-patches agent-context)))
    (unless patches
      (return-from commit-context-patches nil))
    (let* ((log-provider (agent-context-log-provider agent-context))
           (entries-provider (agent-context-entries-provider agent-context))
           (base-epoch (context-epoch agent-context))
           (result-epoch (1+ base-epoch))
           (patch-set (make-instance 'context-patch-set
                                     :patches patches
                                     :actor kli/ext:*call-subject*
                                     :timestamp (get-universal-time)
                                     :base-epoch base-epoch
                                     :result-epoch result-epoch))
           (entry (provider-call entries-provider :make-custom-entry
                                 :context-patch-set
                                 :data patch-set
                                 :source (object-id agent-context))))
      (provider-call log-provider :append-session-entry
                     (agent-context-store agent-context)
                     (agent-context-session agent-context)
                     entry
                     context)
      (funcall (agent-context-capsule agent-context) :clear-staged)
      (rebuild-context-projection
       agent-context
       :leaf-id (session-leaf-id (agent-context-session agent-context)))
      (emit-event context
                  (make-event :context/patch-committed
                              :payload patch-set
                              :source (object-id agent-context)))
      patch-set)))

(defmethod seal-range-projection ((agent-context agent-context)
                                  messages
                                  context)
  (declare (ignore context))
  (kli/ext:require-capability :context/seal)
  (make-instance 'sealed-context
                 :id (next-sealed-context-id)
                 :messages (copy-list messages)
                 :epoch (context-epoch agent-context)
                 :source-context-id (object-id agent-context)
                 :leaf-id (agent-context-leaf-id agent-context)
                 :timestamp (get-universal-time)))

(defmethod seal-context-projection ((agent-context agent-context)
                                    context &key extra-messages)
  ;; EXTRA-MESSAGES ride only this sealed snapshot, never the projection.
  (seal-range-projection agent-context
                         (append (projection-messages
                                  (agent-context-projection agent-context))
                                 extra-messages)
                         context))

(defmethod context-model-messages ((sealed-context sealed-context))
  (copy-list (sealed-context-messages sealed-context)))

(defmethod context-epoch ((agent-context agent-context))
  (projection-epoch (agent-context-projection agent-context)))
