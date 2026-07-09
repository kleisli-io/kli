(in-package #:kli/context/lens)

(defgeneric make-agent-context (session store context &key id leaf-id
                                  metadata))
(defgeneric rebuild-context-projection (agent-context &key leaf-id))
(defgeneric inspect-agent-context (agent-context))
(defgeneric stage-context-patch (agent-context patch))
(defgeneric commit-context-patches (agent-context context))
(defgeneric abort-context-patches (agent-context))
(defgeneric seal-context-projection (agent-context context
                                     &key extra-messages repair-policy))
(defgeneric seal-range-projection (agent-context messages context
                                   &key view-kind policy repair-policy))
(defgeneric editable-context-view (source))
(defgeneric transcript-context-view (source))
(defgeneric summarizer-context-view (source))
(defgeneric provider-replay-context-view (source))
(defgeneric resolve-transcript-repair-policy (policy))
(defgeneric transcript-display-items (source))
(defgeneric summarizer-input-items (source))
(defgeneric provider-replay-items (source))
(defgeneric provider-replay-messages (source))
(defgeneric context-epoch (agent-context))

(defparameter +transcript-repair-policies+
  '(:fail-closed :synthesize-aborted))

(defun make-projection (&key entries messages leaf-id epoch message-provenance)
  (make-instance 'context-projection
                 :entries entries
                 :messages messages
                 :leaf-id leaf-id
                 :epoch epoch
                 :message-provenance (or message-provenance
                                         (make-hash-table :test #'equal))))

(defun require-context-view-payload-kind (kind)
  (unless (member kind +context-view-payload-kinds+ :test #'eq)
    (error "Unsupported context-view payload kind: ~S" kind))
  kind)

(defmethod resolve-transcript-repair-policy ((policy null))
  :fail-closed)

(defmethod resolve-transcript-repair-policy ((policy symbol))
  (unless (member policy +transcript-repair-policies+ :test #'eq)
    (error "Unsupported transcript repair policy: ~S" policy))
  policy)

(defmethod resolve-transcript-repair-policy ((policy cons))
  (resolve-transcript-repair-policy
   (getf policy :transcript-repair-policy :fail-closed)))

(defun make-context-view-payload (kind &key value metadata)
  (make-instance 'context-view-payload
                 :kind (require-context-view-payload-kind kind)
                 :value value
                 :metadata metadata))

(defun context-view-item-payload-kind (item)
  (context-view-payload-kind (context-view-item-payload item)))

(defun context-view-item-payload-value (item)
  (context-view-payload-value (context-view-item-payload item)))

(defun copy-equal-hash-table (table)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash key copy) (copy-list value)))
             table)
    copy))

(defun context-source-view (source)
  (etypecase source
    (sealed-context (sealed-context-view source))
    (context-view source)))

(defun context-view-provenance-for-item (view item-or-id)
  (let* ((source (context-source-view view))
         (id (if (typep item-or-id 'context-view-item)
                 (context-view-item-id item-or-id)
                 item-or-id)))
    (copy-list (gethash id (context-view-provenance source)))))

(defun session-message-provenance (entries)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries table)
      (let ((message (entry-agent-message entry nil)))
        (when message
          (setf (gethash (object-id message) table)
                (append
                 (list :entry-id (object-id entry)
                       :message-id (object-id message)
                       :source (if (typep entry 'transcript-repair-entry)
                                   :repair
                                   :message))
                 (when (typep entry 'transcript-repair-entry)
                   (list :repair-kind (entry-repair-kind entry)
                         :repair-policy
                         (entry-repair-policy entry))))))))))

(defun message-context-view-payload-kind (message)
  (cond
    ((getf (message-metadata message) :context-summary) :summary)
    ((getf (message-metadata message) :transcript-repair) :repair)
    ((eq (message-role message) :tool-result) :tool-result)
    (t :message)))

(defun message-context-view-item-id (message)
  (list (message-context-view-payload-kind message)
        (object-id message)))

(defun assistant-tool-call-group-id (message)
  (let ((ids (loop for call in (getf (message-metadata message) :tool-calls)
                   for id = (getf call :id)
                   when id collect id)))
    (cond
      ((null ids) nil)
      ((null (rest ids)) (list :tool-call (first ids)))
      (t (list :tool-calls ids)))))

(defun message-context-view-group-id (message)
  (case (message-role message)
    (:assistant (assistant-tool-call-group-id message))
    (:tool-result
     (when (tool-call-id message)
       (list :tool-call (tool-call-id message))))
    (otherwise nil)))

(defun fallback-message-provenance (message)
  (let ((metadata (message-metadata message)))
    (if (getf metadata :context-summary)
        (append
         (list :message-id (object-id message)
               :source :summary
               :source-summary-entry-id
               (getf metadata :source-summary-entry-id)
               :first-kept-entry-id
               (getf metadata :first-kept-entry-id)
               :covered-entry-ids
               (copy-list (getf metadata :covered-entry-ids)))
         (when (getf metadata :summary-kind)
           (list :summary-kind (getf metadata :summary-kind))))
        (list :message-id (object-id message)
              :source :projection))))

(defun make-context-view-item-from-message (message ordinal)
  (make-instance 'context-view-item
                 :id (message-context-view-item-id message)
                 :payload (make-context-view-payload
                           (message-context-view-payload-kind message)
                           :value message)
                 :ordinal ordinal
                 :group-id (message-context-view-group-id message)))

(defun build-context-view-from-messages
    (kind messages &key source-id source-epoch policy provenance-source)
  (let ((provenance (make-hash-table :test #'equal))
        (items '()))
    (loop for message in messages
          for ordinal from 0
          for item = (make-context-view-item-from-message message ordinal)
          for id = (context-view-item-id item)
          for message-provenance = (and provenance-source
                                        (gethash (object-id message)
                                                 provenance-source))
          do (push item items)
             (setf (gethash id provenance)
                   (copy-list (or message-provenance
                                  (fallback-message-provenance message)))))
    (make-instance 'context-view
                   :kind kind
                   :source-id source-id
                   :source-epoch source-epoch
                   :items (nreverse items)
                   :provenance provenance
                   :policy policy)))

(defun seal-context-view (view sealed-id leaf-id sealed-at)
  (make-instance 'sealed-context-view
                 :sealed-id sealed-id
                 :base-view-kind (context-view-kind view)
                 :leaf-id leaf-id
                 :sealed-at sealed-at
                 :kind (context-view-kind view)
                 :source-id (context-view-source-id view)
                 :source-epoch (context-view-source-epoch view)
                 :items (copy-list (context-view-items view))
                 :provenance (copy-equal-hash-table
                              (context-view-provenance view))
                 :policy (context-view-policy view)))

(defun rebind-context-view-kind (view kind)
  (make-instance 'context-view
                 :kind kind
                 :source-id (context-view-source-id view)
                 :source-epoch (context-view-source-epoch view)
                 :items (copy-list (context-view-items view))
                 :provenance (copy-equal-hash-table
                              (context-view-provenance view))
                 :policy (context-view-policy view)))

(defun materialize-message-items (items)
  (loop for item in items
        for kind = (context-view-item-payload-kind item)
        when (member kind '(:message :tool-result :repair :summary) :test #'eq)
          collect (context-view-item-payload-value item)))

(defun message-tool-calls (message)
  (when (and message (eq (message-role message) :assistant))
    (copy-list (getf (message-metadata message) :tool-calls))))

(defun provider-replay-diagnostic (items repair-policy)
  (let ((pending '())
        (orphan-results '()))
    (dolist (item items)
      (let ((message (context-view-item-payload-value item)))
        (dolist (call (message-tool-calls message))
          (let ((call-id (getf call :id)))
            (when call-id
              (push (list :id call-id
                          :name (getf call :name)
                          :item-id (context-view-item-id item)
                          :ordinal (context-view-item-ordinal item))
                    pending))))
        (when (member (context-view-item-payload-kind item)
                      '(:tool-result :repair)
                      :test #'eq)
          (let ((call-id (tool-call-id message)))
            (if (find call-id pending
                      :key (lambda (call)
                             (getf call :id))
                      :test #'equal)
                (setf pending (remove call-id pending
                                      :key (lambda (call)
                                             (getf call :id))
                                      :test #'equal
                                      :count 1))
                (push (list :id call-id
                            :item-id (context-view-item-id item)
                            :ordinal (context-view-item-ordinal item))
                      orphan-results))))))
    (when (or pending orphan-results)
      (list :kind :provider-replay-invalid
            :repair-policy repair-policy
            :missing-tool-results (nreverse pending)
            :orphan-tool-results (nreverse orphan-results)))))

(defun validate-provider-replay-view (view)
  (let* ((policy (resolve-transcript-repair-policy
                  (context-view-policy view)))
         (diagnostic (provider-replay-diagnostic
                      (context-view-items view)
                      policy)))
    (when diagnostic
      (error 'context-view-validation-error :diagnostic diagnostic)))
  view)

(defun repairable-missing-calls (messages repair-policy)
  (let* ((view (build-context-view-from-messages
                :provider-replay messages
                :policy (list :transcript-repair-policy repair-policy)))
         (diagnostic (provider-replay-diagnostic
                      (context-view-items view)
                      repair-policy))
         (missing (getf diagnostic :missing-tool-results))
         (orphans (getf diagnostic :orphan-tool-results))
         (last-ordinal (1- (length messages))))
    (when (and missing
               (null orphans)
               (every (lambda (call)
                        (= (getf call :ordinal) last-ordinal))
                      missing))
      missing)))

(defun synthesize-transcript-repairs
    (agent-context context messages repair-policy)
  (let ((missing (repairable-missing-calls messages repair-policy)))
    (unless missing
      (return-from synthesize-transcript-repairs nil))
    (dolist (call missing)
      (let* ((entry (make-transcript-repair-entry
                    :tool-call-id (getf call :id)
                    :tool-name (getf call :name)
                    :reason :missing-tool-result
                    :policy repair-policy
                    :source (object-id agent-context)))
             (log-provider (agent-context-log-provider agent-context)))
        (provider-call log-provider :append-session-entry
                       (agent-context-store agent-context)
                       (agent-context-session agent-context)
                       entry
                       context)
        (emit-event context
                    (make-event :context/transcript-repair
                                :payload (list :entry-id (object-id entry)
                                               :tool-call-id (getf call :id)
                                               :policy repair-policy)
                                :source (object-id agent-context)))))
    (rebuild-context-projection
     agent-context
     :leaf-id (session-leaf-id (agent-context-session agent-context)))
    missing))

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
                    patches))
         (message-provenance (session-message-provenance entries)))
    (setf (agent-context-leaf-id agent-context)
          (or leaf-id (session-leaf-id session))
          (context-committed-patches agent-context)
          patches
          (agent-context-projection agent-context)
          (make-projection :entries entries
                           :messages messages
                           :leaf-id (agent-context-leaf-id agent-context)
                           :epoch (committed-sets-epoch patch-sets)
                           :message-provenance message-provenance))
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
                                  context
                                  &key (view-kind :editable) policy
                                    repair-policy)
  (declare (ignore context))
  (kli/ext:require-capability :context/seal)
  (let* ((repair-policy (resolve-transcript-repair-policy
                         (or repair-policy policy)))
         (policy (or policy
                     (list :transcript-repair-policy repair-policy)))
         (sealed-id (next-sealed-context-id))
         (timestamp (get-universal-time))
         (projection (agent-context-projection agent-context))
         (view (build-context-view-from-messages
                view-kind
                messages
                :source-id (object-id agent-context)
                :source-epoch (context-epoch agent-context)
                :policy policy
                :provenance-source (projection-message-provenance projection))))
    (make-instance 'sealed-context
                   :id sealed-id
                   :messages (copy-list messages)
                   :view (seal-context-view view
                                            sealed-id
                                            (agent-context-leaf-id agent-context)
                                            timestamp)
                   :epoch (context-epoch agent-context)
                   :source-context-id (object-id agent-context)
                   :leaf-id (agent-context-leaf-id agent-context)
                   :timestamp timestamp)))

(defmethod seal-context-projection ((agent-context agent-context)
                                    context &key extra-messages repair-policy)
  ;; EXTRA-MESSAGES ride only this sealed snapshot, never the projection.
  (let* ((repair-policy (resolve-transcript-repair-policy repair-policy))
         (base-messages (projection-messages
                         (agent-context-projection agent-context))))
    (when (eq repair-policy :synthesize-aborted)
      (synthesize-transcript-repairs
       agent-context context base-messages repair-policy)
      (setf base-messages (projection-messages
                           (agent-context-projection agent-context))))
    (seal-range-projection agent-context
                           (append base-messages extra-messages)
                           context
                           :repair-policy repair-policy)))

(defmethod editable-context-view ((source sealed-context))
  (sealed-context-view source))

(defmethod editable-context-view ((source context-view))
  (rebind-context-view-kind source :editable))

(defmethod transcript-context-view ((source sealed-context))
  (transcript-context-view (sealed-context-view source)))

(defmethod transcript-context-view ((source context-view))
  (rebind-context-view-kind source :transcript))

(defmethod summarizer-context-view ((source sealed-context))
  (summarizer-context-view (sealed-context-view source)))

(defmethod summarizer-context-view ((source context-view))
  (rebind-context-view-kind source :summarizer))

(defmethod provider-replay-context-view ((source sealed-context))
  (provider-replay-context-view (sealed-context-view source)))

(defmethod provider-replay-context-view ((source context-view))
  (validate-provider-replay-view
   (rebind-context-view-kind source :provider-replay)))

(defmethod transcript-display-items ((source sealed-context))
  (transcript-display-items (transcript-context-view source)))

(defmethod transcript-display-items ((source context-view))
  (materialize-message-items (context-view-items source)))

(defmethod summarizer-input-items ((source sealed-context))
  (summarizer-input-items (summarizer-context-view source)))

(defmethod summarizer-input-items ((source context-view))
  (copy-list (context-view-items source)))

(defmethod provider-replay-items ((source sealed-context))
  (provider-replay-items (provider-replay-context-view source)))

(defmethod provider-replay-items ((source context-view))
  (copy-list (context-view-items source)))

(defmethod provider-replay-messages ((source sealed-context))
  (provider-replay-messages (provider-replay-context-view source)))

(defmethod provider-replay-messages ((source context-view))
  (materialize-message-items (context-view-items source)))

(defmethod context-epoch ((agent-context agent-context))
  (projection-epoch (agent-context-projection agent-context)))
