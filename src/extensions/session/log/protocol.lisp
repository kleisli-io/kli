(in-package #:kli/session/log)

(defgeneric append-session-entry (store session entry context))
(defgeneric session-leaf-entry (store session))
(defgeneric session-entry-by-id (store session id))
(defgeneric session-branch (store session leaf-id))
(defgeneric branch-session-at-entry (store session entry-id context))
(defgeneric latest-entries-of-types (store session types))
(defgeneric build-session-context (store session &key leaf-id policy))
(defgeneric entry-enters-model-context-p (entry policy))
(defgeneric entry-agent-message (entry policy))
(defgeneric repoint-session-leaf (store session leaf-id context))

(defgeneric note-stored-session (store session context)
  (:documentation "Hook invoked at the end of STORE-SESSION. No-op for the
in-memory store. Durable stores persist the session here."))

(defmethod note-stored-session ((store session-store) session context)
  (declare (ignore store session context))
  nil)

(defun store-session (store session &optional context)
  (let ((id (object-id session)))
    (when (gethash id (store-sessions store))
      (error "Session already exists: ~S" id))
    (setf (gethash id (store-sessions store)) session)
    (when (and context
               (not (find-live-object (context-registry context) id)))
      (register-live-object (context-registry context) session))
    (note-stored-session store session context)
    session))

(defun create-session (store context &key id metadata)
  (store-session store
                 (make-session :id id :metadata metadata)
                 context))

(defun find-session (store id)
  (gethash id (store-sessions store)))

(defun session-entry-count (session)
  (sb-thread:with-recursive-lock ((session-lock session))
    (hash-table-count (session-entry-table session))))

(defun session-entries (session)
  (sb-thread:with-recursive-lock ((session-lock session))
    (mapcar (lambda (id)
              (gethash id (session-entry-table session)))
            (reverse (session-entry-order session)))))

(defmethod append-session-entry ((store session-store)
                                 (session session)
                                 (entry session-entry)
                                 context)
  (declare (ignore context))
  (unless (eq session (find-session store (object-id session)))
    (store-session store session))
  (sb-thread:with-recursive-lock ((session-lock session))
    (let ((id (object-id entry))
          (parent-id (entry-parent-id entry)))
      (when (gethash id (session-entry-table session))
        (error "Session entry already exists: ~S" id))
      (cond
        (parent-id
         (unless (gethash parent-id (session-entry-table session))
           (error "Session entry parent does not exist: ~S" parent-id)))
        ((session-leaf-id session)
         (setf (entry-parent-id entry) (session-leaf-id session))))
      (setf (gethash id (session-entry-table session)) entry)
      (push id (session-entry-order session))
      (setf (session-leaf-id session) id)
      entry)))

(defmethod repoint-session-leaf ((store session-store) (session session)
                                 leaf-id context)
  "Move the session leaf to LEAF-ID. Entries beyond it stay orphaned in the
append-only table but leave model context. NOTE-STORED-SESSION rewrites the
durable header's leaf-id so the move survives reload, a no-op in memory."
  (sb-thread:with-recursive-lock ((session-lock session))
    (setf (session-leaf-id session) leaf-id))
  (note-stored-session store session context)
  session)

(defmethod session-leaf-entry ((store session-store) (session session))
  (sb-thread:with-recursive-lock ((session-lock session))
    (let ((leaf-id (session-leaf-id session)))
      (and leaf-id
           (session-entry-by-id store session leaf-id)))))

(defmethod session-entry-by-id ((store session-store) (session session) id)
  (declare (ignore store))
  (sb-thread:with-recursive-lock ((session-lock session))
    (gethash id (session-entry-table session))))

(defmethod session-branch ((store session-store) (session session) leaf-id)
  (sb-thread:with-recursive-lock ((session-lock session))
    (let ((entries '())
          (current-id (or leaf-id (session-leaf-id session))))
      (loop while current-id
            for entry = (session-entry-by-id store session current-id)
            do (unless entry
                 (error "Session branch references missing entry: ~S"
                        current-id))
               (push entry entries)
               (setf current-id (entry-parent-id entry)))
      entries)))

(defun assistant-tool-call-ids (message)
  (when (and message (eq (message-role message) :assistant))
    (loop for call in (getf (message-metadata message) :tool-calls)
          for id = (getf call :id)
          when id collect id)))

(defun tool-result-call-id (message)
  (when (and message (eq (message-role message) :tool-result))
    (tool-call-id message)))

(defun unresolved-tool-call-entry (chain)
  (let ((pending '()))
    (dolist (entry chain)
      (let ((message (entry-cut-message entry)))
        (dolist (id (assistant-tool-call-ids message))
          (push (cons id entry) pending))
        (let ((result-id (tool-result-call-id message)))
          (when result-id
            (setf pending (remove result-id pending
                                  :key #'car :test #'equal))))))
    (cdr (car (last pending)))))

(defun branch-safe-chain (store session entry-id)
  (let* ((chain (session-branch store session entry-id))
         (unresolved (unresolved-tool-call-entry chain)))
    (if unresolved
        (let ((parent-id (entry-parent-id unresolved)))
          (if parent-id
              (session-branch store session parent-id)
              '()))
        chain)))

(defmethod branch-session-at-entry ((store session-store) (session session)
                                    entry-id context)
  (sb-thread:with-recursive-lock ((session-lock session))
    (unless (gethash entry-id (session-entry-table session))
      (error "Session branch references missing entry: ~S" entry-id)))
  (let* ((chain (branch-safe-chain store session entry-id))
         (leaf-id (and chain (object-id (car (last chain)))))
         (new-session
           (make-session
            :metadata (list :branched-from (object-id session)
                            :branched-at entry-id))))
    (dolist (entry chain)
      (setf (gethash (object-id entry) (session-entry-table new-session))
            entry))
    (setf (session-entry-order new-session)
          (reverse (mapcar #'object-id chain))
          (session-leaf-id new-session) leaf-id)
    (store-session store new-session context)
    new-session))

(defmethod latest-entries-of-types ((store session-store) (session session)
                                    types)
  (sb-thread:with-recursive-lock ((session-lock session))
    (let ((found '()))
      (dolist (id (session-entry-order session))
        (let ((entry (gethash id (session-entry-table session))))
          (when entry
            (dolist (type types)
              (when (and (typep entry type)
                         (not (getf found type)))
                (setf (getf found type) entry))))
          (when (and (consp types)
                     (every (lambda (type) (getf found type)) types))
            (return-from latest-entries-of-types found))))
      found)))

(defmethod entry-enters-model-context-p ((entry session-entry) (policy null))
  (declare (ignore entry))
  nil)

(defmethod entry-enters-model-context-p ((entry message-entry) (policy null))
  (declare (ignore entry))
  t)

(defmethod entry-enters-model-context-p ((entry custom-entry) (policy null))
  (declare (ignore entry))
  nil)

(defmethod entry-enters-model-context-p ((entry custom-message-entry)
                                         (policy null))
  (declare (ignore entry))
  t)

(defmethod entry-enters-model-context-p ((entry session-entry)
                                         (policy function))
  (declare (ignore entry policy))
  nil)

(defmethod entry-enters-model-context-p ((entry message-entry)
                                         (policy function))
  (funcall policy entry))

(defmethod entry-enters-model-context-p ((entry custom-entry)
                                         (policy function))
  (declare (ignore entry policy))
  nil)

(defmethod entry-enters-model-context-p ((entry custom-message-entry)
                                         (policy function))
  (funcall policy entry))

(defmethod entry-agent-message ((entry session-entry) policy)
  (declare (ignore entry policy))
  nil)

(defmethod entry-agent-message ((entry message-entry) policy)
  (declare (ignore policy))
  (entry-message entry))

(defmethod entry-agent-message ((entry custom-message-entry) policy)
  (declare (ignore policy))
  (entry-message entry))
