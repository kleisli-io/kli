(in-package #:kli/session/log)

(defparameter *session-format-version* 1
  "Current on-disk format version. Bumped when the durable shape changes, after
which MIGRATE-SESSION-RECORD upgrades older records to it.")

(define-condition unserializable-value (error)
  ((value :initarg :value :reader unserializable-value-value)
   (reason :initarg :reason :initform nil :reader unserializable-value-reason))
  (:report
   (lambda (condition stream)
     (format stream "Cannot serialize ~S~@[: ~A~]"
             (unserializable-value-value condition)
             (unserializable-value-reason condition)))))

(defvar *record-types* (make-hash-table :test #'eq)
  "Set of registered record type keywords (a value of T marks membership).")

(defun record-type-known-p (type)
  (values (gethash type *record-types*)))

(defun make-record (type &rest plist)
  "Build the tagged record form (:record TYPE . PLIST)."
  (list* :record type plist))

(defun record-type (record)
  (second record))

(defun record-field (record key &optional default)
  "Read KEY from a record's slot plist."
  (getf (cddr record) key default))

(defun record-p (datum)
  "True iff DATUM is a tagged record of a registered type. A plain plist
beginning with :RECORD is treated as a record only when its type is registered,
so data never collides with the record form."
  (and (consp datum)
       (eq (first datum) :record)
       (record-type-known-p (second datum))))

(defun proper-list-p (object)
  (and (listp object)
       (loop for tail = object then (cdr tail)
             while (consp tail)
             finally (return (null tail)))))

(defun advance-keyword-counter (counter-symbol id)
  "If keyword ID ends in -N, raise COUNTER-SYMBOL's id-counter to at least N so
freshly minted ids never collide with reloaded ones. Returns ID."
  (when (keywordp id)
    (let* ((name (symbol-name id))
           (dash (position #\- name :from-end t))
           (n (and dash
                   (ignore-errors (parse-integer name :start (1+ dash))))))
      (when n
        (advance-counter counter-symbol n))))
  id)

(defgeneric serialize-value (value)
  (:documentation "Convert VALUE into a readable serialization datum. Leaves
pass through, proper lists recurse, and objects become records. Leaves are
keyword, string, integer, nil, t, or proper lists of the same, and anything
else signals UNSERIALIZABLE-VALUE rather than encoding lossily."))

(defmethod serialize-value ((value symbol))
  "Dispatches on SYMBOL because KEYWORD is a type, not a class, and cannot be a
method specializer. Keywords and the data atoms nil and t pass through. Any
other symbol is not serializable."
  (cond ((null value) nil)
        ((eq value t) t)
        ((keywordp value) value)
        (t (error 'unserializable-value
                  :value value :reason "non-keyword symbol"))))
(defmethod serialize-value ((value string)) value)
(defmethod serialize-value ((value integer)) value)

(defmethod serialize-value ((value cons))
  (unless (proper-list-p value)
    (error 'unserializable-value :value value :reason "improper list"))
  (mapcar #'serialize-value value))

(defmethod serialize-value ((value live-object))
  (serialize-record value))

(defmethod serialize-value ((value t))
  (error 'unserializable-value
         :value value :reason "no SERIALIZE-VALUE method for this type"))

(defun deserialize-value (datum)
  "Inverse of SERIALIZE-VALUE. Records become objects, other proper lists
recurse, and leaves pass through."
  (cond
    ((record-p datum) (deserialize-record (record-type datum) datum))
    ((consp datum) (mapcar #'deserialize-value datum))
    (t datum)))

(defgeneric serialize-record (object)
  (:documentation "Serialize OBJECT to a tagged (:record TYPE ...) form."))

(defmethod serialize-record ((object t))
  (error 'unserializable-value
         :value object :reason "no SERIALIZE-RECORD method for this class"))

(defgeneric deserialize-record (type record)
  (:documentation "Reconstruct an object from a (:record TYPE ...) form."))

(defmethod deserialize-record (type record)
  (declare (ignore record))
  (error 'unserializable-value
         :value type :reason "unknown record type"))

(defmacro define-record-deserializer (type (record-var) &body body)
  "Define a DESERIALIZE-RECORD method for the keyword TYPE and register TYPE so
RECORD-P recognises it."
  (let ((g!type (gensym "TYPE")))
    `(progn
       (setf (gethash ,type *record-types*) t)
       (defmethod deserialize-record ((,g!type (eql ,type)) ,record-var)
         (declare (ignore ,g!type))
         ,@body))))

(defgeneric migrate-session-record (record from-version)
  (:documentation "Upgrade RECORD read from a file at FROM-VERSION to the current
format. Identity at the current version, and the seam for future migrations."))

(defmethod migrate-session-record (record (from-version (eql 1)))
  record)

(defclass session-file-header ()
  ((format-version :initarg :format-version :reader header-format-version)
   (session-id :initarg :session-id :reader header-session-id)
   (session-uuid :initarg :session-uuid :initform nil :reader header-session-uuid)
   (timestamp :initarg :timestamp :initform nil :reader header-timestamp)
   (cwd :initarg :cwd :initform nil :reader header-cwd)
   (leaf-id :initarg :leaf-id :initform nil :reader header-leaf-id)
   (metadata :initarg :metadata :initform '() :reader header-metadata)))

(defun make-session-file-header (session-id
                                 &key (format-version *session-format-version*)
                                      session-uuid timestamp cwd leaf-id metadata)
  (make-instance 'session-file-header
                 :format-version format-version
                 :session-id session-id
                 :session-uuid session-uuid
                 :timestamp (timestamp-or-now timestamp)
                 :cwd cwd
                 :leaf-id leaf-id
                 :metadata metadata))

(defmethod serialize-record ((header session-file-header))
  (make-record :session-file-header
               :format-version (header-format-version header)
               :session-id (header-session-id header)
               :session-uuid (header-session-uuid header)
               :timestamp (header-timestamp header)
               :cwd (serialize-value (header-cwd header))
               :leaf-id (header-leaf-id header)
               :metadata (serialize-value (header-metadata header))))

(define-record-deserializer :session-file-header (record)
  (make-instance 'session-file-header
                 :format-version (record-field record :format-version)
                 :session-id (record-field record :session-id)
                 :session-uuid (record-field record :session-uuid)
                 :timestamp (record-field record :timestamp)
                 :cwd (deserialize-value (record-field record :cwd))
                 :leaf-id (record-field record :leaf-id)
                 :metadata (deserialize-value (record-field record :metadata))))

(defun message-base-plist (message)
  (list :id (object-id message)
        :role (message-role message)
        :content (serialize-value (message-content message))
        :timestamp (message-timestamp message)
        :metadata (serialize-value (message-metadata message))))

(defun deserialize-message-initargs (record)
  (advance-keyword-counter '*message-counter* (record-field record :id))
  (list :id (record-field record :id)
        :role (record-field record :role)
        :content (deserialize-value (record-field record :content))
        :timestamp (record-field record :timestamp)
        :metadata (deserialize-value (record-field record :metadata))))

(defmethod serialize-record ((message user-message))
  (apply #'make-record :user-message (message-base-plist message)))

(define-record-deserializer :user-message (record)
  (apply #'make-instance 'user-message (deserialize-message-initargs record)))

(defmethod serialize-record ((message assistant-message))
  (apply #'make-record :assistant-message (message-base-plist message)))

(define-record-deserializer :assistant-message (record)
  (apply #'make-instance 'assistant-message (deserialize-message-initargs record)))

(defmethod serialize-record ((message tool-result-message))
  (apply #'make-record :tool-result-message
         (append (message-base-plist message)
                 (list :tool-call-id (serialize-value (tool-call-id message))
                       :tool-name (serialize-value (tool-name message))
                       :error-p (tool-error-p message)))))

(define-record-deserializer :tool-result-message (record)
  (apply #'make-instance 'tool-result-message
         (append (deserialize-message-initargs record)
                 (list :tool-call-id (deserialize-value
                                      (record-field record :tool-call-id))
                       :tool-name (deserialize-value
                                   (record-field record :tool-name))
                       :error-p (record-field record :error-p)))))

(defmethod serialize-record ((message custom-agent-message))
  (apply #'make-record :custom-agent-message
         (append (message-base-plist message)
                 (list :custom-type (custom-message-type message)))))

(define-record-deserializer :custom-agent-message (record)
  (apply #'make-instance 'custom-agent-message
         (append (deserialize-message-initargs record)
                 (list :custom-type (record-field record :custom-type)))))

;; Only the durable (non-ephemeral) injection reaches here; per-turn ephemeral
;; harness context is recomputed each turn and never written. The trust class
;; must round-trip so a reloaded session lowers it to the same channel.
(defmethod serialize-record ((message harness-context-message))
  (apply #'make-record :harness-context-message
         (append (message-base-plist message)
                 (list :trust (message-trust message)))))

(define-record-deserializer :harness-context-message (record)
  (apply #'make-instance 'harness-context-message
         (append (deserialize-message-initargs record)
                 (list :trust (record-field record :trust)))))

(defun entry-base-plist (entry)
  (list :id (object-id entry)
        :parent-id (entry-parent-id entry)
        :timestamp (entry-timestamp entry)
        :source (entry-source entry)
        :metadata (serialize-value (entry-metadata entry))))

(defun deserialize-entry-initargs (record)
  (advance-keyword-counter '*entry-counter* (record-field record :id))
  (list :id (record-field record :id)
        :parent-id (record-field record :parent-id)
        :timestamp (record-field record :timestamp)
        :source (record-field record :source)
        :metadata (deserialize-value (record-field record :metadata))))

(defmethod serialize-record ((entry message-entry))
  (apply #'make-record :message-entry
         (append (entry-base-plist entry)
                 (list :message (serialize-value (entry-message entry))))))

(define-record-deserializer :message-entry (record)
  (apply #'make-instance 'message-entry
         (append (deserialize-entry-initargs record)
                 (list :message (deserialize-value
                                 (record-field record :message))))))

(defmethod serialize-record ((entry transcript-repair-entry))
  (apply #'make-record :transcript-repair-entry
         (append (entry-base-plist entry)
                 (list :repair-kind (entry-repair-kind entry)
                       :reason (serialize-value (entry-repair-reason entry))
                       :policy (serialize-value (entry-repair-policy entry))
                       :message (serialize-value (entry-message entry))))))

(define-record-deserializer :transcript-repair-entry (record)
  (apply #'make-instance 'transcript-repair-entry
         (append (deserialize-entry-initargs record)
                 (list :repair-kind (record-field record :repair-kind)
                       :reason (deserialize-value (record-field record :reason))
                       :policy (deserialize-value (record-field record :policy))
                       :message (deserialize-value
                                 (record-field record :message))))))

(defmethod serialize-record ((entry model-change-entry))
  (apply #'make-record :model-change-entry
         (append (entry-base-plist entry)
                 (list :provider (serialize-value (entry-provider entry))
                       :model-id (serialize-value (entry-model-id entry))
                       :options (serialize-value (entry-options entry))))))

(define-record-deserializer :model-change-entry (record)
  (apply #'make-instance 'model-change-entry
         (append (deserialize-entry-initargs record)
                 (list :provider (deserialize-value
                                  (record-field record :provider))
                       :model-id (deserialize-value
                                  (record-field record :model-id))
                       :options (deserialize-value
                                 (record-field record :options))))))

(defmethod serialize-record ((entry option-change-entry))
  (apply #'make-record :option-change-entry
         (append (entry-base-plist entry)
                 (list :option-id (serialize-value (entry-option-id entry))
                       :value (serialize-value (entry-option-value entry))))))

(define-record-deserializer :option-change-entry (record)
  (apply #'make-instance 'option-change-entry
         (append (deserialize-entry-initargs record)
                 (list :option-id (deserialize-value
                                   (record-field record :option-id))
                       :value (deserialize-value
                               (record-field record :value))))))

(defmethod serialize-record ((entry compaction-entry))
  (apply #'make-record :compaction-entry
         (append (entry-base-plist entry)
                 (list :summary (serialize-value (entry-summary entry))
                       :first-kept-entry-id (entry-first-kept-entry-id entry)
                       :tokens-before (entry-tokens-before entry)
                       :data (serialize-value (entry-data entry))))))

(define-record-deserializer :compaction-entry (record)
  (apply #'make-instance 'compaction-entry
         (append (deserialize-entry-initargs record)
                 (list :summary (deserialize-value (record-field record :summary))
                       :first-kept-entry-id (record-field record :first-kept-entry-id)
                       :tokens-before (record-field record :tokens-before 0)
                       :data (deserialize-value (record-field record :data))))))

(defmethod serialize-record ((entry branch-summary-entry))
  (apply #'make-record :branch-summary-entry
         (append (entry-base-plist entry)
                 (list :from-id (entry-from-id entry)
                       :summary (serialize-value (entry-summary entry))
                       :data (serialize-value (entry-data entry))))))

(define-record-deserializer :branch-summary-entry (record)
  (apply #'make-instance 'branch-summary-entry
         (append (deserialize-entry-initargs record)
                 (list :from-id (record-field record :from-id)
                       :summary (deserialize-value (record-field record :summary))
                       :data (deserialize-value (record-field record :data))))))

(defmethod serialize-record ((entry custom-entry))
  (apply #'make-record :custom-entry
         (append (entry-base-plist entry)
                 (list :custom-type (entry-custom-type entry)
                       :data (serialize-value (entry-data entry))))))

(define-record-deserializer :custom-entry (record)
  (apply #'make-instance 'custom-entry
         (append (deserialize-entry-initargs record)
                 (list :custom-type (record-field record :custom-type)
                       :data (deserialize-value (record-field record :data))))))

(defmethod serialize-record ((entry custom-message-entry))
  (apply #'make-record :custom-message-entry
         (append (entry-base-plist entry)
                 (list :custom-type (entry-custom-type entry)
                       :display-p (entry-display-p entry)
                       :data (serialize-value (entry-data entry))
                       :message (serialize-value (entry-message entry))))))

(define-record-deserializer :custom-message-entry (record)
  (apply #'make-instance 'custom-message-entry
         (append (deserialize-entry-initargs record)
                 (list :custom-type (record-field record :custom-type)
                       :display-p (record-field record :display-p)
                       :data (deserialize-value (record-field record :data))
                       :message (deserialize-value
                                 (record-field record :message))))))
