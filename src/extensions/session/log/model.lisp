(in-package #:kli/session/log)

(defvar *session-counter* (make-id-counter))
(defvar *entry-counter* (make-id-counter))
(defvar *message-counter* (make-id-counter))

(defun next-session-id ()
  (next-keyword-id "SESSION" '*session-counter*))

(defun next-entry-id ()
  (next-keyword-id "SESSION-ENTRY" '*entry-counter*))

(defun next-message-id ()
  (next-keyword-id "AGENT-MESSAGE" '*message-counter*))

(defclass session-store (live-object)
  ((sessions
    :initform (make-hash-table :test #'equal)
    :accessor store-sessions)))

(defclass session (live-object)
  ((entries
    :initform (make-hash-table :test #'equal)
    :accessor session-entry-table)
   (entry-order
    :initform '()
    :accessor session-entry-order)
   (leaf-id
    :initform nil
    :accessor session-leaf-id)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor session-metadata)
   (uuid
    :initarg :uuid
    :initform nil
    :accessor session-uuid
    :documentation "Globally-unique session identity (OS-CSPRNG hex), persisted in
the session-file header and used as the provider cache key.")
   (desynced
    :initform nil
    :accessor session-desynced-p
    :documentation "Set when a durable append faulted and left the file behind
memory. The next append rewrites the whole file from memory instead of
appending incrementally, healing the gap, and clears this on success.")
   (lock
    :initform (sb-thread:make-mutex :name "session")
    :reader session-lock
    :documentation "Guards the entry-table/entry-order/leaf-id triple so they
move together under concurrent appends and reads. Recursive, because reads
compose. An unsynchronized table write also corrupts SBCL's chains outright, so
this is required, not an optimization.")))

(defclass session-entry (live-object)
  ((parent-id
    :initarg :parent-id
    :initform nil
    :accessor entry-parent-id)
   (timestamp
    :initarg :timestamp
    :initform nil
    :accessor entry-timestamp)
   (source
    :initarg :source
    :initform nil
    :accessor entry-source)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor entry-metadata)))

(defclass message-entry (session-entry)
  ((message
    :initarg :message
    :accessor entry-message)))

(defclass model-change-entry (session-entry)
  ((provider
    :initarg :provider
    :accessor entry-provider)
   (model-id
    :initarg :model-id
    :accessor entry-model-id)
   (options
    :initarg :options
    :initform '()
    :accessor entry-options)))

(defclass option-change-entry (session-entry)
  ((option-id
    :initarg :option-id
    :accessor entry-option-id)
   (value
    :initarg :value
    :accessor entry-option-value)))

(defclass compaction-entry (session-entry)
  ((summary
    :initarg :summary
    :accessor entry-summary)
   (first-kept-entry-id
    :initarg :first-kept-entry-id
    :accessor entry-first-kept-entry-id)
   (tokens-before
    :initarg :tokens-before
    :initform 0
    :accessor entry-tokens-before)
   (data
    :initarg :data
    :initform nil
    :accessor entry-data)))

(defclass branch-summary-entry (session-entry)
  ((from-id
    :initarg :from-id
    :accessor entry-from-id)
   (summary
    :initarg :summary
    :accessor entry-summary)
   (data
    :initarg :data
    :initform nil
    :accessor entry-data)))

(defclass custom-entry (session-entry)
  ((custom-type
    :initarg :custom-type
    :accessor entry-custom-type)
   (data
    :initarg :data
    :initform nil
    :accessor entry-data)))

(defclass custom-message-entry (message-entry)
  ((custom-type
    :initarg :custom-type
    :accessor entry-custom-type)
   (display-p
    :initarg :display-p
    :initform t
    :accessor entry-display-p)
   (data
    :initarg :data
    :initform nil
    :accessor entry-data)))

(defclass agent-message (live-object)
  ((role
    :initarg :role
    :reader message-role)
   (content
    :initarg :content
    :initform nil
    :accessor message-content)
   (timestamp
    :initarg :timestamp
    :initform nil
    :accessor message-timestamp)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor message-metadata)))

(defclass user-message (agent-message) ())
(defclass assistant-message (agent-message) ())

(defclass tool-result-message (agent-message)
  ((tool-call-id
    :initarg :tool-call-id
    :initform nil
    :accessor tool-call-id)
   (tool-name
    :initarg :tool-name
    :initform nil
    :accessor tool-name)
   (error-p
    :initarg :error-p
    :initform nil
    :accessor tool-error-p)))

(defclass custom-agent-message (agent-message)
  ((custom-type
    :initarg :custom-type
    :accessor custom-message-type)))

(defclass harness-context-message (agent-message)
  ((trust
    :initarg :trust
    :reader message-trust
    :documentation "Immutable trust class: :operator | :reference. No writer."))
  (:documentation "Harness-injected context. kli lowers it to a channel per
trust; harness content is never authored as :role :user."))

(defclass session-context ()
  ((entries
    :initarg :entries
    :reader session-context-entries)
   (messages
    :initarg :messages
    :reader session-context-messages)
   (model
    :initarg :model
    :initform nil
    :reader session-context-model)
   (options
    :initarg :options
    :initform '()
    :reader session-context-options)
   (leaf-id
    :initarg :leaf-id
    :initform nil
    :reader session-context-leaf-id)))

(defun timestamp-or-now (timestamp)
  (or timestamp (get-universal-time)))

(defun make-session-store (&key (id :session-store))
  (make-instance 'session-store :id id))

(defun mint-session-uuid ()
  "A fresh 32-hex session identity from the OS CSPRNG."
  (ironclad:byte-array-to-hex-string
   (ironclad:random-data 16 (ironclad:make-prng :os))))

(defun make-session (&key id metadata (uuid :mint))
  "UUID defaults to a fresh mint; pass an explicit value to take it verbatim, as
when reinstating a stored session from its header."
  (make-instance 'session
                 :id (or id (next-session-id))
                 :metadata metadata
                 :uuid (if (eq uuid :mint) (mint-session-uuid) uuid)))

(defun make-agent-message (class role content &key id timestamp metadata)
  (make-instance class
                 :id (or id (next-message-id))
                 :role role
                 :content content
                 :timestamp (timestamp-or-now timestamp)
                 :metadata metadata))

(defun make-user-message (content &key id timestamp metadata)
  (make-agent-message 'user-message :user content
                      :id id
                      :timestamp timestamp
                      :metadata metadata))

(defun make-assistant-message (content &key id timestamp metadata)
  (make-agent-message 'assistant-message :assistant content
                      :id id
                      :timestamp timestamp
                      :metadata metadata))

(defun make-tool-result-message (content &key id timestamp metadata
                                           tool-call-id tool-name error-p)
  (make-instance 'tool-result-message
                 :id (or id (next-message-id))
                 :role :tool-result
                 :content content
                 :timestamp (timestamp-or-now timestamp)
                 :metadata metadata
                 :tool-call-id tool-call-id
                 :tool-name tool-name
                 :error-p error-p))

(defun make-custom-agent-message (custom-type content &key id timestamp metadata)
  (make-instance 'custom-agent-message
                 :id (or id (next-message-id))
                 :role :custom
                 :custom-type custom-type
                 :content content
                 :timestamp (timestamp-or-now timestamp)
                 :metadata metadata))

(defparameter +harness-trust-classes+ '(:operator :reference))

(defun %coerce-harness-trust (trust)
  "Honor only an explicit known class; absent/unknown lowers to :reference."
  (if (member trust +harness-trust-classes+) trust :reference))

(defun make-harness-context-message (content &key trust ephemeral metadata)
  (make-instance 'harness-context-message
                 :id (next-message-id)
                 :role :harness-context
                 :trust (%coerce-harness-trust trust)
                 :content content
                 :timestamp (timestamp-or-now nil)
                 :metadata (list* :ephemeral (and ephemeral t) metadata)))

(defun make-session-entry-instance (class &rest initargs)
  (apply #'make-instance
         class
         :id (or (getf initargs :id) (next-entry-id))
         :timestamp (timestamp-or-now (getf initargs :timestamp))
         (loop for (key value) on initargs by #'cddr
               unless (member key '(:id :timestamp))
               append (list key value))))

(defun make-message-entry (message &key id parent-id timestamp source metadata)
  (make-session-entry-instance 'message-entry
                               :id id
                               :parent-id parent-id
                               :timestamp timestamp
                               :source source
                               :metadata metadata
                               :message message))

(defun make-model-change-entry (provider model-id &key options id parent-id timestamp
                                         source metadata)
  (make-session-entry-instance 'model-change-entry
                               :id id
                               :parent-id parent-id
                               :timestamp timestamp
                               :source source
                               :metadata metadata
                               :provider provider
                               :model-id model-id
                               :options options))

(defun make-option-change-entry (option-id value &key id parent-id timestamp
                                                  source metadata)
  (make-session-entry-instance 'option-change-entry
                               :id id
                               :parent-id parent-id
                               :timestamp timestamp
                               :source source
                               :metadata metadata
                               :option-id option-id
                               :value value))

(defun make-compaction-entry (summary first-kept-entry-id &key id parent-id
                                      timestamp source metadata tokens-before data)
  (make-session-entry-instance 'compaction-entry
                               :id id
                               :parent-id parent-id
                               :timestamp timestamp
                               :source source
                               :metadata metadata
                               :summary summary
                               :first-kept-entry-id first-kept-entry-id
                               :tokens-before (or tokens-before 0)
                               :data data))

(defun make-branch-summary-entry (from-id summary &key id parent-id timestamp
                                                  source metadata data)
  (make-session-entry-instance 'branch-summary-entry
                               :id id
                               :parent-id parent-id
                               :timestamp timestamp
                               :source source
                               :metadata metadata
                               :from-id from-id
                               :summary summary
                               :data data))

(defun make-custom-entry (custom-type &key id parent-id timestamp source
                                      metadata data)
  (make-session-entry-instance 'custom-entry
                               :id id
                               :parent-id parent-id
                               :timestamp timestamp
                               :source source
                               :metadata metadata
                               :custom-type custom-type
                               :data data))

(defun make-custom-message-entry (custom-type content &key id parent-id
                                              timestamp source metadata data
                                              display-p)
  (make-session-entry-instance
   'custom-message-entry
   :id id
   :parent-id parent-id
   :timestamp timestamp
   :source source
   :metadata metadata
   :custom-type custom-type
   :display-p (if (null display-p) t display-p)
   :data data
   :message (make-custom-agent-message custom-type content
                                       :timestamp timestamp
                                       :metadata metadata)))

(defun session-p (object)
  (typep object 'session))

(defun agent-message-p (object)
  (typep object 'agent-message))

(defun session-entry-p (object)
  (typep object 'session-entry))

(defun message-entry-p (object)
  (typep object 'message-entry))

(defun custom-entry-p (object)
  (typep object 'custom-entry))
