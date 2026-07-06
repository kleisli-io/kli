(in-package #:kli/model/runtime)

(defvar *model-request-counter* (make-id-counter))
(defvar *model-stream-counter* (make-id-counter))
(defvar *model-response-counter* (make-id-counter))
(defvar *model-delta-counter* (make-id-counter))

(defun next-model-request-id ()
  (next-keyword-id "MODEL-REQUEST" '*model-request-counter*))

(defun next-model-stream-id ()
  (next-keyword-id "MODEL-STREAM" '*model-stream-counter*))

(defun next-model-response-id ()
  (next-keyword-id "MODEL-RESPONSE" '*model-response-counter*))

(defun next-model-delta-id ()
  ;; Uninterned: a keyword per delta would grow the keyword package with
  ;; every streamed token for the life of the image.
  (make-symbol (format nil "MODEL-DELTA-~D"
                       (next-counter-value '*model-delta-counter*))))

(defparameter *completed-request-history-limit* 8
  "Finished request/stream/response triples kept in the runtime tables.
Completions beyond this evict the oldest triple from the tables and the live
registry, so the registries stay bounded for the life of the image.")

(defclass model-runtime (live-object)
  ;; Requests, streams, and responses register from the agent worker thread
  ;; while inspection and projection read from the TUI loop thread; adapters
  ;; install from the loop thread and resolve from the worker. All tables
  ;; are cross-thread and must be synchronized.
  ((requests
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor runtime-requests)
   (streams
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor runtime-streams)
   (responses
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor runtime-responses)
   (stream-adapters
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor runtime-stream-adapters)
   (stream-adapter-refcounts
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor runtime-stream-adapter-refcounts
    :documentation "Per-api install refcount: register increments, unregister
decrements. The adapter fn is dropped only at zero, so coexisting providers
sharing an api each hold a reference.")
   (completed-history
    :initform '()
    :accessor runtime-completed-history
    :documentation "Newest-first (request-id stream-id response-id) triples of
finished requests, bounded by *completed-request-history-limit*. Trimming
evicts a triple's components from the tables and the live registry.")
   (completed-history-lock
    :initform (sb-thread:make-mutex :name "model-runtime-completed-history")
    :reader runtime-completed-history-lock
    :documentation "Completions race across workers -- the agent loop and
command workers finish requests concurrently.")))

(defclass model-request (live-object)
  ((selection
    :initarg :selection
    :reader model-request-selection)
   (provider
    :initarg :provider
    :reader model-request-provider)
   (provider-id
    :initarg :provider-id
    :reader model-request-provider-id)
   (model-id
    :initarg :model-id
    :reader model-request-model-id)
   (sealed-context
    :initarg :sealed-context
    :accessor model-request-sealed-context)
   (state
    :initarg :state
    :initform :pending
    :accessor model-request-state)
   (model-messages
    :initarg :model-messages
    :initform '()
    :accessor model-request-model-messages)
   (instructions
    :initarg :instructions
    :initform nil
    :reader model-request-instructions)
   (session-id
    :initarg :session-id
    :initform nil
    :reader model-request-session-id)
   (tool-schemas
    :initarg :tool-schemas
    :initform '()
    :reader model-request-tool-schemas)
   (sealed-epoch
    :initarg :sealed-epoch
    :reader model-request-sealed-epoch)
   (sealed-context-id
    :initarg :sealed-context-id
    :reader model-request-sealed-context-id)
   (source-context-id
    :initarg :source-context-id
    :reader model-request-source-context-id)
   (leaf-id
    :initarg :leaf-id
    :initform nil
    :reader model-request-leaf-id)
   (credential-reference-id
    :initform nil
    :accessor model-request-credential-reference-id)
   (credential-source-kind
    :initform nil
    :accessor model-request-credential-source-kind)
   (stream
    :initform nil
    :accessor model-request-stream)
   (stream-closer
    :initform nil
    :accessor model-request-stream-closer)
   (response
    :initform nil
    :accessor model-request-response)
   (error
    :initform nil
    :accessor model-request-error)
   (metadata
    :initarg :metadata
    :initform '()
    :reader model-request-metadata)
   (created-at
    :initarg :created-at
    :reader model-request-created-at)
   (completed-at
    :initform nil
    :accessor model-request-completed-at)))

(defclass model-response (live-object)
  ((request-id
    :initarg :request-id
    :reader model-response-request-id)
   (content
    :initarg :content
    :initform ""
    :reader model-response-content)
   (tool-calls
    :initarg :tool-calls
    :initform '()
    :reader model-response-tool-calls)
   (stop-reason
    :initarg :stop-reason
    :reader model-response-stop-reason)
   (timestamp
    :initarg :timestamp
    :reader model-response-timestamp)
   (metadata
    :initarg :metadata
    :initform '()
    :reader model-response-metadata)))

(defclass model-stream (live-object)
  ((request
    :initarg :request
    :reader model-stream-request)
   (events
    :initform '()
    :accessor model-stream-events)
   (timings
    :initform '()
    :accessor model-stream-timings
    :documentation "Newest-first timing markers for request/transport phases.
Markers use internal-real-time ticks so latency offsets are monotonic.")
   (state
    :initform :pending
    :accessor model-stream-state)
   (text-fragments
    :initform '()
    :accessor model-stream-text-fragments)
   (thinking-deltas
    :initform '()
    :accessor model-stream-thinking-deltas)
   (tool-call-deltas
    :initform '()
    :accessor model-stream-tool-call-deltas)
   (usage
    :initform nil
    :accessor model-stream-usage)
   (provider-stop-reason
    :initform nil
    :accessor model-stream-provider-stop-reason
    :documentation "Normalized finish reason reported by the provider
(:length when output was truncated at the max-token limit), NIL when the
stream ended without one. Folded into the response stop-reason at
completion.")))

(defclass model-delta (live-object)
  ((kind
    :initarg :kind
    :reader model-delta-kind)
   (content-index
    :initarg :content-index
    :initform nil
    :reader model-delta-content-index)
   (timestamp
    :initarg :timestamp
    :reader model-delta-timestamp)))

(defclass assistant-delta (model-delta)
  ((text
    :initarg :text
    :initform ""
    :reader assistant-delta-text)))

(defclass thinking-delta (model-delta)
  ((text
    :initarg :text
    :initform ""
    :reader thinking-delta-text)
   (signature
    :initarg :signature
    :initform nil
    :reader thinking-delta-signature)
   (redacted
    :initarg :redacted
    :initform nil
    :reader thinking-delta-redacted)))

(defclass tool-call-delta (model-delta)
  ((call-id
    :initarg :call-id
    :reader tool-call-delta-call-id)
   (name
    :initarg :name
    :reader tool-call-delta-name)
   (arguments
    :initarg :arguments
    :initform '()
    :reader tool-call-delta-arguments)))

(defclass usage-delta (model-delta)
  ((usage
    :initarg :usage
    :initform '()
    :reader usage-delta-usage)))

(defclass block-boundary-delta (model-delta)
  ((content-kind
    :initarg :content-kind
    :reader block-delta-content-kind)))

(defclass stop-reason-delta (model-delta)
  ((reason
    :initarg :reason
    :reader stop-reason-delta-reason)))

(defclass block-start-delta (block-boundary-delta) ())

(defclass block-end-delta (block-boundary-delta) ())

(defun make-model-runtime (&key (id :model-runtime-service))
  (make-instance 'model-runtime :id id))

(defun make-assistant-delta (text &key id timestamp content-index)
  (make-instance 'assistant-delta
                 :id (or id (next-model-delta-id))
                 :kind :assistant-delta
                 :content-index content-index
                 :timestamp (or timestamp (get-universal-time))
                 :text text))

(defun make-thinking-delta (text &key id timestamp content-index
                                   signature redacted)
  (make-instance 'thinking-delta
                 :id (or id (next-model-delta-id))
                 :kind :thinking-delta
                 :content-index content-index
                 :timestamp (or timestamp (get-universal-time))
                 :text text
                 :signature signature
                 :redacted redacted))

(defun make-tool-call-delta (name arguments &key id call-id timestamp
                                              content-index)
  (make-instance 'tool-call-delta
                 :id (or id (next-model-delta-id))
                 :kind :tool-call-delta
                 :content-index content-index
                 :timestamp (or timestamp (get-universal-time))
                 :call-id (or call-id (next-keyword-id
                                       "MODEL-TOOL-CALL"
                                       '*model-delta-counter*))
                 :name name
                 :arguments arguments))

(defun make-usage-delta (usage &key id timestamp)
  (make-instance 'usage-delta
                 :id (or id (next-model-delta-id))
                 :kind :usage-delta
                 :timestamp (or timestamp (get-universal-time))
                 :usage usage))

(defun make-stop-reason-delta (reason &key id timestamp)
  "REASON is a normalized finish keyword. Adapters emit :length when the
provider reports output truncated at the max-token limit."
  (make-instance 'stop-reason-delta
                 :id (or id (next-model-delta-id))
                 :kind :stop-reason-delta
                 :timestamp (or timestamp (get-universal-time))
                 :reason reason))

(defun make-block-start-delta (content-kind &key id timestamp content-index)
  (make-instance 'block-start-delta
                 :id (or id (next-model-delta-id))
                 :kind :block-start-delta
                 :content-kind content-kind
                 :content-index content-index
                 :timestamp (or timestamp (get-universal-time))))

(defun make-block-end-delta (content-kind &key id timestamp content-index)
  (make-instance 'block-end-delta
                 :id (or id (next-model-delta-id))
                 :kind :block-end-delta
                 :content-kind content-kind
                 :content-index content-index
                 :timestamp (or timestamp (get-universal-time))))

(defun make-model-stream (request &key id)
  (make-instance 'model-stream
                 :id (or id (next-model-stream-id))
                 :request request))

(defun make-model-response (request content tool-calls stop-reason &key id
                                      metadata)
  (make-instance 'model-response
                 :id (or id (next-model-response-id))
                 :request-id (object-id request)
                 :content content
                 :tool-calls tool-calls
                 :stop-reason stop-reason
                 :timestamp (get-universal-time)
                 :metadata metadata))
