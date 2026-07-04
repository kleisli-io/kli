(in-package #:kli/agent/loop)

(defvar *agent-counter* (make-id-counter))
(defvar *agent-turn-counter* (make-id-counter))
(defvar *agent-run-counter* (make-id-counter))
(defvar *agent-tool-call-counter* (make-id-counter))
(defvar *agent-tool-execution-counter* (make-id-counter))
(defvar *agent-update-counter* (make-id-counter))

(defun next-agent-id ()
  (next-keyword-id "AGENT" '*agent-counter*))

(defun next-agent-turn-id ()
  (next-keyword-id "AGENT-TURN" '*agent-turn-counter*))

(defun next-agent-run-id ()
  (next-keyword-id "AGENT-RUN" '*agent-run-counter*))

(defun next-agent-tool-call-id ()
  (next-keyword-id "AGENT-TOOL-CALL" '*agent-tool-call-counter*))

(defun next-agent-tool-execution-id ()
  (next-keyword-id "AGENT-TOOL-EXECUTION"
                         '*agent-tool-execution-counter*))

(defun next-agent-update-id ()
  (next-keyword-id "AGENT-TOOL-UPDATE" '*agent-update-counter*))

(defparameter *agent-loop-policy-vocabulary*
  '((:tool-scheduling-policy
     (:serial . execute-tool-calls-serially))
    (:steering-delivery-policy
     (:all . deliver-all-queue-items)
     (:one-at-a-time . deliver-one-queue-item))
    (:follow-up-delivery-policy
     (:all . deliver-all-queue-items)
     (:one-at-a-time . deliver-one-queue-item))
    (:error-to-tool-result-policy
     (:feed-back . feed-back-error-tool-result))
    (:in-flight-context-edit-policy
     (:apply . apply-in-flight-context-edits)))
  "Canonical policy functions per behavior slot, keyword -> function name.
The first entry of each slot is its default. Symbols rather than function
objects so the table can precede the definitions, which live next to the
loop decision points that funcall them.")

(defstruct (agent-loop-behavior
            (:constructor %make-agent-loop-behavior)
            (:copier nil))
  "Behavior cell for one agent: a defstruct of funcall slots owned by the
agent's BEHAVIOR CLOS slot. The loop funcalls each slot at its named
decision point, so a swapped function governs the very next decision.
Construct via MAKE-AGENT-LOOP-BEHAVIOR, swap via RECODE-AGENT-LOOP-BEHAVIOR."
  (tool-scheduling nil :type function)
  (steering-delivery nil :type function)
  (follow-up-delivery nil :type function)
  (error-to-tool-result nil :type function)
  (in-flight-context-edit nil :type function))

(defun resolve-behavior-policy (policy value)
  "Function to install for VALUE in the POLICY slot: a function is taken
as-is, a keyword names a canonical policy function, NIL takes the default
(first) canonical."
  (let ((table (cdr (assoc policy *agent-loop-policy-vocabulary*))))
    (etypecase value
      (null (fdefinition (cdar table)))
      (keyword (let ((entry (assoc value table)))
                 (unless entry
                   (error "Unknown ~(~A~) ~S; known: ~{~S~^, ~}."
                          policy value (mapcar #'car table)))
                 (fdefinition (cdr entry))))
      (function value))))

(defun behavior-policy-name (policy function)
  "Canonical keyword for FUNCTION in the POLICY slot when it is a canonical
policy function, otherwise FUNCTION itself -- inspection never reports a
name the loop is not actually running."
  (let ((entry (find function (cdr (assoc policy *agent-loop-policy-vocabulary*))
                     :key (lambda (entry) (fdefinition (cdr entry))))))
    (if entry (car entry) function)))

(defun make-agent-loop-behavior (&key tool-scheduling-policy
                                      steering-delivery-policy
                                      follow-up-delivery-policy
                                      error-to-tool-result-policy
                                      in-flight-context-edit-policy)
  (%make-agent-loop-behavior
   :tool-scheduling (resolve-behavior-policy :tool-scheduling-policy
                                             tool-scheduling-policy)
   :steering-delivery (resolve-behavior-policy :steering-delivery-policy
                                               steering-delivery-policy)
   :follow-up-delivery (resolve-behavior-policy :follow-up-delivery-policy
                                                follow-up-delivery-policy)
   :error-to-tool-result (resolve-behavior-policy :error-to-tool-result-policy
                                                  error-to-tool-result-policy)
   :in-flight-context-edit (resolve-behavior-policy
                            :in-flight-context-edit-policy
                            in-flight-context-edit-policy)))

(defun behavior-state (behavior)
  (list :tool-scheduling-policy
        (behavior-policy-name :tool-scheduling-policy
                              (agent-loop-behavior-tool-scheduling behavior))
        :steering-delivery-policy
        (behavior-policy-name :steering-delivery-policy
                              (agent-loop-behavior-steering-delivery behavior))
        :follow-up-delivery-policy
        (behavior-policy-name :follow-up-delivery-policy
                              (agent-loop-behavior-follow-up-delivery behavior))
        :error-to-tool-result-policy
        (behavior-policy-name :error-to-tool-result-policy
                              (agent-loop-behavior-error-to-tool-result behavior))
        :in-flight-context-edit-policy
        (behavior-policy-name :in-flight-context-edit-policy
                              (agent-loop-behavior-in-flight-context-edit
                               behavior))))

(defclass agent-loop-service (live-object)
  ((agents
    :initform (make-hash-table :test #'equal)
    :accessor agent-loop-service-agents)))

(defclass agent-state ()
  ((value
    :initarg :value
    :initform :idle
    :accessor agent-state-value)
   (reason
    :initarg :reason
    :initform nil
    :accessor agent-state-reason)
   (updated-at
    :initarg :updated-at
    :initform nil
    :accessor agent-state-updated-at)))

(defclass agent-queue (live-object)
  ((items
    :initarg :items
    :initform '()
    :accessor agent-queue-items)
   (lock
    :initform (sb-thread:make-mutex :name "agent-queue")
    :reader agent-queue-lock
    :documentation "Guards the read-modify-write on items against concurrent enqueue and drain.")))

(defclass agent (live-object)
  ((session
    :initarg :session
    :reader agent-session)
   (store
    :initarg :store
    :reader agent-store)
   (agent-context
    :initarg :agent-context
    :reader agent-context)
   (model-selection
    :initarg :model-selection
    :accessor agent-model-selection)
   (model-runtime
    :initarg :model-runtime
    :reader agent-model-runtime)
   (system-prompt
    :initarg :system-prompt
    :initform nil
    :accessor agent-system-prompt)
   (base-system-prompt
    :initarg :base-system-prompt
    :initform nil
    :reader agent-base-system-prompt
    :documentation "Immutable construction-time system prompt. Each submission
recomposes the rendered system-prompt slot from this base, so prompt layers never
feed their own output back as the next base.")
   (context-extras-fn
    :initarg :context-extras-fn
    :initform nil
    :accessor agent-context-extras-fn
    :documentation "Thunk yielding this turn's ephemeral context messages, or NIL.")
   (state
    :initarg :state
    :initform (make-instance 'agent-state
                             :value :idle
                             :updated-at (get-universal-time))
    :accessor agent-state)
   (current-turn
    :initform nil
    :accessor agent-current-turn)
   (turns
    :initform '()
    :accessor agent-turns)
   (tool-executions
    :initform '()
    :accessor agent-tool-executions)
   (steering-queue
    :initarg :steering-queue
    :initform (make-instance 'agent-queue
                             :id (next-keyword-id "AGENT-QUEUE"
                                                        '*agent-counter*))
    :accessor agent-steering-queue)
   (follow-up-queue
    :initarg :follow-up-queue
    :initform (make-instance 'agent-queue
                             :id (next-keyword-id "AGENT-QUEUE"
                                                        '*agent-counter*))
    :accessor agent-follow-up-queue)
   (abort-requested-p
    :initform nil
    :accessor %agent-abort-requested-p)
   (last-error
    :initform nil
    :accessor agent-last-error)
   (behavior
    :initarg :behavior
    :accessor agent-behavior)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor agent-metadata)
   (subject
    :initarg :subject
    :initform (make-default-subject)
    :accessor agent-subject)
   (principal
    :initarg :principal
    :initform nil
    :accessor agent-principal
    :documentation "This agent's key in the protocol grant-set, or NIL. When set
and the grant-set records it, the agent runs under that delegated authority; the
subject slot is the fallback for an un-delegated agent.")
   (loop-runs
    :initform '()
    :accessor agent-loop-runs
    :documentation "The agent-loop-runs this agent has started, newest-first.
Tracked so deregister-agent drains them from the live registry; without this a
run lives only in the registry and leaks when the agent is torn down.")
   (loop-runs-lock
    :initform (sb-thread:make-mutex :name "agent-loop-runs")
    :reader agent-loop-runs-lock
    :documentation "Guards loop-runs against the worker thread pushing a new run
while the controlling thread drains them.")))

(declaim (inline agent-abort-requested-p (setf agent-abort-requested-p)))

(defun agent-abort-requested-p (agent)
  "Read the abort flag through a memory barrier. The aborting thread writes the flag and the agent worker reads it, so both sides cross a barrier -- the write publishes, the read observes. Every call site routes through these wrappers."
  (sb-thread:barrier (:memory))
  (%agent-abort-requested-p agent))

(defun (setf agent-abort-requested-p) (value agent)
  (prog1 (setf (%agent-abort-requested-p agent) value)
    (sb-thread:barrier (:memory))))

(defclass agent-loop-run (live-object)
  ((agent
    :initarg :agent
    :reader agent-run-agent)
   (turns
    :initform '()
    :accessor agent-run-turns)
   (state
    :initform :running
    :accessor agent-run-state)
   (started-at
    :initarg :started-at
    :reader agent-run-started-at)
   (started-at-real
    :initarg :started-at-real
    :initform nil
    :reader agent-run-started-at-real)
   (completed-at
    :initform nil
    :accessor agent-run-completed-at)))

(defclass agent-turn (live-object)
  ((state
    :initform :pending
    :accessor agent-turn-state)
   (sealed-context
    :initform nil
    :accessor agent-turn-sealed-context)
   (request
    :initform nil
    :accessor agent-turn-request)
   (response
    :initform nil
    :accessor agent-turn-response)
   (tool-calls
    :initform '()
    :accessor agent-turn-tool-calls)
   (error
    :initform nil
    :accessor agent-turn-error)
   (started-at
    :initarg :started-at
    :reader agent-turn-started-at)
   (started-at-real
    :initarg :started-at-real
    :initform nil
    :reader agent-turn-started-at-real)
   (completed-at
    :initform nil
    :accessor agent-turn-completed-at)))

(defclass tool-call (live-object)
  ((call-id
    :initarg :call-id
    :reader agent-tool-call-id)
   (name
    :initarg :name
    :reader agent-tool-call-name)
   (arguments
    :initarg :arguments
    :initform '()
    :reader agent-tool-call-arguments)
   (state
    :initform :pending
    :accessor agent-tool-call-state)
   (metadata
    :initarg :metadata
    :initform '()
    :reader agent-tool-call-metadata)))

(defclass tool-execution (live-object)
  ((call
    :initarg :call
    :reader tool-execution-call)
   (result
    :initform nil
    :accessor tool-execution-result)
   (state
    :initform :pending
    :accessor tool-execution-state)
   (updates
    :initform '()
    :accessor tool-execution-updates)
   (started-at
    :initarg :started-at
    :reader tool-execution-started-at)
   (started-at-real
    :initarg :started-at-real
    :initform nil
    :reader tool-execution-started-at-real)
   (completed-at
    :initform nil
    :accessor tool-execution-completed-at)))

(defclass tool-execution-update (live-object)
  ((execution
    :initarg :execution
    :reader tool-execution-update-execution)
   (payload
    :initarg :payload
    :reader tool-execution-update-payload)
   (timestamp
    :initarg :timestamp
    :reader tool-execution-update-timestamp)))

(defun agent-timestamp (&optional timestamp)
  (or timestamp (get-universal-time)))

(defun event-duration-ms (started-at-real)
  "Elapsed milliseconds since STARTED-AT-REAL, a monotonic get-internal-real-time
stamp, or NIL when unstamped so the field is omitted rather than fabricated. An
interval needs the monotonic clock -- the wall-clock event timestamp can jump
and must never measure one."
  (when started-at-real
    (round (* 1000 (- (get-internal-real-time) started-at-real))
           internal-time-units-per-second)))

(defun payload-with-duration (payload started-at-real)
  "PAYLOAD plus :duration-ms from STARTED-AT-REAL, or PAYLOAD unchanged when
unstamped."
  (let ((ms (event-duration-ms started-at-real)))
    (if ms (append payload (list :duration-ms ms)) payload)))

(defun make-agent-loop-service (&key (id :agent-loop-service))
  (make-instance 'agent-loop-service :id id))

(defun make-agent-state (&key (value :idle) reason timestamp)
  (make-instance 'agent-state
                 :value value
                 :reason reason
                 :updated-at (agent-timestamp timestamp)))

(defun set-agent-state (agent value &key reason)
  (setf (agent-state agent)
        (make-agent-state :value value :reason reason))
  (agent-state agent))

(defun make-agent-queue (&key id items)
  (make-instance 'agent-queue
                 :id (or id (next-keyword-id "AGENT-QUEUE"
                                                    '*agent-counter*))
                 :items items))

(defun make-agent (session store agent-context model-selection model-runtime
                   &key id system-prompt context-extras-fn behavior metadata
                        subject granted-capabilities principal)
  (let ((id (or id (next-agent-id))))
    (make-instance 'agent
                   :id id
                   :principal (or principal id)
                   :session session
                   :store store
                   :agent-context agent-context
                   :model-selection model-selection
                   :model-runtime model-runtime
                   :system-prompt system-prompt
                   :base-system-prompt system-prompt
                   :context-extras-fn context-extras-fn
                   :steering-queue (make-agent-queue
                                    :id (list id :steering-queue))
                   :follow-up-queue (make-agent-queue
                                     :id (list id :follow-up-queue))
                   :behavior (or behavior (make-agent-loop-behavior))
                   :metadata metadata
                   :subject (or subject
                                (and granted-capabilities
                                     (make-subject
                                      :capabilities granted-capabilities))
                                (make-default-subject)))))

(defun make-agent-turn (&key id)
  (make-instance 'agent-turn
                 :id (or id (next-agent-turn-id))
                 :started-at (get-universal-time)
                 :started-at-real (get-internal-real-time)))

(defun make-agent-run (agent &key id)
  (make-instance 'agent-loop-run
                 :id (or id (next-agent-run-id))
                 :agent agent
                 :started-at (get-universal-time)
                 :started-at-real (get-internal-real-time)))

(defun make-tool-call (name arguments &key id call-id metadata)
  "Build a tool-call. Plist arguments are defensively copied. Parsed JSON arguments arrive as a fresh hash-table and are stored as-is."
  (make-instance 'tool-call
                 :id (or id (next-agent-tool-call-id))
                 :call-id (or call-id id (next-agent-tool-call-id))
                 :name name
                 :arguments (if (listp arguments) (copy-list arguments) arguments)
                 :metadata metadata))

(defun make-tool-execution (tool-call &key id)
  (make-instance 'tool-execution
                 :id (or id (next-agent-tool-execution-id))
                 :call tool-call
                 :started-at (get-universal-time)
                 :started-at-real (get-internal-real-time)))

(defun make-tool-execution-update (execution payload &key id timestamp)
  (make-instance 'tool-execution-update
                 :id (or id (next-agent-update-id))
                 :execution execution
                 :payload payload
                 :timestamp (agent-timestamp timestamp)))
