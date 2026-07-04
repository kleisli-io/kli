(in-package #:kli/agent/session)

(define-condition agent-session-error (error)
  ((reason
    :initarg :reason
    :reader agent-session-error-reason)
   (provider-id
    :initarg :provider-id
    :initform nil
    :reader agent-session-error-provider-id))
  (:report
   (lambda (condition stream)
     (let ((reason (agent-session-error-reason condition))
           (provider (agent-session-error-provider-id condition)))
       (case reason
         (:no-credential
          (format stream "No credential for ~A -- run /auth login ~A"
                  provider provider))
         ((:no-model :no-current-model)
          (format stream "No model selected -- run /model to choose one"))
         (t
          (format stream "Agent-session error: ~S~@[ (provider ~S)~]"
                  reason provider)))))))

(defmethod kli/ext:condition-category ((condition agent-session-error))
  "Reasons the user can fix (missing credential, no model selected) classify
:config so they render as actionable guidance instead of an internal error.
Invariant violations (:no-agent, :no-mode-binding and the like) stay :internal."
  (case (agent-session-error-reason condition)
    ((:no-credential :no-model :no-current-model) :config)
    (t :internal)))

(defclass context-usage ()
  ((input-tokens
    :initarg :input-tokens
    :initform 0
    :accessor usage-input-tokens)
   (output-tokens
    :initarg :output-tokens
    :initform 0
    :accessor usage-output-tokens)
   (cache-read-tokens
    :initarg :cache-read-tokens
    :initform 0
    :accessor usage-cache-read-tokens)
   (cache-write-tokens
    :initarg :cache-write-tokens
    :initform 0
    :accessor usage-cache-write-tokens)
   (total-tokens
    :initarg :total-tokens
    :initform 0
    :accessor usage-total-tokens)
   (over-threshold-p
    :initarg :over-threshold-p
    :initform nil
    :accessor usage-over-threshold-p)))

(defun make-context-usage (&key (input-tokens 0) (output-tokens 0)
                             (cache-read-tokens 0) (cache-write-tokens 0)
                             (total-tokens 0) over-threshold-p)
  (make-instance 'context-usage
                 :input-tokens input-tokens
                 :output-tokens output-tokens
                 :cache-read-tokens cache-read-tokens
                 :cache-write-tokens cache-write-tokens
                 :total-tokens total-tokens
                 :over-threshold-p over-threshold-p))

(defclass agent-session-binding ()
  ((session-id
    :initarg :session-id
    :reader session-binding-session-id)
   (leaf-id
    :initarg :leaf-id
    :accessor session-binding-leaf-id))
  (:documentation "Thin pointer. session-id is :reader so a listener holding a binding cannot have it silently re-targeted."))

(defclass agent-context-binding ()
  ((context-id
    :initarg :context-id
    :reader context-binding-context-id)
   (rebuilt-at
    :initarg :rebuilt-at
    :accessor context-binding-rebuilt-at)
   (usage
    :initarg :usage
    :initform nil
    :accessor context-binding-usage)
   (live-usage
    :initform nil
    :accessor context-binding-live-usage
    :documentation "Usage reported by streaming usage-deltas mid-turn. The footer
prefers it over the committed usage so the context readout tracks the in-flight
turn; cleared at agent-end so the footer falls back to the committed value
between turns. The committed usage, which compaction reads, is never touched
mid-stream.")))

(defclass mode-binding ()
  ((mode-id
    :initarg :mode-id
    :reader mode-binding-mode-id)
   (session-binding
    :initarg :session-binding
    :accessor mode-binding-session-binding)
   (context-binding
    :initarg :context-binding
    :accessor mode-binding-context-binding)
   (agent-id
    :initarg :agent-id
    :initform nil
    :accessor mode-binding-agent-id)
   (compaction-interrupt
    :initform nil
    :accessor mode-binding-compaction-interrupt
    :documentation "While a compaction is in flight, a thunk that flags it
aborted and shuts down the summarizer's in-flight model request.
abort-agent-session invokes it so Esc and quit reach the otherwise-anonymous
summarizer call. NIL outside a compaction."))
  (:documentation "One per active mode. Holds the session, context, and agent for that mode."))

(defun make-mode-binding (&key mode-id session-binding context-binding agent-id)
  (make-instance 'mode-binding
                 :mode-id mode-id
                 :session-binding session-binding
                 :context-binding context-binding
                 :agent-id agent-id))

(defclass session-event-listener ()
  ((listener-id
    :initarg :listener-id
    :reader listener-id)
   (handler
    :initarg :handler
    :reader listener-handler)
   (filter
    :initarg :filter
    :initform nil
    :reader listener-filter)
   (registered-by
    :initarg :registered-by
    :initform nil
    :reader listener-registered-by))
  (:documentation "registered-by records the calling subject so an inspector can answer who is sniffing agent events."))

(defun make-session-event-listener (listener-id handler
                                    &key filter registered-by)
  (make-instance 'session-event-listener
                 :listener-id listener-id
                 :handler handler
                 :filter filter
                 :registered-by registered-by))

(defclass agent-session (live-object)
  ;; Mode bindings mutate from the TUI loop thread while event handlers on
  ;; the agent worker thread resolve modes per event, so both tables must be
  ;; synchronized.
  ((mode-bindings
    :initform (make-hash-table :test #'equal :synchronized t)
    :reader session-mode-bindings)
   (source->mode
    :initform (make-hash-table :test #'equal :synchronized t)
    :reader session-source->mode
    :documentation "Holds both agent-ids and agent-context-ids so :after-driven handlers can resolve mode-id from event source regardless of which producer emitted.")
   (event-listeners
    :initform '()
    :accessor session-event-listeners)
   (event-persistence-policy
    :initarg :event-persistence-policy
    :accessor session-event-persistence-policy)
   (prompt-expansion-policy
    :initarg :prompt-expansion-policy
    :accessor session-prompt-expansion-policy)
   (context-transform-policy
    :initarg :context-transform-policy
    :accessor session-context-transform-policy)
   (retry-policy
    :initarg :retry-policy
    :accessor session-retry-policy)
   (compaction-policy
    :initarg :compaction-policy
    :accessor session-compaction-policy)
   (pending-command-records
    :initform '()
    :accessor session-pending-command-records
    :documentation "Model-visible command records awaiting injection into the
next prompt, oldest first. Commands record from the TUI loop or a command
worker thread while the agent worker drains, so access goes through
pending-command-lock.")
   (pending-command-dropped
    :initform 0
    :accessor session-pending-command-dropped)
   (pending-command-lock
    :initform (sb-thread:make-mutex :name "session-pending-commands")
    :reader session-pending-command-lock)
   (active-mode-id
    :initform nil
    :accessor session-active-mode-id
    :documentation "The focused mode -- which mode the TUI shows on top and whose
model seeds newly constructed agents. NIL until the first focus. Sole writer is
focus-agent-session-mode; it round-trips through the snapshot.")
   (restore-unfocused-mode
    :initform nil
    :accessor session-restore-unfocused-mode
    :documentation "The captured focused mode a restore could not rebind, its
session being unavailable, so the restore command can name the loss. NIL after a
clean restore."))
  (:documentation "The orchestrator live-object. Hashtables use :test #'equal to match the codebase convention for id-keyed maps."))
