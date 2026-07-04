(in-package #:kli/tui/app)

(defvar *tui-app-counter* (make-id-counter))

(defun make-tui-app-id ()
  (next-keyword-id "TUI-APP" '*tui-app-counter*))

(defclass tui-app (live-object)
  ((terminal
    :initarg :terminal
    :accessor tui-app-terminal)
   (decoder
    :initarg :decoder
    :accessor tui-app-decoder)
   (transcript
    :initarg :transcript
    :accessor tui-app-transcript)
   (view
    :initarg :view
    :accessor tui-app-view)
   (renderer
    :initarg :renderer
    :accessor tui-app-renderer)
   (context
    :initarg :context
    :initform nil
    :accessor tui-app-context)
   (route-context
    :initarg :route-context
    :initform nil
    :accessor tui-app-route-context)
   (runtime-behavior
    :initarg :runtime-behavior
    :accessor tui-app-runtime-behavior)
   (running-p
    :initarg :running-p
    :initform t
    :accessor tui-app-running-p)
   (interrupt-armed-p
    :initarg :interrupt-armed-p
    :initform nil
    :accessor tui-app-interrupt-armed-p)
   (abort-armed-at
    :initarg :abort-armed-at
    :initform nil
    :accessor tui-app-abort-armed-at)
   (rewind-armed-at
    :initarg :rewind-armed-at
    :initform nil
    :accessor tui-app-rewind-armed-at
    :documentation "Internal-time of the first idle-prompt Esc, arming a rewind a second Esc within +tui-abort-rearm-window+ confirms.")
   (notice-expires-at
    :initarg :notice-expires-at
    :initform nil
    :accessor tui-app-notice-expires-at)
   (pending-prompt
    :initarg :pending-prompt
    :initform nil
    :accessor tui-app-pending-prompt
    :documentation "Prompt text captured at submit for a pre-stream-abort retract, cleared on the first delta.")
   (pre-turn-leaf-id
    :initarg :pre-turn-leaf-id
    :initform nil
    :accessor tui-app-pre-turn-leaf-id
    :documentation "Pre-turn session leaf captured at submit, cleared on the first delta.")
   (retract-armed-p
    :initarg :retract-armed-p
    :initform nil
    :accessor tui-app-retract-armed-p
    :documentation "Set on pre-stream abort, consumed at agent/turn-end to retract the prompt.")
   (escape-pending-since
    :initarg :escape-pending-since
    :initform nil
    :accessor tui-app-escape-pending-since)
   (on-submit
    :initarg :on-submit
    :initform nil
    :accessor tui-app-on-submit)
   (mode-id
    :initarg :mode-id
    :initform :default-mode
    :reader tui-app-mode-id)
   (listener-id
    :initarg :listener-id
    :initform nil
    :accessor tui-app-listener-id)
   (projection-buffer
    :initarg :projection-buffer
    :initform (kli/tui/transcript:make-projection-buffer)
    :reader tui-app-projection-buffer)
   (main-thread-tasks
    :initform nil
    :accessor tui-app-main-thread-tasks)
   (main-thread-tasks-lock
    :initform (sb-thread:make-mutex :name "tui-app-main-thread-tasks")
    :reader tui-app-main-thread-tasks-lock)
   (agent-worker-thread
    :initform nil
    :accessor tui-app-agent-worker-thread
    :documentation "Per-submit thread running the agent loop off the input thread, so the trampoline stays responsive to Esc while a turn streams.")
   (command-worker-thread
    :initform nil
    :accessor tui-app-command-worker-thread
    :documentation "Thread running a one-shot blocking command body (e.g. /compact's summarizer call or a remote /install). Distinct from the agent slot so a command spawned mid-turn neither orphans the agent thread nor reads as the live agent to submit routing.")
   (render-pending-p
    :initform nil
    :accessor tui-app-render-pending-p
    :documentation "Set by a marshaled projection that mutated state. The trampoline drain repaints once after the batch, coalescing a burst of stream deltas into one frame.")
   (render-reset-pending-p
    :initform nil
    :accessor tui-app-render-reset-pending-p
    :documentation "Render state is suspect. The next frame clears screen and scrollback and reprints from the model instead of diffing.")
   (render-fault-streak
    :initform 0
    :accessor tui-app-render-fault-streak
    :documentation "Consecutive contained render faults. Any completed frame resets it.")
   (step-fault-streak
    :initform 0
    :accessor tui-app-step-fault-streak
    :documentation "Consecutive contained runtime-step faults. Any non-fault step resets it.")
   (loop-thread
    :initform nil
    :accessor tui-app-loop-thread
    :documentation "Thread running the trampoline, captured by run-tui-app. While set, renders from any other thread are refused because the renderer is stateful and single-threaded.")
   (spinner-active-p
    :initform nil
    :accessor tui-app-spinner-active-p
    :documentation "True while the working indicator shows. Set when a turn goes in flight, cleared when it ends. Read by the working-indicator widget.")
   (spinner-phase
    :initform 0
    :accessor tui-app-spinner-phase
    :documentation "Working-indicator frame index, advanced by poll-tui-app-spinner while a turn is in flight.")
   (spinner-tick-at
    :initform nil
    :accessor tui-app-spinner-tick-at
    :documentation "Internal-time of the last spinner advance, gating the next by +spinner-tick-interval+.")
   (tool-update-text
    :initform nil
    :accessor tui-app-tool-update-text
    :documentation "Latest tool-execution progress update, shown after the working-indicator label by the working-indicator widget. Set by the update projection, cleared when the execution or turn ends.")))
