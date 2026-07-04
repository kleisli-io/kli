(in-package #:kli/tui/app)

(defparameter +tui-interrupted-notice-window+
  (* internal-time-units-per-second 2)
  "How long (~2s, in internal-time units) a transient acknowledgment like
   \"Interrupted.\" stays up before POLL-TUI-APP-NOTICE-EXPIRY drops it.")

(defun make-app-terminal (protocol kind columns rows)
  (ecase kind
    (:memory
     (make-memory-terminal :protocol protocol
                           :columns columns
                           :rows rows))
    (:process
     (make-process-terminal :protocol protocol
                            :columns columns
                            :rows rows))))

(defun emit-tui-user-submitted (app input)
  "Single echo point. The :mode in the payload lets agent-session resolve the listener."
  (let ((ctx (tui-app-context app)))
    (when ctx
      (kli/event:emit-event
       ctx
       (kli/event:make-event
        :tui/user-submitted
        :payload (list :mode (tui-app-mode-id app)
                       :input input))))))

(defun handle-tui-app-submit (app input)
  (setf (tui-app-interrupt-armed-p app) nil
        (tui-app-abort-armed-at app) nil
        (tui-app-rewind-armed-at app) nil)
  (clear-tui-app-notice app)
  (emit-tui-user-submitted app input)
  (multiple-value-bind (handled-p events)
      (let ((*defer-agent-turn-p* t))
        (dispatch-slash-command (tui-app-context app)
                                input
                                :app app
                                :mode-id (tui-app-mode-id app)
                                :worker-spawner
                                (tui-app-command-worker-spawner app)))
    (when handled-p
      (maybe-drain-deferred-follow-up app)
      (return-from handle-tui-app-submit (or events '()))))
  (let ((handler (tui-app-on-submit app)))
    (when handler
      (funcall handler app input))))

(defun submit-tui-app-input (app input)
  "Run APP's on-submit handler on INPUT as raw user input, returning the
events it could not deliver. Bypasses command dispatch: the caller already
produced the final text for the model."
  (funcall (tui-app-on-submit app) app input))

(defun make-app-route-context (app)
  (let ((protocol (object-protocol app)))
    (make-input-route-context
     :id (list (object-id app) :input-route-context)
     :protocol protocol
     :interrupt-handler
     (lambda ()
       (request-tui-app-interrupt app))
     :abort-handler
     (lambda ()
       (request-tui-app-abort app))
     :clear-screen-handler
     (lambda ()
       (clear-tui-app-screen app))
     :tool-output-handler
     (lambda ()
       (toggle-tui-app-tool-output app))
     :next-surface-handler
     (lambda ()
       (cycle-tui-app-surface app)))))

(defun toggle-tui-app-tool-output (app)
  "Flip the active protocol's tool-output expansion state and hard-reprint so
   committed tool-result events re-collapse/expand. A reprint is required
   because finalized events are written into native scrollback exactly once."
  (toggle-tool-output-expansion (object-protocol app))
  (redraw-tui-app app))

(defun make-app-runtime-behavior (app-id)
  (make-behavior-cell
   :id (list app-id :runtime-behavior)
   :state '(:strategy :default-tui-app-runtime)
   :metadata '(:owner :tui-app)
   :capabilities '(:tui-app/runtime :behavior/hotpatch)
   :fault-policy :reify
   :fault-fallback :error-handled
   :function #'default-tui-app-runtime-step))

(defun make-tui-app (&key id terminal (terminal-kind :memory)
                          (columns 80) (rows 24)
                          decoder transcript view renderer route-context
                          runtime-behavior
                          (prompt "> ") on-submit
                          (mode-id :default-mode)
                          context protocol)
  (let* ((protocol (or protocol
                       (and context (active-protocol context))
                       (error "make-tui-app requires :protocol or :context with an active protocol.")))
         (app nil))
    (let* ((id (or id (make-tui-app-id)))
           (terminal (or terminal
                         (make-app-terminal protocol terminal-kind columns rows)))
           (decoder (or decoder
                        (make-input-decoder :protocol protocol)))
           (transcript (or transcript
                           (make-transcript :protocol protocol)))
           (view (or view
                     (make-transcript-view
                      :protocol protocol
                      :transcript transcript
                      :prompt prompt
                      :on-submit (lambda (input)
                                   (handle-tui-app-submit app input)))))
           (renderer (or renderer
                         (make-scrollback-renderer
                          transcript
                          view
                          terminal
                          :protocol protocol))))
      (setf app
            (make-instance 'tui-app
                           :id id
                           :protocol protocol
                           :terminal terminal
                           :decoder decoder
                           :transcript transcript
                           :view view
                           :renderer renderer
                           :context context
                           :route-context route-context
                           :runtime-behavior (or runtime-behavior
                                                 (make-app-runtime-behavior id))
                           :on-submit on-submit
                           :mode-id mode-id))
      (unless (tui-app-on-submit app)
        (setf (tui-app-on-submit app) #'default-tui-app-on-submit))
      (unless (tui-app-route-context app)
        (setf (tui-app-route-context app)
              (make-app-route-context app)))
      ;; Surface 0 aliases the transcript slots: the dozens of call sites that
      ;; read tui-app-view/-renderer directly and the surface dispatch see the
      ;; same objects.
      (setf (tui-app-surfaces app)
            (list (make-tui-surface :id :transcript :label "transcript"
                                    :view view :renderer renderer
                                    :route-context (tui-app-route-context app))))
      (set-focused view t)
      (when context
        (register-tui-app context app)
        (register-tui-app-listener app context))
      app)))

(defun tui-app-active-surface (app)
  (nth (tui-app-active-surface-index app) (tui-app-surfaces app)))

(defun agent-session-service-for-app (context)
  (and context
       (find-live-object (context-registry context)
                         :agent-session-service)))

(defvar *marshal-projection* nil
  "Bound to T inside the agent worker thread. While set, project-tui-app-event
   queues its transcript mutation and render onto the loop thread instead of
   running them off-thread, where they would race the renderer. Emissions on the
   loop thread itself (e.g. the user echo) leave it nil and project immediately.")

(defun call-with-agent-worker-supervision (app mode-id thunk)
  "Run THUNK under the worker fault barrier plus an observer for
serious-conditions that are not errors (stack/heap exhaustion): note + reify,
then decline — they stay fatal to the worker by design, but never silently."
  (handler-bind (((and serious-condition (not error))
                  (lambda (condition)
                    (ignore-errors
                     (kli/ext:note-fault :agent-worker mode-id condition)
                     (enqueue-main-thread-task
                      app (lambda () (reify-tui-error app condition)))))))
    (kli/ext:with-extension-fault-barrier
        (:seam :agent-worker :id mode-id :policy :reify)
      (funcall thunk))))

(defun spawn-tui-worker (app mode-id thunk &key (name "kli-tui-worker") (role :agent))
  "Run THUNK on a fresh thread so the loop thread keeps reading and rendering.
   Re-establishes *call-subject* and the fault policy because dynamic bindings
   don't cross threads. Projections marshal back to the loop thread, and a
   fault reifies as a :tui-error through that same path, never the debugger.
   ROLE selects which slot holds the thread: :agent for a steerable turn,
   :command for a one-shot command body. The thread is bound before the setf so
   the slot place isn't chosen lazily."
  (let ((subject kli/ext:*call-subject*)
        (policy kli/ext:*extension-fault-policy*))
    (let ((thread
            (sb-thread:make-thread
             (lambda ()
               (let ((kli/ext:*call-subject* subject)
                     (kli/ext:*extension-fault-policy* policy)
                     (*marshal-projection* t)
                     (kli/ext:*fault-note-hook* (tui-app-fault-hook app))
                     (kli/ext:*fault-reify-hook*
                       (lambda (condition seam id)
                         (declare (ignore seam id))
                         (enqueue-main-thread-task
                          app (lambda () (reify-tui-error app condition))))))
                 (call-with-agent-worker-supervision app mode-id thunk)))
             :name name)))
      (ecase role
        (:agent (setf (tui-app-agent-worker-thread app) thread))
        (:command (setf (tui-app-command-worker-thread app) thread)))
      thread)))

(defun spawn-agent-worker (app service mode-id input ctx)
  (spawn-tui-worker app mode-id
                    (lambda ()
                      (submit-agent-session-prompt service mode-id input ctx))
                    :name "kli-agent-worker"))

(defun command-worker-alive-p (app)
  "True while a previously spawned command worker is still running, so a second
   command body refuses rather than overwriting the slot and orphaning the first."
  (let ((thread (tui-app-command-worker-thread app)))
    (and thread (sb-thread:thread-alive-p thread) t)))

(defun tui-app-command-worker-spawner (app)
  "Spawner handed to slash-command dispatch so a command body that would block
   the loop thread (e.g. /compact's summarizer model call) runs on its own
   command-worker slot instead, reporting through events. Refuses while a
   command worker is already live -- one blocking command at a time -- so a
   second can't orphan the first. The spawner runs on the loop thread, so the
   refusal notice is set directly."
  (lambda (thunk)
    (if (command-worker-alive-p app)
        (set-tui-app-transient-notice app "A command is already running."
                                      +tui-interrupted-notice-window+)
        (spawn-tui-worker app (tui-app-mode-id app) thunk
                          :name "kli-command-worker"
                          :role :command))))

(defun agent-worker-alive-p (app)
  "True while a previously spawned worker thread is still running. Submit
   routing treats a live worker as busy even before the agent raises its
   active state -- the window between a worker spawn (a fresh turn or a
   /compact summarizer) and its state transition would otherwise let a
   second worker spawn into the same slot."
  (let ((thread (tui-app-agent-worker-thread app)))
    (and thread (sb-thread:thread-alive-p thread) t)))

(defun spawn-drain-worker (app service mode-id ctx)
  "Run queued steering the same way a fresh submit runs its turn: on a
   worker, with projections marshaled back to the loop thread."
  (spawn-tui-worker app mode-id
                    (lambda ()
                      (drain-agent-session-work service mode-id ctx))
                    :name "kli-agent-worker"))
(defun maybe-drain-deferred-follow-up (app)
  "After a slash command deferred its follow-up turn (see *defer-agent-turn-p*),
drain the queued work on a worker so :command/result was already visible
before any model output. No-op when the mode is unbound, busy, or a worker is
already live -- the drain would either race or there is nothing to run. A
command that enqueued nothing (the common case) drains to a no-op, so spawning
is safe for every handled slash command."
  (let* ((ctx (tui-app-context app))
         (service (agent-session-service-for-app ctx))
         (mode-id (tui-app-mode-id app)))
    (when (and service ctx
               (gethash mode-id (session-mode-bindings service))
               (not (agent-session-busy-p service mode-id ctx))
               (not (agent-worker-alive-p app))
               (not (command-worker-alive-p app)))
      (spawn-drain-worker app service mode-id ctx))))

(defun default-tui-app-on-submit (app input)
  "Submit INPUT to the agent session bound to APP's mode. A turn in flight
(or a live worker that has not raised the busy state yet) is steered into
rather than spawning a second loop on the same agent, leaving the retract
slots clear because retract is for an unanswered prompt, not a mid-turn
steer. The steer only queues -- it never runs a turn on this thread. When
the turn ends between the routing check and the enqueue, the queued steer
has no boundary left to drain it: the loop's exit drain covers a steer that
lands before the loop is gone, and the re-check here hands a later one to a
worker, exactly like a fresh submit. An idle session captures the pre-turn
leaf before the spawn, since the worker appends the user turn on its own
thread, then spawns the worker. With a service but no binding, returns a
system event. With no service, no listener is registered and emission has no
projector, so a direct-append event keeps the echo visible."
  (let* ((ctx (tui-app-context app))
         (service (agent-session-service-for-app ctx))
         (mode-id (tui-app-mode-id app)))
    (cond
      ((and service ctx
            (gethash mode-id (session-mode-bindings service)))
       (cond
         ((or (agent-session-busy-p service mode-id ctx)
              (agent-worker-alive-p app))
          (queue-agent-session-steer service mode-id input ctx)
          (unless (or (agent-session-busy-p service mode-id ctx)
                      (agent-worker-alive-p app))
            (spawn-drain-worker app service mode-id ctx))
          nil)
         (t
          (setf (tui-app-pending-prompt app) input
                (tui-app-pre-turn-leaf-id app) (current-agent-session-leaf-id
                                                service mode-id ctx)
                (tui-app-retract-armed-p app) nil)
          (spawn-agent-worker app service mode-id input ctx)
          nil)))
      (service
       (list (kli/tui/transcript:make-transcript-event
              :system nil
              (format nil "No agent bound to ~S. Use /reset to start a session."
                      mode-id))))
      (t
       (list (kli/tui/transcript:make-transcript-event :message :user input))))))

(defun replay-session-into-transcript (app session-id)
  "Rebuild the transcript from SESSION-ID's stored entries so the switched-to
session shows its own conversation instead of the previous session's rows (or
a blank screen on a continue-last boot). Runs through reset-tui-app first
because a switch also invalidates pending-prompt and retract state. The old
rows are already committed to the visible screen, so this arms the hard-redraw
flag -- the next frame clears the viewport and repaints only this conversation
instead of appending a second copy below the previous one. Native scrollback
keeps the prior paint, as on every redraw path."
  (reset-tui-app app)
  (setf (tui-app-render-reset-pending-p app) t)
  (let* ((ctx (tui-app-context app))
         (store (and ctx (find-live-object (context-registry ctx)
                                           :session-store)))
         (session (and store session-id
                       (kli/session/log:find-session store session-id))))
    (when session
      (dolist (entry (kli/session/log:session-entries session))
        (dolist (te (kli/tui/transcript:session-entry-transcript-events entry))
          (tui-app-add-event app te)))))
  app)

(defun tool-update-display-text (payload)
  "One-line display text for a tool-execution update event PAYLOAD, or NIL
when the update carries nothing displayable. Tools send opaque update
payloads, so a string shows as-is, a plist prefers its :message or :text
entry, and anything else prints readably."
  (let* ((update (getf payload :payload))
         (text (cond
                 ((stringp update) update)
                 ((and (consp update) (stringp (getf update :message)))
                  (getf update :message))
                 ((and (consp update) (stringp (getf update :text)))
                  (getf update :text))
                 ((null update) nil)
                 (t (princ-to-string update)))))
    (when (and text (plusp (length text)))
      (substitute #\Space #\Newline text))))

(defun apply-tui-app-event-projection (app event mode-id)
  "Mutate the transcript and renderer state for EVENT and return T when the frame
needs a redraw. Touches the stateful renderer, so it must run on the loop thread.
Rendering is the caller's job, so a burst of marshaled deltas can collapse into a
single repaint per trampoline drain.

agent/message-end closes the stream boundary to freeze the live event the deltas
grew. agent/turn-end performs a race-free retract once the worker has unwound.
agent/tool-execution-update surfaces its latest payload on the working line
rather than the transcript, cleared when the execution or turn ends. An
event projecting to a transcript event is added once, guarded by membership and
not a last-element check, because an abort notice or any event appended after the
open stream would otherwise let the next delta re-append the live event,
duplicating the whole reply once per notice. The first delta commits the reply,
dropping the pending retract, which also wins the late-delta-after-Esc race in
favor of keeping the reply."
  (let* ((etype (kli/event:event-type event))
         (te (kli/tui/transcript:project-event-to-transcript
              etype
              event
              mode-id
              (tui-app-projection-buffer app))))
    (cond
      ((eq etype :agent/message-end)
       (finalize-scrollback-stream (tui-app-renderer app))
       t)
      ((eq etype :agent/turn-end)
       (setf (tui-app-tool-update-text app) nil)
       (when (tui-app-retract-armed-p app)
         (perform-tui-app-retract app mode-id)))
      ((eq etype :agent/tool-execution-update)
       (let ((text (tool-update-display-text
                    (kli/event:event-payload event))))
         (when text
           (setf (tui-app-tool-update-text app) text)
           t)))
      ((eq etype :session-switch)
       (replay-session-into-transcript
        app (getf (kli/event:event-payload event) :session-id))
       t)
      ((eq etype :session-rewind)
       ;; Arrives after the :session-switch replay, so the restore lands in
       ;; the editor the replay's reset left untouched.
       (restore-tui-app-editor-prompt
        app (getf (kli/event:event-payload event) :text))
       t)
      (te
       (when (eq etype :agent/tool-execution-end)
         (setf (tui-app-tool-update-text app) nil))
       (unless (member te (tui-app-transcript-events app))
         (tui-app-add-event app te))
       (when (member etype '(:agent/delta :agent/thinking-delta))
         (begin-scrollback-stream (tui-app-renderer app) te))
       (when (eq etype :agent/delta)
         (setf (tui-app-pending-prompt app) nil
               (tui-app-pre-turn-leaf-id app) nil
               (tui-app-retract-armed-p app) nil))
       t)
      (t nil))))

(defun project-tui-app-event (app event mode-id ctx)
  "Project EVENT for APP's mode. When marshaling, one enqueue per emission under
the queue mutex so the FIFO drain replays them in emission order, preserving
delta ordering and repainting once after the batch rather than per delta.
Before run-tui-app captures the loop thread (e.g. the boot-time session reset),
the paint defers as render-pending so the takeover clear is the first thing the
terminal sees instead of a frame at the shell cursor."
  (declare (ignore ctx))
  (when (eq mode-id (tui-app-mode-id app))
    (if *marshal-projection*
        (enqueue-main-thread-task
         app (lambda ()
               (when (apply-tui-app-event-projection app event mode-id)
                 (setf (tui-app-render-pending-p app) t))))
        (when (apply-tui-app-event-projection app event mode-id)
          (if (tui-app-loop-thread app)
              (render-tui-app app)
              (setf (tui-app-render-pending-p app) t))))))

(defun register-tui-app-listener (app context)
  (let ((service (agent-session-service-for-app context)))
    (when service
      (let* ((listener-id (list (object-id app) :session-listener))
             (listener (make-session-event-listener
                        listener-id
                        (lambda (event mode-id ctx)
                          (project-tui-app-event app event mode-id ctx)))))
        (register-session-event-listener service listener context)
        (setf (tui-app-listener-id app) listener-id))))
  app)

(defun unregister-tui-app-listener (app)
  (let ((listener-id (tui-app-listener-id app))
        (context (tui-app-context app)))
    (when listener-id
      (let ((service (agent-session-service-for-app context)))
        (when service
          (unregister-session-event-listener service listener-id context)))
      (setf (tui-app-listener-id app) nil)))
  app)

(defun register-tui-app (context app)
  (let ((registry (context-registry context)))
    (register-live-object registry app)
    (register-live-object registry (tui-app-terminal app))
    (register-live-object registry (tui-app-decoder app))
    (register-live-object registry (tui-app-transcript app))
    (register-live-object registry (tui-app-view app))
    (register-live-object registry (tui-app-renderer app))
    (register-live-object registry (tui-app-runtime-behavior app))
    (when (tui-app-route-context app)
      (register-live-object registry (tui-app-route-context app))))
  ;; Announce the app on the stream so extensions installed before or after
  ;; it exists can contribute surfaces without polling the registry.
  (kli/event:emit-event
   context
   (kli/event:make-event
    :tui/app-started
    :payload (list :app-id (object-id app)
                   :mode (tui-app-mode-id app))))
  app)

(defun add-tui-app-surface (app &key id label view renderer
                                  (route-context (tui-app-route-context app))
                                  activate)
  "Append a surface. Loop-thread-only, like every surface mutator -- the
renderer is stateful; off-thread callers go through :enqueue-tui-app-task.
ROUTE-CONTEXT defaults to the app's, so app-level chords (:next-surface,
user-bound actions) work on every surface unless overridden. With ACTIVATE,
select it. Returns ID, NIL when refused."
  (unless (tui-app-foreign-thread-p app "add-surface refused off the loop thread")
    (setf (tui-app-surfaces app)
          (append (tui-app-surfaces app)
                  (list (make-tui-surface :id id :label label
                                          :view view :renderer renderer
                                          :route-context route-context))))
    (when activate
      (select-tui-app-surface app id))
    id))

(defun remove-tui-app-surface (app id)
  "Remove the surface named ID. Surface 0 refuses -- the transcript is the
app's floor. Removing the active surface falls back to surface 0. Returns T
on removal, NIL when unknown or refused."
  (unless (tui-app-foreign-thread-p app "remove-surface refused off the loop thread")
    (let ((index (position id (tui-app-surfaces app) :key #'surface-id)))
      (cond
        ((null index) nil)
        ((zerop index)
         (kli/ext:note-fault :render (list :surface id)
                             "surface 0 refused removal")
         nil)
        (t
         (let ((active (tui-app-active-surface-index app)))
           (setf (tui-app-surfaces app)
                 (remove id (tui-app-surfaces app)
                         :key #'surface-id :count 1))
           (cond
             ((= active index) (select-tui-app-surface app 0))
             ((> active index) (decf (tui-app-active-surface-index app)))))
         t)))))

(defun surface-descriptor (surface active-p)
  (list :id (surface-id surface)
        :label (surface-label surface)
        :active-p active-p))

(defun list-tui-app-surfaces (app)
  "Surface descriptors (:id :label :active-p), in surface order."
  (loop for surface in (tui-app-surfaces app)
        for index from 0
        collect (surface-descriptor
                 surface (= index (tui-app-active-surface-index app)))))

(defun active-tui-app-surface (app)
  "The active surface's descriptor (:id :label :active-p)."
  (surface-descriptor (tui-app-active-surface app) t))

(defun select-tui-app-surface (app id-or-index)
  "Activate a surface by id or index. Selecting surface 0 rides the
hard-redraw doctrine (reset-pending clears and reprints from the model); any
other surface repaints from a cleared screen. Returns the selected id, NIL
when unknown or refused."
  (unless (tui-app-foreign-thread-p app "select-surface refused off the loop thread")
    (let* ((surfaces (tui-app-surfaces app))
           (index (if (integerp id-or-index)
                      (and (< -1 id-or-index (length surfaces)) id-or-index)
                      (position id-or-index surfaces :key #'surface-id))))
      (when index
        (setf (tui-app-active-surface-index app) index)
        (if (zerop index)
            (redraw-tui-app app)
            (render-tui-app app :clear t :force t))
        (surface-id (nth index surfaces))))))

(defun cycle-tui-app-surface (app &optional (delta 1))
  "Activate the surface DELTA steps from the active one, wrapping."
  (unless (tui-app-foreign-thread-p app "cycle-surface refused off the loop thread")
    (select-tui-app-surface
     app (mod (+ (tui-app-active-surface-index app) delta)
              (length (tui-app-surfaces app))))))

(defun add-tui-app-route-interceptor (app id fn)
  "Register FN under ID at the end of the chain; a live ID re-registers in
place, keeping its position. FN is called (fn APP EVENT) ahead of the active
surface and consumes the event by returning :handled. Returns ID, NIL when
refused."
  (unless (tui-app-foreign-thread-p app "add-interceptor refused off the loop thread")
    (let ((entry (assoc id (tui-app-route-interceptors app))))
      (if entry
          (setf (cdr entry) fn)
          (setf (tui-app-route-interceptors app)
                (append (tui-app-route-interceptors app)
                        (list (cons id fn))))))
    id))

(defun remove-tui-app-route-interceptor (app id)
  "Drop ID's interceptor. Returns ID, NIL when refused."
  (unless (tui-app-foreign-thread-p app "remove-interceptor refused off the loop thread")
    (setf (tui-app-route-interceptors app)
          (remove id (tui-app-route-interceptors app) :key #'car :count 1))
    id))

(defun request-tui-app-render (app)
  "Ask for a repaint of the active surface at the next trampoline drain.
Thread-safe: rides the main-thread task queue, and render-pending coalesces a
burst of requests into one frame."
  (enqueue-main-thread-task
   app (lambda () (setf (tui-app-render-pending-p app) t))))

(defun tui-app-editor (app)
  (transcript-view-editor (tui-app-view app)))

(defun tui-app-editor-value (app)
  (editor-value (tui-app-editor app)))

(defun tui-app-transcript-events (app)
  (transcript-events (tui-app-transcript app)))

(defun tui-app-add-event (app event)
  (transcript-add-event (tui-app-transcript app) event))

(defun tui-app-add-system-event (app text)
  (tui-app-add-event
   app
   (make-transcript-event :system nil text)))

(defun set-tui-app-notice (app text)
  "Show TEXT as the hint line just above the prompt. It lives on the renderer,
   not the transcript, so clearing it leaves no :system row behind. A plain
   notice projects live state and is cleared by its state transition, so it
   carries no expiry (NOTICE-EXPIRES-AT is set iff the notice is transient)."
  (setf (scrollback-renderer-notice (tui-app-renderer app)) text
        (tui-app-notice-expires-at app) nil))

(defun clear-tui-app-notice (app)
  (setf (scrollback-renderer-notice (tui-app-renderer app)) nil
        (tui-app-notice-expires-at app) nil))

(defun set-tui-app-transient-notice (app text duration
                                     &optional (now (get-internal-real-time)))
  "Show TEXT as an ephemeral acknowledgment that auto-clears after DURATION.
   Unlike a plain notice it has no backing state to clear it, so it carries an
   expiry the trampoline's POLL-TUI-APP-NOTICE-EXPIRY honors."
  (setf (scrollback-renderer-notice (tui-app-renderer app)) text
        (tui-app-notice-expires-at app) (+ now duration)))

(defun mark-tui-app-reply-aborted (app)
  "Annotate the in-flight assistant reply as aborted (pi-style), so the abort
   shows as a marker on the reply itself rather than a separate transcript row.
   Returns the stamped event, or NIL when nothing was streaming (e.g. Esc before
   any delta arrived) — the caller uses that to decide on alternate feedback."
  (let ((streaming (scrollback-renderer-streaming-event (tui-app-renderer app))))
    (when streaming
      (setf (event-status streaming) :aborted)
      streaming)))

(defun restore-tui-app-editor-prompt (app text)
  "Prepend TEXT before the current editor value, newline-joined when both non-empty."
  (when (and text (plusp (length text)))
    (let* ((current (tui-app-editor-value app))
           (joined (if (plusp (length current))
                       (concatenate 'string text (string #\Newline) current)
                       text)))
      (set-editor-value (tui-app-editor app) joined))))

(defun perform-tui-app-retract (app mode-id)
  "Un-send the pre-stream-aborted prompt: repoint the session leaf, drop the
   transcript row, restore the text to the editor, clear notice + slots. When
   the session refuses the repoint (turn entries landed, so un-sending would
   orphan committed work) the prompt stays sent: keep the row, leave the editor,
   acknowledge the abort with a transient notice. Returns T."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx)))
         (prompt  (tui-app-pending-prompt app))
         (retracted (or (null service)
                        (retract-agent-session-pending-prompt
                         service mode-id ctx (tui-app-pre-turn-leaf-id app)))))
    (cond
      (retracted
       (transcript-remove-last-user-event (tui-app-transcript app))
       (restore-tui-app-editor-prompt app prompt)
       (clear-tui-app-notice app))
      (t
       (set-tui-app-transient-notice app "Interrupted."
                                     +tui-interrupted-notice-window+)))
    (setf (tui-app-retract-armed-p app) nil
          (tui-app-pending-prompt app) nil
          (tui-app-pre-turn-leaf-id app) nil))
  t)

(defun clear-tui-app-screen (app)
  (scrollback-clear-screen (tui-app-renderer app)))

(defun reset-tui-app (app)
  (transcript-clear (tui-app-transcript app))
  (scrollback-reset (tui-app-renderer app))
  ;; Per-turn stream accumulators reference rows the clear just dropped -- a
  ;; late delta from an aborted turn would re-append its stale live event.
  (clrhash (tui-app-projection-buffer app))
  (setf (tui-app-interrupt-armed-p app) nil
        (tui-app-abort-armed-at app) nil
        (tui-app-rewind-armed-at app) nil
        (tui-app-retract-armed-p app) nil
        (tui-app-pending-prompt app) nil
        (tui-app-pre-turn-leaf-id app) nil
        (tui-app-tool-update-text app) nil)
  app)

(defparameter +agent-worker-join-timeout+ 5
  "Seconds stop-tui-app waits for a live agent worker to finish before giving up.
   Cooperative abort only halts between turns, so a worker blocked mid-stream
   can't be unstuck here — that's the socket-close path — but the bound wait keeps
   quit from hanging on it.")

(defun join-agent-worker (app)
  "Wait (bounded) for the in-flight agent worker to finish, then forget it, so the
   loop thread doesn't tear the app down underneath a running turn."
  (let ((thread (tui-app-agent-worker-thread app)))
    (when (and thread (sb-thread:thread-alive-p thread))
      (ignore-errors
       (sb-thread:join-thread thread
                              :timeout +agent-worker-join-timeout+
                              :default nil)))
    (setf (tui-app-agent-worker-thread app) nil))
  app)

(defun join-command-worker (app)
  "Wait (bounded) for an in-flight command worker (e.g. /compact's summarizer or
   a remote /install) to finish, then forget it. A command body has no
   cooperative abort the way an agent turn has socket-close, so the bound wait is
   what keeps quit from hanging on one parked in a model call."
  (let ((thread (tui-app-command-worker-thread app)))
    (when (and thread (sb-thread:thread-alive-p thread))
      (ignore-errors
       (sb-thread:join-thread thread
                              :timeout +agent-worker-join-timeout+
                              :default nil)))
    (setf (tui-app-command-worker-thread app) nil))
  app)

(defun stop-tui-app (app)
  "Tear the app down. Aborts the in-flight turn before joining: the abort
   shuts down the request socket, unblocking a worker parked mid-stream, so
   the bounded join reaps it instead of timing out and abandoning the thread
   and its connection. Both worker slots are joined, since an agent turn and a
   command body can be in flight at once."
  (unregister-tui-app-listener app)
  (let* ((ctx (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx))))
    (when service
      (abort-agent-session service (tui-app-mode-id app) ctx)))
  (join-agent-worker app)
  (join-command-worker app)
  (setf (tui-app-running-p app) nil)
  app)

(defun request-tui-app-interrupt (app)
  (if (tui-app-interrupt-armed-p app)
      (progn
        (setf (tui-app-interrupt-armed-p app) nil)
        (tui-app-add-system-event app "Exiting.")
        (stop-tui-app app)
        :quit)
      (progn
        (setf (tui-app-interrupt-armed-p app) t)
        (tui-app-add-system-event app "Press Ctrl+C again to quit.")
        :armed)))

(defparameter +tui-abort-rearm-window+
  (round (* 3 internal-time-units-per-second) 2)
  "Span (~1.5s, in internal-time units) within which a second Esc aborts the
   running turn. A later Esc re-arms instead, so a lone misclick can't interrupt.")

(defun request-tui-app-abort (app &optional (now (get-internal-real-time)))
  "Esc while a turn is in flight. The first press arms and hints, a second press
within +TUI-ABORT-REARM-WINDOW+ aborts the turn. At an idle prompt the press
routes to the rewind arc instead, which is inert when there is nothing to
rewind, so Esc stays a no-op on an empty conversation.

The abort branch decides on pending-prompt, which is non-nil iff no delta landed
this turn since the first delta clears it, so it and not the stale
streaming-event governs. With a pending prompt, retract at turn-end where the
editor restore is the feedback. Mid-stream, stamp the in-flight reply.
Pre-stream with nothing to retract, acknowledge with a transient notice."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx)))
         (mode-id (tui-app-mode-id app))
         (armed   (tui-app-abort-armed-at app)))
    (cond
      ((not (and service (agent-session-busy-p service mode-id ctx)))
       (setf (tui-app-abort-armed-at app) nil)
       (request-tui-app-rewind app now))
      ((and armed (< (- now armed) +tui-abort-rearm-window+))
       (setf (tui-app-abort-armed-at app) nil
             (tui-app-rewind-armed-at app) nil)
       (clear-tui-app-notice app)
       (cond
         ((tui-app-pending-prompt app)
          (setf (tui-app-retract-armed-p app) t))
         ((mark-tui-app-reply-aborted app))
         (t
          (set-tui-app-transient-notice app "Interrupted."
                                         +tui-interrupted-notice-window+ now)))
       (abort-agent-session service mode-id ctx)
       (render-tui-app app)
       :aborted)
      (t
       (setf (tui-app-abort-armed-at app) now
             (tui-app-rewind-armed-at app) nil)
       (set-tui-app-notice app "Press Esc again to interrupt.")
       (render-tui-app app)
       :armed))))

(defun rewind-target-preview (text)
  "One-line menu preview of a prompt's text."
  (if (stringp text)
      (substitute #\Space #\Newline text)
      ""))

(defun perform-tui-app-rewind (app entry-id)
  "Rewind the bound session to before ENTRY-ID's prompt. The session switch
replays the shortened transcript and the :session-rewind projection restores
the prompt text to the editor. Refused with a notice when a turn went in
flight while the menu was up."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx)))
         (mode-id (tui-app-mode-id app)))
    (cond
      ((or (null service)
           (agent-session-busy-p service mode-id ctx))
       (set-tui-app-transient-notice app "Session is busy."
                                     +tui-interrupted-notice-window+))
      (t
       (rewind-agent-session service mode-id ctx :entry-id entry-id)))
    (render-tui-app app)))

(defun open-tui-app-rewind-menu (app)
  "Open a selection menu over the bound session's user turns, newest first.
Up/Down move, Enter rewinds to before the chosen turn, Esc dismisses.
Returns T when the menu opened, NIL when there is nothing to rewind."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx)))
         (mode-id (tui-app-mode-id app))
         (targets (and service (list-rewind-targets service mode-id ctx))))
    (when targets
      (open-editor-selection
       (tui-app-editor app)
       (loop for target in targets
             for back from 1
             collect (make-completion-candidate
                      :insert (format nil "~D back" back)
                      :match ""
                      :description (rewind-target-preview (getf target :text))
                      :value (getf target :entry-id)))
       :action (lambda (candidate)
                 (perform-tui-app-rewind
                  app (completion-candidate-value candidate))))
      t)))

(defun branch-candidate-description (row active-id)
  "Menu row text for a session listing ROW: the active-session marker, the
trunk glyphs locating it in the tree, and the label that distinguishes it
-- its branch context when it has any, its opening prompt otherwise,
falling back to the display name for rows without one (corrupt files list
as \"(corrupt)\")."
  (format nil "~A~A~A"
          (if (eq (getf row :id) active-id) "* " "  ")
          (getf row :tree-prefix "")
          (rewind-target-preview
           (or (kli/session/log:session-row-branch-label row)
               (getf row :preview)
               (getf row :name)))))

(defun perform-tui-app-session-switch (app row)
  "Switch the bound mode onto ROW's stored session. Refused with a notice
when the row is corrupt or a turn went in flight while the menu was up. The
session switch replays the chosen conversation into a cleared viewport.
The menu accept that invokes this repaints afterwards."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx)))
         (mode-id (tui-app-mode-id app)))
    (cond
      ((getf row :corrupt)
       (set-tui-app-transient-notice
        app
        (format nil "Cannot switch to ~(~A~) -- the session file is corrupt."
                (getf row :id))
        +tui-interrupted-notice-window+))
      ((or (null service)
           (agent-session-busy-p service mode-id ctx))
       (set-tui-app-transient-notice app "Session is busy."
                                     +tui-interrupted-notice-window+))
      (t
       (resume-agent-session service mode-id (getf row :id) ctx)))))

(defun tui-app-active-session-id (app)
  "The session id bound to APP's mode, NIL without a context or service."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx))))
    (and service
         (getf (session-mode-info service (tui-app-mode-id app) ctx) :id))))

(defun tui-app-session-store (app)
  "The session store behind APP's context, NIL without one."
  (let ((ctx (tui-app-context app)))
    (and ctx (find-live-object (context-registry ctx) :session-store))))

(defun reattach-tui-app (app)
  "Re-wire APP after a snapshot restore reconstructed the agent-session service.
The old listener is stranded on the discarded service and the :session-switch
that drives the replay already fired before this call, so bind a fresh listener
to the new service, replay the bound session, and repaint."
  (register-tui-app-listener app (tui-app-context app))
  (let ((session-id (tui-app-active-session-id app)))
    (when session-id
      (replay-session-into-transcript app session-id)))
  (render-tui-app app)
  app)

(defun open-tui-app-menu (app rows action)
  "Open a selection menu over ROWS, each a plist with :insert, :description,
and :value entries. Up/Down move, Enter calls ACTION with the chosen row's
:value and repaints, Esc dismisses. Returns T when the menu opened, NIL
when ROWS is empty."
  (when rows
    (open-editor-selection
     (tui-app-editor app)
     (loop for row in rows
           collect (make-completion-candidate
                    :insert (getf row :insert)
                    :match ""
                    :description (getf row :description)
                    :value (getf row :value)))
     :action (lambda (candidate)
               (funcall action (completion-candidate-value candidate))
               (render-tui-app app)))
    t))

(defun open-tui-app-session-menu (app rows describe)
  "Open a selection menu over session listing ROWS, with DESCRIBE producing
each row's menu text beside its session id. Up/Down move, Enter switches
onto the chosen session, Esc dismisses. Returns T when the menu opened,
NIL when ROWS is empty."
  (open-tui-app-menu
   app
   (loop for row in rows
         collect (list :insert (string-downcase (symbol-name (getf row :id)))
                       :description (funcall describe row)
                       :value row))
   (lambda (row) (perform-tui-app-session-switch app row))))

(defun open-tui-app-branches-menu (app)
  "Open a selection menu over the stored-session forest, each session
indented under the one it branched from with the current session marked.
Up/Down move, Enter switches onto the chosen session, Esc dismisses.
Returns T when the menu opened, NIL when nothing is stored."
  (let ((store (tui-app-session-store app))
        (active-id (tui-app-active-session-id app)))
    (open-tui-app-session-menu
     app
     (kli/session/log:session-forest
      (remove-if #'kli/session/log:blank-session-row-p
                 (and store (kli/session/log:list-stored-sessions store))))
     (lambda (row) (branch-candidate-description row active-id)))))

(defun resume-candidate-description (row active-id)
  "Menu row text for a stored-session listing ROW: the active-session
marker, the display name when one is set, the entry count, and the opening
prompt flattened to one line."
  (format nil "~A~@[~A  ~]~D msgs~@[  ~A~]"
          (if (eq (getf row :id) active-id) "* " "  ")
          (getf row :name)
          (getf row :entry-count)
          (let ((preview (getf row :preview)))
            (and preview (rewind-target-preview preview)))))

(defun open-tui-app-resume-menu (app)
  "Open a selection menu over the stored sessions, newest first with the
current session marked. Up/Down move, Enter switches onto the chosen
session, Esc dismisses. Returns T when the menu opened, NIL when nothing
is stored."
  (let ((store (tui-app-session-store app))
        (active-id (tui-app-active-session-id app)))
    (open-tui-app-session-menu
     app
     (remove-if #'kli/session/log:blank-session-row-p
                (and store (kli/session/log:list-stored-sessions store)))
     (lambda (row) (resume-candidate-description row active-id)))))

(defun request-tui-app-rewind (app &optional (now (get-internal-real-time)))
  "Esc at an idle prompt. The first press arms and hints when the session has
a user turn to step past, a second press within +TUI-ABORT-REARM-WINDOW+
opens the rewind menu over the session's user turns. Inert when there is
nothing to rewind, so Esc stays a no-op on an empty conversation."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx)))
         (mode-id (tui-app-mode-id app))
         (armed   (tui-app-rewind-armed-at app)))
    (setf (tui-app-rewind-armed-at app) nil)
    (cond
      ((not (and service
                 (rewind-agent-session-available-p service mode-id ctx)))
       (clear-tui-app-notice app)
       :idle)
      ((and armed (< (- now armed) +tui-abort-rearm-window+))
       (clear-tui-app-notice app)
       (open-tui-app-rewind-menu app)
       (render-tui-app app)
       :rewind-menu)
      (t
       (setf (tui-app-rewind-armed-at app) now)
       (set-tui-app-notice app "Press Esc again to rewind.")
       (render-tui-app app)
       :rewind-armed))))

(defun poll-abort-rearm-lapse (app &optional (now (get-internal-real-time)))
  "Once the re-arm window since the last Esc lapses, drop the armed state and the
   transient 'Press Esc again' hint: a later Esc would only re-arm anyway, so the
   stale offer shouldn't linger above the prompt. Covers both Esc arcs (abort
   while busy, rewind at idle), which are mutually exclusive. Cheap no-op unless
   armed. Returns :lapsed when it cleared, :idle otherwise."
  (let ((abort-armed  (tui-app-abort-armed-at app))
        (rewind-armed (tui-app-rewind-armed-at app)))
    (if (or (and abort-armed (>= (- now abort-armed) +tui-abort-rearm-window+))
            (and rewind-armed
                 (>= (- now rewind-armed) +tui-abort-rearm-window+)))
        (progn
          (setf (tui-app-abort-armed-at app) nil
                (tui-app-rewind-armed-at app) nil)
          (clear-tui-app-notice app)
          (render-tui-app app)
          :lapsed)
        :idle)))

(defun poll-tui-app-notice-expiry (app &optional (now (get-internal-real-time)))
  "Drop a transient acknowledgment once its window lapses, so it doesn't linger
   above the prompt. Cheap no-op unless a transient notice is up (only those
   carry NOTICE-EXPIRES-AT). Returns :expired when it cleared, :idle otherwise."
  (let ((deadline (tui-app-notice-expires-at app)))
    (if (and deadline (>= now deadline))
        (progn
          (clear-tui-app-notice app)
          (render-tui-app app)
          :expired)
        :idle)))

(defparameter +spinner-tick-interval+
  (round internal-time-units-per-second 10)
  "Span (~100ms, in internal-time units) between working-indicator frame advances,
   about 10 Hz while a turn streams.")

(defun tui-app-agent-busy-p (app)
  "True when APP's bound agent session has a turn in flight."
  (let* ((ctx     (tui-app-context app))
         (service (and ctx (agent-session-service-for-app ctx)))
         (mode-id (tui-app-mode-id app)))
    (and service (agent-session-busy-p service mode-id ctx) t)))

(defun advance-tui-app-spinner (app busy now)
  "Drive the working-indicator state from BUSY at time NOW. While busy, show it and
   step one frame every +SPINNER-TICK-INTERVAL+. When idle, retire it once. Returns
   T when the indicator changed and needs a repaint, NIL on a no-op tick."
  (cond
    (busy
     (cond
       ((not (tui-app-spinner-active-p app))
        (setf (tui-app-spinner-active-p app) t
              (tui-app-spinner-phase app) 0
              (tui-app-spinner-tick-at app) now)
        t)
       ((>= (- now (tui-app-spinner-tick-at app)) +spinner-tick-interval+)
        (incf (tui-app-spinner-phase app))
        (setf (tui-app-spinner-tick-at app) now)
        t)
       (t nil)))
    ((tui-app-spinner-active-p app)
     (setf (tui-app-spinner-active-p app) nil
           (tui-app-spinner-tick-at app) nil)
     t)
    (t nil)))

(defun poll-tui-app-spinner (app &optional (now (get-internal-real-time)))
  "Advance the working indicator while a turn streams and retire it when the turn
   ends, repainting on a change. Cheap no-op at an idle prompt. Returns
   :ticked when it repainted, :idle otherwise."
  (if (advance-tui-app-spinner app (tui-app-agent-busy-p app) now)
      (progn (render-tui-app app) :ticked)
      :idle))

(defparameter *render-fault-escalation-streak* 3
  "Contained render faults in a row before the render barrier escalates.")

(defparameter *step-fault-escalation-streak* 250
  "Contained runtime-step faults in a row before the step site fail-stops. At
the idle cadence (~50 iterations/s) this leaves a few seconds of visible
flooding to repair a broken hot-patch before the image exits honestly.")

(defun tui-app-foreign-thread-p (app what)
  "True (and noted to the :render sink) when APP's loop thread is captured and
the calling thread is not it. The renderer is stateful and single-threaded, so
an off-thread frame corrupts it."
  (let ((owner (tui-app-loop-thread app)))
    (when (and owner (not (eq owner sb-thread:*current-thread*)))
      (kli/ext:note-fault
       :render
       (list :cross-thread (sb-thread:thread-name sb-thread:*current-thread*))
       what)
      t)))

(defun render-tui-app (app &key force clear)
  "Render a frame. With CLEAR, wipe the visible screen and home the cursor
   inside the same batched frame, so the takeover and the paint reach the
   terminal as one write. Native scrollback is never touched -- the hard-redraw
   reset path also clears only the visible screen, duplicating the reprinted
   transcript in scrollback rather than destroying the shell's pre-launch
   history."
  (when (tui-app-foreign-thread-p app "render refused off the loop thread")
    (return-from render-tui-app nil))
  (kli/ext:with-extension-fault-barrier
      (:seam :render
       :policy (if (>= (tui-app-render-fault-streak app)
                       *render-fault-escalation-streak*)
                   :escalate
                   :continue)
       :on-fault (progn
                   (incf (tui-app-render-fault-streak app))
                   (setf (tui-app-render-reset-pending-p app) t)
                   (tui-app-add-system-event
                    app (format nil "Internal render fault — frame skipped (see ~A)."
                                (kli/ext:fault-log-path :render)))
                   nil))
    (if (zerop (tui-app-active-surface-index app))
        (let ((force force)
              (terminal (tui-app-terminal app)))
          (terminal-begin-frame terminal)
          (unwind-protect
               (progn
                 (when clear
                   ;; The screen is blank after the clear, so a line diff against
                   ;; the region cache would skip "unchanged" rows - force the
                   ;; full repaint.
                   (terminal-clear-screen terminal)
                   (scrollback-anchor-bottom (tui-app-renderer app))
                   (setf force t))
                 (when (shiftf (tui-app-render-reset-pending-p app) nil)
                   (terminal-clear-screen terminal)
                   ;; A hard reset during an open stream must keep the stream open.
                   ;; Dropping it would let the reprint commit the half-streamed
                   ;; event, and the next delta would re-open it behind the commit
                   ;; boundary - inconsistent renderer geometry.
                   (let* ((renderer (tui-app-renderer app))
                          (streaming (scrollback-renderer-streaming-event renderer)))
                     (scrollback-reset renderer)
                     (when streaming
                       (begin-scrollback-stream renderer streaming))
                     (scrollback-anchor-bottom renderer))
                   (setf force t))
                 (prog1 (render-frame (tui-app-renderer app) terminal :force force)
                   (setf (tui-app-render-fault-streak app) 0)))
            (terminal-end-frame terminal)))
        ;; Generic arm: no scrollback-renderer state to preserve, so clear and
        ;; a consumed reset-pending both collapse to a forced full repaint.
        ;; The surface-0 open-stream dance stays in the arm above -- switching
        ;; back re-arms reset-pending via select -> redraw-tui-app.
        (let ((force force)
              (terminal (tui-app-terminal app))
              (reset (shiftf (tui-app-render-reset-pending-p app) nil)))
          (terminal-begin-frame terminal)
          (unwind-protect
               (progn
                 (when (or clear reset)
                   (terminal-clear-screen terminal)
                   (setf force t))
                 (prog1 (render-frame
                         (surface-renderer (tui-app-active-surface app))
                         terminal :force force)
                   (setf (tui-app-render-fault-streak app) 0)))
            (terminal-end-frame terminal))))))

(defun redraw-tui-app (app)
  "Hard redraw: clear the visible screen and reprint everything from the model.
   Native scrollback keeps the prior paint -- resize and Ctrl+O route here, and
   wiping scrollback (ESC[3J) would destroy the surrounding shell's pre-launch
   history on every reflow."
  (when (tui-app-foreign-thread-p app "redraw refused off the loop thread")
    (return-from redraw-tui-app app))
  (setf (tui-app-render-reset-pending-p app) t)
  (render-tui-app app)
  app)

(defun apply-terminal-size (app columns rows)
  "Resync the terminal's cached dimensions to COLUMNS x ROWS and hard-redraw on a
   change. Committed scrollback was wrapped at the old width, so a width change
   needs a full reflow, not a line diff. Returns T when it reflowed."
  (let ((terminal (tui-app-terminal app)))
    (when (or (/= columns (terminal-columns terminal))
              (/= rows (terminal-rows terminal)))
      (setf (terminal-columns terminal) columns
            (terminal-rows terminal) rows)
      (redraw-tui-app app)
      t)))

(defun poll-terminal-resize (app)
  "Reflow when the live window size has drifted from the cached dimensions. The
   ioctl is sub-microsecond, so this is cheap enough to run on every idle tick."
  (let ((terminal (tui-app-terminal app)))
    (when (typep terminal 'process-terminal)
      (multiple-value-bind (columns rows) (process-terminal-size terminal)
        (apply-terminal-size app columns rows)))))

(defun run-tui-app-route-interceptors (app event)
  "Walk the interceptor chain in insertion order, stopping at the first FN
that returns :handled. T when one consumed EVENT. A signaling interceptor is
contained by the runtime-step behavior cell the caller already runs under."
  (loop for (nil . fn) in (tui-app-route-interceptors app)
          thereis (eq (funcall fn app event) :handled)))

(defun route-tui-app-event (app event)
  (if (eq (input-event-kind event) :interrupt)
      ;; Ctrl+C is the app's own arc, never a surface's: bypass the chain so
      ;; a broken interceptor cannot swallow the quit path.
      (route-input-event (tui-app-view app) event (tui-app-route-context app))
      (or (run-tui-app-route-interceptors app event)
          (let ((surface (tui-app-active-surface app)))
            (route-input-event (surface-view surface) event
                               (surface-route-context surface))))))

(defun route-tui-app-events (app events)
  (dolist (event events)
    (route-tui-app-event app event))
  events)

(defun flush-tui-app-input (app &key (render t) force)
  (let ((events (input-decoder-flush (tui-app-decoder app))))
    (route-tui-app-events app events)
    (when (and render events)
      (render-tui-app app :force force))
    events))

(defun tui-app-feed (app data &key (render t) force)
  (let ((events (input-decoder-feed (tui-app-decoder app) data)))
    (route-tui-app-events app events)
    (when (and render events)
      (render-tui-app app :force force))
    events))

(defun tui-app-step (app data &key (render t) force)
  (tui-app-feed app data :render render :force force))

(defun tui-app-runtime-step (app input-stream)
  "Run one runtime step. Binds the reify hook at this site because the runtime
cell's :reify disposition needs it and the bindings inside
default-tui-app-runtime-step are lost on hot-patch. :error-handled is every step
barrier's contained-fault fallback. A long streak means input handling is gone
and each iteration floods one tui-error row, so failing stop beats looping as a
zombie."
  (let* ((kli/ext:*fault-note-hook* (tui-app-fault-hook app))
         (kli/ext:*fault-reify-hook* (tui-app-reify-hook app))
         (result (call-behavior (tui-app-runtime-behavior app)
                                app
                                input-stream)))
    (if (eq result :error-handled)
        (when (>= (incf (tui-app-step-fault-streak app))
                  *step-fault-escalation-streak*)
          (error "Runtime step faulted ~D consecutive iterations."
                 (tui-app-step-fault-streak app)))
        (setf (tui-app-step-fault-streak app) 0))
    result))

(defun recode-tui-app-runtime (app &key function version
                                     (state nil state-p)
                                     (metadata nil metadata-p)
                                     (capabilities nil capabilities-p))
  (apply #'recode-tui-behavior
         (tui-app-runtime-behavior app)
         (append (when function
                   (list :function function))
                 (when version
                   (list :version version))
                 (when state-p
                   (list :state state))
                 (when metadata-p
                   (list :metadata metadata))
                 (when capabilities-p
                   (list :capabilities capabilities))))
  app)

(defun read-tui-app-input-chunk (stream first-char)
  (with-output-to-string (out)
    (write-char first-char out)
    (loop for char = (read-char-no-hang stream nil nil)
          while char
          do (write-char char out))))

(defun reify-tui-error (app condition)
  "Reify a step-escaped CONDITION as a :tui-error event. Stamps the app's
   mode-id into the payload so persist-agent-event's mode resolution
   succeeds and the TUI listener projects it onto the transcript."
  (kli/event:emit-event
   (tui-app-context app)
   (kli/event:make-event
    :tui-error
    :payload (list :mode (tui-app-mode-id app)
                   :condition-type (type-of condition)
                   :category (kli/ext:condition-category condition)
                   :message (princ-to-string condition)))))

(defun tui-app-reify-hook (app)
  "Reify hook for loop-thread barriers: surface the condition as a :tui-error."
  (lambda (condition seam id)
    (declare (ignore seam id))
    (reify-tui-error app condition)))

(defun tui-app-fault-hook (app)
  "Note hook for every contained fault: emit it as a :fault event onto the app's
stream so the observability sink and any other subscriber see it. Bound wherever
the reify hook is, since both ride the same barrier lifecycle."
  (kli/event:make-fault-emitter (tui-app-context app)))

(defparameter +tui-escape-flush-delay+
  (round internal-time-units-per-second 25)
  "Idle span (~40ms, in internal-time units) a dangling Esc waits for a
   continuation byte before it is flushed as a standalone keypress. The terminal
   sends a lone Esc and the bytes of a multi-byte escape sequence the same way —
   one read — so the only way to tell them apart is to wait briefly.")

(defun escape-pending-p (decoder)
  "T when the decoder is holding an unresolved escape sequence (buffer led by Esc)."
  (let ((buffer (input-decoder-buffer decoder)))
    (and (plusp (length buffer))
         (char= #\Esc (char buffer 0)))))

(defun tui-app-flush-dangling-escape (app now)
  "Flush a lone Esc the terminal left buffered once it has gone unanswered for
   +TUI-ESCAPE-FLUSH-DELAY+, so a single keypress registers instead of waiting
   for the next byte. Returns :input when it flushes, :idle otherwise."
  (let ((since (tui-app-escape-pending-since app)))
    (cond
      ((not (escape-pending-p (tui-app-decoder app)))
       (setf (tui-app-escape-pending-since app) nil)
       :idle)
      ((null since)
       (setf (tui-app-escape-pending-since app) now)
       :idle)
      ((>= (- now since) +tui-escape-flush-delay+)
       (setf (tui-app-escape-pending-since app) nil)
       (flush-tui-app-input app)
       :input)
      (t :idle))))

(defun default-tui-app-runtime-step (app input-stream)
  (let ((kli/ext:*fault-note-hook* (tui-app-fault-hook app))
        (kli/ext:*fault-reify-hook* (tui-app-reify-hook app)))
    (kli/ext:with-extension-fault-barrier
        (:seam :runtime-step :policy :reify :on-fault :error-handled)
      (let ((char (read-char-no-hang input-stream nil :eof)))
        (cond
          ((eq char :eof)
           (stop-tui-app app)
           :eof)
          (char
           (tui-app-feed app
                         (read-tui-app-input-chunk input-stream char))
           (setf (tui-app-escape-pending-since app)
                 (when (escape-pending-p (tui-app-decoder app))
                   (get-internal-real-time)))
           :input)
          (t
           (tui-app-flush-dangling-escape app (get-internal-real-time))))))))

(defun enqueue-main-thread-task (app thunk)
  "Queue THUNK to run on the TUI loop thread at the next trampoline iteration.
   Safe to call from another thread (e.g. a swank worker) — it never touches the
   runtime step, so unlike recoding the runtime it cannot wedge input handling."
  (sb-thread:with-mutex ((tui-app-main-thread-tasks-lock app))
    (push thunk (tui-app-main-thread-tasks app)))
  app)

(defun call-on-main-thread-task (app thunk)
  "Run THUNK on the loop thread and block the caller until it returns, passing
   back its primary value or re-signaling its error. Built on the same queue as
   enqueue-main-thread-task, so it is safe from a worker thread. Calling it FROM
   the loop thread deadlocks — on-loop callers run THUNK directly."
  (let ((sem (sb-thread:make-semaphore))
        (value nil)
        (failure nil))
    (enqueue-main-thread-task
     app
     (lambda ()
       (unwind-protect
            (handler-case (setf value (funcall thunk))
              (error (c) (setf failure c)))
         (sb-thread:signal-semaphore sem))))
    (sb-thread:wait-on-semaphore sem)
    (if failure (error failure) value)))

(defun run-pending-main-thread-tasks (app)
  "Drain queued tasks and run them in FIFO order on the calling thread. Each task
   is isolated: one that signals is reified as a :tui-error and the rest still
   run. A single repaint after the batch coalesces the renders requested by a
   burst of marshaled projections, so the editor cursor settles once per drain
   instead of flickering through one repaint per stream delta."
  (let ((tasks (sb-thread:with-mutex ((tui-app-main-thread-tasks-lock app))
                 (prog1 (nreverse (tui-app-main-thread-tasks app))
                   (setf (tui-app-main-thread-tasks app) nil)))))
    (let ((kli/ext:*fault-note-hook* (tui-app-fault-hook app))
          (kli/ext:*fault-reify-hook* (tui-app-reify-hook app)))
      (dolist (task tasks)
        (kli/ext:with-extension-fault-barrier (:seam :main-thread-task :policy :reify)
          (funcall task))))
    (when (tui-app-render-pending-p app)
      (setf (tui-app-render-pending-p app) nil)
      (render-tui-app app))))

(defun run-tui-app-trampoline (app input-stream &key (idle-sleep 0.02))
  (loop while (tui-app-running-p app)
        do (run-pending-main-thread-tasks app)
           (kli/ext:safely-invoke #'poll-abort-rearm-lapse
                                  :loop-poll :abort-rearm app)
           (kli/ext:safely-invoke #'poll-tui-app-notice-expiry
                                  :loop-poll :notice-expiry app)
           (kli/ext:safely-invoke #'poll-tui-app-spinner
                                  :loop-poll :spinner app)
           (case (tui-app-runtime-step app input-stream)
             ((:idle :error-handled)
              (kli/ext:safely-invoke #'poll-terminal-resize
                                     :loop-poll :terminal-resize app)
              (sleep idle-sleep))
             (:eof
              (return))
             (otherwise
              nil)))
  (flush-tui-app-input app))

(defun run-tui-app-character-loop (app input-stream)
  (terminal-enable-bracketed-paste (tui-app-terminal app))
  (unwind-protect
       (run-tui-app-trampoline app input-stream)
    (terminal-disable-bracketed-paste (tui-app-terminal app))))

(defun run-tui-app-line-loop (app input-stream)
  "Non-tty fallback: read a line, submit it, then wait for the agent worker and
   drain its marshaled projections before reading the next line. The trampoline
   drains on every iteration, but this loop has no such tick, so without an
   explicit drain a bound-mode turn's transcript mutations and renders would
   queue on main-thread-tasks and never run, growing it without bound."
  (loop while (tui-app-running-p app)
        for line = (read-line input-stream nil nil)
        while line
        do (tui-app-feed app line :render nil)
           (tui-app-feed app (string #\Return))
           (dolist (worker (list (tui-app-agent-worker-thread app)
                                 (tui-app-command-worker-thread app)))
             (when (and worker (sb-thread:thread-alive-p worker))
               (ignore-errors (sb-thread:join-thread worker))))
           (run-pending-main-thread-tasks app)))

(defun auto-theme-active-p (protocol)
  "True when no explicit theme is pinned: the stored mode is :auto or absent."
  (let ((mode (protocol-storage protocol +theme-mode-key+)))
    (or (null mode) (eq mode :auto))))

(defun resolve-auto-theme-mode (colorfgbg query-thunk)
  "Resolve the auto theme mode string and any input residual. A parseable
COLORFGBG decides directly and QUERY-THUNK is never called; otherwise QUERY-THUNK
runs the OSC 11 handshake, returning (values STATUS RESIDUAL) that
resolve-background-mode classifies."
  (let ((from-env (parse-colorfgbg colorfgbg)))
    (if from-env
        (values (string-downcase from-env) "")
        (multiple-value-bind (status residual) (funcall query-thunk)
          (values (string-downcase (resolve-background-mode status colorfgbg))
                  residual)))))

(defun resolve-auto-theme (app input-stream)
  "When auto theme is active and fd 0 is a terminal outside GNU screen, set the
dark or light theme. A parseable COLORFGBG decides directly; otherwise query the
background over OSC 11, replaying any keystrokes typed during the handshake."
  (let ((protocol (object-protocol app)))
    (when (and protocol
               (auto-theme-active-p protocol)
               (terminal-input-tty-p)
               (null (sb-ext:posix-getenv "STY")))
      (multiple-value-bind (mode residual)
          (resolve-auto-theme-mode
           (sb-ext:posix-getenv "COLORFGBG")
           (lambda ()
             (with-boot-stage ("terminal-theme-query")
               (call-with-raw-terminal
                (lambda ()
                  (query-terminal-background input-stream
                                             #'write-terminal-control))
                :fallback (lambda () (values :timeout ""))))))
        (set-active-theme protocol mode)
        (when (plusp (length residual))
          (tui-app-feed app residual :render nil))))))

(defun run-tui-app (app &key (input-stream *standard-input*)
                              (initial-render t))
  (setf (tui-app-running-p app) t
        (tui-app-loop-thread app) sb-thread:*current-thread*)
  (unwind-protect
       (progn
         (boot-marker "run-tui-app-start")
         (let ((terminal (tui-app-terminal app)))
           (when (typep terminal 'process-terminal)
             (multiple-value-bind (columns rows) (process-terminal-size terminal)
               (setf (terminal-columns terminal) columns
                     (terminal-rows terminal) rows))))
         (resolve-auto-theme app input-stream)
         (setf *color-mode* (detect-color-mode))
         (when initial-render
           (boot-marker "initial-render-start")
           (render-tui-app app :force t :clear t)
           (boot-marker "initial-render-done"))
         (boot-marker "input-loop-start")
         (call-with-terminal-character-input
          (lambda ()
            (run-tui-app-character-loop app input-stream))
          :fallback
          (lambda ()
            (run-tui-app-line-loop app input-stream))))
    (setf (tui-app-loop-thread app) nil))
  app)
