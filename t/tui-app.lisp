(in-package #:kli/tests)

(defun load-tui-app-stack (context)
  (install-extensions context
                      commands:*commands-extension-manifest*
                      event:*events-extension-manifest*
                      sess:*session-log-extension-manifest*
                      ctx:*context-lens-extension-manifest*
                      auth:*auth-extension-manifest*
                      models:*model-registry-extension-manifest*
                      rt:*model-runtime-extension-manifest*
                      agents:*agent-loop-extension-manifest*
                      agent-session:*agent-session-extension-manifest*
                      tui-views:*tui-views-extension-manifest*
                      tui-input:*tui-input-extension-manifest*
                      tui-editor:*tui-editor-extension-manifest*
                      tui-terminal:*tui-terminal-extension-manifest*
                      tui-transcript:*tui-transcript-extension-manifest*
                      tui-app:*tui-app-extension-manifest*))

(defun ensure-tui-app-stack ()
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-tui-app-stack context)
    context))

(defun ensure-tui-app-stack-with-session (&key (deltas '("ok")))
  (let ((context (ensure-tui-app-stack)))
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (bind-agent-session-mode context :deltas deltas)
    context))

(test tui-app-extension-registers-service
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (load-tui-app-stack context)
    (is (ext:extension-loaded-p protocol :tui-app))))

(test (tui-app-registers-listener-on-make :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (before (length (agent-session:session-event-listeners service)))
         (app (tui-app:make-tui-app :context context :columns 32)))
    (is (not (null (tui-app:tui-app-listener-id app))))
    (is (= (1+ before)
           (length (agent-session:session-event-listeners service))))))

(test (tui-app-submit-invokes-orchestrator :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (app (tui-app:make-tui-app :context context :columns 32))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding))))
    (tui-app:tui-app-feed app "hello" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (settle-tui-app app)
    (is (equal '(:user :assistant)
               (agent-session-message-roles agent)))
    (is (equal '("hello" "ack")
               (agent-session-message-contents agent)))))

(test (tui-app-user-echo-projects-from-tui-submitted-emission :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (app (tui-app:make-tui-app :context context :columns 32)))
    (is (zerop (length (tui-app:tui-app-transcript-events app))))
    (tui-app:tui-app-feed app "hello" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (settle-tui-app app)
    (let* ((events (tui-app:tui-app-transcript-events app))
           (user-events (remove-if-not
                         (lambda (e)
                           (and (eq :message (tui-transcript:event-kind e))
                                (eq :user (tui-transcript:event-role e))))
                         events)))
      (is (= 1 (length user-events))
          ":tui/user-submitted projects exactly one user echo")
      (is (string= "hello"
                   (tui-transcript:event-text (first user-events)))))))

(test (tui-app-line-loop-drains-worker-projections :fixture interactive-authority)
  "The non-tty line loop drains the agent worker's marshaled projections after
   each submitted line. Drives the loop directly with a piped prompt and, with
   no separate settle, the assistant reply must have landed in the transcript,
   or it would sit on main-thread-tasks unrun."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (app (tui-app:make-tui-app :context context :columns 32)))
    (tui-app::run-tui-app-line-loop
     app (make-string-input-stream (format nil "hello~%")))
    (let ((assistant-events
            (remove-if-not
             (lambda (e)
               (and (eq :message (tui-transcript:event-kind e))
                    (eq :assistant (tui-transcript:event-role e))))
             (tui-app:tui-app-transcript-events app))))
      (is (= 1 (length assistant-events))
          "the worker's assistant reply was drained into the transcript")
      (is (search "ack"
                  (tui-transcript:event-text (first assistant-events)))))))

(test (tui-app-unregisters-listener-on-stop :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (before (length (agent-session:session-event-listeners service)))
         (app (tui-app:make-tui-app :context context :columns 32)))
    (is (= (1+ before)
           (length (agent-session:session-event-listeners service))))
    (tui-app:stop-tui-app app)
    (is (null (tui-app:tui-app-listener-id app)))
    (is (= before (length (agent-session:session-event-listeners service))))))

(test (tui-app-builds-runnable-memory-session :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 32 :rows 8)))
    (is (typep app 'kli:live-object))
    (is (typep (tui-app:tui-app-terminal app)
               'tui-terminal:memory-terminal))
    (is (typep (tui-app:tui-app-view app)
               'tui-transcript:transcript-view))
    (is (tui-app:tui-app-running-p app))
    (tui-app:render-tui-app app :force t)
    (is (search "> "
                (tui-terminal:terminal-output
                 (tui-app:tui-app-terminal app))))))

(test (tui-app-registers-runtime-roots-when-given-context :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 32)))
    (is (eq app
            (kli:find-live-object (kli:context-registry context)
                                  (kli:object-id app))))
    (dolist (object (list (tui-app:tui-app-terminal app)
                          (tui-app:tui-app-decoder app)
                          (tui-app:tui-app-transcript app)
                          (tui-app:tui-app-view app)
                          (tui-app:tui-app-renderer app)
                          (tui-app:tui-app-runtime-behavior app)
                          (tui-app:tui-app-route-context app)))
      (is (eq object
              (kli:find-live-object (kli:context-registry context)
                                    (kli:object-id object)))))))

(test (tui-app-provider-exposes-live-introspection :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (protocol (kli:active-protocol context))
         (provider (ext:require-capability-provider
                    protocol
                    :tui/app
                    :contract :tui/app/v1))
         (app (tui-app:make-tui-app
               :context context
               :columns 32
               :on-submit
               (lambda (app input)
                 (declare (ignore app input))
                 nil))))
    (tui-app:tui-app-feed app "hello" :render nil)
    (is (string= "hello"
                 (ext:provider-call provider
                                    :tui-app-editor-value
                                    app)))
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (is (= 1 (length (ext:provider-call provider
                                        :tui-app-transcript-events
                                        app))))))

(test (tui-app-runtime-behavior-recodes-inside-running-trampoline :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app
               :context context
               :columns 48
               :on-submit
               (lambda (app input)
                 (declare (ignore app input))
                 nil)))
         (calls 0))
    (tui-app:recode-tui-app-runtime
     app
     :function
     (lambda (app input-stream)
       (declare (ignore input-stream))
       (incf calls)
       (if (= calls 1)
           (progn
             (tui-app:recode-tui-app-runtime
              app
              :function
              (lambda (app input-stream)
                (declare (ignore input-stream))
                (incf calls)
                (tui-app:tui-app-feed app "swapped" :render nil)
                (tui-app:tui-app-feed app (string #\Return) :render nil)
                (tui-app:stop-tui-app app)
                :swapped))
             :patched)
           (error "Old app runtime behavior was re-entered."))))
    (kli/tui/app::run-tui-app-trampoline
     app
     (make-string-input-stream "")
     :idle-sleep 0)
    (is (= 2 calls))
    (is (string= "swapped"
                 (kli/tui/transcript:event-text
                  (first (tui-app:tui-app-transcript-events app)))))))

(test (tui-app-main-thread-tasks-drain-on-trampoline-iteration :fixture interactive-authority)
  "Tasks queued via enqueue-main-thread-task run on the loop thread in FIFO order
   at the next trampoline iteration, without recoding the runtime step (the safe
   alternative to the runtime-recode foot-gun that could wedge input handling)."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (ran '()))
    (tui-app:enqueue-main-thread-task app (lambda () (push :first ran)))
    (tui-app:enqueue-main-thread-task app (lambda () (push :second ran)))
    (kli/tui/app::run-tui-app-trampoline
     app (make-string-input-stream "") :idle-sleep 0)
    (is (equal '(:first :second) (nreverse ran))
        "queued tasks run in FIFO order on the trampoline thread")))

(test (tui-app-main-thread-task-error-is-isolated-and-reified :fixture interactive-authority)
  "A signaling main-thread task is reified as a :tui-error and does not abort the
   remaining queued tasks, matching how the runtime step handles escaped errors."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((ext:*extension-fault-policy* nil)
           (context (ensure-tui-app-stack-with-session :deltas '("ok")))
           (app (tui-app:make-tui-app :context context :columns 48))
           (ran nil))
      (tui-app:enqueue-main-thread-task app (lambda () (error "boom-task")))
      (tui-app:enqueue-main-thread-task app (lambda () (setf ran t)))
      (kli/tui/app:run-pending-main-thread-tasks app)
      (is-true ran "a signaling task must not prevent later tasks from running")
      (let ((matches (remove-if-not
                      (lambda (e)
                        (and (eq :notice (tui-transcript:event-kind e))
                             (search "boom-task"
                                     (tui-transcript:event-text e))))
                      (tui-app:tui-app-transcript-events app))))
        (is (= 1 (length matches))
            "the signaling task reifies as one :tui-error transcript entry")))))

(test (tui-app-call-on-main-thread-task-runs-on-loop-and-awaits :fixture interactive-authority)
  "call-on-main-thread-task runs its thunk on the loop thread, blocks the calling
   worker until it returns, hands back the value, and re-signals an error across
   the thread boundary -- the seam the darwin native load marshals through."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (worker-thread nil)
         (thunk-thread nil)
         (value nil)
         (propagated nil)
         (done nil)
         (worker (sb-thread:make-thread
                  (lambda ()
                    (setf worker-thread sb-thread:*current-thread*)
                    (setf value
                          (kli/tui/app:call-on-main-thread-task
                           app (lambda ()
                                 (setf thunk-thread sb-thread:*current-thread*)
                                 42)))
                    (setf propagated
                          (handler-case
                              (kli/tui/app:call-on-main-thread-task
                               app (lambda () (error "boom-on-loop")))
                            (error (c) (princ-to-string c))))
                    (setf done t))
                  :name "test-marshal-worker")))
    (loop repeat 2000 until done
          do (kli/tui/app:run-pending-main-thread-tasks app)
             (sleep 0.002))
    (sb-thread:join-thread worker)
    (is (= 42 value) "the caller receives the thunk's value once the loop runs it")
    (is (not (eq thunk-thread worker-thread))
        "the thunk runs on the draining loop thread, not the calling worker")
    (is (string= "boom-on-loop" propagated)
        "an error raised on the loop thread re-signals on the caller")))

(test (tui-app-survives-input-extension-replacement-while-running :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (protocol (kli:active-protocol context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (old-decoder (tui-app:tui-app-decoder app)))
    (tui-app:tui-app-feed app "before" :render nil)
    (is (string= "before" (tui-app:tui-app-editor-value app)))
    (ext:deactivate-extension
     protocol
     (kli:find-live-object (kli:context-registry context) :tui-app)
     context)
    (ext:deactivate-extension
     protocol
     (kli:find-live-object (kli:context-registry context) :tui-input)
     context)
    (is (tui-app:tui-app-running-p app))
    (install-extension context tui-input:*tui-input-extension-manifest*)
    (install-extension context tui-app:*tui-app-extension-manifest*)
    (let ((new-decoder (tui-input:make-input-decoder :protocol protocol)))
      (setf (tui-app:tui-app-decoder app) new-decoder)
      (kli:register-live-object (kli:context-registry context) new-decoder)
      (is (not (eq old-decoder new-decoder)))
      (tui-app:tui-app-feed app " after" :render nil)
      (is (tui-app:tui-app-running-p app))
      (is (string= "before after" (tui-app:tui-app-editor-value app))))))

(test (tui-app-feeds-input-through-decoder-transcript-and-renderer :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app
               :context context
               :columns 48
               :on-submit
               (lambda (app input)
                 (declare (ignore app))
                 (list (tui-transcript:make-transcript-event
                        :message
                        :assistant
                        (format nil "received: ~A" input))))))
         (terminal (tui-app:tui-app-terminal app))
         (transcript (tui-app:tui-app-transcript app))
         output)
    (tui-app:tui-app-feed app "hello")
    (tui-app:tui-app-feed app (string #\Return))
    (setf output (tui-terminal:terminal-output terminal))
    (is (= 2 (length (tui-transcript:transcript-events transcript)))
        "[user] from emission, [assistant] from on-submit return")
    (is (search "[user] hello" output))
    (is (search "[assistant] received: hello" output))))

(test (tui-app-routes-clear-screen-through-route-context :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (terminal (tui-app:tui-app-terminal app))
         output)
    (tui-app:tui-app-feed app "hello")
    (tui-terminal:terminal-clear terminal)
    (tui-app:tui-app-feed app (string (code-char 12)))
    (setf output (tui-terminal:terminal-output terminal))
    (is (search (format nil "~C[2J" #\Esc) output))
    (is (search "> hello" output))))

(test (tui-app-redraw-clears-and-repaints-transcript :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (terminal (tui-app:tui-app-terminal app))
         output)
    (tui-app:tui-app-add-system-event app "visible")
    (tui-app:render-tui-app app)
    (tui-terminal:terminal-clear terminal)
    (tui-app:redraw-tui-app app)
    (setf output (tui-terminal:terminal-output terminal))
    (is (search (format nil "~C[2J" #\Esc) output))
    (is (not (search (format nil "~C[3J" #\Esc) output))
        "redraw preserves the shell's native scrollback")
    (is (search "· visible" output))
    (is (search "> " output))))

(test (tui-app-render-fault-skips-frame-logs-and-recovers :fixture interactive-authority)
  "Render fault. The frame is skipped, logged, and reified as a :system row. The next frame force-redraws clean."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let* ((context (ensure-tui-app-stack))
             (app (tui-app:make-tui-app :context context :columns 48))
             (original (fdefinition 'kli/tui/core:render-frame)))
        (unwind-protect
             (progn
               (setf (fdefinition 'kli/tui/core:render-frame)
                     (lambda (frame terminal &key force)
                       (declare (ignore frame terminal force))
                       (error "frame boom")))
               (is (null (tui-app:render-tui-app app)))
               (is (= 1 (kli/tui/app:tui-app-render-fault-streak app)))
               (is-true (kli/tui/app:tui-app-render-reset-pending-p app)))
          (setf (fdefinition 'kli/tui/core:render-frame) original))
        (tui-app:render-tui-app app)
        (is (= 0 (kli/tui/app:tui-app-render-fault-streak app))
            "a completed frame resets the streak")
        (is (null (kli/tui/app:tui-app-render-reset-pending-p app))
            "the recovery consumed the reset flag")
        (let ((rows (remove-if-not
                     (lambda (e)
                       (and (eq :system (tui-transcript:event-kind e))
                            (search "render fault" (tui-transcript:event-text e))))
                     (tui-app:tui-app-transcript-events app))))
          (is (= 1 (length rows)) "the skipped frame left one durable :system row"))
        (is (= 1 (length (fault-log-lines :render))))))))

(test (tui-app-render-fault-breaker-escalates-after-streak :fixture interactive-authority)
  "A renderer that never completes a frame escalates after the streak limit."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let* ((context (ensure-tui-app-stack))
             (app (tui-app:make-tui-app :context context :columns 48))
             (original (fdefinition 'kli/tui/core:render-frame)))
        (unwind-protect
             (progn
               (setf (fdefinition 'kli/tui/core:render-frame)
                     (lambda (frame terminal &key force)
                       (declare (ignore frame terminal force))
                       (error "frame boom")))
               (loop repeat kli/tui/app::*render-fault-escalation-streak*
                     do (is (null (tui-app:render-tui-app app))))
               (is (eq :escalated
                       (handler-case (tui-app:render-tui-app app)
                         (error () :escalated)))))
          (setf (fdefinition 'kli/tui/core:render-frame) original))
        (is (= (1+ kli/tui/app::*render-fault-escalation-streak*)
               (length (fault-log-lines :render)))
            "every fault logs exactly once, including the escalated one")))))

(test (tui-app-hotpatched-renderer-fault-contained-at-render-funnel :fixture interactive-authority)
  "A recoded scrollback-render behavior that faults escalates from its cell
   barrier into the :render funnel: frame skipped, reset pending, one line in
   each sink, clean recovery once the behavior is recoded back."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let* ((context (ensure-tui-app-stack))
             (app (tui-app:make-tui-app :context context :columns 48)))
        (tui-core:recode-tui-behavior
         (tui-app:tui-app-renderer app)
         :function (lambda (renderer &key force)
                     (declare (ignore renderer force))
                     (error "hot-patched renderer boom")))
        (is (null (tui-app:render-tui-app app)))
        (is (= 1 (kli/tui/app:tui-app-render-fault-streak app)))
        (is-true (kli/tui/app:tui-app-render-reset-pending-p app))
        (is (= 1 (length (fault-log-lines :behavior)))
            "the cell barrier logs before escalating")
        (is (= 1 (length (fault-log-lines :render)))
            "the render funnel logs the same fault at its seam")
        (tui-core:recode-tui-behavior
         (tui-app:tui-app-renderer app)
         :function #'kli/tui/transcript::default-scrollback-render)
        (tui-app:render-tui-app app)
        (is (= 0 (kli/tui/app:tui-app-render-fault-streak app)))
        (is (null (kli/tui/app:tui-app-render-reset-pending-p app)))))))

(test (tui-app-hotpatched-runtime-step-fault-reifies-and-degrades :fixture interactive-authority)
  "A recoded runtime-step behavior that faults is contained at its cell barrier
   (:reify, fallback :error-handled): the trampoline keeps its idle arm and the
   fault surfaces as one :tui-error row."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
             (app (tui-app:make-tui-app :context context :columns 48)))
        (tui-app:recode-tui-app-runtime
         app :function (lambda (app input-stream)
                         (declare (ignore app input-stream))
                         (error "hot-patched step boom")))
        (is (eq :error-handled
                (tui-app:tui-app-runtime-step app (make-string-input-stream "x"))))
        (is (= 1 (length (fault-log-lines :behavior))))
        (let ((matches (remove-if-not
                        (lambda (e)
                          (and (eq :notice (tui-transcript:event-kind e))
                               (search "step boom" (tui-transcript:event-text e))))
                        (tui-app:tui-app-transcript-events app))))
          (is (= 1 (length matches))
              "the hot-patched step fault reifies as one :tui-error row"))))))

(test (tui-app-render-refused-off-the-loop-thread :fixture interactive-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((context (ensure-tui-app-stack))
           (app (tui-app:make-tui-app :context context :columns 48))
           (other (sb-thread:make-thread (lambda () nil))))
      (sb-thread:join-thread other)
      (setf (kli/tui/app::tui-app-loop-thread app) other)
      (is (null (tui-app:render-tui-app app :force t)))
      (let ((lines (fault-log-lines :render)))
        (is (= 1 (length lines)))
        (is (search "CROSS-THREAD" (first lines))))
      (setf (kli/tui/app::tui-app-loop-thread app) nil)
      (is (not (null (tui-app:render-tui-app app :force t)))
          "guard inert once the loop thread is released"))))

(test (tui-app-step-fault-streak-breaker-fails-stop :fixture interactive-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let* ((context (ensure-tui-app-stack))
             (app (tui-app:make-tui-app :context context :columns 48)))
        (tui-app:recode-tui-app-runtime
         app :function (lambda (app input-stream)
                         (declare (ignore app input-stream))
                         (error "persistent step boom")))
        (setf (kli/tui/app::tui-app-step-fault-streak app)
              (1- kli/tui/app::*step-fault-escalation-streak*))
        (signals error
          (tui-app:tui-app-runtime-step app (make-string-input-stream "")))))))

(test (tui-app-step-fault-streak-resets-on-a-clean-step :fixture interactive-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((context (ensure-tui-app-stack))
           (app (tui-app:make-tui-app :context context :columns 48)))
      (setf (kli/tui/app::tui-app-step-fault-streak app) 5)
      (tui-app:tui-app-runtime-step app (make-string-input-stream ""))
      (is (zerop (kli/tui/app::tui-app-step-fault-streak app))))))

(test (tui-app-interrupt-requires-second-interrupt-to-stop :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (terminal (tui-app:tui-app-terminal app))
         output)
    (is (eq :armed (tui-app:request-tui-app-interrupt app)))
    (is (tui-app:tui-app-running-p app))
    (is (tui-app:tui-app-interrupt-armed-p app))
    (is (eq :quit (tui-app:request-tui-app-interrupt app)))
    (is (not (tui-app:tui-app-running-p app)))
    (tui-app:render-tui-app app)
    (setf output (tui-terminal:terminal-output terminal))
    (is (search "· Press Ctrl+C again to quit." output))
    (is (search "· Exiting." output))))

(test (tui-app-binds-escape-to-abort-action :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (is (eq :abort
            (tui-keymap:keymap-action (kli:object-protocol (tui-app:tui-app-view app))
                                      "escape")))))

(test (tui-app-routes-abort-through-route-context :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (fired 0)
         (route (tui-input:make-input-route-context
                 :protocol (kli:object-protocol (tui-app:tui-app-view app))
                 :abort-handler (lambda () (incf fired)))))
    (tui-input:route-input-event
     (tui-app:tui-app-view app)
     (tui-input:make-key-input-event "escape" :key-id "escape")
     route)
    (is (= 1 fired))))

(test (tui-app-double-escape-aborts-running-turn :fixture interactive-authority)
  "At an idle prompt Esc is inert and arms nothing. While a turn is streaming, the first Esc arms and hints without aborting, a second Esc past the rearm window re-arms instead of aborting, and an Esc within the window of the latest arm aborts the turn."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (terminal (tui-app:tui-app-terminal app)))
    (is (eq :idle (tui-app:request-tui-app-abort app 0)))
    (is (null (tui-app:tui-app-abort-armed-at app)))
    (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
    (is (eql 1000 (tui-app:tui-app-abort-armed-at app)))
    (is (null (agents:agent-abort-requested-p agent)))
    (is (eq :armed (tui-app:request-tui-app-abort
                    app (+ 1000 tui-app::+tui-abort-rearm-window+))))
    (is (null (agents:agent-abort-requested-p agent)))
    (is (eq :aborted (tui-app:request-tui-app-abort
                      app (1+ (tui-app:tui-app-abort-armed-at app)))))
    (is (agents:agent-abort-requested-p agent))
    (is (null (tui-app:tui-app-abort-armed-at app)))
    (tui-app:render-tui-app app)
    (let ((output (tui-terminal:terminal-output terminal)))
      (is (search "Press Esc again to interrupt." output)
          "the armed hint renders as a transient line above the prompt")
      (is (not (search "[system]" output))
          "abort notices are no longer :system transcript rows")
      (is (string= "Interrupted."
                   (tui-transcript:scrollback-renderer-notice
                    (tui-app:tui-app-renderer app)))
          "the armed hint gives way to the transient interrupted notice"))))

(test (tui-app-idle-after-abort-leaves-esc-inert :fixture interactive-authority)
  "An aborted turn settles the agent in :aborted (not :idle), so Esc at the prompt
   must be inert. A (not idle) busy check read :aborted as busy and re-armed, and
   :error is the same shape."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding))))
    (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
    (is (eq :aborted (tui-app:request-tui-app-abort app 1001)))
    (is (eq :aborted (agents:agent-state-value (agents:agent-state agent))))
    (is (eq :idle (tui-app:request-tui-app-abort app 2000)))
    (is (null (tui-app:tui-app-abort-armed-at app)))
    (is (eq :idle (tui-app:request-tui-app-abort app 2001)))
    (setf (agents:agent-state-value (agents:agent-state agent)) :error)
    (is (eq :idle (tui-app:request-tui-app-abort app 3000)))
    (is (null (tui-app:tui-app-abort-armed-at app)))))

(test (tui-app-abort-annotates-the-reply-instead-of-a-system-row :fixture interactive-authority)
  "Aborting a streaming turn marks the in-flight reply as aborted (a dim trailing
   marker, pi-style) rather than appending an \"Interrupted.\" :system row, and
   the 'Press Esc again' hint clears. The transcript holds only the reply."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (mode-id (tui-app:tui-app-mode-id app))
         (terminal (tui-app:tui-app-terminal app)))
    (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (kli/tui/app:apply-tui-app-event-projection
     app
     (event:make-event :agent/delta
                       :payload (list :mode mode-id :turn-id :turn-1
                                      :text "The meaning of life"))
     mode-id)
    (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
    (is (eq :aborted (tui-app:request-tui-app-abort app 1001)))
    (let* ((events (tui-app:tui-app-transcript-events app))
           (reply (first events)))
      (is (= 1 (length events))
          "only the reply is in the transcript, no :system notice rows")
      (is (eq :aborted (tui-transcript:event-status reply))
          "the in-flight reply is stamped aborted")
      (is (null (tui-transcript:scrollback-renderer-notice
                 (tui-app:tui-app-renderer app)))
          "the abort hint clears once the turn is aborted"))
    (tui-app:render-tui-app app)
    (let ((output (tui-terminal:terminal-output terminal)))
      (is (search "Interrupted" output)
          "the aborted reply renders its trailing marker")
      (is (not (search "[system]" output))
          "no :system row is emitted on the abort path"))))

(test (tui-app-abort-rearm-lapse-clears-the-hint :fixture interactive-authority)
  "When the re-arm window passes with no second Esc, the trampoline's lapse poll
   disarms and clears the transient hint, so a stale 'Press Esc again' offer does
   not linger above the prompt."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding))))
    (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
    (is (string= "Press Esc again to interrupt."
                 (tui-transcript:scrollback-renderer-notice
                  (tui-app:tui-app-renderer app))))
    (is (eq :idle (tui-app::poll-abort-rearm-lapse
                   app (+ 1000 (1- tui-app::+tui-abort-rearm-window+)))))
    (is (eql 1000 (tui-app:tui-app-abort-armed-at app)))
    (is (eq :lapsed (tui-app::poll-abort-rearm-lapse
                     app (+ 1000 tui-app::+tui-abort-rearm-window+))))
    (is (null (tui-app:tui-app-abort-armed-at app)))
    (is (null (tui-transcript:scrollback-renderer-notice
               (tui-app:tui-app-renderer app))))))

(test (tui-app-idle-double-escape-opens-rewind-menu :fixture interactive-authority)
  "At an idle prompt with user turns on the session, the first Esc arms the
rewind and hints, a second within the window opens a selection menu over the
user turns, newest first. Accepting the selected row branches before that
prompt, switches onto the shortened session, restores the prompt text to the
editor, and closes the menu. Esc with nothing to rewind stays inert."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (store (agents:agent-store agent))
         (session (agents:agent-session agent))
         (old-session-id (kli:object-id session)))
    (is (eq :idle (tui-app:request-tui-app-abort app 0)))
    (is (null (tui-app:tui-app-rewind-armed-at app)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "first prompt"))
     context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "first reply"))
     context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "second prompt"))
     context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "second reply"))
     context)
    (is (eq :rewind-armed (tui-app:request-tui-app-abort app 1000)))
    (is (eql 1000 (tui-app:tui-app-rewind-armed-at app)))
    (is (string= "Press Esc again to rewind."
                 (tui-transcript:scrollback-renderer-notice
                  (tui-app:tui-app-renderer app))))
    (is (eq :rewind-menu (tui-app:request-tui-app-abort app 1001)))
    (is (null (tui-app:tui-app-rewind-armed-at app)))
    (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
      (is (not (null popup)) "the second Esc opens the selection menu")
      (is (equal '("1 back" "2 back")
                 (mapcar #'tui-editor:completion-candidate-insert
                         (tui-editor:completion-popup-candidates popup))))
      (is (equal '("second prompt" "first prompt")
                 (mapcar #'tui-editor:completion-candidate-description
                         (tui-editor:completion-popup-candidates popup)))
          "menu rows preview the user turns, newest first"))
    (tui-terminal:terminal-clear (tui-app:tui-app-terminal app))
    (tui-editor::accept-completion (tui-app:tui-app-editor app))
    (is (not (eq old-session-id (mode-session-id service :default-mode))))
    (is (string= "second prompt" (tui-app:tui-app-editor-value app))
        "the rewound prompt lands back in the editor for re-editing")
    (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
        "the menu closes on accept")
    (is (null (tui-transcript:scrollback-renderer-notice
               (tui-app:tui-app-renderer app))))
    (let ((output (tui-terminal:terminal-output
                   (tui-app:tui-app-terminal app))))
      (is (search (format nil "~C[2J" #\Esc) output)
          "the rewind frame clears the visible screen before repainting")
      (is (search "first reply" output)
          "the repaint shows the rewound-to conversation")
      (is (null (search "second reply" output))
          "rows after the rewind point are gone from the repaint"))))

(test (tui-app-rewind-menu-reaches-older-turns :fixture interactive-authority)
  "Moving the menu selection down picks an older turn: accepting '2 back'
rewinds before the first prompt, which has no parent entry to branch at, so
the mode lands on a fresh empty session with that prompt's text restored to
the editor."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (store (agents:agent-store agent))
         (session (agents:agent-session agent))
         (old-session-id (kli:object-id session)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "first prompt"))
     context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "first reply"))
     context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "second prompt"))
     context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "second reply"))
     context)
    (is (eq :rewind-armed (tui-app:request-tui-app-abort app 1000)))
    (is (eq :rewind-menu (tui-app:request-tui-app-abort app 1001)))
    (tui-editor::move-completion-selection (tui-app:tui-app-editor app) 1)
    (tui-terminal:terminal-clear (tui-app:tui-app-terminal app))
    (tui-editor::accept-completion (tui-app:tui-app-editor app))
    (let ((new-session-id (mode-session-id service :default-mode)))
      (is (not (eq old-session-id new-session-id)))
      (is (null (sess:session-entries
                 (sess:find-session store new-session-id)))
          "rewinding past the first turn lands on a fresh empty session"))
    (is (string= "first prompt" (tui-app:tui-app-editor-value app)))
    (let ((output (tui-terminal:terminal-output
                   (tui-app:tui-app-terminal app))))
      (is (search (format nil "~C[2J" #\Esc) output)
          "the rewind frame clears the visible screen before repainting")
      (is (null (search "first reply" output))
          "the repaint shows none of the rewound-away conversation"))))

(test (tui-app-rewind-arm-lapse-clears-the-hint :fixture interactive-authority)
  "When the rewind window passes with no second Esc, the lapse poll disarms
and clears the hint, same as the abort arc."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (store (agents:agent-store agent))
         (session (agents:agent-session agent)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "a prompt"))
     context)
    (is (eq :rewind-armed (tui-app:request-tui-app-abort app 1000)))
    (is (string= "Press Esc again to rewind."
                 (tui-transcript:scrollback-renderer-notice
                  (tui-app:tui-app-renderer app))))
    (is (eq :idle (tui-app::poll-abort-rearm-lapse
                   app (+ 1000 (1- tui-app::+tui-abort-rearm-window+)))))
    (is (eql 1000 (tui-app:tui-app-rewind-armed-at app)))
    (is (eq :lapsed (tui-app::poll-abort-rearm-lapse
                     app (+ 1000 tui-app::+tui-abort-rearm-window+))))
    (is (null (tui-app:tui-app-rewind-armed-at app)))
    (is (null (tui-transcript:scrollback-renderer-notice
               (tui-app:tui-app-renderer app))))))

(test (tui-app-tool-update-surfaces-on-the-working-line :fixture interactive-authority)
  "A tool-execution progress update lands in the app's tool-update slot (read
   by the spinner footer widget) rather than the transcript, with the latest
   update winning. The slot clears when the execution ends and on turn end."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 80))
         (mode-id (tui-app:tui-app-mode-id app)))
    (is (kli/tui/app:apply-tui-app-event-projection
         app
         (event:make-event :agent/tool-execution-update
                           :payload (list :execution-id :e1 :update-id :u1
                                          :payload "halfway there"))
         mode-id)
        "an update with displayable text requests a repaint")
    (is (string= "halfway there" (tui-app:tui-app-tool-update-text app)))
    (is (null (tui-app:tui-app-transcript-events app))
        "updates surface on the working line, not as transcript rows")
    (kli/tui/app:apply-tui-app-event-projection
     app
     (event:make-event :agent/tool-execution-update
                       :payload (list :execution-id :e1 :update-id :u2
                                      :payload '(:message "nearly done")))
     mode-id)
    (is (string= "nearly done" (tui-app:tui-app-tool-update-text app)))
    (kli/tui/app:apply-tui-app-event-projection
     app
     (event:make-event :agent/tool-execution-end
                       :payload (list :execution-id :e1 :error-p nil
                                      :result-text "done"))
     mode-id)
    (is (null (tui-app:tui-app-tool-update-text app))
        "execution end clears the working-line detail")
    (kli/tui/app:apply-tui-app-event-projection
     app
     (event:make-event :agent/tool-execution-update
                       :payload (list :execution-id :e2 :update-id :u3
                                      :payload "lingering"))
     mode-id)
    (kli/tui/app:apply-tui-app-event-projection
     app
     (event:make-event :agent/turn-end
                       :payload (list :turn-id :turn-1 :state :completed))
     mode-id)
    (is (null (tui-app:tui-app-tool-update-text app))
        "turn end clears a detail left by an execution that never ended")))

(test (tui-app-pre-stream-abort-shows-a-transient-interrupted-notice :fixture interactive-authority)
  "Aborting before any delta arrives has no reply row to stamp, so the abort
   acknowledges with a transient \"Interrupted.\" notice instead of going silent.
   The notice carries an expiry the trampoline's poll drops once it lapses, and
   the rearm-lapse poll leaves it alone (armed is already cleared)."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding))))
    (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
    (is (eq :aborted (tui-app:request-tui-app-abort app 1001)))
    (is (null (tui-app:tui-app-transcript-events app))
        "pre-stream abort leaves no reply in the transcript")
    (is (string= "Interrupted."
                 (tui-transcript:scrollback-renderer-notice
                  (tui-app:tui-app-renderer app)))
        "a transient acknowledgment stands in for the missing reply marker")
    (is (eql (+ 1001 tui-app::+tui-interrupted-notice-window+)
             (tui-app:tui-app-notice-expires-at app))
        "the notice carries an expiry so the poll can drop it")
    (is (eq :idle (tui-app::poll-abort-rearm-lapse app 99999999)))
    (is (string= "Interrupted."
                 (tui-transcript:scrollback-renderer-notice
                  (tui-app:tui-app-renderer app))))
    (is (eq :idle (tui-app::poll-tui-app-notice-expiry
                   app (+ 1001 (1- tui-app::+tui-interrupted-notice-window+)))))
    (is (string= "Interrupted."
                 (tui-transcript:scrollback-renderer-notice
                  (tui-app:tui-app-renderer app))))
    (is (eq :expired (tui-app::poll-tui-app-notice-expiry
                      app (+ 1001 tui-app::+tui-interrupted-notice-window+))))
    (is (null (tui-transcript:scrollback-renderer-notice
               (tui-app:tui-app-renderer app))))
    (is (null (tui-app:tui-app-notice-expires-at app)))))

(test (tui-app-mid-stream-notice-does-not-duplicate-the-streaming-event :fixture interactive-authority)
  "A system notice appended mid-stream (the Esc abort hint, then \"Interrupted.\")
   must not make the next delta re-append the live streaming event. The live event
   joins the transcript exactly once however many notices land after it, so the
   abort path never re-renders the whole reply per keypress. Regresses the
   duplication that stacked the streamed essay on every Esc."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (mode-id (tui-app:tui-app-mode-id app))
         (delta (lambda (text)
                  (event:make-event
                   :agent/delta
                   :payload (list :mode mode-id :turn-id :turn-1 :text text))))
         (assistant-events
           (lambda ()
             (remove-if-not
              (lambda (e)
                (and (eq :message (tui-transcript:event-kind e))
                     (eq :assistant (tui-transcript:event-role e))))
              (tui-app:tui-app-transcript-events app)))))
    (kli/tui/app:apply-tui-app-event-projection app (funcall delta "The ") mode-id)
    (tui-app:tui-app-add-system-event app "Press Esc again to interrupt.")
    (kli/tui/app:apply-tui-app-event-projection app (funcall delta "meaning ") mode-id)
    (tui-app:tui-app-add-system-event app "Interrupted.")
    (kli/tui/app:apply-tui-app-event-projection app (funcall delta "of life") mode-id)
    (is (= 1 (length (funcall assistant-events)))
        "the streaming event is added once despite notices interleaved after it")
    (is (string= "The meaning of life"
                 (tui-transcript:event-text (first (funcall assistant-events))))
        "deltas still accumulate into the single live event")
    (is (= 2 (count-if
              (lambda (e) (eq :system (tui-transcript:event-kind e)))
              (tui-app:tui-app-transcript-events app)))
        "both mid-stream notices remain, each appended once")))

(test (tui-app-thinking-streams-full-text-past-the-first-repaint :fixture interactive-authority)
  "Reasoning streams as a live event like the assistant reply: deltas that arrive
   after the first repaint keep growing on screen instead of freezing at the first
   committed batch. Without opening the scrollback stream on the thinking event,
   the first render commits its opening text and later deltas never re-render,
   truncating the gutter to its first lines. Regresses that truncation."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (mode-id (tui-app:tui-app-mode-id app))
         (terminal (tui-app:tui-app-terminal app))
         (think (lambda (text)
                  (event:make-event
                   :agent/thinking-delta
                   :payload (list :mode mode-id :turn-id :turn-1
                                  :text text :level :xhigh)))))
    (kli/tui/app:apply-tui-app-event-projection app (funcall think "Reason ") mode-id)
    (tui-app:render-tui-app app)
    (kli/tui/app:apply-tui-app-event-projection app (funcall think "step ") mode-id)
    (kli/tui/app:apply-tui-app-event-projection app (funcall think "by step") mode-id)
    (tui-app:render-tui-app app)
    (is (string= "Reason step by step"
                 (tui-transcript:event-text
                  (first (tui-app:tui-app-transcript-events app))))
        "the thinking event accumulates every delta")
    (is (search "step by step" (tui-terminal:terminal-output terminal))
        "deltas after the first repaint reach the screen, not just the first batch")))

(test (tui-app-flushes-dangling-escape-as-keypress :fixture interactive-authority)
  "A bare Esc buffers rather than emits — it could begin an escape sequence.
   The first idle observation arms the flush timer, and once the wait elapses
   with no continuation the Esc flushes as a keypress."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (decoder (tui-app:tui-app-decoder app)))
    (is (null (tui-app:tui-app-feed app (string #\Esc) :render nil)))
    (is (= 1 (length (kli/tui/input:input-decoder-buffer decoder))))
    (is (eq :idle (tui-app::tui-app-flush-dangling-escape app 0)))
    (is (eq :idle (tui-app::tui-app-flush-dangling-escape app 1)))
    (is (eq :input (tui-app::tui-app-flush-dangling-escape
                    app (1+ tui-app::+tui-escape-flush-delay+))))
    (is (zerop (length (kli/tui/input:input-decoder-buffer decoder))))
    (is (null (tui-app::tui-app-escape-pending-since app)))))

(test (tui-app-complete-escape-sequence-does-not-dangle :fixture interactive-authority)
  "A full escape sequence arriving in one read decodes with nothing left
   over, so the dangling-escape flush stays idle."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (decoder (tui-app:tui-app-decoder app)))
    (tui-app:tui-app-feed app (format nil "~C[A" #\Esc) :render nil)
    (is (zerop (length (kli/tui/input:input-decoder-buffer decoder))))
    (is (eq :idle (tui-app::tui-app-flush-dangling-escape
                   app (1+ tui-app::+tui-escape-flush-delay+))))))

(test (tui-app-run-loop-submits-lines-when-character-input-is-unavailable :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app
               :context context
               :columns 48
               :on-submit
               (lambda (app input)
                 (declare (ignore app input))
                 nil)))
         (terminal (tui-app:tui-app-terminal app))
         (stream (make-string-input-stream "hi")))
    (let ((tui-style:*color-mode* :bogus))
      (tui-app:run-tui-app app
                           :input-stream stream
                           :initial-render nil)
      (is (eq tui-style:*color-mode* (tui-style:detect-color-mode))
          "the boot site sets the color mode from detect-color-mode"))
    (let ((output (tui-terminal:terminal-output terminal)))
      (is (search "[user] hi" output))
      (is (string= "" (tui-app:tui-app-editor-value app)))
      (is (= 1 (length (tui-app:tui-app-transcript-events app)))))))

(test (tui-app-initial-render-takes-over-the-screen :fixture interactive-authority)
  "Entering the TUI clears the visible screen and homes the cursor so the first
   frame owns the viewport instead of drawing below the shell output that
   launched it. Native scrollback is left alone - the shell history above
   stays reachable."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (terminal (tui-app:tui-app-terminal app))
         (stream (make-string-input-stream "")))
    (tui-app:run-tui-app app :input-stream stream)
    (let* ((output (tui-terminal:terminal-output terminal))
           (clear (search (format nil "~C[2J" #\Esc) output))
           (prompt (search " > " output)))
      (is-true clear "the first frame clears the visible screen")
      (is (search (format nil "~C[H" #\Esc) output)
          "the cursor homes before the first paint")
      (is (not (search (format nil "~C[3J" #\Esc) output))
          "the shell's native scrollback is preserved")
      (is (and clear prompt (< clear prompt))
          "the prompt paints after the takeover clear"))))

(test (tui-app-initial-render-anchors-the-prompt-to-the-bottom :fixture interactive-authority)
  "The takeover frame jumps the cursor down after the clear so the prompt box
   lands against the bottom of the screen - a chat input with room for history
   above - instead of sitting at the top of an empty viewport."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48 :rows 12))
         (terminal (tui-app:tui-app-terminal app))
         (stream (make-string-input-stream "")))
    (tui-app:run-tui-app app :input-stream stream)
    (let* ((output (tui-terminal:terminal-output terminal))
           (clear (search (format nil "~C[2J" #\Esc) output))
           (region (tui-transcript:scrollback-renderer-region-lines
                    (tui-app:tui-app-renderer app)))
           (move (format nil "~C[~D;1H" #\Esc (1+ (- 12 (length region)))))
           (jump (search move output)))
      (is-true jump "the takeover jumps to the bottom before painting")
      (is (< clear jump) "the jump follows the clear")
      (is (< jump (search " > " output)) "the prompt paints after the jump"))))

(test (tui-app-coexists-across-protocols-without-image-global-leakage :fixture interactive-authority)
  "Two TUI stacks installed into two protocols simultaneously must produce
   apps whose protocol slots, sub-object protocols, and runtime state are
   independent. This regresses the image-global defvar era, where the second
   protocol's installer clobbered shared *tui-X-provider* state and apps on
   the first protocol started routing through the second's providers."
  (let* ((context-a (ensure-tui-app-stack))
         (protocol-a (kli:active-protocol context-a))
         (context-b (ensure-tui-app-stack))
         (protocol-b (kli:active-protocol context-b))
         (app-a (tui-app:make-tui-app :context context-a :columns 32))
         (app-b (tui-app:make-tui-app :context context-b :columns 32)))
    (is (not (eq protocol-a protocol-b))
        "two independent extension-protocols must be distinct objects")
    (is (eq protocol-a (kli:object-protocol app-a))
        "app-a must carry protocol-a in its receiver slot")
    (is (eq protocol-b (kli:object-protocol app-b))
        "app-b must carry protocol-b in its receiver slot")
    (dolist (slot-pair (list (cons (tui-app:tui-app-terminal app-a)
                                   (tui-app:tui-app-terminal app-b))
                             (cons (tui-app:tui-app-decoder app-a)
                                   (tui-app:tui-app-decoder app-b))
                             (cons (tui-app:tui-app-transcript app-a)
                                   (tui-app:tui-app-transcript app-b))
                             (cons (tui-app:tui-app-view app-a)
                                   (tui-app:tui-app-view app-b))
                             (cons (tui-app:tui-app-renderer app-a)
                                   (tui-app:tui-app-renderer app-b))))
      (is (eq protocol-a (kli:object-protocol (car slot-pair)))
          "sub-object of app-a must carry protocol-a")
      (is (eq protocol-b (kli:object-protocol (cdr slot-pair)))
          "sub-object of app-b must carry protocol-b"))
    (tui-app:tui-app-feed app-a "alpha" :render nil)
    (is (string= "alpha" (tui-app:tui-app-editor-value app-a)))
    (is (string= "" (tui-app:tui-app-editor-value app-b))
        "feeding input to app-a must not change app-b's editor state")
    (tui-app:tui-app-feed app-b "beta" :render nil)
    (is (string= "alpha" (tui-app:tui-app-editor-value app-a))
        "feeding input to app-b must not change app-a's editor state")
    (is (string= "beta" (tui-app:tui-app-editor-value app-b)))
    (tui-app:render-tui-app app-a :force t)
    (tui-app:render-tui-app app-b :force t)
    (is (search "alpha"
                (tui-terminal:terminal-output
                 (tui-app:tui-app-terminal app-a))))
    (is (not (search "alpha"
                     (tui-terminal:terminal-output
                      (tui-app:tui-app-terminal app-b))))
        "app-a's render must not leak into app-b's terminal")
    (is (search "beta"
                (tui-terminal:terminal-output
                 (tui-app:tui-app-terminal app-b))))
    (is (not (search "beta"
                     (tui-terminal:terminal-output
                      (tui-app:tui-app-terminal app-a))))
        "app-b's render must not leak into app-a's terminal")))

(test (tui-app-end-to-end-chunked-delta-flows-through-listener-to-transcript :fixture interactive-authority)
  "Full integration: a multi-chunk assistant response from the fake provider
   must reach the transcript as one concatenated assistant message, alongside
   exactly one user message routed via the listener. Captures the agent-loop
   event stream with a sidecar listener to confirm `:agent/delta` chunks fire
   and `:agent/message-end` brackets the turn."
  (let* ((chunks '("Hel" "lo " "world"))
         (context (ensure-tui-app-stack-with-session :deltas chunks))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (captured '())
         (capture-listener
           (agent-session:make-session-event-listener
            :witness-capture
            (lambda (event mode-id ctx)
              (declare (ignore ctx))
              (push (list (event:event-type event) mode-id) captured))))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (agent-session:register-session-event-listener service capture-listener
                                                   context)
    (tui-app:tui-app-feed app "hello" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (settle-tui-app app)
    (let* ((captured (nreverse captured))
           (delta-events (remove-if-not
                          (lambda (e) (eq (first e) :agent/delta))
                          captured))
           (end-events (remove-if-not
                        (lambda (e) (eq (first e) :agent/message-end))
                        captured)))
      (is (= (length chunks) (length delta-events))
          "one :agent/delta event must fire per chunk")
      (is (= 1 (length end-events))
          "exactly one :agent/message-end must close the turn")
      (is (every (lambda (e) (eq :default-mode (second e))) delta-events)
          "each delta must carry the bound mode-id"))
    (let* ((events (tui-app:tui-app-transcript-events app))
           (user-events (remove-if-not
                         (lambda (e)
                           (and (eq :message (tui-transcript:event-kind e))
                                (eq :user (tui-transcript:event-role e))))
                         events))
           (assistant-events (remove-if-not
                              (lambda (e)
                                (and (eq :message
                                         (tui-transcript:event-kind e))
                                     (eq :assistant
                                         (tui-transcript:event-role e))))
                              events)))
      (is (= 1 (length user-events)))
      (is (string= "hello" (tui-transcript:event-text (first user-events))))
      (is (= 1 (length assistant-events)))
      (is (string= (apply #'concatenate 'string chunks)
                   (tui-transcript:event-text (first assistant-events)))
          "assistant transcript text must concatenate every delta chunk"))))

(test (tui-app-runtime-step-reifies-escaped-error-as-tui-error :fixture interactive-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((ext:*extension-fault-policy* nil)
           (context (ensure-tui-app-stack-with-session :deltas '("ok")))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (app (tui-app:make-tui-app :context context :columns 48))
           (saw-tui-error nil))
      (agent-session:register-session-event-listener
       service
       (agent-session:make-session-event-listener
        :tui-error-witness
        (lambda (event mode-id ctx)
          (declare (ignore mode-id ctx))
          (when (eq (event:event-type event) :tui-error)
            (setf saw-tui-error event))))
       context)
      (let ((original (fdefinition 'kli/tui/app::read-tui-app-input-chunk)))
        (unwind-protect
             (progn
               (setf (fdefinition 'kli/tui/app::read-tui-app-input-chunk)
                     (lambda (stream first-char)
                       (declare (ignore stream first-char))
                       (error "boom-step")))
               (let ((result (tui-app:tui-app-runtime-step
                              app (make-string-input-stream "x"))))
                 (is (eq :error-handled result))))
          (setf (fdefinition 'kli/tui/app::read-tui-app-input-chunk)
                original)))
      (is (tui-app:tui-app-running-p app)
          "trampoline must keep running after a reified step error")
      (is (not (null saw-tui-error))
          ":tui-error must reach the listener fan-out")
      (let* ((events (tui-app:tui-app-transcript-events app))
             (matches (remove-if-not
                       (lambda (e)
                         (and (eq :notice (tui-transcript:event-kind e))
                              (search "Internal error"
                                      (tui-transcript:event-text e))))
                       events)))
        (is (= 1 (length matches))
            "exactly one Internal error transcript entry must appear")
        (is (search "boom-step"
                    (tui-transcript:event-text (first matches))))))))

(defun capture-events-on (service context)
  "Register a capture listener and return a 0-arg closure yielding the captured list."
  (let ((captured '()))
    (agent-session:register-session-event-listener
     service
     (agent-session:make-session-event-listener
      :tui-submit-test-witness
      (lambda (event _mode-id _ctx)
        (declare (ignore _mode-id _ctx))
        (push (event:event-type event) captured)))
     context)
    (lambda () (nreverse captured))))

(test (tui-app-default-on-submit-bound-mode-submits-and-returns-nil :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (app (tui-app:make-tui-app :context context :columns 32))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding))))
    (is (null (kli/tui/app:default-tui-app-on-submit app "hello"))
        "bound mode returns nil, agent pipeline owns subsequent events")
    (settle-tui-app app)
    (is (equal '(:user :assistant) (agent-session-message-roles agent)))))

(test (tui-app-bound-submit-runs-the-loop-on-a-worker-thread :fixture interactive-authority)
  "default-tui-app-on-submit hands a bound-mode turn to a worker thread instead of
   running it on the input thread, and the worker's renders reach the transcript
   only once the loop thread drains the marshaled queue."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (is (null (kli/tui/app:default-tui-app-on-submit app "hi")))
    (is (typep (tui-app:tui-app-agent-worker-thread app) 'sb-thread:thread)
        "the agent loop runs off the input thread")
    (kli/tui/app::join-agent-worker app)
    (is (null (tui-app:tui-app-agent-worker-thread app))
        "join-agent-worker waits for the turn and forgets the worker")
    (kli/tui/app:run-pending-main-thread-tasks app)
    (let ((assistant (remove-if-not
                      (lambda (e)
                        (and (eq :message (tui-transcript:event-kind e))
                             (eq :assistant (tui-transcript:event-role e))))
                      (tui-app:tui-app-transcript-events app))))
      (is (= 1 (length assistant)))
      (is (string= "ok" (tui-transcript:event-text (first assistant)))))))

(test (tui-app-stop-aborts-the-in-flight-turn-before-joining :fixture interactive-authority)
  "stop-tui-app must abort the agent session before joining the worker: the
   abort is what unblocks a worker parked in a stream read, so quitting
   mid-turn reaps the thread promptly instead of timing out the 5s join and
   abandoning it."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id
                                       binding))))
    (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (setf (tui-app:tui-app-agent-worker-thread app)
          (sb-thread:make-thread
           (lambda ()
             (loop until (agents:agent-abort-requested-p agent)
                   do (sleep 0.01)))
           :name "parked-worker"))
    (let ((started (get-internal-real-time)))
      (tui-app:stop-tui-app app)
      (is (< (/ (- (get-internal-real-time) started)
                internal-time-units-per-second)
             2)
          "the abort unblocks the worker well inside the join timeout")
      (is (null (tui-app:tui-app-agent-worker-thread app)))
      (is (agents:agent-abort-requested-p agent)
          "stop-tui-app aborted the session before joining"))))

(test (tui-app-worker-fault-reifies-as-tui-error-and-logs :fixture interactive-authority)
  "A worker fault is contained, logged, and reified as one :tui-error at drain."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
             (app (tui-app:make-tui-app :context context :columns 48))
             (original (fdefinition 'agent-session:submit-agent-session-prompt)))
        (unwind-protect
             (progn
               (setf (fdefinition 'agent-session:submit-agent-session-prompt)
                     (lambda (service mode-id input ctx)
                       (declare (ignore service mode-id input ctx))
                       (error "boom-worker")))
               (is (null (kli/tui/app:default-tui-app-on-submit app "hi")))
               (kli/tui/app::join-agent-worker app)
               (kli/tui/app:run-pending-main-thread-tasks app))
          (setf (fdefinition 'agent-session:submit-agent-session-prompt) original))
        (let ((matches (remove-if-not
                        (lambda (e)
                          (and (eq :notice (tui-transcript:event-kind e))
                               (search "boom-worker"
                                       (tui-transcript:event-text e))))
                        (tui-app:tui-app-transcript-events app))))
          (is (= 1 (length matches))
              "the worker fault reifies as one :tui-error transcript entry"))
        (let ((lines (fault-log-lines :agent-worker)))
          (is (= 1 (length lines)))
          (is (not (null (search "id=:DEFAULT-MODE" (first lines))))))))))

(test (tui-app-submit-while-busy-steers-instead-of-spawning-a-second-worker :fixture interactive-authority)
  "With the loop on a worker thread the input thread stays live, so a second Enter
   can land mid-turn. A submit while a turn is in flight must route to steering
   (drained between tool calls) — not spawn a second loop on the same agent — and
   must not arm the pre-stream retract (that is the idle, unanswered-prompt case)."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding))))
    (setf (agents:agent-state-value (agents:agent-state agent)) :running)
    (is (null (kli/tui/app:default-tui-app-on-submit app "steer me"))
        "the steer path owns subsequent events, on-submit returns nil")
    (is (null (tui-app:tui-app-agent-worker-thread app))
        "no second worker is spawned for a mid-turn submit")
    (is (equal '("steer me")
               (agents:agent-queue-items (agents:agent-steering-queue agent)))
        "the submitted text is enqueued as steering")
    (is (null (tui-app:tui-app-pending-prompt app))
        "a mid-turn submit does not arm the pre-stream retract")
    (is (null (tui-app:tui-app-retract-armed-p app)))))

(test (tui-app-submit-steer-toctou-runs-stranded-steer-on-a-worker :fixture interactive-authority)
  "When the turn ends between the routing check and the enqueue, the queued
   steer has no boundary left to drain it. The submit path re-checks and hands
   the stranded steer to a worker -- exactly like a fresh submit -- instead of
   running a full turn synchronously on the loop thread."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id
                                       binding)))
         (original (fdefinition 'agent-session:agent-session-busy-p))
         (calls 0))
    (unwind-protect
         (progn
           (setf (fdefinition 'agent-session:agent-session-busy-p)
                 (lambda (service mode-id ctx)
                   (declare (ignore service mode-id ctx))
                   (= 1 (incf calls))))
           (is (null (kli/tui/app:default-tui-app-on-submit app "catch me"))))
      (setf (fdefinition 'agent-session:agent-session-busy-p) original))
    (is (typep (tui-app:tui-app-agent-worker-thread app) 'sb-thread:thread)
        "the stranded steer runs on a worker, never the loop thread")
    (kli/tui/app::join-agent-worker app)
    (let ((contents (agent-session-message-contents agent)))
      (is (find "catch me" contents :test #'string=)
          "the drain worker delivered the stranded steer")
      (is (find "ok" contents :test #'string=)
          "the steer ran a full turn off-thread"))))

(test (tui-app-submit-while-worker-alive-steers-without-clobbering-the-slot :fixture interactive-authority)
  "A live worker can mean a turn or a /compact summarizer that has not raised
   the busy state yet -- the sub-100ms spawn-to-state window. Submits route to
   steering on worker liveness alone and never spawn a second worker into the
   occupied slot."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id
                                       binding)))
         (gate (sb-thread:make-semaphore))
         (worker (sb-thread:make-thread
                  (lambda () (sb-thread:wait-on-semaphore gate))
                  :name "fake-live-worker")))
    (setf (tui-app:tui-app-agent-worker-thread app) worker)
    (unwind-protect
         (progn
           (is (null (kli/tui/app:default-tui-app-on-submit
                      app "queued behind work")))
           (is (eq worker (tui-app:tui-app-agent-worker-thread app))
               "the live worker's slot is not clobbered")
           (is (equal '("queued behind work")
                      (agents:agent-queue-items
                       (agents:agent-steering-queue agent)))
               "the submit queues as steering even though the agent is idle"))
      (sb-thread:signal-semaphore gate)
      (sb-thread:join-thread worker))))

(test (tui-app-command-worker-spawn-keeps-the-agent-slot :fixture interactive-authority)
  "A command body spawned while an agent turn is live lands in its own slot, so
   the agent-worker-thread reference is never overwritten -- the slot-sharing bug
   that orphaned the agent thread (and made quit reap only the survivor). The two
   workers are distinct threads."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (agent-gate (sb-thread:make-semaphore))
         (command-gate (sb-thread:make-semaphore))
         (agent-worker (sb-thread:make-thread
                        (lambda () (sb-thread:wait-on-semaphore agent-gate))
                        :name "fake-agent-worker")))
    (setf (tui-app:tui-app-agent-worker-thread app) agent-worker)
    (unwind-protect
         (progn
           (funcall (kli/tui/app::tui-app-command-worker-spawner app)
                    (lambda () (sb-thread:wait-on-semaphore command-gate)))
           (is (eq agent-worker (tui-app:tui-app-agent-worker-thread app))
               "the agent slot keeps its worker, not clobbered by the command spawn")
           (is (typep (tui-app:tui-app-command-worker-thread app) 'sb-thread:thread)
               "the command body runs in the distinct command slot")
           (is (not (eq agent-worker (tui-app:tui-app-command-worker-thread app)))
               "the agent and command workers are distinct threads"))
      (sb-thread:signal-semaphore agent-gate)
      (sb-thread:signal-semaphore command-gate)
      (ignore-errors (sb-thread:join-thread agent-worker))
      (let ((cw (tui-app:tui-app-command-worker-thread app)))
        (when cw (ignore-errors (sb-thread:join-thread cw)))))))

(test (tui-app-second-command-worker-refused-while-first-alive :fixture interactive-authority)
  "One command body at a time: a second command spawn while one is live refuses
   rather than overwriting the slot and orphaning the first worker."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (gate (sb-thread:make-semaphore))
         (second-ran nil))
    (unwind-protect
         (let ((spawner (kli/tui/app::tui-app-command-worker-spawner app)))
           (funcall spawner (lambda () (sb-thread:wait-on-semaphore gate)))
           (let ((first (tui-app:tui-app-command-worker-thread app)))
             (is (typep first 'sb-thread:thread))
             (funcall spawner (lambda () (setf second-ran t)))
             (is (eq first (tui-app:tui-app-command-worker-thread app))
                 "the live command slot is not clobbered by a second spawn")
             (is (null second-ran)
                 "the refused second command body never ran")))
      (sb-thread:signal-semaphore gate)
      (let ((cw (tui-app:tui-app-command-worker-thread app)))
        (when cw (ignore-errors (sb-thread:join-thread cw)))))))

(test (tui-app-worker-projection-marshals-and-drains-in-order :fixture interactive-authority)
  "While *marshal-projection* is set (as inside the agent worker), every event
   projection is queued rather than applied: the transcript stays untouched until
   the loop thread drains the queue, which replays the deltas in emission order."
  (let* ((context (ensure-tui-app-stack-with-session
                   :deltas '("Hel" "lo " "world")))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (let ((kli/tui/app::*marshal-projection* t))
      (agent-session:submit-agent-session-prompt service :default-mode "hi"
                                                 context))
    (is (zerop (length (tui-app:tui-app-transcript-events app)))
        "marshaled projections are queued, so the transcript is untouched")
    (kli/tui/app:run-pending-main-thread-tasks app)
    (let ((assistant (remove-if-not
                      (lambda (e)
                        (and (eq :message (tui-transcript:event-kind e))
                             (eq :assistant (tui-transcript:event-role e))))
                      (tui-app:tui-app-transcript-events app))))
      (is (= 1 (length assistant)))
      (is (string= "Hello world"
                   (tui-transcript:event-text (first assistant)))
          "the drained deltas concatenate in emission order"))))

(defun count-string-occurrences (needle haystack)
  (loop with n = (length needle) with start = 0 with total = 0
        for hit = (search needle haystack :start2 start)
        while hit do (incf total) (setf start (+ hit n))
        finally (return total)))

(test (tui-app-marshaled-drain-coalesces-into-one-render :fixture interactive-authority)
  "A burst of marshaled projections (stream deltas plus lifecycle events) repaints
   once per trampoline drain, not once per event — so the editor cursor settles
   once instead of flickering through a repaint per delta. Each render writes the
   autowrap-off prologue exactly once, so its count is the render count."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("a" "b" "c" "d")))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (app (tui-app:make-tui-app :context context :columns 48))
         (terminal (tui-app:tui-app-terminal app))
         (autowrap-off (format nil "~C[?7l" #\Esc)))
    (let ((kli/tui/app::*marshal-projection* t))
      (agent-session:submit-agent-session-prompt service :default-mode "hi" context))
    (is (zerop (count-string-occurrences
                autowrap-off (tui-terminal:terminal-output terminal)))
        "queued projections draw nothing until the drain")
    (tui-terminal:terminal-clear terminal)
    (kli/tui/app:run-pending-main-thread-tasks app)
    (is (= 1 (count-string-occurrences
              autowrap-off (tui-terminal:terminal-output terminal)))
        "the whole drained batch coalesces into exactly one repaint")))

(test (tui-app-pre-loop-projection-defers-render-until-the-loop-runs :fixture interactive-authority)
  "An event projected before run-tui-app captures the loop thread (the boot-time
   session reset) mutates the transcript but paints nothing - the takeover clear
   must be the first thing the terminal sees. The skipped paint defers as
   pending rather than dropping, and the same path renders immediately once the
   loop owns the terminal."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (mode-id (tui-app:tui-app-mode-id app))
         (terminal (tui-app:tui-app-terminal app))
         (delta (lambda (text)
                  (event:make-event
                   :agent/delta
                   :payload (list :mode mode-id :turn-id :turn-1 :text text)))))
    (kli/tui/app::project-tui-app-event app (funcall delta "early") mode-id nil)
    (is (= 1 (length (tui-app:tui-app-transcript-events app)))
        "the projection still lands in the transcript")
    (is (string= "" (tui-terminal:terminal-output terminal))
        "nothing paints before the loop starts")
    (is-true (kli/tui/app::tui-app-render-pending-p app)
             "the skipped paint defers as pending")
    (setf (kli/tui/app::tui-app-loop-thread app) sb-thread:*current-thread*)
    (kli/tui/app::project-tui-app-event app (funcall delta " bird") mode-id nil)
    (is (plusp (length (tui-terminal:terminal-output terminal)))
        "the same path renders immediately while the loop runs")))

(test (tui-app-surfaces-captured-boot-diagnostics :fixture interactive-authority)
  "Diagnostics captured while loading user extensions surface as transcript
   system events once the app exists - the takeover wipes anything printed to
   the tty, so the transcript is where the user learns an extension warned."
  (let* ((context (ensure-tui-app-stack))
         (protocol (kli:active-protocol context))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (app::record-extension-diagnostic
     protocol #p"/tmp/noisy-extension.lisp" "caught STYLE-WARNING: probe")
    (app::surface-boot-diagnostics app context)
    (let ((events (tui-app:tui-app-transcript-events app)))
      (is (= 1 (length events)))
      (is (eq :system (tui-transcript:event-kind (first events))))
      (is (search "noisy-extension.lisp"
                  (tui-transcript:event-text (first events)))
          "the system event names the diagnostic source"))))

(test (tui-app-boot-diagnostics-render-short-text-inline :fixture interactive-authority)
  "A short one-line diagnostic (a settings warning, typically) renders its
   text directly in the transcript so the user reads the problem without
   opening the cache log; longer or multi-line output keeps the pointer row."
  (let* ((context (ensure-tui-app-stack))
         (protocol (kli:active-protocol context))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (app::record-extension-diagnostic
     protocol :boot "Ignoring settings value for \"theme\": 42")
    (app::record-extension-diagnostic
     protocol #p"/tmp/noisy.lisp" (format nil "line one~%line two"))
    (app::surface-boot-diagnostics app context)
    (let* ((texts (mapcar #'tui-transcript:event-text
                          (tui-app:tui-app-transcript-events app)))
           (joined (format nil "~{~A~%~}" texts)))
      (is (= 2 (length texts)))
      (is (search "boot: Ignoring settings value for \"theme\": 42" joined)
          "short single-line diagnostics render inline")
      (is (search "Extension diagnostics from noisy.lisp" joined)
          "multi-line diagnostics keep the pointer row")
      (is (not (search "line one" joined))
          "multi-line text never inlines into the transcript"))))

(test (tui-app-default-on-submit-unbound-mode-returns-system-event-only :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 32))
         (result (kli/tui/app:default-tui-app-on-submit app "hello")))
    (is (= 1 (length result)))
    (is (eq :system (tui-transcript:event-kind (first result))))
    (is (search "No agent bound"
                (tui-transcript:event-text (first result))))))

(test (tui-app-default-on-submit-no-service-direct-appends-user-echo :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 32)))
    (setf (tui-app:tui-app-context app) nil)
    (let ((result (kli/tui/app:default-tui-app-on-submit app "hello")))
      (is (= 1 (length result))
          "no context => no listener, direct-append is the only echo route")
      (is (eq :user (tui-transcript:event-role (first result))))
      (is (string= "hello" (tui-transcript:event-text (first result)))))))

(test (tui-app-handle-submit-emits-tui-user-submitted-for-typed-input :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (app (tui-app:make-tui-app :context context :columns 32))
         (witness (capture-events-on service context)))
    (tui-app:tui-app-feed app "hello" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (settle-tui-app app)
    (let ((types (funcall witness)))
      (is (find :tui/user-submitted types)
          "handle-tui-app-submit emits :tui/user-submitted")
      (is (find :agent/user-message-appended types)
          "agent-loop still emits the session-log event for the same input"))))

(test (tui-app-programmatic-prompt-skips-tui-user-submitted :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (_app (tui-app:make-tui-app :context context :columns 32))
         (witness (capture-events-on service context)))
    (declare (ignore _app))
    (agent-session:submit-agent-session-prompt service :default-mode
                                               "programmatic" context)
    (let ((types (funcall witness)))
      (is (null (find :tui/user-submitted types))
          "programmatic submission must not raise the TUI display event")
      (is (find :agent/user-message-appended types)
          "the session-log event still fires for the programmatic path"))))

(test (tui-app-render-width-follows-terminal-columns :fixture interactive-authority)
  "default-scrollback-render reads the live width from the terminal slot each
   frame, so changing terminal-columns reflows the open region to the new width."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 40 :rows 24))
         (renderer (tui-app:tui-app-renderer app))
         (event (tui-transcript:make-transcript-event
                 :message :assistant "hello world")))
    (tui-app:tui-app-add-event app event)
    (tui-transcript:begin-scrollback-stream renderer event)
    (tui-app:render-tui-app app :force t)
    (is (= 40 (length (first (tui-transcript:scrollback-renderer-region-lines
                              renderer))))
        "open region line is padded to the initial width")
    (setf (tui-terminal:terminal-columns (tui-app:tui-app-terminal app)) 100)
    (tui-app:render-tui-app app)
    (is (= 100 (length (first (tui-transcript:scrollback-renderer-region-lines
                               renderer))))
        "next frame follows the new terminal width")))

(test (tui-app-redraw-during-open-stream-keeps-the-stream-open :fixture interactive-authority)
  "A hard redraw (Ctrl+O reprint, render-fault recovery) during an open stream
   keeps the stream open across the renderer reset, so the reprint commits only
   the events before the commit boundary and the next delta cannot land behind
   it."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (renderer (tui-app:tui-app-renderer app))
         (live (tui-transcript:make-transcript-event :message :assistant "thinking")))
    (tui-app:tui-app-add-event
     app (tui-transcript:make-transcript-event :message :user "hi"))
    (tui-app:tui-app-add-event app live)
    (tui-transcript:begin-scrollback-stream renderer live)
    (tui-app:render-tui-app app :force t)
    (tui-app:redraw-tui-app app)
    (is (eq live (tui-transcript:scrollback-renderer-streaming-event renderer))
        "the reset preserves the open stream")
    (is (= 1 (tui-transcript:scrollback-renderer-printed-events renderer))
        "the reprint commits only events before the open stream")
    (setf (tui-transcript:event-text live) "thinking more")
    (tui-app:render-tui-app app)
    (is (zerop (kli/tui/app:tui-app-render-fault-streak app))
        "the post-redraw delta frame renders without a fault")))

(test (tui-app-apply-terminal-size-resizes-and-reflows :fixture interactive-authority)
  "apply-terminal-size resyncs both dimension slots and hard-redraws on a change,
   and is a silent no-op when the size is unchanged."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 80 :rows 24))
         (terminal (tui-app:tui-app-terminal app)))
    (tui-app:tui-app-add-event
     app (tui-transcript:make-transcript-event :message :assistant "hi"))
    (tui-app:render-tui-app app :force t)
    (is-true (tui-app::apply-terminal-size app 120 40)
             "a dimension change reflows and returns T")
    (is (= 120 (tui-terminal:terminal-columns terminal)))
    (is (= 40 (tui-terminal:terminal-rows terminal)))
    (let ((after-reflow (length (tui-terminal:terminal-output terminal))))
      (is (null (tui-app::apply-terminal-size app 120 40))
          "an unchanged size returns NIL")
      (is (= after-reflow (length (tui-terminal:terminal-output terminal)))
          "the no-op writes nothing to the terminal"))))

(test (tui-app-pre-stream-abort-retracts-the-prompt :fixture interactive-authority)
  "Esc-aborting before any delta fully un-sends the prompt: its transcript row is
   dropped, its text returns to the editor (prepended before text typed since), the
   session leaf repoints past it, the notice clears, and the slots reset. The
   retract runs at :agent/turn-end. The fixture replays the worker's pre-stream
   window by hand — append the user turn, echo it, capture the retract state."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (session (agents:agent-session agent))
         (store (agents:agent-store agent))
         (mode-id (tui-app:tui-app-mode-id app)))
    (sess:append-session-entry store session
                               (sess:make-message-entry (sess:make-user-message "first"))
                               context)
    (sess:append-session-entry store session
                               (sess:make-message-entry (sess:make-assistant-message "reply"))
                               context)
    (let ((pre-turn-leaf (sess:session-leaf-id session)))
      (sess:append-session-entry store session
                                 (sess:make-message-entry (sess:make-user-message "retract me"))
                                 context)
      (kli/tui/app:apply-tui-app-event-projection
       app (event:make-event :tui/user-submitted
                             :payload (list :mode mode-id :input "retract me"))
       mode-id)
      (setf (tui-app:tui-app-pending-prompt app) "retract me"
            (tui-app:tui-app-pre-turn-leaf-id app) pre-turn-leaf)
      (tui-editor:set-editor-value (tui-app:tui-app-editor app) "draft")
      (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
      (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
      (is (eq :aborted (tui-app:request-tui-app-abort app 1001)))
      (is (tui-app:tui-app-retract-armed-p app))
      (is (null (tui-transcript:scrollback-renderer-notice
                 (tui-app:tui-app-renderer app))))
      (is (= 1 (length (tui-app:tui-app-transcript-events app)))
          "the user row stands until the turn unwinds")
      (kli/tui/app:apply-tui-app-event-projection
       app (event:make-event :agent/turn-end
                             :payload (list :mode mode-id :turn-id :turn-1
                                            :state :aborted))
       mode-id)
      (is (null (tui-app:tui-app-transcript-events app)))
      (is (string= (format nil "retract me~%draft")
                   (tui-app:tui-app-editor-value app)))
      (is (eq pre-turn-leaf (sess:session-leaf-id session)))
      (is (equal '("first" "reply")
                 (mapcar #'sess:message-content
                         (sess:session-context-messages
                          (sess:build-session-context store session))))
          "the retracted prompt left model context")
      (is (null (tui-app:tui-app-retract-armed-p app)))
      (is (null (tui-app:tui-app-pending-prompt app)))
      (is (null (tui-app:tui-app-pre-turn-leaf-id app))))))

(test (tui-app-retract-refused-when-turn-entries-landed :fixture interactive-authority)
  "A turn of thinking and tool calls emits no text delta, so pending-prompt
   stays set and Esc-Esc arms the retract — but assistant and tool entries have
   landed. The session refuses the repoint (un-sending would orphan that work):
   the leaf stays, the row stays, the editor stays, and the abort acknowledges
   with the transient notice instead."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (session (agents:agent-session agent))
         (store (agents:agent-store agent))
         (mode-id (tui-app:tui-app-mode-id app)))
    (let ((pre-turn-leaf (sess:session-leaf-id session)))
      (sess:append-session-entry store session
                                 (sess:make-message-entry (sess:make-user-message "keep me"))
                                 context)
      (sess:append-session-entry store session
                                 (sess:make-message-entry (sess:make-assistant-message "calling a tool"))
                                 context)
      (sess:append-session-entry store session
                                 (sess:make-message-entry (sess:make-tool-result-message "tool output" :tool-call-id "tool-1"))
                                 context)
      (kli/tui/app:apply-tui-app-event-projection
       app (event:make-event :tui/user-submitted
                             :payload (list :mode mode-id :input "keep me"))
       mode-id)
      (setf (tui-app:tui-app-pending-prompt app) "keep me"
            (tui-app:tui-app-pre-turn-leaf-id app) pre-turn-leaf)
      (tui-editor:set-editor-value (tui-app:tui-app-editor app) "draft")
      (setf (agents:agent-state-value (agents:agent-state agent)) :streaming)
      (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
      (is (eq :aborted (tui-app:request-tui-app-abort app 1001)))
      (is (tui-app:tui-app-retract-armed-p app))
      (let ((leaf-before (sess:session-leaf-id session)))
        (kli/tui/app:apply-tui-app-event-projection
         app (event:make-event :agent/turn-end
                               :payload (list :mode mode-id :turn-id :turn-1
                                              :state :aborted))
         mode-id)
        (is (eq leaf-before (sess:session-leaf-id session))
            "the leaf stays on the landed work, not the pre-turn entry")
        (is (= 1 (length (tui-app:tui-app-transcript-events app)))
            "the user row stays - the prompt was answered with work")
        (is (string= "draft" (tui-app:tui-app-editor-value app))
            "the editor keeps only what was typed since")
        (is (string= "Interrupted."
                     (tui-transcript:scrollback-renderer-notice
                      (tui-app:tui-app-renderer app)))
            "the refusal acknowledges the abort")
        (is (null (tui-app:tui-app-retract-armed-p app)))
        (is (null (tui-app:tui-app-pending-prompt app)))
        (is (null (tui-app:tui-app-pre-turn-leaf-id app)))))))

(test (tui-app-mid-stream-abort-does-not-retract-the-prompt :fixture interactive-authority)
  "Once a delta streams the pending retract clears: aborting annotates and keeps
   the reply, a following turn-end retracts nothing, and the editor is untouched."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (mode-id (tui-app:tui-app-mode-id app)))
    (setf (tui-app:tui-app-pending-prompt app) "retract me"
          (tui-app:tui-app-pre-turn-leaf-id app) :some-leaf
          (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (kli/tui/app:apply-tui-app-event-projection
     app (event:make-event :agent/delta
                           :payload (list :mode mode-id :turn-id :turn-1
                                          :text "partial answer"))
     mode-id)
    (is (null (tui-app:tui-app-pending-prompt app)))
    (is (null (tui-app:tui-app-pre-turn-leaf-id app)))
    (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
    (is (eq :aborted (tui-app:request-tui-app-abort app 1001)))
    (is (null (tui-app:tui-app-retract-armed-p app)))
    (let ((reply (first (tui-app:tui-app-transcript-events app))))
      (is (= 1 (length (tui-app:tui-app-transcript-events app))))
      (is (eq :aborted (tui-transcript:event-status reply))))
    (kli/tui/app:apply-tui-app-event-projection
     app (event:make-event :agent/turn-end
                           :payload (list :mode mode-id :turn-id :turn-1
                                          :state :aborted))
     mode-id)
    (is (= 1 (length (tui-app:tui-app-transcript-events app))))
    (is (string= "" (tui-app:tui-app-editor-value app)))))

(test (tui-app-pre-stream-abort-retracts-despite-a-prior-aborted-reply :fixture interactive-authority)
  "A prior turn's aborted reply lingers as the renderer's streaming-event. A fresh
   prompt aborted pre-stream must still retract — pending-prompt decides, not the
   stale reply that mark-tui-app-reply-aborted reads."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (session (agents:agent-session agent))
         (store (agents:agent-store agent))
         (mode-id (tui-app:tui-app-mode-id app)))
    (setf (tui-app:tui-app-pending-prompt app) "essay"
          (tui-app:tui-app-pre-turn-leaf-id app) :leaf-1
          (agents:agent-state-value (agents:agent-state agent)) :streaming)
    (kli/tui/app:apply-tui-app-event-projection
     app (event:make-event :agent/delta
                           :payload (list :mode mode-id :turn-id :turn-1
                                          :text "a long essay reply"))
     mode-id)
    (is (null (tui-app:tui-app-pending-prompt app))
        "this turn's first delta cleared the pending prompt")
    (is (eq :armed (tui-app:request-tui-app-abort app 1000)))
    (is (eq :aborted (tui-app:request-tui-app-abort app 1001)))
    (is (null (tui-app:tui-app-retract-armed-p app)))
    (let ((stale (tui-transcript:scrollback-renderer-streaming-event
                  (tui-app:tui-app-renderer app))))
      (is (and stale (eq :aborted (tui-transcript:event-status stale)))
          "the prior reply is stamped and still set on the renderer"))
    (let ((pre-turn-leaf (sess:session-leaf-id session)))
      (sess:append-session-entry store session
                                 (sess:make-message-entry (sess:make-user-message "so"))
                                 context)
      (kli/tui/app:apply-tui-app-event-projection
       app (event:make-event :tui/user-submitted
                             :payload (list :mode mode-id :input "so"))
       mode-id)
      (setf (tui-app:tui-app-pending-prompt app) "so"
            (tui-app:tui-app-pre-turn-leaf-id app) pre-turn-leaf
            (agents:agent-state-value (agents:agent-state agent)) :streaming)
      (is (eq :armed (tui-app:request-tui-app-abort app 2000)))
      (is (eq :aborted (tui-app:request-tui-app-abort app 2001)))
      (is (tui-app:tui-app-retract-armed-p app)
          "pending-prompt, not the stale reply, decides pre-stream")
      (kli/tui/app:apply-tui-app-event-projection
       app (event:make-event :agent/turn-end
                             :payload (list :mode mode-id :turn-id :turn-2
                                            :state :aborted))
       mode-id)
      (is (= 1 (length (tui-app:tui-app-transcript-events app)))
          "only the prior aborted reply remains, the \"so\" row is gone")
      (is (string= "so" (tui-app:tui-app-editor-value app)))
      (is (eq pre-turn-leaf (sess:session-leaf-id session))
          "the session leaf repoints past the retracted prompt"))))

(test (tui-app-spinner-animates-while-busy-and-retires-when-idle :fixture interactive-authority)
  "The working indicator shows a frame while a turn is in flight, advances one
   frame per tick interval, and clears once the turn ends. An idle prompt reserves
   no row. The poll detects busy-state from the bound agent and repaints on change."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48 :rows 12))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object (kli:context-registry context)
                                      (agent-session:mode-binding-agent-id binding)))
         (state (agents:agent-state agent))
         (terminal (tui-app:tui-app-terminal app))
         (interval tui-app::+spinner-tick-interval+))
    (tui-status:register-widget
     (kli:active-protocol context)
     :spinner
     (lambda (protocol theme width)
       (declare (ignore protocol theme))
       (tui-status:render-spinner-line (tui-app:tui-app-spinner-active-p app)
                                       (tui-app:tui-app-spinner-phase app)
                                       width))
     :placement :above-input)
    (is (eq :idle (tui-app:poll-tui-app-spinner app 0)))
    (is (not (tui-app:tui-app-spinner-active-p app)))
    (tui-terminal:terminal-clear terminal)
    (tui-app:render-tui-app app)
    (is (not (search "working" (tui-terminal:terminal-output terminal)))
        "no indicator at an idle prompt")
    (setf (agents:agent-state-value state) :streaming)
    (tui-terminal:terminal-clear terminal)
    (is (eq :ticked (tui-app:poll-tui-app-spinner app 1000)))
    (is (tui-app:tui-app-spinner-active-p app))
    (is (= 0 (tui-app:tui-app-spinner-phase app)))
    (is (search "⠋ working" (tui-terminal:terminal-output terminal))
        "the first frame shows once a turn is in flight")
    (let* ((out (tui-terminal:terminal-output terminal))
           (spin-at (search "⠋ working" out))
           (rule-at (search "───" out)))
      (is (and spin-at rule-at (< spin-at rule-at))
          "the indicator draws above the input box top rule, not in the footer"))
    (is (eq :idle (tui-app:poll-tui-app-spinner app 1000))
        "a poll before the interval lapses does not advance")
    (is (= 0 (tui-app:tui-app-spinner-phase app)))
    (tui-terminal:terminal-clear terminal)
    (is (eq :ticked (tui-app:poll-tui-app-spinner app (+ 1000 interval))))
    (is (= 1 (tui-app:tui-app-spinner-phase app)))
    (is (search "⠙ working" (tui-terminal:terminal-output terminal))
        "the frame advances once the interval lapses")
    (setf (agents:agent-state-value state) :idle)
    (is (eq :ticked (tui-app:poll-tui-app-spinner app 9000)))
    (is (not (tui-app:tui-app-spinner-active-p app)))
    (tui-terminal:terminal-clear terminal)
    (tui-app:render-tui-app app)
    (is (not (search "working" (tui-terminal:terminal-output terminal)))
        "the indicator retires when the turn ends")))

(test (tui-app-session-switch-replays-stored-conversation :fixture interactive-authority)
  "Switching sessions rebuilds the transcript from the switched-to session's
   stored entries: a reset clears the previous conversation's rows, and a
   resume shows the resumed conversation instead of the previous session's."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("first reply")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service)))
    (flet ((transcript-messages ()
             (mapcar (lambda (e) (list (tui-transcript:event-role e)
                                       (tui-transcript:event-text e)))
                     (remove-if-not
                      (lambda (e) (eq :message (tui-transcript:event-kind e)))
                      (tui-app:tui-app-transcript-events app)))))
      (tui-app:tui-app-feed app "hello" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (settle-tui-app app)
      (is (equal '((:user "hello") (:assistant "first reply"))
                 (transcript-messages)))
      (agent-session:reset-agent-session service :default-mode context)
      (is (null (transcript-messages))
          "switching to the fresh session clears the previous conversation")
      (agent-session:resume-agent-session service :default-mode
                                          :agent-session-test-session context)
      (is (equal '((:user "hello") (:assistant "first reply"))
                 (transcript-messages))
          "resume replays the stored conversation into the transcript"))))

(test resolve-auto-theme-mode-prefers-colorfgbg-and-skips-the-osc-query
  (let ((queried nil))
    (flet ((q () (setf queried t) (values (list 1d0 1d0 1d0) "resid")))
      (multiple-value-bind (mode residual)
          (tui-app:resolve-auto-theme-mode "15;0" #'q)
        (is (string= "dark" mode))
        (is (string= "" residual))
        (is (null queried)
            "a parseable COLORFGBG decides directly; the OSC query is skipped")))))

(test resolve-auto-theme-mode-falls-back-to-the-osc-query
  (let ((queried nil))
    (flet ((q () (setf queried t) (values (list 1d0 1d0 1d0) "resid")))
      (multiple-value-bind (mode residual)
          (tui-app:resolve-auto-theme-mode nil #'q)
        (is (string= "light" mode))
        (is (string= "resid" residual))
        (is-true queried "absent COLORFGBG runs the OSC handshake")))))

(test (tui-app-reattach-rebinds-listener-and-replays :fixture interactive-authority)
  "reattach-tui-app repairs the wiring the reconstruct branch of /snapshot restore
leaves behind, where the old listener is stranded on the discarded service and the
replay-driving :session-switch already fired: it binds a fresh listener to the
current service (one, no duplicate) and replays the bound session."
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (store (kli:find-live-object (kli:context-registry context)
                                      :session-store))
         (saved (sess:find-session store :agent-session-test-session)))
    (sess:append-session-entry
     store saved
     (sess:make-message-entry (sess:make-user-message "REPLAY-ME")) context)
    (tui-app:unregister-tui-app-listener app)
    (tui-app:reset-tui-app app)
    (let ((before (length (agent-session:session-event-listeners service))))
      (tui-app:reattach-tui-app app)
      (is (not (null (tui-app:tui-app-listener-id app)))
          "reattach binds a fresh listener")
      (is (= (1+ before)
             (length (agent-session:session-event-listeners service)))
          "exactly one listener, no duplicate")
      (is (some (lambda (e)
                  (let ((text (tui-transcript:event-text e)))
                    (and text (search "REPLAY-ME" text))))
                (tui-app:tui-app-transcript-events app))
          "reattach replays the bound session into the transcript")
      (is (search "agent-session-model"
                  (app::context-usage-footer-text context :default-mode))
          "the footer reads the bound mode's model after the reconstruct-path replay"))))
