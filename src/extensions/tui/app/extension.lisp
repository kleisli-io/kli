(in-package #:kli/tui/app)

(defun make-tui-app-contract ()
  (make-provider-contract
   :id :tui/app/v1
   :capability :tui/app
   :required-entries
   '(:make-tui-app
     :register-tui-app
     :reattach-tui-app
     :tui-app-editor
     :tui-app-editor-value
     :tui-app-transcript-events
     :tui-app-context
     :tui-app-runtime-behavior
     :tui-app-add-event
     :tui-app-add-system-event
     :clear-tui-app-screen
     :reset-tui-app
     :request-tui-app-interrupt
     :open-tui-app-rewind-menu
     :open-tui-app-branches-menu
     :open-tui-app-resume-menu
     :open-tui-app-menu
     :stop-tui-app
     :render-tui-app
     :redraw-tui-app
     :route-tui-app-event
     :route-tui-app-events
     :flush-tui-app-input
     :tui-app-feed
     :tui-app-step
     :tui-app-runtime-step
     :recode-tui-app-runtime
     :run-tui-app
     :submit
     :add-tui-app-surface
     :remove-tui-app-surface
     :list-tui-app-surfaces
     :active-tui-app-surface
     :select-tui-app-surface
     :cycle-tui-app-surface
     :add-tui-app-route-interceptor
     :remove-tui-app-route-interceptor
     :enqueue-tui-app-task
     :request-tui-app-render)))

(defun make-tui-app-provider ()
  (make-provider
   :id :tui-app-provider
   :capability :tui/app
   :contracts '(:tui/app/v1)
   :entries
   (list :make-tui-app #'make-tui-app
         :register-tui-app #'register-tui-app
         :reattach-tui-app #'reattach-tui-app
         :tui-app-editor #'tui-app-editor
         :tui-app-editor-value #'tui-app-editor-value
         :tui-app-transcript-events #'tui-app-transcript-events
         :tui-app-context #'tui-app-context
         :tui-app-runtime-behavior #'tui-app-runtime-behavior
         :tui-app-add-event #'tui-app-add-event
         :tui-app-add-system-event #'tui-app-add-system-event
         :clear-tui-app-screen #'clear-tui-app-screen
         :reset-tui-app #'reset-tui-app
         :request-tui-app-interrupt #'request-tui-app-interrupt
         :open-tui-app-rewind-menu #'open-tui-app-rewind-menu
         :open-tui-app-branches-menu #'open-tui-app-branches-menu
         :open-tui-app-resume-menu #'open-tui-app-resume-menu
         :open-tui-app-menu #'open-tui-app-menu
         :stop-tui-app #'stop-tui-app
         :render-tui-app #'render-tui-app
         :redraw-tui-app #'redraw-tui-app
         :route-tui-app-event #'route-tui-app-event
         :route-tui-app-events #'route-tui-app-events
         :flush-tui-app-input #'flush-tui-app-input
         :tui-app-feed #'tui-app-feed
         :tui-app-step #'tui-app-step
         :tui-app-runtime-step #'tui-app-runtime-step
         :recode-tui-app-runtime #'recode-tui-app-runtime
         :run-tui-app #'run-tui-app
         :submit #'submit-tui-app-input
         :add-tui-app-surface #'add-tui-app-surface
         :remove-tui-app-surface #'remove-tui-app-surface
         :list-tui-app-surfaces #'list-tui-app-surfaces
         :active-tui-app-surface #'active-tui-app-surface
         :select-tui-app-surface #'select-tui-app-surface
         :cycle-tui-app-surface #'cycle-tui-app-surface
         :add-tui-app-route-interceptor #'add-tui-app-route-interceptor
         :remove-tui-app-route-interceptor #'remove-tui-app-route-interceptor
         :enqueue-tui-app-task #'enqueue-main-thread-task
         :request-tui-app-render #'request-tui-app-render)))

(defextension tui-app
  ;; Only the capabilities tui-app resolves directly: session/log for the
  ;; :session-store live object and agent/session for the :agent-session-service.
  ;; events, session/entries, and context/lens reach the install closure
  ;; transitively through agent/session, which requires them in its own right.
  (:requires
   (capability session/log :contract session/log/v1)
   (capability agent/session :contract agent/session/v1))
  (:provides
   (event-type :tui/app-started)
   (contract tui/app/v1
     (make-tui-app-contract))
   (capability tui/app (make-tui-app-provider))))
