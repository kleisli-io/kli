(in-package #:kli/tests)

;; A minimal switchable surface: a view that records inputs and a renderer
;; that stamps a marker into the terminal, enough to observe render and route
;; dispatch without any scrollback machinery.

(defclass stub-surface-view (kli:live-object)
  ((inputs :initform nil :accessor stub-surface-view-inputs)))

(defmethod tui-core:handle-input ((view stub-surface-view) input)
  (push input (stub-surface-view-inputs view))
  t)

(defmethod tui-core:set-focused ((view stub-surface-view) state)
  (declare (ignore state))
  view)

(defclass stub-surface-renderer ()
  ((frames :initform 0 :accessor stub-surface-renderer-frames)))

(defmethod tui-core:render-frame ((renderer stub-surface-renderer) terminal &key force)
  (declare (ignore force))
  (incf (stub-surface-renderer-frames renderer))
  (tui-core:write-terminal terminal "STUB-SURFACE-FRAME"))

(defun add-stub-surface (app id &key activate)
  (let ((view (make-instance 'stub-surface-view
                             :protocol (kli:object-protocol app)))
        (renderer (make-instance 'stub-surface-renderer)))
    (tui-app:add-tui-app-surface app
                                 :id id
                                 :label (string-downcase (symbol-name id))
                                 :view view
                                 :renderer renderer
                                 :activate activate)
    (values view renderer)))

(defun surface-ids (app)
  (mapcar (lambda (d) (getf d :id)) (tui-app:list-tui-app-surfaces app)))

(test (tui-surfaces-app-boots-with-transcript-surface-0 :fixture interactive-authority)
  "Surface 0 exists from birth, is active, and aliases the app's transcript
view/renderer/route-context rather than copying them."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (is (equal '(:transcript) (surface-ids app)))
    (let ((descriptor (tui-app:active-tui-app-surface app)))
      (is (eq :transcript (getf descriptor :id)))
      (is (string= "transcript" (getf descriptor :label)))
      (is-true (getf descriptor :active-p)))
    (let ((surface (tui-app:tui-app-active-surface app)))
      (is (eq (tui-app:tui-app-view app) (tui-app:surface-view surface)))
      (is (eq (tui-app:tui-app-renderer app) (tui-app:surface-renderer surface)))
      (is (eq (tui-app:tui-app-route-context app)
              (tui-app:surface-route-context surface))))))

(test (tui-surfaces-add-list-select-cycle-remove :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (add-stub-surface app :alpha)
    (add-stub-surface app :beta)
    (is (equal '(:transcript :alpha :beta) (surface-ids app)))
    (is (eq :transcript (getf (tui-app:active-tui-app-surface app) :id))
        "adding without :activate leaves surface 0 active")
    (is (eq :alpha (tui-app:select-tui-app-surface app :alpha)))
    (is (eq :alpha (getf (tui-app:active-tui-app-surface app) :id)))
    (is (eq :beta (tui-app:cycle-tui-app-surface app)))
    (is (eq :transcript (tui-app:cycle-tui-app-surface app))
        "cycling wraps past the end")
    (is (eq :beta (tui-app:cycle-tui-app-surface app -1))
        "a negative delta cycles backwards, wrapping")
    (is (null (tui-app:select-tui-app-surface app :no-such-surface))
        "selecting an unknown id declines")
    (is (eq :beta (getf (tui-app:active-tui-app-surface app) :id)))
    (is (eq :transcript (tui-app:select-tui-app-surface app 0))
        "selection also accepts an index")
    (tui-app:select-tui-app-surface app :beta)
    (is (eq t (tui-app:remove-tui-app-surface app :alpha)))
    (is (equal '(:transcript :beta) (surface-ids app)))
    (is (eq :beta (getf (tui-app:active-tui-app-surface app) :id))
        "removing a surface below the active one keeps the same surface active")
    (is (null (tui-app:remove-tui-app-surface app :alpha))
        "removing an unknown id declines")))

(test (tui-surfaces-remove-active-falls-back-to-transcript :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (add-stub-surface app :alpha :activate t)
    (is (eq :alpha (getf (tui-app:active-tui-app-surface app) :id)))
    (is (eq t (tui-app:remove-tui-app-surface app :alpha)))
    (is (eq :transcript (getf (tui-app:active-tui-app-surface app) :id)))))

(test (tui-surfaces-surface-0-refuses-removal :fixture interactive-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((context (ensure-tui-app-stack))
           (app (tui-app:make-tui-app :context context :columns 48)))
      (is (null (tui-app:remove-tui-app-surface app :transcript)))
      (is (equal '(:transcript) (surface-ids app))))))

(test (tui-surfaces-mutators-refused-off-the-loop-thread :fixture interactive-authority)
  "Surface and interceptor mutators are loop-thread-only, like render:
off-thread callers must go through :enqueue-tui-app-task."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((context (ensure-tui-app-stack))
           (app (tui-app:make-tui-app :context context :columns 48))
           (other (sb-thread:make-thread (lambda () nil))))
      (sb-thread:join-thread other)
      (add-stub-surface app :alpha)
      (setf (kli/tui/app::tui-app-loop-thread app) other)
      (is (null (tui-app:add-tui-app-surface
                 app :id :beta
                 :view (make-instance 'stub-surface-view)
                 :renderer (make-instance 'stub-surface-renderer))))
      (is (null (tui-app:select-tui-app-surface app :alpha)))
      (is (null (tui-app:cycle-tui-app-surface app)))
      (is (null (tui-app:remove-tui-app-surface app :alpha)))
      (is (null (tui-app:add-tui-app-route-interceptor
                 app :probe (lambda (app event)
                              (declare (ignore app event))
                              nil))))
      (is (null (tui-app:remove-tui-app-route-interceptor app :probe)))
      (is (equal '(:transcript :alpha) (surface-ids app))
          "refused mutators left the surface list untouched")
      (is (eq :transcript (getf (tui-app:active-tui-app-surface app) :id)))
      (is (null (kli/tui/app::tui-app-route-interceptors app)))
      (setf (kli/tui/app::tui-app-loop-thread app) nil)
      (is (eq :alpha (tui-app:select-tui-app-surface app :alpha))
          "guards inert once the loop thread is released"))))

(test (tui-surfaces-select-paints-the-new-surface :fixture interactive-authority)
  "Selecting a non-transcript surface clears the screen and paints its
renderer's frame; selecting back rides the hard-redraw doctrine and repaints
the transcript."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (terminal (tui-app:tui-app-terminal app)))
    (multiple-value-bind (view renderer) (add-stub-surface app :alpha)
      (declare (ignore view))
      (tui-terminal:terminal-clear terminal)
      (tui-app:select-tui-app-surface app :alpha)
      (let ((output (tui-terminal:terminal-output terminal)))
        (is (search (format nil "~C[2J" #\Esc) output)
            "the switch clears the visible screen")
        (is (search "STUB-SURFACE-FRAME" output))
        (is (= 1 (stub-surface-renderer-frames renderer))))
      (tui-terminal:terminal-clear terminal)
      (tui-app:select-tui-app-surface app 0)
      (let ((output (tui-terminal:terminal-output terminal)))
        (is (search (format nil "~C[2J" #\Esc) output))
        (is (search "> " output)
            "switching back repaints the transcript prompt")
        (is (= 1 (stub-surface-renderer-frames renderer))
            "the transcript frame does not touch the hidden stub renderer")))))

(test (tui-surfaces-hidden-transcript-takes-no-terminal-writes :fixture interactive-authority)
  "Agent events keep projecting into the transcript model while another
surface is active, but nothing of them reaches the terminal until switch-back."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (terminal (tui-app:tui-app-terminal app))
         (mode-id (tui-app:tui-app-mode-id app)))
    (add-stub-surface app :alpha :activate t)
    (tui-terminal:terminal-clear terminal)
    (kli/tui/app:apply-tui-app-event-projection
     app
     (event:make-event :agent/delta
                       :payload (list :mode mode-id :turn-id :turn-1
                                      :text "hidden reply"))
     mode-id)
    (tui-app:render-tui-app app)
    (is (= 1 (length (tui-app:tui-app-transcript-events app)))
        "the delta projected into the transcript model")
    (let ((output (tui-terminal:terminal-output terminal)))
      (is (null (search "hidden reply" output))
          "no transcript write reaches the terminal while hidden")
      (is (search "STUB-SURFACE-FRAME" output)))
    (tui-terminal:terminal-clear terminal)
    (tui-app:select-tui-app-surface app 0)
    (is (search "hidden reply" (tui-terminal:terminal-output terminal))
        "switch-back repaints the accumulated transcript")))

(test (tui-surfaces-drain-repaints-active-surface-once-per-batch :fixture interactive-authority)
  "request-tui-app-render coalesces: a burst of requests drains into one frame
of the active surface."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (multiple-value-bind (view renderer) (add-stub-surface app :alpha :activate t)
      (declare (ignore view))
      (let ((before (stub-surface-renderer-frames renderer)))
        (tui-app:request-tui-app-render app)
        (tui-app:request-tui-app-render app)
        (kli/tui/app:run-pending-main-thread-tasks app)
        (is (= (1+ before) (stub-surface-renderer-frames renderer))
            "two requests coalesce into one repaint per drain")
        (kli/tui/app:run-pending-main-thread-tasks app)
        (is (= (1+ before) (stub-surface-renderer-frames renderer))
            "a drain with no pending request paints nothing")))))

(test (tui-surfaces-active-surface-owns-route-dispatch :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (multiple-value-bind (view renderer) (add-stub-surface app :alpha :activate t)
      (declare (ignore renderer))
      (tui-app:tui-app-feed app "z" :render nil)
      (is (equal '("z") (stub-surface-view-inputs view))
          "input routes to the active surface's view")
      (is (string= "" (tui-app:tui-app-editor-value app))
          "the hidden transcript editor saw nothing"))))

(test (tui-surfaces-interceptors-run-in-insertion-order-and-stop-at-handled :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (order '()))
    (tui-app:add-tui-app-route-interceptor
     app :first (lambda (app event)
                  (declare (ignore app event))
                  (push :first order)
                  nil))
    (tui-app:add-tui-app-route-interceptor
     app :second (lambda (app event)
                   (declare (ignore app event))
                   (push :second order)
                   :handled))
    (tui-app:add-tui-app-route-interceptor
     app :third (lambda (app event)
                  (declare (ignore app event))
                  (push :third order)
                  nil))
    (tui-app:tui-app-feed app "x" :render nil)
    (is (equal '(:first :second) (nreverse (shiftf order nil)))
        "the walk runs in insertion order and stops at :handled")
    (is (string= "" (tui-app:tui-app-editor-value app))
        "a :handled event never reaches the active surface")
    (tui-app:add-tui-app-route-interceptor
     app :second (lambda (app event)
                   (declare (ignore app event))
                   (push :second-replaced order)
                   nil))
    (tui-app:tui-app-feed app "y" :render nil)
    (is (equal '(:first :second-replaced :third) (nreverse (shiftf order nil)))
        "re-registering an id replaces the fn in place, keeping its position")
    (is (string= "y" (tui-app:tui-app-editor-value app))
        "with every interceptor passing, the event reaches the active surface")
    (tui-app:remove-tui-app-route-interceptor app :first)
    (tui-app:remove-tui-app-route-interceptor app :second)
    (tui-app:remove-tui-app-route-interceptor app :third)
    (tui-app:tui-app-feed app "z" :render nil)
    (is (null order) "removed interceptors see nothing")
    (is (string= "yz" (tui-app:tui-app-editor-value app)))))

(test (tui-surfaces-interrupt-bypasses-interceptors :fixture interactive-authority)
  "Ctrl+C is the app's own arc: :interrupt events route to the app handler
even with an interceptor that swallows everything installed."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (seen 0))
    (tui-app:add-tui-app-route-interceptor
     app :swallow-all (lambda (app event)
                        (declare (ignore app event))
                        (incf seen)
                        :handled))
    (tui-app:route-tui-app-event app (tui-input:make-interrupt-input-event))
    (is (zerop seen) "the interceptor never saw the :interrupt event")
    (is (tui-app:tui-app-interrupt-armed-p app)
        "the app interrupt handler ran and armed the quit arc")))

(test (tui-surfaces-signaling-interceptor-is-contained-at-the-step-barrier :fixture interactive-authority)
  "A signaling interceptor is contained by the runtime-step behavior cell
(:reify, fallback :error-handled); the loop keeps stepping and the next event
still routes."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (let* ((context (ensure-tui-app-stack))
             (app (tui-app:make-tui-app :context context :columns 48)))
        (tui-app:add-tui-app-route-interceptor
         app :boom (lambda (app event)
                     (declare (ignore app event))
                     (error "interceptor boom")))
        (is (eq :error-handled
                (tui-app:tui-app-runtime-step app (make-string-input-stream "x"))))
        (is (= 1 (kli/tui/app::tui-app-step-fault-streak app)))
        (tui-app:remove-tui-app-route-interceptor app :boom)
        (is (eq :input
                (tui-app:tui-app-runtime-step app (make-string-input-stream "y"))))
        (is (string= "y" (tui-app:tui-app-editor-value app))
            "the next event routes normally once the faulty interceptor is gone")
        (is (zerop (kli/tui/app::tui-app-step-fault-streak app))
            "a clean step resets the fault streak")))))

(test (tui-editor-accepts-action-keywords-as-input :fixture interactive-authority)
  "handle-input takes action keywords directly: no keymap resolution, no
string classifier, no insertion. Unknown keywords die silently."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (view (tui-app:tui-app-view app))
         (editor (tui-app:tui-app-editor app)))
    (tui-app:tui-app-feed app "hello" :render nil)
    (is (= 5 (tui-editor:editor-cursor editor)))
    (is-true (tui-core:handle-input view :move-line-start))
    (is (zerop (tui-editor:editor-cursor editor)))
    (is-true (tui-core:handle-input view :move-char-right))
    (is (= 1 (tui-editor:editor-cursor editor)))
    (is-true (tui-core:handle-input view :delete-to-line-end))
    (is (string= "h" (tui-app:tui-app-editor-value app)))
    (is (null (tui-core:handle-input view :no-such-action))
        "an unknown action keyword declines without signaling")
    (is (string= "h" (tui-app:tui-app-editor-value app))
        "nothing was inserted or submitted for the unknown keyword")
    (is (null (tui-core:handle-input view :submit))
        "keyword :submit is not widened in v1 -- the router canonicalizes
key events to strings, so the keyword dies in the editor-command dispatch")
    (is (string= "h" (tui-app:tui-app-editor-value app))
        "the buffer did not submit")))

(test (tui-surfaces-next-surface-action-cycles-via-keybinding :fixture interactive-authority)
  "No chord ships for :next-surface; a user binding routes it through the
route context to cycle-tui-app-surface."
  (let* ((context (ensure-tui-app-stack))
         (app (tui-app:make-tui-app :context context :columns 48))
         (protocol (kli:object-protocol (tui-app:tui-app-view app))))
    (add-stub-surface app :alpha)
    (is (null (tui-keymap:keymap-action protocol "f6"))
        "no default binding targets :next-surface")
    (tui-keymap:register-keybinding protocol "f6" :next-surface)
    (unwind-protect
         (progn
           (tui-app:route-tui-app-event
            app (tui-input:make-key-input-event "f6" :key-id "f6"))
           (is (eq :alpha (getf (tui-app:active-tui-app-surface app) :id)))
           (tui-app:route-tui-app-event
            app (tui-input:make-key-input-event "f6" :key-id "f6"))
           (is (eq :transcript (getf (tui-app:active-tui-app-surface app) :id))
               "cycling wraps back to the transcript"))
      (tui-keymap:unregister-keybinding protocol "f6"))))

(defvar *tui-app-started-seen* nil)

(ext:defextension test-tui-app-started-probe
  (:provides
   (on :tui/app-started
       (lambda (event context)
         (declare (ignore context))
         (push event *tui-app-started-seen*)))))

(test (tui-app-started-event-reaches-on-handler :fixture interactive-authority)
  "register-tui-app announces the app on the stream, so an (:on
:tui/app-started ...) handler installed beforehand observes it -- the seam a
surface-contributing extension uses to catch apps born after install."
  (let ((*tui-app-started-seen* nil))
    (let* ((context (ensure-tui-app-stack)))
      (install-extension context *test-tui-app-started-probe-extension-manifest*)
      (let ((app (tui-app:make-tui-app :context context :columns 48)))
        (is (= 1 (length *tui-app-started-seen*)))
        (let ((payload (event:event-payload (first *tui-app-started-seen*))))
          (is (eq (kli:object-id app) (getf payload :app-id)))
          (is (eq :default-mode (getf payload :mode))))))))
