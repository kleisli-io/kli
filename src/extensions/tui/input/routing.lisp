(in-package #:kli/tui/input)

(defun route-context-value (context key)
  (cond
    ((typep context 'input-route-context)
     (ecase key
       (:interrupt-handler (route-interrupt-handler context))
       (:abort-handler (route-abort-handler context))
       (:clear-screen-handler (route-clear-screen-handler context))
       (:tool-output-handler (route-tool-output-handler context))))
    ((listp context)
     (getf context key))
    (t
     nil)))

(defun route-key-handler (context handler-key)
  (let ((handler (route-context-value context handler-key)))
    (when handler
      (funcall handler)
      t)))

(defparameter +input-canonical+
  '(:submit "enter" :newline "newline" :backspace "backspace")
  "Actions the router rewrites to the canonical string the editor classifiers
already match (submit-input-p / newline-input-p / backspace-input-p).")

(defun route-key-input (view event context)
  (let* ((key-id (input-event-key-id event))
         (action (keymap-action (object-protocol view) key-id)))
    (case action
      (:clear-screen (route-key-handler context :clear-screen-handler))
      (:tool-output  (route-key-handler context :tool-output-handler))
      (:abort        (or (dismiss-overlay view)
                         (route-key-handler context :abort-handler)))
      ((:submit :newline :backspace)
       (handle-input view (getf +input-canonical+ action)))
      (t (when key-id (handle-input view key-id))))))

(defun route-input-event (view event context)
  (case (input-event-kind event)
    (:interrupt
     (let ((handler (route-context-value context :interrupt-handler)))
       (when handler
         (funcall handler))))
    (:text
     (handle-input view (input-event-text event)))
    (:paste
     (or (handle-paste view (input-event-text event))
         (handle-input view (input-event-text event))))
    (:key
     (route-key-input view event context))
    (otherwise
     nil)))
