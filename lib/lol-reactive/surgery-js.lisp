;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Client-side JavaScript for Component Surgery
;;;;
;;;; ALL code generated via Parenscript. NO raw JavaScript strings.
;;;; Colors use CSS variables from tokens, not hardcoded values.

(in-package :lol-reactive)

;;; ============================================================================
;;; SURGERY RUNTIME JAVASCRIPT (Parenscript)
;;;
;;; Client-side code for x-ray panel interactions.
;;; All colors via CSS variables (--color-primary, --color-secondary, --color-accent).
;;; ============================================================================

(defun surgery-runtime-js ()
  "Generate the client-side surgery runtime JavaScript via Parenscript.
   NO hardcoded colors - all styling uses CSS variables from tokens."
  (parenscript:ps
    ;; Get CSS variable value at runtime
    (defun get-css-var (name)
      (ps:chain (get-computed-style (ps:@ document document-element))
             (get-property-value name)))

    ;; Surgery Runtime - X-ray inspection and live modification
    (defvar *surgery*
      (ps:create
       ;; Currently selected component for surgery
       :active-component nil

       ;; Toggle x-ray mode for a component
       :toggle-xray
       (lambda (component-id)
         (let ((panel (ps:chain document (get-element-by-id
                       (+ "surgery-panel-" component-id))))
               (wrapper (ps:chain document (query-selector
                         (+ "[data-component-id=\"" component-id "\"]")))))
           (if (= (ps:@ this active-component) component-id)
               (ps:chain this (close-xray component-id))
               (progn
                 ;; Close any open panel
                 (when (ps:@ this active-component)
                   (ps:chain this (close-xray (ps:@ this active-component))))
                 ;; Open this panel
                 (setf (ps:@ this active-component) component-id)
                 (ps:chain panel class-list (remove "translate-x-full"))
                 (ps:chain wrapper class-list (add "xray-active"))
                 ;; Add neon border effect using CSS variable
                 (let ((primary (get-css-var "--color-primary")))
                   (setf (ps:@ wrapper style box-shadow)
                         (+ "0 0 0 3px " primary ", 0 0 20px " primary)))
                 ;; Fetch fresh state
                 (ps:chain this (refresh-state component-id))))))

       :close-xray
       (lambda (component-id)
         (let ((panel (ps:chain document (get-element-by-id
                       (+ "surgery-panel-" component-id))))
               (wrapper (ps:chain document (query-selector
                         (+ "[data-component-id=\"" component-id "\"]")))))
           (when panel
             (ps:chain panel class-list (add "translate-x-full")))
           (when wrapper
             (ps:chain wrapper class-list (remove "xray-active"))
             (setf (ps:@ wrapper style box-shadow) ""))
           (when (= (ps:@ this active-component) component-id)
             (setf (ps:@ this active-component) nil))))

       ;; Refresh state display
       :refresh-state
       (lambda (component-id)
         (let ((self this))
           (ps:chain
            (fetch "/api/surgery/state"
                   (ps:create :method "POST"
                              :headers (ps:create "Content-Type" "application/json")
                              :body (ps:chain -j-s-o-n (stringify
                                     (ps:create :component-id component-id)))))
            (then (lambda (r) (ps:chain r (json))))
            (then (lambda (data)
                    (ps:chain self (update-state-display component-id data)))))))

       ;; Update state display in panel
       :update-state-display
       (lambda (component-id state-tree)
         (let ((panel (ps:chain document (get-element-by-id
                       (+ "surgery-panel-" component-id)))))
           (when panel
             (let ((state-items (ps:chain panel (query-selector-all "[data-state-key]"))))
               (ps:chain state-items (for-each
                (lambda (item)
                  (let* ((key (ps:@ item dataset state-key))
                         (state-entry (ps:chain (ps:@ state-tree state)
                                             (find (lambda (s)
                                                     (= (ps:@ s key) key))))))
                    (when state-entry
                      (setf (ps:@ item text-content) (ps:@ state-entry value)))))))))))

       ;; Edit a state value inline
       :edit-state
       (lambda (component-id key)
         (let ((current-value (prompt (+ "Edit " key ":") "")))
           (when (not (= current-value nil))
             (ps:chain this (set-state component-id key
              (ps:chain this (parse-value current-value)))))))

       ;; Parse user input to appropriate type
       :parse-value
       (lambda (str)
         (cond
           ;; Try number
           ((ps:chain (regex "^-?\\d+(\\.\\d+)?$") (test str))
            (parse-float str))
           ;; Try boolean
           ((or (= (ps:chain str (to-lower-case)) "true") (= str "t"))
            t)
           ((or (= (ps:chain str (to-lower-case)) "false") (= str "nil"))
            nil)
           ;; String
           (t str)))

       ;; Set state via API
       :set-state
       (lambda (component-id key value)
         (let ((self this))
           (ps:chain
            (fetch "/api/surgery/update"
                   (ps:create :method "POST"
                              :headers (ps:create "Content-Type" "application/json")
                              :body (ps:chain -j-s-o-n (stringify
                                     (ps:create :component-id component-id
                                                :key key
                                                :value value)))))
            (then (lambda (r) (ps:chain r (json))))
            (then (lambda (data)
                    ;; Re-render component
                    (when (ps:@ data html)
                      (let ((wrapper (ps:chain document (query-selector
                                      (+ "[data-component-id=\"" component-id "\"] .xray-content")))))
                        (when wrapper
                          (setf (ps:@ wrapper inner-h-t-m-l) (ps:@ data html)))))
                    ;; Update state display
                    (ps:chain self (refresh-state component-id))
                    ;; Flash effect to show change
                    (ps:chain self (flash-component component-id)))))))

       ;; Eval Lisp form in component context
       :eval-in-context
       (lambda (component-id)
         (let ((input (ps:chain document (get-element-by-id (+ "repl-input-" component-id))))
               (output (ps:chain document (get-element-by-id (+ "repl-output-" component-id))))
               (self this))
           (let ((form (ps:chain (ps:@ input value) (trim))))
             (when form
               ;; Add to output
               (let ((prompt-el (ps:chain document (create-element "div"))))
                 (setf (ps:@ prompt-el class-name) "text-primary")
                 (setf (ps:@ prompt-el text-content) (+ "> " form))
                 (ps:chain output (append-child prompt-el)))
               (ps:chain
                (fetch "/api/surgery/eval"
                       (ps:create :method "POST"
                                  :headers (ps:create "Content-Type" "application/json")
                                  :body (ps:chain -j-s-o-n (stringify
                                         (ps:create :component-id component-id
                                                    :form form)))))
                (then (lambda (r) (ps:chain r (json))))
                (then (lambda (data)
                        (let ((result (ps:chain document (create-element "div"))))
                          (if (ps:@ data success)
                              (progn
                                (setf (ps:@ result class-name) "text-secondary pl-2")
                                (setf (ps:@ result text-content) (+ "=> " (ps:@ data result)))
                                ;; Re-render component if state changed
                                (when (ps:@ data html)
                                  (let ((wrapper (ps:chain document (query-selector
                                                  (+ "[data-component-id=\"" component-id "\"] .xray-content")))))
                                    (when wrapper
                                      (setf (ps:@ wrapper inner-h-t-m-l) (ps:@ data html))))))
                              (progn
                                (setf (ps:@ result class-name) "text-error pl-2")
                                (setf (ps:@ result text-content) (+ "ERROR: " (ps:@ data error)))))
                          (ps:chain output (append-child result))
                          ;; Scroll to bottom
                          (setf (ps:@ output scroll-top) (ps:@ output scroll-height))
                          ;; Clear input
                          (setf (ps:@ input value) "")
                          ;; Refresh state display
                          (ps:chain self (refresh-state component-id)))))
                (catch (lambda (e)
                         (let ((error-el (ps:chain document (create-element "div"))))
                           (setf (ps:@ error-el class-name) "text-error pl-2")
                           (setf (ps:@ error-el text-content) (+ "ERROR: " (ps:@ e message)))
                           (ps:chain output (append-child error-el))))))))))

       ;; Capture snapshot
       :capture-snapshot
       (lambda (component-id)
         (let ((description (prompt "Snapshot description (optional):" ""))
               (self this))
           (ps:chain
            (fetch "/api/surgery/snapshot"
                   (ps:create :method "POST"
                              :headers (ps:create "Content-Type" "application/json")
                              :body (ps:chain -j-s-o-n (stringify
                                     (ps:create :component-id component-id
                                                :action "capture"
                                                :description description)))))
            (then (lambda ()
                    ;; Refresh panel to show new snapshot
                    (ps:chain self (refresh-panel component-id))
                    ;; Flash with accent color
                    (let ((accent (get-css-var "--color-accent")))
                      (ps:chain self (flash-component component-id accent))))))))

       ;; Restore snapshot
       :restore-snapshot
       (lambda (component-id timestamp)
         (let ((self this))
           (ps:chain
            (fetch "/api/surgery/snapshot"
                   (ps:create :method "POST"
                              :headers (ps:create "Content-Type" "application/json")
                              :body (ps:chain -j-s-o-n (stringify
                                     (ps:create :component-id component-id
                                                :action "restore"
                                                :timestamp timestamp)))))
            (then (lambda (r) (ps:chain r (json))))
            (then (lambda (data)
                    ;; Re-render component
                    (when (ps:@ data html)
                      (let ((wrapper (ps:chain document (query-selector
                                      (+ "[data-component-id=\"" component-id "\"] .xray-content")))))
                        (when wrapper
                          (setf (ps:@ wrapper inner-h-t-m-l) (ps:@ data html)))))
                    (ps:chain self (refresh-state component-id))
                    ;; Flash with primary color
                    (let ((primary (get-css-var "--color-primary")))
                      (ps:chain self (flash-component component-id primary))))))))

       ;; Refresh entire surgery panel
       :refresh-panel
       (lambda (component-id)
         (ps:chain
          (fetch "/api/surgery/panel"
                 (ps:create :method "POST"
                            :headers (ps:create "Content-Type" "application/json")
                            :body (ps:chain -j-s-o-n (stringify
                                   (ps:create :component-id component-id)))))
          (then (lambda (r) (ps:chain r (json))))
          (then (lambda (data)
                  (when (ps:@ data panel-html)
                    (let ((old-panel (ps:chain document (get-element-by-id
                                      (+ "surgery-panel-" component-id)))))
                      (when old-panel
                        (setf (ps:@ old-panel outer-h-t-m-l) (ps:@ data panel-html))
                        ;; Re-open since we replaced the element
                        (let ((new-panel (ps:chain document (get-element-by-id
                                          (+ "surgery-panel-" component-id)))))
                          (ps:chain new-panel class-list (remove "translate-x-full"))))))))))

       ;; Flash effect to indicate change
       :flash-component
       (lambda (component-id &optional color)
         (let ((wrapper (ps:chain document (query-selector
                         (+ "[data-component-id=\"" component-id "\"]"))))
               (flash-color (or color (get-css-var "--color-secondary"))))
           (when wrapper
             (let ((original (ps:@ wrapper style transition)))
               (setf (ps:@ wrapper style transition) "box-shadow 0.15s ease-out")
               (setf (ps:@ wrapper style box-shadow)
                     (+ "0 0 0 4px " flash-color ", 0 0 30px " flash-color))
               (set-timeout
                (let ((self *surgery*))
                  (lambda ()
                    (if (= (ps:@ self active-component) component-id)
                        (let ((primary (get-css-var "--color-primary")))
                          (setf (ps:@ wrapper style box-shadow)
                                (+ "0 0 0 3px " primary ", 0 0 20px " primary)))
                        (setf (ps:@ wrapper style box-shadow) ""))
                    (setf (ps:@ wrapper style transition) original)))
                300)))))

       ;; Inspect component (log full state to console)
       :inspect-component
       (lambda (component-id)
         (ps:chain
          (fetch "/api/surgery/state"
                 (ps:create :method "POST"
                            :headers (ps:create "Content-Type" "application/json")
                            :body (ps:chain -j-s-o-n (stringify
                                   (ps:create :component-id component-id)))))
          (then (lambda (r) (ps:chain r (json))))
          (then (lambda (data)
                  (ps:chain console (group (+ "(INSPECT \"" component-id "\")")))
                  (ps:chain console (log "State:" (ps:@ data state)))
                  (ps:chain console (log "Mounted:" (ps:@ data mounted)))
                  (ps:chain console (log "Subscribers:" (ps:@ data subscribers)))
                  (ps:chain console (group-end))))))

       ;; Undo
       :undo
       (lambda (component-id)
         (let ((self this))
           (ps:chain
            (fetch "/api/surgery/undo"
                   (ps:create :method "POST"
                              :headers (ps:create "Content-Type" "application/json")
                              :body (ps:chain -j-s-o-n (stringify
                                     (ps:create :component-id component-id)))))
            (then (lambda (r) (ps:chain r (json))))
            (then (lambda (data)
                    (when (ps:@ data html)
                      (let ((wrapper (ps:chain document (query-selector
                                      (+ "[data-component-id=\"" component-id "\"] .xray-content")))))
                        (when wrapper
                          (setf (ps:@ wrapper inner-h-t-m-l) (ps:@ data html)))))
                    (ps:chain self (refresh-state component-id)))))))

       ;; Redo
       :redo
       (lambda (component-id)
         (let ((self this))
           (ps:chain
            (fetch "/api/surgery/redo"
                   (ps:create :method "POST"
                              :headers (ps:create "Content-Type" "application/json")
                              :body (ps:chain -j-s-o-n (stringify
                                     (ps:create :component-id component-id)))))
            (then (lambda (r) (ps:chain r (json))))
            (then (lambda (data)
                    (when (ps:@ data html)
                      (let ((wrapper (ps:chain document (query-selector
                                      (+ "[data-component-id=\"" component-id "\"] .xray-content")))))
                        (when wrapper
                          (setf (ps:@ wrapper inner-h-t-m-l) (ps:@ data html)))))
                    (ps:chain self (refresh-state component-id)))))))))

    ;; Global shortcuts
    (defun toggle-xray (id) (ps:chain *surgery* (toggle-xray id)))
    (defun close-xray (id) (ps:chain *surgery* (close-xray id)))
    (defun edit-state (id key) (ps:chain *surgery* (edit-state id key)))
    (defun eval-in-context (id) (ps:chain *surgery* (eval-in-context id)))
    (defun capture-snapshot (id) (ps:chain *surgery* (capture-snapshot id)))
    (defun restore-snapshot (id ts) (ps:chain *surgery* (restore-snapshot id ts)))
    (defun inspect-component (id) (ps:chain *surgery* (inspect-component id)))

    ;; Keyboard shortcuts
    (ps:chain document (add-event-listener "keydown"
     (lambda (e)
       ;; Ctrl+Shift+X = Toggle x-ray on hovered component
       (when (and (ps:@ e ctrl-key) (ps:@ e shift-key) (= (ps:@ e key) "X"))
         (let ((hovered (ps:chain document (query-selector ".xray-wrapper:hover"))))
           (when hovered
             (let ((id (ps:@ hovered dataset component-id)))
               (ps:chain *surgery* (toggle-xray id))))))
       ;; Escape = Close x-ray
       (when (and (= (ps:@ e key) "Escape") (ps:@ *surgery* active-component))
         (ps:chain *surgery* (close-xray (ps:@ *surgery* active-component))))
       ;; Ctrl+Z = Undo (when x-ray active)
       (when (and (ps:@ e ctrl-key) (= (ps:@ e key) "z") (ps:@ *surgery* active-component))
         (ps:chain e (prevent-default))
         (ps:chain *surgery* (undo (ps:@ *surgery* active-component))))
       ;; Ctrl+Y = Redo (when x-ray active)
       (when (and (ps:@ e ctrl-key) (= (ps:@ e key) "y") (ps:@ *surgery* active-component))
         (ps:chain e (prevent-default))
         (ps:chain *surgery* (redo (ps:@ *surgery* active-component)))))))

    (ps:chain console (log "(SURGERY :status :loaded)"))))

;;; ============================================================================
;;; SURGERY CSS (Generated, not raw strings)
;;;
;;; Uses CSS generation utilities and token system for colors.
;;; ============================================================================

(defun surgery-css ()
  "CSS for surgery UI. Uses CSS variables for all colors (no hardcoded values)."
  (concatenate 'string
    ;; Surgery panel slide animation
    (css-rule ".surgery-panel"
      `(("transition" . ,(get-effect :transition-slow))))

    ;; X-ray wrapper hover effect
    (css-rule ".xray-wrapper"
      `(("transition" . ,(get-effect :transition-base))))

    (css-rule ".xray-wrapper:hover .xray-toggle"
      '(("opacity" . "1 !important")))

    ;; X-ray active state
    (css-rule ".xray-wrapper.xray-active"
      '(("position" . "relative")
        ("z-index" . "40")))

    (css-rule ".xray-wrapper.xray-active::before"
      '(("content" . "''")
        ("position" . "absolute")
        ("inset" . "-2px")
        ("border" . "2px dashed var(--color-primary)")
        ("pointer-events" . "none")
        ("animation" . "xray-scan 2s linear infinite")))

    ;; Keyframes for xray-scan animation (uses CSS variables)
    (css-keyframes "xray-scan"
      '("0%" . (("border-color" . "var(--color-primary)")))
      '("50%" . (("border-color" . "var(--color-secondary)")))
      '("100%" . (("border-color" . "var(--color-primary)"))))

    ;; State value hover for editing - use CSS variable
    (css-rule ".surgery-panel [data-state-key]:hover"
      '(("background-color" . "var(--color-surface-alt)")
        ("cursor" . "pointer")))

    ;; REPL styling - use CSS variable
    (css-rule ".surgery-panel input::placeholder"
      '(("color" . "var(--color-muted)")
        ("font-style" . "italic")))

    ;; Snapshot list item hover
    (css-rule ".surgery-panel .space-y-2 > div:hover"
      '(("border-color" . "var(--color-primary)")))

    ;; X-ray toggle button
    (css-rule ".xray-toggle"
      '(("z-index" . "100")))))
