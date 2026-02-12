;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; LOL-REACTIVE Signals - Fine-grained reactive primitives
;;;;
;;;; This module adds Solid.js/Vue 3-style reactivity to LOL-REACTIVE:
;;;; - Signals: Auto-tracking reactive values
;;;; - Effects: Side effects with automatic dependency tracking
;;;; - Computed: Derived values that cache and auto-update
;;;; - Stores: Redux-like centralized state
;;;; - Evolving components: Hot-swappable behavior at runtime
;;;;
;;;; All primitives use Let Over Lambda patterns (pandoric closures, dlambda)

(in-package :lol-reactive)

;;; ============================================================================
;;; DEPENDENCY TRACKING INFRASTRUCTURE
;;; ============================================================================

(defparameter *current-effect* nil
  "The currently executing effect (for automatic dependency tracking).
   When an effect function runs, accessing any signal automatically
   subscribes that effect to future changes.")

(defparameter *batch-depth* 0
  "When > 0, defer effect execution until batch completes.
   This allows multiple state changes without intermediate re-renders.")

(defparameter *pending-effects* '()
  "Effects waiting to run after batch completes.")

(defparameter *max-signal-history* 100
  "Default maximum history entries for pandoric signals. NIL = unlimited.")

;;; ============================================================================
;;; SIGNAL - Basic Reactive Value with Auto-Tracking
;;; ============================================================================

(defun make-signal (initial-value)
  "Create a reactive signal with automatic dependency tracking.

   Returns two values: (GETTER SETTER)

   The GETTER returns the current value. When called during an effect's
   execution, it automatically subscribes that effect to future changes.

   The SETTER updates the value and triggers all subscribed effects.

   Example:
     (multiple-value-bind (count set-count) (make-signal 0)
       (make-effect
         (lambda ()
           (format t \"Count: ~a~%\" (funcall count))
           nil))  ; return nil = no cleanup
       (funcall set-count 1)  ; prints \"Count: 1\"
       (funcall set-count 2)) ; prints \"Count: 2\""
  (let ((value initial-value)
        (subscribers (make-hash-table :test 'eq)))
    (values
     ;; Getter - tracks dependency when called during effect
     (lambda ()
       (when *current-effect*
         (setf (gethash *current-effect* subscribers) t))
       value)
     ;; Setter - triggers all subscribed effects
     (lambda (new-value)
       (unless (equal value new-value)
         (setf value new-value)
         (if (> *batch-depth* 0)
             ;; Defer effects during batch
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (pushnew effect *pending-effects*))
                      subscribers)
             ;; Run immediately
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (when effect (funcall effect)))
                      subscribers)))
       value))))

;;; ============================================================================
;;; EFFECT - Side Effect with Auto-Tracking
;;; ============================================================================

(defun make-effect (fn)
  "Create a reactive effect that auto-tracks signal dependencies.

   FN is called immediately and whenever any signal it accesses changes.
   FN can return a cleanup function that runs before re-execution or disposal.

   Returns a DISPOSE function to stop the effect.

   Example:
     (multiple-value-bind (name set-name) (make-signal \"Alice\")
       (let ((dispose (make-effect
                        (lambda ()
                          (format t \"Hello, ~a!~%\" (funcall name))
                          ;; Return cleanup function (optional)
                          (lambda () (format t \"Cleaning up~%\"))))))
         (funcall set-name \"Bob\")   ; prints cleanup, then \"Hello, Bob!\"
         (funcall dispose)))          ; prints cleanup, stops tracking"
  (let ((effect-fn nil)
        (cleanup-fn nil))
    (setf effect-fn
          (lambda ()
            ;; Run cleanup from previous execution
            (when (functionp cleanup-fn)
              (funcall cleanup-fn)
              (setf cleanup-fn nil))
            ;; Track this effect during execution
            (let ((*current-effect* effect-fn))
              (setf cleanup-fn (funcall fn)))))
    ;; Run immediately to establish initial dependencies
    (funcall effect-fn)
    ;; Return disposer
    (lambda ()
      (when (functionp cleanup-fn)
        (funcall cleanup-fn))
      ;; Prevent further runs by nullifying
      (setf effect-fn nil))))

;;; ============================================================================
;;; BATCH - Group Multiple Updates
;;; ============================================================================

(defmacro batch (&body body)
  "Batch multiple signal updates - effects run once at end.

   Example:
     (batch
       (funcall set-x 1)
       (funcall set-y 2)
       (funcall set-z 3))
     ; Effects depending on x, y, z run once, not three times"
  `(progn
     (incf *batch-depth*)
     (unwind-protect
          (progn ,@body)
       (decf *batch-depth*)
       (when (zerop *batch-depth*)
         (let ((effects *pending-effects*))
           (setf *pending-effects* '())
           (dolist (effect effects)
             (when effect (funcall effect))))))))

;;; ============================================================================
;;; COMPUTED - Derived Reactive Value
;;; ============================================================================

(defun make-computed (compute-fn &key (test #'equal))
  "Create a computed value that automatically tracks dependencies.

   The value is derived by calling COMPUTE-FN, which can access signals.
   The result is cached and only recomputed when dependencies change.
   Downstream effects are only notified if the computed value actually changed
   (compared using TEST, default EQUAL).

   TEST: Equality function for comparing values. Common choices:
     - EQUAL (default): Structural equality for strings, lists, numbers
     - EQUALP: Case-insensitive strings, hash-tables, arrays
     - EQ: Reference equality (like JavaScript ===)

   Returns a GETTER function.

   Example:
     (multiple-value-bind (first-name set-first) (make-signal \"John\")
       (multiple-value-bind (last-name set-last) (make-signal \"Doe\")
         (let ((full-name (make-computed
                            (lambda ()
                              (format nil \"~a ~a\"
                                      (funcall first-name)
                                      (funcall last-name))))))
           (funcall full-name)        ; => \"John Doe\"
           (funcall set-first \"Jane\")
           (funcall full-name))))     ; => \"Jane Doe\"

   Example with custom test (for hash-table results):
     (make-computed (lambda () (build-state-hash-table)) :test #'equalp)"
  (let ((value nil)
        (subscribers (make-hash-table :test 'eq)))
    ;; Create an effect that recomputes when deps change
    (make-effect
     (lambda ()
       (let ((new-value (funcall compute-fn)))
         ;; Only notify subscribers if value actually changed
         (unless (funcall test value new-value)
           (setf value new-value)
           (maphash (lambda (effect v)
                      (declare (ignore v))
                      (when effect (funcall effect)))
                    subscribers)))
       nil))
    ;; Return getter
    (lambda ()
      (when *current-effect*
        (setf (gethash *current-effect* subscribers) t))
      value)))

;;; ============================================================================
;;; PANDORIC SIGNAL - Introspectable Reactive Value
;;; ============================================================================

(defun make-pandoric-signal (name initial-value &key (max-history *max-signal-history*))
  "Create a signal with full introspection capabilities via dlambda.

   This is the 'Let Over Lambda' version of signals - the closure's
   internal state can be inspected and modified from outside.

   MAX-HISTORY: Maximum history entries to keep (default *max-signal-history*).
               NIL = unlimited history.

   Messages:
     :get () - Get value (tracks dependency)
     :set (value) - Set value (triggers effects, respects batch)
     :peek () - Get value without tracking
     :history () - Get list of previous values with timestamps
     :undo () - Restore previous value
     :inspect () - Get full introspection data
     :subscriber-count () - Number of subscribed effects

   Example:
     (let ((score (make-pandoric-signal :player-score 0)))
       (funcall score :set 100)
       (funcall score :set 200)
       (funcall score :history)  ; => ((time . 0) (time . 100))
       (funcall score :undo)     ; => 100
       (funcall score :inspect)) ; => (:name :player-score :value 100 ...)

   Example with custom history limit:
     (make-pandoric-signal :mouse-pos '(0 . 0) :max-history 10)
     (make-pandoric-signal :debug-state nil :max-history nil)  ; unlimited"
  (let ((value initial-value)
        (subscribers (make-hash-table :test 'eq))
        (history '())
        (history-limit max-history)
        (created-at (get-universal-time)))
    (dlambda
      ;; Get value (reactive - tracks dependency)
      (:get ()
       (when *current-effect*
         (setf (gethash *current-effect* subscribers) t))
       value)

      ;; Set value (triggers effects, respects batch)
      (:set (new-value)
       (unless (equal value new-value)
         (push (cons (get-universal-time) value) history)
         ;; Enforce history limit
         (when (and history-limit (> (length history) history-limit))
           (setf (cdr (nthcdr (1- history-limit) history)) nil))
         (setf value new-value)
         ;; Respect batch depth like regular signals
         (if (> *batch-depth* 0)
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (pushnew effect *pending-effects*))
                      subscribers)
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (when effect (funcall effect)))
                      subscribers)))
       value)

      ;; Peek without tracking
      (:peek () value)

      ;; Get history of previous values
      (:history () (reverse history))

      ;; Undo to previous value (also respects batch)
      (:undo ()
       (when history
         (let ((prev (pop history)))
           (setf value (cdr prev))
           (if (> *batch-depth* 0)
               (maphash (lambda (e v)
                          (declare (ignore v))
                          (pushnew e *pending-effects*))
                        subscribers)
               (maphash (lambda (e v)
                          (declare (ignore v))
                          (when e (funcall e)))
                        subscribers)))
         value))

      ;; Full introspection
      (:inspect ()
       (list :name name
             :value value
             :subscribers (hash-table-count subscribers)
             :history-length (length history)
             :history-limit history-limit
             :age (- (get-universal-time) created-at)))

      ;; Subscriber count
      (:subscriber-count ()
       (hash-table-count subscribers))

      ;; Reset to new value, clearing history (also respects batch)
      (:reset (new-value)
       (setf history '()
             value new-value)
       (if (> *batch-depth* 0)
           (maphash (lambda (e v)
                      (declare (ignore v))
                      (pushnew e *pending-effects*))
                    subscribers)
           (maphash (lambda (e v)
                      (declare (ignore v))
                      (when e (funcall e)))
                    subscribers))
       value))))

;;; ============================================================================
;;; STORE - Redux-like Centralized State
;;; ============================================================================

(defun make-store (initial-state)
  "Create a Redux-like store with multiple pandoric signals.

   INITIAL-STATE is an alist of (KEY . VALUE) pairs.
   Each key becomes a pandoric signal with full introspection.

   Messages:
     :get (key) - Get value (reactive)
     :set (key value) - Set value
     :reducer (action-type fn) - Register action handler
     :dispatch (action-type &rest args) - Dispatch action to reducer
     :state () - Get all state as alist
     :inspect () - Full introspection of all signals

   Example:
     (let ((store (make-store '((score . 0) (lives . 3)))))
       ;; Register reducer
       (funcall store :reducer :gain-points
         (lambda (action points)
           (declare (ignore action))
           `((score . ,(+ (funcall store :get 'score) points)))))
       ;; Dispatch action
       (funcall store :dispatch :gain-points 100)
       (funcall store :get 'score))  ; => 100"
  (let ((signals (make-hash-table :test 'eq))
        (reducers (make-hash-table :test 'eq))
        (middleware '()))
    ;; Initialize signals for each state key
    (dolist (pair initial-state)
      (setf (gethash (car pair) signals)
            (make-pandoric-signal (car pair) (cdr pair))))
    (dlambda
      ;; Get a value (reactive)
      (:get (key)
       (let ((sig (gethash key signals)))
         (when sig (funcall sig :get))))

      ;; Set a value
      (:set (key value)
       (let ((sig (gethash key signals)))
         (if sig
             (funcall sig :set value)
             ;; Create new signal if doesn't exist
             (setf (gethash key signals)
                   (make-pandoric-signal key value)))))

      ;; Register a reducer
      (:reducer (action-type fn)
       (setf (gethash action-type reducers) fn))

      ;; Dispatch an action (Redux-style)
      (:dispatch (action-type &rest payload)
       (let ((reducer (gethash action-type reducers)))
         (when reducer
           (let ((new-state (apply reducer action-type payload)))
             ;; Update signals from new state
             (dolist (pair new-state)
               (let ((sig (gethash (car pair) signals)))
                 (when sig
                   (funcall sig :set (cdr pair)))))))))

      ;; Add middleware
      (:use (middleware-fn)
       (push middleware-fn middleware))

      ;; Get all state as alist
      (:state ()
       (let ((state '()))
         (maphash (lambda (key sig)
                    (push (cons key (funcall sig :peek)) state))
                  signals)
         state))

      ;; Full introspection
      (:inspect ()
       (let ((info '()))
         (maphash (lambda (key sig)
                    (push (cons key (funcall sig :inspect)) info))
                  signals)
         info)))))

;;; ============================================================================
;;; EVOLVING COMPONENT - Self-Modifying Behavior
;;; ============================================================================

(defun make-evolving-component (name initial-state behavior)
  "Create a component that can hot-swap its own behavior at runtime.

   The component has:
   - Reactive state (pandoric signals)
   - Dispatch function (can be replaced via :evolve)
   - Full introspection
   - Behavior history with rollback

   Messages:
     :get (key) - Get state value
     :set (key value) - Set state value
     :dispatch (action &rest args) - Dispatch to current behavior
     :render () - Render using current behavior
     :evolve (new-behavior &optional description) - Hot-swap behavior
     :devolve () - Roll back to previous behavior
     :state () - Get all state as alist
     :inspect () - Full introspection

   Example:
     (let ((widget (make-evolving-component :my-widget
                     '((count . 0))
                     (lambda (action &rest args)
                       (case action
                         (:render \"<div>v1</div>\")
                         (:inc ...))))))
       (funcall widget :evolve
         (lambda (action &rest args)
           (case action
             (:render \"<div class='v2'>new!</div>\")
             ...))
         \"v2: new design\")
       (funcall widget :devolve)) ; roll back to v1"
  (let ((state-signals (make-hash-table))
        (current-behavior behavior)
        (behavior-history '())
        (render-count 0)
        (created (get-universal-time)))
    ;; Initialize state signals
    (dolist (pair initial-state)
      (setf (gethash (car pair) state-signals)
            (make-pandoric-signal (car pair) (cdr pair))))
    (dlambda
      ;; Get state value
      (:get (key)
       (let ((sig (gethash key state-signals)))
         (when sig (funcall sig :get))))

      ;; Set state value
      (:set (key value)
       (let ((sig (gethash key state-signals)))
         (if sig
             (funcall sig :set value)
             (setf (gethash key state-signals)
                   (make-pandoric-signal key value)))))

      ;; Dispatch through current behavior
      (:dispatch (action &rest args)
       (apply current-behavior action args))

      ;; Hot-swap behavior!
      (:evolve (new-behavior &optional description)
       (push (list (get-universal-time)
                   description
                   current-behavior)
             behavior-history)
       (setf current-behavior new-behavior)
       (format nil "Evolved to: ~a" (or description "new behavior")))

      ;; Roll back behavior
      (:devolve ()
       (when behavior-history
         (let ((prev (pop behavior-history)))
           (setf current-behavior (third prev))
           (format nil "Rolled back (was: ~a)" (second prev)))))

      ;; Render using current behavior
      (:render ()
       (incf render-count)
       (funcall current-behavior :render))

      ;; Get all state as alist
      (:state ()
       (let ((result '()))
         (maphash (lambda (k sig)
                    (push (cons k (funcall sig :peek)) result))
                  state-signals)
         result))

      ;; Full introspection
      (:inspect ()
       (let ((state-info '()))
         (maphash (lambda (k sig)
                    (push (cons k (funcall sig :inspect)) state-info))
                  state-signals)
         (list :name name
               :state state-info
               :render-count render-count
               :behavior-versions (1+ (length behavior-history))
               :age (- (get-universal-time) created)))))))

;;; ============================================================================
;;; COMPONENT FACTORY - Dynamic Component Creation
;;; ============================================================================

(defparameter *factory-registry* (make-hash-table :test 'equal)
  "Global registry for factory-spawned components.")

(defun make-component-factory ()
  "Create a meta-component that creates other components at runtime.

   The factory maintains:
   - Template registry (named component definitions)
   - Instance registry (spawned components)
   - Creation statistics

   Messages:
     :define-template (name initial-state behavior-generator) - Register template
     :spawn (template-name instance-id &optional state-overrides) - Create instance
     :instance (id) - Get instance by ID
     :destroy (id) - Remove instance
     :instances () - List all instance IDs
     :broadcast (action &rest args) - Send action to all instances
     :stats () - Factory statistics

   Example:
     (let ((factory (make-component-factory)))
       ;; Define template
       (funcall factory :define-template :counter
         '((count . 0))
         (lambda (id)
           (lambda (action &rest args) ...)))
       ;; Spawn instances
       (funcall factory :spawn :counter :counter-1)
       (funcall factory :spawn :counter :counter-2 '((count . 100)))
       ;; Broadcast to all
       (funcall factory :broadcast :increment))"
  (let ((templates (make-hash-table :test 'equal))
        (instances (make-hash-table :test 'equal))
        (creation-count 0))
    (dlambda
      ;; Define a component template
      (:define-template (name initial-state behavior-generator)
       (setf (gethash name templates)
             (list :initial-state initial-state
                   :behavior-generator behavior-generator))
       (format nil "Template '~a' registered" name))

      ;; Spawn an instance from a template
      (:spawn (template-name instance-id &optional state-overrides)
       (let ((template (gethash template-name templates)))
         (when template
           (incf creation-count)
           (let* ((base-state (getf template :initial-state))
                  (merged-state (append state-overrides base-state))
                  (instance (make-evolving-component
                             instance-id
                             merged-state
                             (funcall (getf template :behavior-generator)
                                      instance-id))))
             (setf (gethash instance-id instances) instance
                   (gethash instance-id *factory-registry*) instance)
             instance-id))))

      ;; Get an instance
      (:instance (id)
       (gethash id instances))

      ;; Destroy an instance
      (:destroy (id)
       (remhash id instances)
       (remhash id *factory-registry*))

      ;; List all instances
      (:instances ()
       (let ((ids '()))
         (maphash (lambda (k v)
                    (declare (ignore v))
                    (push k ids))
                  instances)
         ids))

      ;; Broadcast action to all instances
      (:broadcast (action &rest args)
       (let ((results '()))
         (maphash (lambda (id instance)
                    (push (cons id (apply instance :dispatch action args))
                          results))
                  instances)
         results))

      ;; Factory statistics
      (:stats ()
       (list :templates (hash-table-count templates)
             :instances (hash-table-count instances)
             :total-created creation-count)))))

;;; ============================================================================
;;; REACTIVE LIST - Fine-Grained Collection Updates
;;; ============================================================================

(defun make-reactive-list (initial-items)
  "Create a reactive list with fine-grained tracking.

   Returns a dlambda with messages:
     :items () - Get all items (reactive)
     :push (item) - Add to end
     :pop () - Remove from end
     :nth (index) - Get item at index (reactive)
     :set-nth (index value) - Set item at index
     :length () - Get length (reactive via signal)
     :map (fn) - Map function over items (reactive)
     :filter (pred) - Filter items (reactive)

   Example:
     (let ((todos (make-reactive-list '(\"Buy milk\" \"Learn Lisp\"))))
       (funcall todos :push \"Write code\")
       (funcall todos :items))  ; => (\"Buy milk\" \"Learn Lisp\" \"Write code\")"
  (let ((items (coerce initial-items 'vector))
        (subscribers (make-hash-table :test 'eq)))
    (flet ((notify ()
             (maphash (lambda (e v)
                        (declare (ignore v))
                        (when e (funcall e)))
                      subscribers)))
      (dlambda
        ;; Get all items (reactive)
        (:items ()
         (when *current-effect*
           (setf (gethash *current-effect* subscribers) t))
         (coerce items 'list))

        ;; Push item to end
        (:push (item)
         (setf items (concatenate 'vector items (vector item)))
         (notify)
         item)

        ;; Pop from end
        (:pop ()
         (when (> (length items) 0)
           (let ((item (aref items (1- (length items)))))
             (setf items (subseq items 0 (1- (length items))))
             (notify)
             item)))

        ;; Get item at index (reactive)
        (:nth (idx)
         (when *current-effect*
           (setf (gethash *current-effect* subscribers) t))
         (when (< idx (length items))
           (aref items idx)))

        ;; Set item at index
        (:set-nth (idx value)
         (when (< idx (length items))
           (setf (aref items idx) value)
           (notify)
           value))

        ;; Length
        (:length ()
         (when *current-effect*
           (setf (gethash *current-effect* subscribers) t))
         (length items))

        ;; Map over items (reactive)
        (:map (fn)
         (when *current-effect*
           (setf (gethash *current-effect* subscribers) t))
         (map 'list fn items))

        ;; Filter items (reactive)
        (:filter (pred)
         (when *current-effect*
           (setf (gethash *current-effect* subscribers) t))
         (remove-if-not pred (coerce items 'list)))))))
