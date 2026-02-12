;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Core component system using Let Over Lambda patterns
;;;; Components are pandoric closures with reactive state

(in-package :lol-reactive)

;;; ============================================================================
;;; COMPONENT REGISTRY
;;; ============================================================================

(defparameter *components* (make-hash-table :test 'equal)
  "Registry of all component instances by ID.")

(defun register-component (id component)
  "Register a component in the global registry."
  (setf (gethash id *components*) component))

(defun find-component (id)
  "Find a component by ID."
  (gethash id *components*))

(defun unregister-component (id)
  "Remove a component from the registry."
  (remhash id *components*))

;;; ============================================================================
;;; COMPONENT PROTOCOL
;;;
;;; Components respond to these messages:
;;;   :render () -> HTML string
;;;   :state (key) -> value
;;;   :set-state (key value) -> value
;;;   :dispatch (action &rest args) -> result
;;;   :subscribe (callback) -> unsubscribe-fn
;;;   :id () -> component ID
;;; ============================================================================

;; Helper functions needed at macro-expansion time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-handler (key body)
    "Find a handler form like (:render () ...) in body."
    (find-if (lambda (form)
               (and (listp form)
                    (eq (car form) key)))
             body))

  (defun generate-component-id (component-name)
    "Generate a unique component ID."
    (format nil "~a-~a" component-name (random 1000000))))

(defmacro! defcomponent (name (&rest state-vars) &body body)
  "Define a reactive component using pandoric closures.

   STATE-VARS are (name initial-value) pairs that become pandoric-accessible.
   BODY should contain:
     (:render () ...) - Returns HTML via cl-who
     (:dispatch (action &rest args) ...) - Handle actions
     (:on-mount () ...) - Called when component mounts (optional)
     (:on-unmount () ...) - Called when component unmounts (optional)

   Example:
   (defcomponent counter ((count 0))
     (:render ()
       (with-html-output-to-string (s)
         (:div :class (brutal-card-classes)
           (:span (str count)))))
     (:dispatch (action &rest args)
       (case action
         (:increment (incf count))
         (:decrement (decf count)))))"
  (let* ((render-form (find-handler :render body))
         (dispatch-form (find-handler :dispatch body))
         (mount-form (find-handler :on-mount body))
         (unmount-form (find-handler :on-unmount body))
         (state-names (mapcar #'car state-vars)))
    `(defun ,name (&key (id (generate-component-id ',name)) ,@state-vars)
       (let ((,g!subscribers '())
             (,g!mounted nil))
         (pandoriclet ((id id)
                       ;; Bind pandoric vars to the function params (not defaults)
                       ,@(mapcar (lambda (sv) (list (car sv) (car sv))) state-vars)
                       (subscribers ,g!subscribers)
                       (mounted ,g!mounted))
           (let ((,g!self nil))
             (setf ,g!self
                   (dlambda
                     ;; Core protocol
                     (:id () id)

                     (:render ()
                      ,(if render-form
                           `(progn ,@(cddr render-form))
                           `(error "No :render handler defined")))

                     (:state (key)
                      (case key
                        ,@(mapcar (lambda (name)
                                    `((,name) ,name))
                                  state-names)
                        (t (error "Unknown state key: ~a" key))))

                     (:set-state (key value)
                      (case key
                        ,@(mapcar (lambda (name)
                                    `((,name)
                                      (setf ,name value)
                                      (notify-subscribers ,g!self subscribers)
                                      value))
                                  state-names)
                        (t (error "Unknown state key: ~a" key))))

                     ;; Use the user's exact parameter names from their dispatch form
                     ,(if dispatch-form
                          (let ((user-params (cadr dispatch-form))  ; (action &rest args)
                                (user-body (cddr dispatch-form)))   ; ((case action ...))
                            `(:dispatch ,user-params
                              (let ((result (progn ,@user-body)))
                                (notify-subscribers ,g!self subscribers)
                                result)))
                          `(:dispatch (action &rest args)
                            (error "No :dispatch handler defined")))

                     (:subscribe (callback)
                      (push callback subscribers)
                      ;; Return unsubscribe function (a closure!)
                      (lambda ()
                        (setf subscribers (remove callback subscribers))))

                     (:mount ()
                      (unless mounted
                        (setf mounted t)
                        (register-component id ,g!self)
                        ,(when mount-form
                           `(progn ,@(cddr mount-form)))))

                     (:unmount ()
                      (when mounted
                        (setf mounted nil)
                        (unregister-component id)
                        ,(when unmount-form
                           `(progn ,@(cddr unmount-form)))))

                     (:mounted-p () mounted)

                     ;; Debug/introspection
                     (:inspect ()
                      (list :id id
                            :state (list ,@(mapcar (lambda (name)
                                                     `(cons ',name ,name))
                                                   state-names))
                            :subscribers (length subscribers)
                            :mounted mounted))

                     (t (&rest args)
                      (error "Unknown component message: ~a" args))))
             ;; Auto-mount and return the component
             (funcall ,g!self :mount)
             ,g!self))))))

(defun notify-subscribers (component subscribers)
  "Notify all subscribers of state change."
  (dolist (callback subscribers)
    (funcall callback component)))

;;; ============================================================================
;;; COMPONENT STATE ACCESS MACRO
;;; ============================================================================

(defmacro with-component-state ((&rest state-keys) component &body body)
  "Access component state with lexical bindings.
   Uses Let Over Lambda's with-pandoric under the hood."
  `(with-pandoric ,state-keys ,component
     ,@body))

;;; ============================================================================
;;; HIGHER-ORDER COMPONENTS (Let Over Lambda Style)
;;; ============================================================================

(defmacro! defhoc (name (wrapped-component &rest extra-state) &body body)
  "Define a Higher-Order Component - a component that wraps another.
   Uses pandoric access to reach into the wrapped component's state.

   Example:
   (defhoc with-loading (component (loading nil))
     (:render ()
       (if loading
           (render-loading-spinner)
           (funcall component :render)))
     (:dispatch (action &rest args)
       (case action
         (:set-loading (setf loading (car args)))
         (t (apply component :dispatch action args)))))"
  `(defun ,name (,wrapped-component &key ,@extra-state)
     (let ((,g!inner ,wrapped-component))
       (pandoriclet ((inner ,g!inner) ,@extra-state)
         (dlambda
           ,@body
           ;; Default: delegate to inner component
           (t (&rest args)
            (apply inner args)))))))

;;; ============================================================================
;;; REACTIVE STATE CONTAINER (Inspired by React's useState)
;;; ============================================================================

(defun make-reactive-state (initial-value)
  "Create a reactive state container using pandoric closures.
   Returns (getter setter subscriber) functions.

   This is the 'Let Over Lambda' pattern in its purest form:
   A closure (getter) over a let binding, with pandoric access (setter)."
  (let ((value initial-value)
        (subscribers '()))
    (values
     ;; Getter
     (lambda () value)
     ;; Setter
     (lambda (new-value)
       (setf value new-value)
       (dolist (sub subscribers)
         (funcall sub new-value))
       value)
     ;; Subscribe
     (lambda (callback)
       (push callback subscribers)
       (lambda ()
         (setf subscribers (remove callback subscribers)))))))

;;; ============================================================================
;;; ANAPHORIC COMPONENT HELPERS
;;; ============================================================================

(defmacro arender (component &body transform)
  "Anaphoric render - binds IT to the render result for transformation."
  `(let ((it (funcall ,component :render)))
     ,@transform))

(defmacro awith-state (component state-key &body body)
  "Anaphoric state access - binds IT to the state value."
  `(let ((it (funcall ,component :state ,state-key)))
     ,@body))
