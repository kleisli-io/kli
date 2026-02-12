;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Context API
;;;; Enables shared state down component trees without prop drilling
;;;; Uses Lisp's dynamic variables for natural scoping

(in-package :lol-reactive)

;;; ============================================================================
;;; CONTEXT REGISTRY
;;; Track all defined contexts for introspection
;;; ============================================================================

(defvar *context-registry* (make-hash-table :test 'eq)
  "Registry of all defined contexts for Surgery introspection.")

(defun register-context (name var-name default documentation)
  "Register a context in the global registry."
  (setf (gethash name *context-registry*)
        (list :var var-name
              :default default
              :documentation documentation)))

(defun list-contexts ()
  "List all registered contexts."
  (let ((contexts nil))
    (maphash (lambda (name info)
               (push (list* name info) contexts))
             *context-registry*)
    (nreverse contexts)))

(defun get-context-info (name)
  "Get info about a registered context."
  (gethash name *context-registry*))

;;; ============================================================================
;;; DEFCONTEXT - Simple Dynamic Variable Context
;;; ============================================================================

(defmacro defcontext (name &key default documentation)
  "Define a context that can be provided and consumed.

   Creates:
   - *NAME* - dynamic variable holding context value
   - WITH-NAME - macro to provide context value to descendants
   - USE-NAME - function to consume context value

   Example:
   (defcontext theme :default :light :documentation \"UI theme context\")

   ;; Providing context
   (with-theme :dark
     (render-app))  ; All descendants see :dark theme

   ;; Consuming context
   (defcomponent button ()
     (let ((theme (use-theme)))
       (htm-str (:button :class (if (eq theme :dark) \"btn-dark\" \"btn-light\")
                 \"Click\"))))"
  (let ((var-name (symb "*" name "*"))
        (with-name (symb "WITH-" name))
        (use-name (symb "USE-" name)))
    `(progn
       ;; Define the dynamic variable
       (defvar ,var-name ,default ,documentation)

       ;; Register in context registry
       (register-context ',name ',var-name ,default ,documentation)

       ;; WITH-NAME macro for providing context
       (defmacro ,with-name (value &body body)
         ,(format nil "Provide ~A context to BODY.~%VALUE becomes the context value for all descendants." name)
         `(let ((,',var-name ,value))
            ,@body))

       ;; USE-NAME function for consuming context
       (defun ,use-name ()
         ,(format nil "Get the current ~A context value." name)
         ,var-name)

       ;; Export the symbols
       (export '(,var-name ,with-name ,use-name) :lol-reactive)

       ',name)))

;;; ============================================================================
;;; DEFCONTEXT-SIGNAL - Reactive Context with Signals
;;; ============================================================================

(defmacro defcontext-signal (name &key default documentation)
  "Define a reactive context using signals.
   Changes propagate to all consumers automatically via the signal system.

   Creates:
   - *NAME-CONTEXT* - dynamic variable holding (getter . setter) cons
   - WITH-NAME - macro to provide reactive context
   - USE-NAME - function to get current value (reads signal)
   - SET-NAME - function to update context (writes signal)

   Example:
   (defcontext-signal user :default nil :documentation \"Current user context\")

   ;; Providing reactive context
   (with-user nil
     (set-user '(:name \"Alice\" :id 1))
     (use-user))  ; => (:name \"Alice\" :id 1)

   ;; Consuming and updating
   (when-let ((user (use-user)))
     (render-user-profile user))

   (set-user new-user-data)  ; Triggers re-render"
  (let ((var-name (symb "*" name "-CONTEXT*"))
        (with-name (symb "WITH-" name))
        (use-name (symb "USE-" name))
        (set-name (symb "SET-" name)))
    `(progn
       ;; Dynamic variable holds (getter . setter) cons or nil
       (defvar ,var-name nil ,documentation)

       ;; Register in context registry
       (register-context ',name ',var-name ,default
                         ,(format nil "Reactive context: ~A" (or documentation "")))

       ;; WITH-NAME creates a new signal context
       (defmacro ,with-name (initial-value &body body)
         ,(format nil "Provide reactive ~A context to BODY.~%Creates a new signal initialized to INITIAL-VALUE." name)
         `(multiple-value-bind (getter setter) (make-signal ,initial-value)
            (let ((,',var-name (cons getter setter)))
              ,@body)))

       ;; USE-NAME reads from the signal (calls getter)
       (defun ,use-name ()
         ,(format nil "Get the current ~A context value (reads signal)." name)
         (when ,var-name
           (funcall (car ,var-name))))

       ;; SET-NAME writes to the signal (calls setter)
       (defun ,set-name (new-value)
         ,(format nil "Set the ~A context value (writes signal, triggers updates)." name)
         (when ,var-name
           (funcall (cdr ,var-name) new-value)))

       ;; Export the symbols
       (export '(,var-name ,with-name ,use-name ,set-name) :lol-reactive)

       ',name)))

;;; ============================================================================
;;; CONTEXT INSPECTION (Surgery Support)
;;; ============================================================================

(defun inspect-context (name)
  "Inspect a context's current state.
   Returns info about the context including current value."
  (let ((info (get-context-info name)))
    (when info
      (let* ((var-name (getf info :var))
             (current-value (when (boundp var-name)
                              (symbol-value var-name))))
        (list :name name
              :var var-name
              :default (getf info :default)
              :documentation (getf info :documentation)
              :current-value current-value
              :bound-p (boundp var-name))))))

(defun inspect-all-contexts ()
  "Inspect all registered contexts.
   Useful for Surgery panel to show context state."
  (mapcar (lambda (ctx)
            (inspect-context (car ctx)))
          (list-contexts)))
