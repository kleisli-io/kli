;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Parenscript utilities for reactive client-side code generation

(in-package :lol-reactive)

;;; ============================================================================
;;; PARENSCRIPT HELPERS
;;;
;;; Generate JavaScript from Lisp using Parenscript, maintaining the
;;; "Let Over Lambda" philosophy on the client side.
;;; ============================================================================

(defmacro ps (&body body)
  "Shorthand for parenscript:ps."
  `(parenscript:ps ,@body))

(defmacro ps* (&body body)
  "Shorthand for parenscript:ps*."
  `(parenscript:ps* ,@body))

;;; ============================================================================
;;; REACTIVE SCRIPT GENERATION
;;; ============================================================================

(defmacro reactive-script (&body body)
  "Generate a script tag with Parenscript code.
   Wraps output in a script tag for embedding in cl-who."
  `(cl-who:with-html-output-to-string (s)
     (:script
      (cl-who:str (ps ,@body)))))

(defmacro inline-handler ((&rest args) &body body)
  "Generate an inline JavaScript handler string.
   Useful for onclick, onchange, etc."
  `(ps (lambda ,args ,@body)))

;;; ============================================================================
;;; COMPONENT EVENT HANDLERS
;;; ============================================================================

(defun on-click (component-id action &rest args)
  "Generate an onclick handler that dispatches to a component.
   Uses Parenscript for type-safe JavaScript generation."
  (parenscript:ps* `(funcall dispatch ,component-id ,action ,@args)))

(defun on-change (component-id state-key)
  "Generate an onchange handler that updates component state.
   Uses Parenscript for type-safe JavaScript generation."
  (parenscript:ps* `(funcall set-state ,component-id ,state-key
                             (ps:@ this value))))

(defun on-submit (component-id action)
  "Generate an onsubmit handler.
   Uses Parenscript for type-safe JavaScript generation."
  (concatenate 'string
    (parenscript:ps* `((ps:@ event prevent-default)))
    " "
    (parenscript:ps* `(funcall dispatch ,component-id ,action
                               (ps:new (-Form-Data this))))))

(defun js-value (val)
  "Convert a Lisp value to JavaScript literal."
  (typecase val
    (null "null")  ; Must come before symbol since NIL is both
    (string (format nil "'~a'" val))
    (number (format nil "~a" val))
    (symbol (format nil "'~a'" (string-downcase val)))
    (t (format nil "~a" val))))

;;; ============================================================================
;;; CLIENT-SIDE REACTIVE STATE (Parenscript)
;;;
;;; These generate JavaScript closures that mirror the Let Over Lambda
;;; patterns on the client side.
;;; ============================================================================

(parenscript:defpsmacro make-state (initial-value)
  "Create a reactive state container in JavaScript.
   Returns [getter, setter] like React's useState but with closures."
  `(let ((value ,initial-value)
         (subscribers (array)))
     (array
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
        ((ps:@ subscribers push) callback)
        ;; Return unsubscribe
        (lambda ()
          (setf subscribers
                ((ps:@ subscribers filter)
                 (lambda (s) (not (= s callback))))))))))

(parenscript:defpsmacro with-state ((getter setter &optional subscriber) init &body body)
  "Destructure state container and execute body."
  `(let* ((state-tuple (make-state ,init))
          (,getter (aref state-tuple 0))
          (,setter (aref state-tuple 1))
          ,@(when subscriber
              `((,subscriber (aref state-tuple 2)))))
     ,@body))

;;; ============================================================================
;;; REACTIVE DOM UPDATES
;;; ============================================================================

(parenscript:defpsmacro bind-element (id state-getter &key (attr "innerText"))
  "Bind a DOM element to reactive state."
  `(let ((el ((ps:@ document get-element-by-id) ,id)))
     (when el
       (setf (ps:@ el ,attr) (funcall ,state-getter)))))

(parenscript:defpsmacro reactive-render (component-id render-fn)
  "Set up reactive rendering for a component."
  `(let ((container ((ps:@ document query-selector)
                     (+ "[data-component-id='" ,component-id "']"))))
     (when container
       (setf (ps:@ container inner-h-t-m-l) (funcall ,render-fn)))))

;;; ============================================================================
;;; WEBSOCKET REACTIVE BRIDGE
;;; ============================================================================

(defun generate-ws-client (component-id)
  "Generate WebSocket client code for real-time updates."
  (ps*
    `(let ((ws (ps:new (-Web-Socket
                        (+ "ws://" (ps:@ window location host) "/ws/" ,component-id)))))
       (setf (ps:@ ws onmessage)
             (lambda (event)
               (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                 (when (ps:@ data html)
                   (let ((el (ps:chain document (query-selector
                              (+ "[data-component-id='" (ps:@ data component-id) "']")))))
                     (when el
                       (setf (ps:@ el inner-h-t-m-l) (ps:@ data html))))))))
       (setf (ps:@ ws onopen)
             (lambda ()
               (ps:chain console (log "(WS :connected)"))))
       (setf (ps:@ ws onclose)
             (lambda ()
               (ps:chain console (log "(WS :disconnected)"))
               (ps:chain window (set-timeout
                (lambda ()
                  ;; Reconnect logic would go here
                  nil)
                1000))))
       ws)))

;;; ============================================================================
;;; ANAPHORIC JS HELPERS
;;; ============================================================================

(parenscript:defpsmacro aif-js (test then &optional else)
  "Anaphoric if for JavaScript - binds 'it' to test result."
  `(let ((it ,test))
     (if it ,then ,else)))

(parenscript:defpsmacro awhen-js (test &body body)
  "Anaphoric when for JavaScript."
  `(aif-js ,test (progn ,@body)))

;;; ============================================================================
;;; COMPONENT CLIENT BEHAVIOR
;;; ============================================================================

(defun component-client-script (component-id &key
                                               (on-mount nil)
                                               (on-unmount nil)
                                               (state-bindings nil))
  "Generate client-side script for a component.
   Uses ps* to interpolate runtime values into Parenscript."
  (parenscript:ps*
    `(let ((component-id ,component-id))
       ;; Register with runtime
       (ps:chain -lol-reactive (register
        component-id
        (ps:create
         :on-mount (lambda () ,@(or on-mount '(nil)))
         :on-unmount (lambda () ,@(or on-unmount '(nil))))))

       ;; Set up state bindings
       ,@(when state-bindings
           (mapcar (lambda (binding)
                     `(bind-element ,(car binding) ,(cadr binding)))
                   state-bindings))

       ;; Call mount
       (ps:chain (ps:chain -lol-reactive components (get component-id)) (ps:@ :on-mount)))))

;;; ============================================================================
;;; HTMX-STYLE ATTRIBUTES (Alternative to full Parenscript)
;;; ============================================================================

(defun hx-dispatch (component-id action &rest args)
  "Generate data attributes for HTMX-style behavior."
  (format nil "data-dispatch=\"~a\" data-action=\"~a\"~{~a~}"
          component-id
          action
          (iter (for (k v) on args by #'cddr)
            (collect (format nil " data-arg-~a=\"~a\"" k v)))))

(defun hx-bind (component-id state-key)
  "Generate data attributes for two-way binding."
  (format nil "data-bind=\"~a\" data-state=\"~a\""
          component-id state-key))
