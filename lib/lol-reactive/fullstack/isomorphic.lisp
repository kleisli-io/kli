;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; fullstack/isomorphic.lisp - Isomorphic Components (Server + Client)
;;;;
;;;; PURPOSE:
;;;;   Define components that render on server and hydrate on client.
;;;;   Single source of truth generates both server HTML and client JS.
;;;;
;;;; KEY MACRO:
;;;;   DEFISOMORPHIC-COMPONENT - Component renders server-side, hydrates client-side
;;;;
;;;; GENERATED OUTPUT:
;;;;   - Server render function (HTML string with embedded state)
;;;;   - Client Parenscript component (attaches to existing DOM)
;;;;   - State serialization in data attributes
;;;;   - Hydration script to resume component

(in-package :lol-reactive)

;;; ============================================================================
;;; STATE SERIALIZATION
;;; ============================================================================

(defun serialize-state (state)
  "Serialize component state to JSON string for embedding in HTML."
  (cl-json:encode-json-to-string state))

(defun serialize-state-for-attr (state)
  "Serialize state and escape for HTML attribute."
  (escape-html (serialize-state state)))

(defun deserialize-state (json-string)
  "Deserialize JSON state string back to Lisp."
  (cl-json:decode-json-from-string json-string))

;;; ============================================================================
;;; HYDRATION SCRIPT GENERATION
;;; ============================================================================

(defun generate-hydration-script (component-id component-name state actions)
  "Generate JavaScript to hydrate a server-rendered component."
  (declare (ignore actions))  ; Actions handled by API routes
  (parenscript:ps*
    `(progn
       ;; Wait for DOM ready
       ((ps:@ document add-event-listener) "DOMContentLoaded"
        (lambda ()
          ;; Find the component container
          (let ((container ((ps:@ document query-selector)
                            (+ "[data-component-id='" ,component-id "']"))))
            (when container
              ;; Parse embedded state
              (let ((initial-state ((ps:@ -j-s-o-n parse)
                                    ((ps:@ container get-attribute) "data-state"))))
                ;; Create client-side reactive state
                (let ((state (ps:create ,@(mapcan
                                        (lambda (s)
                                          (let ((key (car s)))
                                            `(,(intern (kebab-to-path key) :keyword)
                                              (ps:@ initial-state
                                                 ,(intern (kebab-to-path key) :keyword)))))
                                        state))))
                  ;; Register component on window
                  (setf (ps:@ window ,(symb (string-upcase component-name) "-" component-id))
                        (ps:create
                         :id ,component-id
                         :state state
                         :container container
                         ;; Dispatch action to server
                         :dispatch (lambda (action)
                                     (ps:chain
                                      (fetch (+ "/api/" ,(kebab-to-path component-name) "/" action)
                                             (ps:create
                                              :method "POST"
                                              :headers (ps:create "Content-Type" "application/json")
                                              :body ((ps:@ -j-s-o-n stringify)
                                                     (ps:create :component-id ,component-id))))
                                      (then (lambda (r) ((ps:@ r json))))
                                      (then (lambda (data)
                                              (when (ps:@ data html)
                                                (setf (ps:@ container inner-h-t-m-l) (ps:@ data html)))))))
                         ;; Update HTML directly
                         :update (lambda (html)
                                   (setf (ps:@ container inner-h-t-m-l) html))))
                  ;; Attach click handler for data-action elements
                  ((ps:@ container add-event-listener) "click"
                   (lambda (e)
                     (let ((action ((ps:@ (ps:@ e target) get-attribute) "data-action")))
                       (when action
                         (let ((comp (ps:@ window ,(symb (string-upcase component-name) "-" component-id))))
                           ((ps:@ comp dispatch) action))))))
                  ((ps:@ console log) "(hydrated:" ,component-id ")"))))))))))

;;; ============================================================================
;;; DEFISOMORPHIC-COMPONENT MACRO
;;; ============================================================================

(defmacro! defisomorphic-component (name (&rest props) &key state actions render)
  "Define an isomorphic component that renders on server and hydrates on client.

   NAME: Component name
   PROPS: Component properties
   STATE: List of (state-name initial-value) pairs
   ACTIONS: List of (action-name (params) &body) for mutations
   RENDER: cl-who render expression

   The component:
   1. Renders HTML on server with embedded state (data-state attribute)
   2. Includes hydration script that attaches event handlers
   3. Client-side actions call API endpoints and update DOM

   Example:
     (defisomorphic-component counter ()
       :state ((count 0))
       :actions ((increment () (incf count))
                 (decrement () (decf count)))
       :render (htm-str
                 (:div :class \"counter\"
                   (:button :data-action \"decrement\" \"-\")
                   (:span (cl-who:str count))
                   (:button :data-action \"increment\" \"+\"))))

   Server output includes:
   - Component HTML
   - data-component-id attribute
   - data-state attribute with serialized state
   - Hydration script tag

   Client behavior:
   - Clicks on [data-action] elements dispatch to server
   - Server returns new HTML
   - Client updates container innerHTML"
  ;; Note: actions appears unused but is used in backquoted template
    `(progn
       ;; Define the server-side component using defcomponent-with-api
       (defcomponent-with-api ,name (,@props)
         :state ,state
         :actions ,actions
         :render ,render)

       ;; Define isomorphic render function
       (defun ,(symb name "-ISOMORPHIC") (&key (id (generate-component-id ',name)) ,@props
                                               ,@(mapcar (lambda (s) (list (car s) (cadr s))) state))
         "Render component with hydration wrapper."
         (let* ((component (,name :id id ,@(mapcan (lambda (p) `(,(intern (symbol-name p) :keyword) ,p)) props)))
                (html (funcall component :render))
                (state-json (serialize-state-for-attr
                             (list ,@(mapcan (lambda (s)
                                               `(,(intern (symbol-name (car s)) :keyword) ,(car s)))
                                             state)))))
           (format nil
                   "<div data-component-id=\"~A\" data-component=\"~A\" data-state=\"~A\">~A</div>~A"
                   id
                   ',name
                   state-json
                   html
                   (format nil "<script>~A</script>"
                           (generate-hydration-script id ',name ',state ',actions)))))

       ;; Export action dispatch helper for templates
       (defun ,(symb name "-ACTION") (action)
         "Generate data-action attribute for event binding."
         (format nil "data-action=\"~A\"" (string-downcase action)))

       ',name))

;;; ============================================================================
;;; HYDRATION UTILITIES
;;; ============================================================================

(defun with-hydration-wrapper (component-id component-name state-alist html)
  "Wrap rendered HTML with hydration data attributes."
  (format nil "<div data-component-id=\"~A\" data-component=\"~A\" data-state=\"~A\">~A</div>"
          component-id
          component-name
          (serialize-state-for-attr state-alist)
          html))

(defun client-action-attr (action &rest args)
  "Generate data-action attribute with optional arguments."
  (if args
      (format nil "data-action=\"~A\" data-args=\"~A\""
              (string-downcase action)
              (escape-html (cl-json:encode-json-to-string args)))
      (format nil "data-action=\"~A\"" (string-downcase action))))

;;; ============================================================================
;;; RUNTIME HYDRATION SUPPORT
;;; ============================================================================

(defun hydration-runtime-js ()
  "Generate the client-side hydration runtime."
  (parenscript:ps
    (defvar *lol-hydration*
      (ps:create
       :components (ps:create)

       :register (lambda (id component)
                   (setf (ps:@ *lol-hydration* components id) component))

       :get (lambda (id)
              (ps:@ *lol-hydration* components id))

       :dispatch (lambda (id action &rest args)
                   (let ((component ((ps:@ *lol-hydration* get) id)))
                     (when component
                       (apply (ps:@ component dispatch) action args))))

       :hydrate-all (lambda ()
                      (let ((elements ((ps:@ document query-selector-all)
                                       "[data-component-id]")))
                        ((ps:@ elements for-each)
                         (lambda (el)
                           (let ((id ((ps:@ elget-attribute) "data-component-id"))
                                 (name ((ps:@ elget-attribute) "data-component")))
                             ((ps:@ console log) "(hydrating:" id name ")"))))))))))

(defun include-hydration-runtime ()
  "Generate script tag with hydration runtime."
  (format nil "<script>~A</script>" (hydration-runtime-js)))

;;; ============================================================================
;;; SERVER-SIDE RENDERING HELPERS
;;; ============================================================================

(defmacro render-isomorphic (component-form)
  "Render an isomorphic component with automatic hydration.
   Use this in page templates."
  (let ((component-name (car component-form)))
    `(,(symb component-name "-ISOMORPHIC") ,@(cdr component-form))))

(defun isomorphic-page (title &key head body components)
  "Generate a full page with isomorphic components.
   COMPONENTS: List of isomorphic component render forms."
  (html-page
   :title title
   :head (concatenate 'string
                      (or head "")
                      (include-hydration-runtime))
   :body (format nil "~A~{~A~}"
                 (or body "")
                 components)))
