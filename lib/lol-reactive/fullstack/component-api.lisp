;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; fullstack/component-api.lisp - Components with Auto-Generated API Endpoints
;;;;
;;;; PURPOSE:
;;;;   Single component definition generates both server-side rendering
;;;;   and API endpoints for client-side interaction. No manual route
;;;;   registration required.
;;;;
;;;; KEY MACRO:
;;;;   DEFCOMPONENT-WITH-API - Define a component with auto-generated REST API
;;;;
;;;; GENERATED API:
;;;;   POST /api/{component-name}/{action-name}
;;;;   POST /api/{component-name}/state - Get/set state
;;;;   POST /api/{component-name}/render - Re-render component

(in-package :lol-reactive)

;;; ============================================================================
;;; COMPONENT-API REGISTRY
;;; ============================================================================

(defparameter *api-components* (make-hash-table :test 'equal)
  "Registry of API-enabled components: name -> (component . routes)")

(defun register-api-component (name component routes)
  "Register an API-enabled component."
  (setf (gethash name *api-components*)
        (cons component routes)))

(defun find-api-component (name)
  "Find an API-enabled component by name."
  (car (gethash name *api-components*)))

(defun list-api-routes (name)
  "List all routes for an API component."
  (cdr (gethash name *api-components*)))

;;; ============================================================================
;;; ROUTE GENERATION HELPERS
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kebab-to-path (symbol)
    "Convert a kebab-case symbol to URL path segment."
    (string-downcase (substitute #\- #\_ (symbol-name symbol))))

  (defun generate-api-path (component-name action-name)
    "Generate API endpoint path."
    (format nil "/api/~A/~A"
            (kebab-to-path component-name)
            (kebab-to-path action-name)))

  (defun extract-action-name (action-spec)
    "Extract action name from action specification."
    (if (consp action-spec)
        (car action-spec)
        action-spec))

  (defun extract-action-params (action-spec)
    "Extract action parameters from action specification."
    (if (consp action-spec)
        (cadr action-spec)
        nil))

  (defun extract-action-body (action-spec)
    "Extract action body from action specification."
    (if (consp action-spec)
        (cddr action-spec)
        nil)))

;;; ============================================================================
;;; DEFCOMPONENT-WITH-API MACRO
;;; ============================================================================

(defmacro! defcomponent-with-api (name (&rest props) &key state actions render)
  "Define a component with auto-generated API endpoints.

   NAME: Component name (becomes part of API path)
   PROPS: Component properties (keyword arguments)
   STATE: List of (state-name initial-value) pairs
   ACTIONS: List of (action-name (params) &body) - generates POST routes
   RENDER: Render expression using state and props

   Generated API endpoints:
   - POST /api/{name}/{action} - For each action
   - POST /api/{name}/get-state - Get current state
   - POST /api/{name}/set-state - Set state value
   - POST /api/{name}/render - Re-render component

   Example:
     (defcomponent-with-api task-list ()
       :state ((tasks '())
               (filter :all))
       :actions ((add-task (text)
                   (push (list :id (gensym) :text text :done nil) tasks))
                 (toggle-task (id)
                   (let ((task (find id tasks :key (lambda (t) (getf t :id)))))
                     (when task (setf (getf task :done) (not (getf task :done))))))
                 (delete-task (id)
                   (setf tasks (remove id tasks :key (lambda (t) (getf t :id))))))
       :render (htm-str
                 (:ul :class \"task-list\"
                   (dolist (task tasks)
                     (htm (:li :class (if (getf task :done) \"done\" \"\")
                            (cl-who:esc (getf task :text))))))))"
  (let* ((action-names (mapcar #'extract-action-name actions))
         (component-path (kebab-to-path name))
         (routes-var (symb "*" name "-ROUTES*")))
    `(progn
       ;; Define the component
       (defun ,name (&key (id (generate-component-id ',name)) ,@props)
         (let (;; Initialize state
               ,@(mapcar (lambda (s)
                           `(,(car s) ,(cadr s)))
                         state))
           (pandoriclet ((id id)
                         ,@(mapcar (lambda (s) `(,(car s) ,(car s))) state)
                         ,@(mapcar (lambda (p) `(,p ,p)) props))
             (dlambda
               (:id () id)

               (:render ()
                ,render)

               (:state (&optional key)
                (if key
                    (ecase key
                      ,@(mapcar (lambda (s) `((,(car s)) ,(car s))) state))
                    (list ,@(mapcan (lambda (s)
                                      `(,(intern (symbol-name (car s)) :keyword)
                                        ,(car s)))
                                    state))))

               (:set-state (key value)
                (ecase key
                  ,@(mapcar (lambda (s)
                              `((,(car s)) (setf ,(car s) value)))
                            state))
                value)

               (:dispatch (action &rest args)
                (ecase action
                  ,@(mapcar (lambda (act)
                              (let ((act-name (extract-action-name act))
                                    (act-params (extract-action-params act))
                                    (act-body (extract-action-body act)))
                                `((,act-name)
                                  (destructuring-bind ,act-params args
                                    ,@act-body))))
                            actions)))

               (:props ()
                (list ,@(mapcan (lambda (p)
                                  `(,(intern (symbol-name p) :keyword) ,p))
                                props)))

               (:inspect ()
                (list :id id
                      :component ',name
                      :state (list ,@(mapcan (lambda (s)
                                               `(,(intern (symbol-name (car s)) :keyword)
                                                 ,(car s)))
                                             state))
                      :props (list ,@(mapcan (lambda (p)
                                               `(,(intern (symbol-name p) :keyword) ,p))
                                             props))
                      :actions ',action-names))))))

       ;; Register API routes
       ,@(mapcar (lambda (act)
                   (let* ((act-name (extract-action-name act))
                          (act-params (extract-action-params act))
                          (api-path (generate-api-path name act-name)))
                     `(defapi ,api-path (:method :post)
                        (let* ((component-id (cdr (assoc :component-id body-json)))
                               (component (find-component component-id)))
                          (if component
                              (let ((args (list ,@(mapcar
                                                   (lambda (p)
                                                     `(cdr (assoc
                                                            ,(intern (symbol-name p) :keyword)
                                                            body-json)))
                                                   act-params))))
                                (apply component :dispatch ',act-name args)
                                (list :success t
                                      :html (funcall component :render)
                                      :state (funcall component :state)))
                              (list :success nil
                                    :error "Component not found"))))))
                 actions)

       ;; State getter route
       (defapi ,(format nil "/api/~A/get-state" component-path) (:method :post)
         (let* ((component-id (cdr (assoc :component-id body-json)))
                (component (find-component component-id)))
           (if component
               (list :success t :state (funcall component :state))
               (list :success nil :error "Component not found"))))

       ;; State setter route
       (defapi ,(format nil "/api/~A/set-state" component-path) (:method :post)
         (let* ((component-id (cdr (assoc :component-id body-json)))
                (key (intern (string-upcase (cdr (assoc :key body-json))) :keyword))
                (value (cdr (assoc :value body-json)))
                (component (find-component component-id)))
           (if component
               (progn
                 (funcall component :set-state key value)
                 (list :success t
                       :html (funcall component :render)
                       :state (funcall component :state)))
               (list :success nil :error "Component not found"))))

       ;; Render route
       (defapi ,(format nil "/api/~A/render" component-path) (:method :post)
         (let* ((component-id (cdr (assoc :component-id body-json)))
                (component (find-component component-id)))
           (if component
               (list :success t :html (funcall component :render))
               (list :success nil :error "Component not found"))))

       ;; Store route list for introspection
       (defparameter ,routes-var
         ',(cons (format nil "/api/~A/get-state" component-path)
                 (cons (format nil "/api/~A/set-state" component-path)
                       (cons (format nil "/api/~A/render" component-path)
                             (mapcar (lambda (act)
                                       (generate-api-path name (extract-action-name act)))
                                     actions)))))

       ',name)))

;;; ============================================================================
;;; CLIENT-SIDE API HELPERS
;;; ============================================================================

(defun generate-api-client-js (component-name actions)
  "Generate JavaScript client for component API."
  (parenscript:ps*
    `(defvar ,(intern (format nil "~A-API" (string-upcase component-name)))
       (create
        ,@(mapcan
           (lambda (act)
             (let ((act-name (extract-action-name act))
                   (act-params (extract-action-params act)))
               `(,(intern (kebab-to-path act-name) :keyword)
                 (lambda (component-id ,@act-params)
                   (fetch ,(generate-api-path component-name act-name)
                          (ps:create :method "POST"
                                  :headers (ps:create "Content-Type" "application/json")
                                  :body ((ps:@ -j-s-o-n stringify)
                                         (ps:create :component-id component-id
                                                 ,@(mapcan (lambda (p)
                                                             `(,(intern (kebab-to-path p) :keyword) ,p))
                                                           act-params)))))))))
           actions)
        :get-state (lambda (component-id)
                     (fetch ,(format nil "/api/~A/get-state" (kebab-to-path component-name))
                            (ps:create :method "POST"
                                    :headers (ps:create "Content-Type" "application/json")
                                    :body ((ps:@ -j-s-o-n stringify)
                                           (ps:create :component-id component-id)))))
        :set-state (lambda (component-id key value)
                     (fetch ,(format nil "/api/~A/set-state" (kebab-to-path component-name))
                            (ps:create :method "POST"
                                    :headers (ps:create "Content-Type" "application/json")
                                    :body ((ps:@ -j-s-o-n stringify)
                                           (ps:create :component-id component-id
                                                   :key key
                                                   :value value)))))
        :render (lambda (component-id)
                  (fetch ,(format nil "/api/~A/render" (kebab-to-path component-name))
                         (ps:create :method "POST"
                                 :headers (ps:create "Content-Type" "application/json")
                                 :body ((ps:@ -j-s-o-n stringify)
                                        (ps:create :component-id component-id)))))))))

(defun api-client-script-tag (component-name actions)
  "Generate script tag with API client."
  (format nil "<script>~A</script>"
          (generate-api-client-js component-name actions)))

;;; ============================================================================
;;; INTROSPECTION
;;; ============================================================================

(defun list-api-components ()
  "List all registered API components."
  (let ((components nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k components))
             *api-components*)
    components))

(defun inspect-api-component (name)
  "Inspect an API component's configuration."
  (let ((entry (gethash name *api-components*)))
    (when entry
      (list :name name
            :routes (cdr entry)))))
