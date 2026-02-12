;;;; LOL-REACTIVE Routes
;;;; Enhanced route handling with integrated security and error handling
;;;;
;;;; Provides:
;;;; - defroute: Define routes with automatic security headers and error handling
;;;; - defapi: Define JSON API routes
;;;; - Built-in component API routes

(in-package :lol-reactive)

;;; ============================================================================
;;; ROUTE DEFINITION MACROS (Clack-compatible)
;;; ============================================================================

;; Note: *routes* is defined in app.lisp

(defmacro defroute (path (&key (method :get)
                               (content-type "text/html")
                               (secure t))
                   &body body)
  "Define a route with automatic error handling and security.

   Options:
   - METHOD: HTTP method (:get, :post, :put, :delete)
   - CONTENT-TYPE: Response content type (default \"text/html\")
   - SECURE: Add security headers (default t)

   The body should return a string (response body) or a full response list.
   If a string is returned, it will be wrapped with appropriate headers.

   Example:
     (defroute \"/\" (:method :get)
       \"<h1>Welcome</h1>\")

     (defroute \"/api/data\" (:method :post :content-type \"application/json\" :secure t)
       (cl-json:encode-json-to-string (get-data)))"
  (let ((handler-name (gensym "ROUTE-HANDLER-")))
    `(progn
       (defun ,handler-name ()
         (let ((result (handler-case
                           ,(if secure
                                `(with-security ,@body)
                                `(progn ,@body))
                         (error (e)
                           (if *debug-mode*
                               (format nil "Error: ~A" e)
                               "Internal Server Error")))))
           ;; If result is a string, wrap in response
           (if (stringp result)
               (response 200 :content-type ,content-type :body result)
               result)))
       (setf (gethash (cons ,method ,path) *routes*) #',handler-name)
       (values ,path ,method))))

(defmacro! defapi (path (&key (method :post)) &body body)
  "Define a JSON API route with security and error handling.

   Automatically:
   - Sets content-type to application/json
   - Adds security headers
   - Wraps in error handling
   - Encodes return value as JSON

   The variable BODY-JSON is bound to the parsed request body.

   Example:
     (defapi \"/api/users\" (:method :post)
       (let ((name (cdr (assoc :name body-json))))
         (create-user name)
         (list :success t :name name)))"
  ;; Intern BODY-JSON in the caller's package at macro-expansion time
  (let ((body-json-sym (intern "BODY-JSON" *package*)))
    `(defroute ,path (:method ,method :content-type "application/json" :secure t)
       (let ((,g!body-json (json-body)))
         (declare (ignorable ,g!body-json))
         (cl-json:encode-json-to-string
           (symbol-macrolet ((,body-json-sym ,g!body-json))
             ,@body))))))

;;; ============================================================================
;;; JSON UTILITIES (Clack-compatible)
;;; ============================================================================

(defun json-body ()
  "Parse JSON from request body. Returns NIL if body is empty or invalid."
  (handler-case
      (let ((body (request-body)))
        (when (and body (> (length body) 0))
          (cl-json:decode-json-from-string body)))
    (error () nil)))

;;; ============================================================================
;;; BUILT-IN API ROUTES
;;; ============================================================================

(defapi "/api/dispatch" (:method :post)
  "Dispatch an action to a component."
  (let* ((component-id (cdr (assoc :component-id body-json)))
         (action (intern (string-upcase (cdr (assoc :action body-json))) :keyword))
         (args (cdr (assoc :args body-json)))
         (component (find-component component-id)))
    (if component
        (progn
          (apply #'funcall component :dispatch action args)
          `((:success . t)
            (:html . ,(render-component component))))
        `((:success . nil)
          (:error . "Component not found")))))

(defapi "/api/set-state" (:method :post)
  "Set state on a component."
  (let* ((component-id (cdr (assoc :component-id body-json)))
         (key (intern (string-upcase (cdr (assoc :key body-json))) :keyword))
         (value (cdr (assoc :value body-json)))
         (component (find-component component-id)))
    (if component
        (progn
          (funcall component :set-state key value)
          `((:success . t)
            (:html . ,(render-component component))))
        `((:success . nil)
          (:error . "Component not found")))))

(defapi "/api/component-state" (:method :post)
  "Get component state for inspection."
  (let* ((component-id (cdr (assoc :component-id body-json)))
         (component (find-component component-id)))
    (if component
        (funcall component :inspect)
        `((:error . "Component not found")))))
