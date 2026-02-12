;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; async/resources.lisp - Async Data Resources with Loading States
;;;;
;;;; PURPOSE:
;;;;   Define async data resources with loading, error, and success states.
;;;;   Declarative data fetching with caching support.
;;;;
;;;; KEY MACROS:
;;;;   DEFRESOURCE - Define an async data resource
;;;;   WITH-RESOURCE - Use a resource with automatic state handling
;;;;
;;;; FEATURES:
;;;;   - Loading state rendering
;;;;   - Error state handling
;;;;   - Memory and session caching
;;;;   - Automatic retry support

(in-package :lol-reactive)

;;; ============================================================================
;;; RESOURCE REGISTRY
;;; ============================================================================

(defvar *resources* (make-hash-table :test 'eq)
  "Registry of defined resources.")

(defvar *resource-cache* (make-hash-table :test 'equal)
  "In-memory cache for resource data.")

(defvar *resource-cache-timestamps* (make-hash-table :test 'equal)
  "Timestamps for cached resource data.")

(defun register-resource (name spec)
  "Register a resource specification."
  (setf (gethash name *resources*) spec))

(defun get-resource-spec (name)
  "Get resource specification by name."
  (gethash name *resources*))

(defun list-resources ()
  "List all registered resources."
  (let (resources)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k resources))
             *resources*)
    (nreverse resources)))

;;; ============================================================================
;;; RESOURCE STATES
;;; ============================================================================

(defstruct (resource-state (:constructor make-resource-state))
  "State of a resource fetch operation."
  (status :idle :type keyword)    ; :idle :loading :success :error
  (data nil)                      ; Fetched data on success
  (error nil)                     ; Error object on failure
  (timestamp nil)                 ; When data was fetched
  (params nil))                   ; Parameters used for fetch

(defun resource-loading-p (state)
  "Check if resource is currently loading."
  (eq (resource-state-status state) :loading))

(defun resource-success-p (state)
  "Check if resource fetch succeeded."
  (eq (resource-state-status state) :success))

(defun resource-error-p (state)
  "Check if resource fetch failed."
  (eq (resource-state-status state) :error))

(defun resource-idle-p (state)
  "Check if resource hasn't been fetched yet."
  (eq (resource-state-status state) :idle))

;;; ============================================================================
;;; CACHING
;;; ============================================================================

(defun make-cache-key (resource-name params)
  "Create a cache key from resource name and parameters."
  (format nil "~A:~S" resource-name params))

(defun get-cached-data (resource-name params &key (max-age nil))
  "Get cached data if available and not expired.
   MAX-AGE: Maximum age in seconds (nil = no expiry)"
  (let* ((key (make-cache-key resource-name params))
         (data (gethash key *resource-cache*))
         (timestamp (gethash key *resource-cache-timestamps*)))
    (when (and data timestamp)
      (if (or (null max-age)
              (<= (- (get-universal-time) timestamp) max-age))
          data
          (progn
            ;; Cache expired, remove it
            (remhash key *resource-cache*)
            (remhash key *resource-cache-timestamps*)
            nil)))))

(defun set-cached-data (resource-name params data)
  "Store data in cache."
  (let ((key (make-cache-key resource-name params)))
    (setf (gethash key *resource-cache*) data
          (gethash key *resource-cache-timestamps*) (get-universal-time))))

(defun clear-resource-cache (&optional resource-name)
  "Clear cached data, optionally for specific resource."
  (if resource-name
      ;; Clear specific resource
      (let ((prefix (format nil "~A:" resource-name)))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (and (stringp k) (>= (length k) (length prefix))
                              (string= prefix (subseq k 0 (length prefix))))
                     (remhash k *resource-cache*)
                     (remhash k *resource-cache-timestamps*)))
                 *resource-cache*))
      ;; Clear all
      (progn
        (clrhash *resource-cache*)
        (clrhash *resource-cache-timestamps*))))

;;; ============================================================================
;;; RESOURCE FETCHING
;;; ============================================================================

(defun fetch-resource (resource-name &rest params)
  "Fetch a resource synchronously.
   Returns a resource-state struct."
  (let* ((spec (get-resource-spec resource-name))
         (fetcher (getf spec :fetcher))
         (cache-strategy (getf spec :cache :none))
         (cache-max-age (getf spec :cache-max-age)))

    (unless spec
      (return-from fetch-resource
        (make-resource-state :status :error
                             :error (format nil "Resource ~A not found" resource-name)
                             :params params)))

    ;; Check cache first
    (when (and (member cache-strategy '(:memory :session))
               (not (eq cache-strategy :none)))
      (let ((cached (get-cached-data resource-name params :max-age cache-max-age)))
        (when cached
          (return-from fetch-resource
            (make-resource-state :status :success
                                 :data cached
                                 :timestamp (get-universal-time)
                                 :params params)))))

    ;; Fetch data
    (handler-case
        (let ((data (apply fetcher params)))
          ;; Cache result
          (when (member cache-strategy '(:memory :session))
            (set-cached-data resource-name params data))
          (make-resource-state :status :success
                               :data data
                               :timestamp (get-universal-time)
                               :params params))
      (error (e)
        (make-resource-state :status :error
                             :error (princ-to-string e)
                             :params params)))))

;;; ============================================================================
;;; RESOURCE RENDERING
;;; ============================================================================

(defun render-resource-loading (resource-name)
  "Render loading state for a resource using Tailwind classes.
   Uses custom :loading from spec, or default Tailwind-styled spinner."
  (let* ((spec (get-resource-spec resource-name))
         (loading (getf spec :loading)))
    (if loading
        (if (functionp loading)
            (funcall loading)
            loading)
        ;; Default loading UI with Tailwind classes via htm-str
        (htm-str
          (:div :class (classes "p-4" "text-center" "text-muted")
            (:span :class (classes "inline-block" "w-5" "h-5" "border-2"
                                   "border-muted" "border-t-primary"
                                   "rounded-full" "animate-spin" "mr-2" "align-middle"))
            "Loading...")))))

(defun render-resource-error (resource-name error)
  "Render error state for a resource using Tailwind classes.
   Uses custom :error from spec, or default Tailwind-styled error box."
  (let* ((spec (get-resource-spec resource-name))
         (error-handler (getf spec :error)))
    (if error-handler
        (if (functionp error-handler)
            (funcall error-handler error)
            error-handler)
        ;; Default error UI with Tailwind classes via htm-str
        (htm-str
          (:div :class (classes "p-4" "bg-error/10" "border" "border-error/30"
                                "rounded-md" "text-error")
            (:strong "Error:") " " (cl-who:esc (princ-to-string error)))))))

;;; ============================================================================
;;; DEFRESOURCE MACRO
;;; ============================================================================

(defmacro defresource (name (&rest params) &key fetcher loading error cache cache-max-age)
  "Define an async data resource with loading states.

   NAME: Resource identifier
   PARAMS: Parameter list for the fetcher function
   FETCHER: Function that fetches the data (receives params)
   LOADING: Loading state component (string or function)
   ERROR: Error state component (function receiving error object)
   CACHE: Cache strategy (:none :memory :session)
   CACHE-MAX-AGE: Maximum cache age in seconds

   Creates:
   - (fetch-NAME params...) - Fetch resource, returns resource-state
   - (NAME-loading) - Render loading state
   - (NAME-error err) - Render error state
   - (with-resource (data (NAME params...)) body) - Use with automatic state handling

   Example:
     (defresource user-data (user-id)
       :fetcher (lambda (id) (get-user-from-db id))
       :loading \"<div class='spinner'>Loading user...</div>\"
       :error (lambda (e) (format nil \"<div class='error'>~A</div>\" e))
       :cache :memory
       :cache-max-age 300)"
  (let ((fetch-fn-name (symb "FETCH-" name))
        (loading-fn-name (symb name "-LOADING"))
        (error-fn-name (symb name "-ERROR")))
    `(progn
       ;; Register resource spec
       (register-resource ',name
                          (list :fetcher ,fetcher
                                :loading ,loading
                                :error ,error
                                :cache ,(or cache :none)
                                :cache-max-age ,cache-max-age
                                :params ',params))

       ;; Define fetch function
       (defun ,fetch-fn-name (,@params)
         ,(format nil "Fetch ~A resource." name)
         (fetch-resource ',name ,@params))

       ;; Define loading renderer
       (defun ,loading-fn-name ()
         ,(format nil "Render loading state for ~A." name)
         (render-resource-loading ',name))

       ;; Define error renderer
       (defun ,error-fn-name (error)
         ,(format nil "Render error state for ~A." name)
         (render-resource-error ',name error))

       ',name)))

;;; ============================================================================
;;; WITH-RESOURCE MACRO
;;; ============================================================================

(defmacro with-resource ((data-var resource-call) &body body)
  "Use a resource in component, handling loading/error states automatically.

   DATA-VAR: Variable to bind the fetched data
   RESOURCE-CALL: Form like (resource-name params...)
   BODY: Code to execute when data is successfully loaded

   Automatically renders:
   - Loading component while fetching
   - Error component on failure
   - BODY with data bound on success

   Example:
     (with-resource (user (user-data user-id))
       (htm-str (:h1 \"Welcome, \" (cl-who:esc (getf user :name)))))"
  (let* ((resource-name (car resource-call))
         (resource-params (cdr resource-call))
         (fetch-fn (symb "FETCH-" resource-name))
         (state-var (gensym "STATE")))
    `(let ((,state-var (,fetch-fn ,@resource-params)))
       (cond
         ((resource-loading-p ,state-var)
          (render-resource-loading ',resource-name))
         ((resource-error-p ,state-var)
          (render-resource-error ',resource-name (resource-state-error ,state-var)))
         ((resource-success-p ,state-var)
          (let ((,data-var (resource-state-data ,state-var)))
            ,@body))
         (t
          ;; Idle state - trigger fetch
          (setf ,state-var (,fetch-fn ,@resource-params))
          (if (resource-success-p ,state-var)
              (let ((,data-var (resource-state-data ,state-var)))
                ,@body)
              (render-resource-error ',resource-name (resource-state-error ,state-var))))))))

;;; ============================================================================
;;; RESOURCE STYLES
;;; ============================================================================

(defun resource-styles-css ()
  "OPTIONAL: CSS for projects NOT using Tailwind.
   The default render functions use Tailwind classes. This function provides
   fallback CSS with CSS variables for non-Tailwind projects."
  (concatenate 'string
    (css-section "Resource Loading"
      (css-rule ".resource-loading"
                `(("padding" . ,(css-var "spacing-4"))
                  ("text-align" . "center")
                  ("color" . ,(css-var "color-muted"))))
      (css-rule ".resource-loading .spinner"
                `(("display" . "inline-block")
                  ("width" . "20px")
                  ("height" . "20px")
                  ("border" . ,(format nil "2px solid ~A" (css-var "color-muted")))
                  ("border-top-color" . ,(css-var "color-primary"))
                  ("border-radius" . "50%")
                  ("animation" . "lol-spin 1s linear infinite")
                  ("margin-right" . ,(css-var "spacing-2"))
                  ("vertical-align" . "middle"))))
    (format nil "~%")
    (css-keyframes "lol-spin"
      '("to" . (("transform" . "rotate(360deg)"))))
    (format nil "~%")
    (css-section "Resource Error"
      (css-rule ".resource-error"
                `(("padding" . ,(css-var "spacing-4"))
                  ("background" . ,(format nil "color-mix(in srgb, ~A 10%, ~A)"
                                           (css-var "color-error")
                                           (css-var "color-surface")))
                  ("border" . ,(format nil "1px solid color-mix(in srgb, ~A 30%, ~A)"
                                       (css-var "color-error")
                                       (css-var "color-surface")))
                  ("border-radius" . ,(css-var "radius-md"))
                  ("color" . ,(css-var "color-error")))))))

;;; ============================================================================
;;; RESOURCE INTROSPECTION
;;; ============================================================================

(defun inspect-resource (name)
  "Return introspection data for a resource."
  (let ((spec (get-resource-spec name)))
    (when spec
      (list :name name
            :params (getf spec :params)
            :cache (getf spec :cache)
            :cache-max-age (getf spec :cache-max-age)
            :has-loading (not (null (getf spec :loading)))
            :has-error-handler (not (null (getf spec :error)))))))

(defun resource-cache-stats ()
  "Get statistics about the resource cache."
  (let ((count 0)
        (total-age 0)
        (now (get-universal-time)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (incf count)
               (let ((timestamp (gethash k *resource-cache-timestamps*)))
                 (when timestamp
                   (incf total-age (- now timestamp)))))
             *resource-cache*)
    (list :cached-items count
          :average-age-seconds (if (> count 0) (/ total-age count) 0))))
