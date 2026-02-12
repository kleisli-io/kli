;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; forms/form-dsl.lisp - Type-Safe Form DSL
;;;;
;;;; PURPOSE:
;;;;   Define type-safe forms with validation, CSRF protection, and client-side JS.
;;;;
;;;; KEY MACRO:
;;;;   DEFFORM - Define a form with field specifications and handlers
;;;;
;;;; GENERATED OUTPUT:
;;;;   - Server-side validation function
;;;;   - HTML form rendering function
;;;;   - Client-side Parenscript validation
;;;;   - Form registry for introspection

(in-package :lol-reactive)

;;; ============================================================================
;;; FORM REGISTRY
;;; ============================================================================

(defvar *forms* (make-hash-table :test 'eq)
  "Registry of defined forms.")

(defun register-form (name spec)
  "Register a form specification for later rendering and validation."
  (setf (gethash name *forms*) spec))

(defun get-form-spec (name)
  "Retrieve a registered form specification."
  (gethash name *forms*))

(defun list-forms ()
  "List all registered forms."
  (let (forms)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k forms))
             *forms*)
    (nreverse forms)))

;;; ============================================================================
;;; FIELD TYPES AND INPUT GENERATION
;;; ============================================================================

(defparameter *field-type-to-input*
  '((:string . "text")
    (:text . "textarea")
    (:email . "email")
    (:password . "password")
    (:number . "number")
    (:integer . "number")
    (:tel . "tel")
    (:url . "url")
    (:date . "date")
    (:time . "time")
    (:datetime . "datetime-local")
    (:checkbox . "checkbox")
    (:hidden . "hidden")
    (:file . "file")
    (:color . "color")
    (:range . "range"))
  "Mapping from field type keywords to HTML input types.")

(defun field-type-to-html-input (field-type)
  "Convert field type keyword to HTML input type string."
  (or (cdr (assoc field-type *field-type-to-input*))
      "text"))

(defun html-attrs (&rest pairs)
  "Generate HTML attribute string from key-value pairs.
   NIL values are omitted. T values become boolean attributes.
   All values are properly sanitized.

   Example: (html-attrs \"id\" \"foo\" \"required\" t \"disabled\" nil)
            => \" id=\\\"foo\\\" required\""
  (with-output-to-string (s)
    (loop for (name value) on pairs by #'cddr
          when value do
          (if (eq value t)
              (format s " ~A" name)  ; boolean attribute
              (format s " ~A=\"~A\"" name (sanitize-attribute (princ-to-string value)))))))

(defun generate-input-element (field-type input-type field-id field-name input-class
                                value required placeholder min-val max-val minlength maxlength)
  "Generate the input or textarea element HTML.
   Uses html-attrs helper for clean conditional attribute handling.

   Note: Uses format+html-attrs rather than htm-str because cl-who doesn't
   support runtime conditional attributes (,@ splicing only works at macro
   expansion time). This pattern maintains proper escaping via html-attrs."
  (if (eq field-type :text)
      ;; Textarea for :text type
      (format nil "<textarea~A>~A</textarea>"
              (html-attrs "id" field-id
                          "name" field-name
                          "class" input-class
                          "required" (when required t)
                          "minlength" minlength
                          "maxlength" maxlength)
              (escape-html (if value (princ-to-string value) "")))
      ;; Regular input (self-closing)
      (format nil "<input~A/>"
              (html-attrs "type" input-type
                          "id" field-id
                          "name" field-name
                          "class" input-class
                          "value" value
                          "required" (when required t)
                          "placeholder" placeholder
                          "min" (when (member field-type '(:number :integer :range)) min-val)
                          "max" (when (member field-type '(:number :integer :range)) max-val)
                          "minlength" minlength
                          "maxlength" maxlength))))

(defun generate-input-html (field-spec &key (value nil) (errors nil))
  "Generate HTML for a single form field using Tailwind classes.
   FIELD-SPEC: (name :type TYPE :min MIN :max MAX :required BOOL :placeholder STR :label STR)
   VALUE: Current value for the field
   ERRORS: List of validation errors for this field"
  (let* ((name (car field-spec))
         (plist (cdr field-spec))
         (field-type (getf plist :type :string))
         (input-type (field-type-to-html-input field-type))
         (required (getf plist :required))
         (min-val (getf plist :min))
         (max-val (getf plist :max))
         (minlength (getf plist :minlength (when (member field-type '(:string :text :password))
                                             min-val)))
         (maxlength (getf plist :maxlength (when (member field-type '(:string :text :password))
                                             max-val)))
         (placeholder (getf plist :placeholder))
         (label (getf plist :label (string-capitalize (substitute #\Space #\- (string-downcase name)))))
         (field-id (format nil "field-~A" (string-downcase name)))
         (field-name (string-downcase name))
         (has-errors (and errors (listp errors)))
         ;; Tailwind classes
         (container-class (classes "mb-4" (when has-errors "has-errors")))
         (label-class (classes "block" "mb-2" "font-medium" "text-text"))
         (input-class (classes "w-full" "p-2" "border" "rounded-md" "text-text" "bg-surface"
                               (if has-errors "border-error" "border-muted")
                               "focus:outline-none" "focus:border-primary" "focus:ring-2" "focus:ring-primary/20"))
         (error-class (classes "block" "text-error" "text-sm" "mt-1"))
         (required-class "text-error"))

    (htm-str
      (:div :class container-class
        ;; Label
        (:label :for field-id :class label-class
          (cl-who:esc label)
          (when required
            (cl-who:htm " " (:span :class required-class "*"))))
        ;; Input element (via helper)
        (cl-who:str (generate-input-element field-type input-type field-id field-name input-class
                                            value required placeholder min-val max-val minlength maxlength))
        ;; Error messages
        (when has-errors
          (dolist (err errors)
            (cl-who:htm (:span :class error-class (cl-who:esc err)))))))))

;;; ============================================================================
;;; SERVER-SIDE VALIDATION
;;; ============================================================================

(defun validate-field (field-spec value)
  "Validate a single field value against its specification.
   Returns NIL if valid, or a list of error messages."
  (let* ((name (car field-spec))
         (plist (cdr field-spec))
         (field-type (getf plist :type :string))
         (required (getf plist :required))
         (min-val (getf plist :min))
         (max-val (getf plist :max))
         (pattern (getf plist :pattern))
         (custom-validator (getf plist :validate))
         (errors nil))

    ;; Required check
    (when (and required (or (null value) (equal value "")))
      (push (format nil "~A is required" (string-capitalize (string-downcase name))) errors))

    ;; Only validate non-empty values further
    (when (and value (not (equal value "")))
      ;; Type-specific validation
      (case field-type
        (:email
         (unless (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" value)
           (push "Invalid email address" errors)))

        (:url
         (unless (cl-ppcre:scan "^https?://" value)
           (push "Invalid URL (must start with http:// or https://)" errors)))

        ((:number :integer)
         (let ((num (ignore-errors (parse-integer value :junk-allowed t))))
           (cond
             ((null num)
              (push "Must be a number" errors))
             ((and min-val (< num min-val))
              (push (format nil "Must be at least ~A" min-val) errors))
             ((and max-val (> num max-val))
              (push (format nil "Must be at most ~A" max-val) errors)))))

        ((:string :text :password)
         (let ((len (length value)))
           (when (and min-val (< len min-val))
             (push (format nil "Must be at least ~A characters" min-val) errors))
           (when (and max-val (> len max-val))
             (push (format nil "Must be at most ~A characters" max-val) errors)))))

      ;; Pattern validation
      (when (and pattern (stringp value))
        (unless (cl-ppcre:scan pattern value)
          (push "Invalid format" errors)))

      ;; Custom validator
      (when custom-validator
        (let ((custom-result (funcall custom-validator value)))
          (when custom-result
            (if (stringp custom-result)
                (push custom-result errors)
                (push "Invalid value" errors))))))

    (nreverse errors)))

(defun validate-form-data (form-name data)
  "Validate form data against form specification.
   DATA: Plist of field-name -> value
   Returns (values valid-p errors-alist)"
  (let* ((spec (get-form-spec form-name))
         (fields (getf spec :fields))
         (all-errors nil)
         (valid t))
    (dolist (field-spec fields)
      (let* ((name (car field-spec))
             (value (getf data (intern (string-upcase name) :keyword)))
             (field-errors (validate-field field-spec value)))
        (when field-errors
          (setf valid nil)
          (push (cons name field-errors) all-errors))))
    (values valid (nreverse all-errors))))

;;; ============================================================================
;;; CLIENT-SIDE VALIDATION (PARENSCRIPT)
;;; ============================================================================

(defun generate-field-validation-js (field-spec)
  "Generate Parenscript validation for a single field."
  (let* ((name (car field-spec))
         (plist (cdr field-spec))
         (field-type (getf plist :type :string))
         (required (getf plist :required))
         (min-val (getf plist :min))
         (max-val (getf plist :max))
         (field-name (string-downcase name))
         (checks nil))

    ;; Required check
    (when required
      (push `(when (or (null value) (equal value ""))
               (push ,(format nil "~A is required" (string-capitalize field-name)) errors))
            checks))

    ;; Length checks for strings
    (when (and min-val (member field-type '(:string :text :password)))
      (push `(when (and value (< (ps:@ value length) ,min-val))
               (push ,(format nil "Must be at least ~A characters" min-val) errors))
            checks))

    (when (and max-val (member field-type '(:string :text :password)))
      (push `(when (and value (> (ps:@ value length) ,max-val))
               (push ,(format nil "Must be at most ~A characters" max-val) errors))
            checks))

    ;; Numeric checks
    (when (member field-type '(:number :integer))
      (push `(when (and value (not (equal value "")) (is-na-n (parse-float value)))
               (push "Must be a number" errors))
            checks)
      (when min-val
        (push `(when (and value (not (is-na-n (parse-float value)))
                          (< (parse-float value) ,min-val))
                 (push ,(format nil "Must be at least ~A" min-val) errors))
              checks))
      (when max-val
        (push `(when (and value (not (is-na-n (parse-float value)))
                          (> (parse-float value) ,max-val))
                 (push ,(format nil "Must be at most ~A" max-val) errors))
              checks)))

    ;; Email check
    (when (eq field-type :email)
      (push `(when (and value (not (equal value ""))
                        (not (ps:chain (ps:new (-Reg-Exp "^[^@]+@[^@]+\\.[^@]+$")) (test value))))
                 (push "Invalid email address" errors))
            checks))

    `(lambda (value)
       (let ((errors (list)))
         ,@(nreverse checks)
         errors))))

(defun generate-form-validation-js (form-name)
  "Generate complete client-side validation script for a form."
  (let* ((spec (get-form-spec form-name))
         (fields (getf spec :fields))
         (form-id (format nil "form-~A" (string-downcase form-name))))
    (parenscript:ps*
     `(progn
        (defvar ,(symb "*" form-name "-VALIDATORS*")
          (ps:create
           ,@(mapcan (lambda (field-spec)
                       (list (intern (string-downcase (car field-spec)) :keyword)
                             (generate-field-validation-js field-spec)))
                     fields)))

        ((ps:@ document add-event-listener) "DOMContentLoaded"
         (lambda ()
           (let ((form ((ps:@ document get-element-by-id) ,form-id)))
             (when form
               ;; Validate on submit
               ((ps:@ form add-event-listener) "submit"
                (lambda (e)
                  (let ((valid t)
                        (validators ,(symb "*" form-name "-VALIDATORS*")))
                    ;; Validate each field
                    ,@(mapcar (lambda (field-spec)
                                (let* ((name (car field-spec))
                                       (field-name (string-downcase name))
                                       (field-key (intern field-name :keyword)))
                                  `(let* ((input ((ps:@ form query-selector) ,(format nil "[name='~A']" field-name)))
                                          (value (if input (ps:@ input value) ""))
                                          (errors ((ps:@ validators ,field-key) value))
                                          (container ((ps:@ input closest) ".form-field")))
                                     (when (and errors (> (ps:@ errors length) 0))
                                       (setf valid nil)
                                       (when container
                                         ((ps:@ container class-list add) "has-errors")
                                         ;; Remove old error messages
                                         (let ((old-errors ((ps:@ container query-selector-all) ".field-error")))
                                           ((ps:@ old-errors for-each) (lambda (el) ((ps:@ elremove)))))
                                         ;; Add new error messages
                                         ((ps:@ errors for-each)
                                          (lambda (msg)
                                            (let ((err-el ((ps:@ document create-element) "span")))
                                              (setf (ps:@ err-el class-name) "field-error")
                                              (setf (ps:@ err-el text-content) msg)
                                              ((ps:@ container append-child) err-el)))))))))
                              fields)
                    (unless valid
                      ((ps:@ eprevent-default))))))
               ;; Clear errors on input
               ((ps:@ form add-event-listener) "input"
                (lambda (e)
                  (let ((container ((ps:@ (ps:@ etarget) closest) ".form-field")))
                    (when container
                      ((ps:@ container class-list remove) "has-errors")
                      (let ((errors ((ps:@ container query-selector-all) ".field-error")))
                        ((ps:@ errors for-each) (lambda (el) ((ps:@ elremove))))))))
                t)))))))))

;;; ============================================================================
;;; FORM RENDERING
;;; ============================================================================

(defun render-form-content (fields values errors include-csrf method actions-class button-class submit-text)
  "Render the inner content of a form (CSRF, fields, submit button).
   Helper for render-form."
  (htm-str
    ;; CSRF token
    (when (and include-csrf (string-equal method "POST"))
      (cl-who:str (csrf-token-input)))
    ;; Fields
    (dolist (field-spec fields)
      (let* ((name (car field-spec))
             (value (getf values (intern (string-upcase name) :keyword)))
             (field-errors (cdr (assoc name errors :test #'string-equal))))
        (cl-who:str (generate-input-html field-spec :value value :errors field-errors))))
    ;; Submit button
    (:div :class actions-class
      (:button :type "submit" :class button-class
        (cl-who:esc submit-text)))))

(defun render-form (form-name &key (action nil) (method "POST") (values nil) (errors nil)
                                   (submit-text "Submit") (include-csrf t) (extra-classes ""))
  "Render a registered form as HTML with Tailwind classes.
   ACTION: Form action URL (default: current URL)
   METHOD: HTTP method (default: POST)
   VALUES: Plist of field values to pre-fill
   ERRORS: Alist of (field-name . error-list) from validation
   SUBMIT-TEXT: Text for submit button
   INCLUDE-CSRF: Include CSRF token hidden field
   EXTRA-CLASSES: Additional Tailwind classes for form element"
  (let* ((spec (get-form-spec form-name))
         (fields (getf spec :fields))
         (form-id (format nil "form-~A" (string-downcase form-name)))
         ;; Tailwind classes
         (form-class (classes "max-w-md" extra-classes))
         (actions-class "mt-6")
         (button-class (classes "px-4" "py-2" "bg-primary" "text-surface" "rounded-md"
                                "cursor-pointer" "hover:brightness-90")))
    (unless spec
      (error "Form ~A not found. Did you call DEFFORM?" form-name))

    (concatenate 'string
      ;; Form tag with conditional action attribute (uses html-attrs for consistency)
      (format nil "<form~A>"
              (html-attrs "id" form-id
                          "class" form-class
                          "action" action
                          "method" method))
      ;; Form content via htm-str
      (render-form-content fields values errors include-csrf method actions-class button-class submit-text)
      "</form>"
      ;; Validation script
      (htm-str
        (:script (cl-who:str (generate-form-validation-js form-name)))))))

;;; ============================================================================
;;; DEFFORM MACRO
;;; ============================================================================

(defmacro defform (name () &key fields on-submit on-error)
  "Define a type-safe form with validation.

   NAME: Form identifier
   FIELDS: List of field specifications:
     (field-name :type TYPE :min MIN :max MAX :required BOOL
                 :placeholder STR :label STR :pattern REGEX :validate FN)
   ON-SUBMIT: Handler receiving validated field values as keyword arguments
   ON-ERROR: Handler receiving validation errors alist

   Supported field types:
     :string :text :email :password :number :integer
     :tel :url :date :time :datetime :checkbox :hidden :file :color :range

   Creates:
   - (render-form 'NAME ...) - Render form HTML
   - (validate-form-data 'NAME data) - Server-side validation
   - (process-NAME-submission request) - Handle form submission

   Example:
     (defform user-registration ()
       :fields ((username :type :string :min 3 :max 20 :required t)
                (email :type :email :required t)
                (password :type :password :min 8)
                (age :type :integer :min 18 :max 120))
       :on-submit (register-user :username username :email email
                                 :password password :age age)
       :on-error (show-validation-errors errors))"
  (let ((field-names (mapcar #'car fields))
        (process-fn-name (symb "PROCESS-" name "-SUBMISSION")))
    `(progn
       ;; Register form spec
       (register-form ',name
                      '(:fields ,fields
                        :on-submit ,on-submit
                        :on-error ,on-error))

       ;; Define submission handler
       (defun ,process-fn-name (request-data)
         "Process form submission with validation.
          REQUEST-DATA: Plist of form values
          Returns: Result of on-submit handler or error handler"
         (multiple-value-bind (valid errors)
             (validate-form-data ',name request-data)
           (if valid
               (let (,@(mapcar (lambda (fname)
                                 `(,fname (getf request-data
                                               ,(intern (string-upcase fname) :keyword))))
                               field-names))
                 ,on-submit)
               ,(if on-error
                    `(let ((errors errors))
                       ,on-error)
                    `errors))))

       ',name)))

;;; ============================================================================
;;; FORM STYLES
;;; ============================================================================

(defun form-styles-css ()
  "OPTIONAL: CSS for projects NOT using Tailwind.
   The default render-form uses Tailwind classes. This function provides
   fallback CSS for the .has-errors class used by client-side validation.
   Only needed if you're not using Tailwind CDN."
  (concatenate 'string
    (css-section "Form Container"
      (css-rule ".lol-form"
                `(("max-width" . "500px"))))
    (format nil "~%")
    (css-section "Form Fields"
      (css-rule ".form-field"
                `(("margin-bottom" . ,(css-var "spacing-4"))))
      (css-rule ".form-field label"
                `(("display" . "block")
                  ("margin-bottom" . ,(css-var "spacing-2"))
                  ("font-weight" . "500")
                  ("color" . ,(css-var "color-text"))))
      (css-rule ".form-field input, .form-field textarea"
                `(("width" . "100%")
                  ("padding" . ,(css-var "spacing-2"))
                  ("border" . ,(format nil "1px solid ~A" (css-var "color-muted")))
                  ("border-radius" . ,(css-var "radius-md"))
                  ("font-size" . "1rem")
                  ("background" . ,(css-var "color-surface"))
                  ("color" . ,(css-var "color-text"))))
      (css-rule ".form-field input:focus, .form-field textarea:focus"
                `(("outline" . "none")
                  ("border-color" . ,(css-var "color-primary"))
                  ("box-shadow" . ,(format nil "0 0 0 2px color-mix(in srgb, ~A 20%, transparent)"
                                           (css-var "color-primary"))))))
    (format nil "~%")
    (css-section "Form Validation States"
      (css-rule ".form-field.has-errors input, .form-field.has-errors textarea"
                `(("border-color" . ,(css-var "color-error"))))
      (css-rule ".form-field .required"
                `(("color" . ,(css-var "color-error"))))
      (css-rule ".form-field .field-error"
                `(("display" . "block")
                  ("color" . ,(css-var "color-error"))
                  ("font-size" . "0.875rem")
                  ("margin-top" . ,(css-var "spacing-1")))))
    (format nil "~%")
    (css-section "Form Actions"
      (css-rule ".form-actions"
                `(("margin-top" . ,(css-var "spacing-6"))))
      (css-rule ".btn"
                `(("padding" . ,(format nil "~A ~A" (css-var "spacing-2") (css-var "spacing-4")))
                  ("border" . "none")
                  ("border-radius" . ,(css-var "radius-md"))
                  ("cursor" . "pointer")
                  ("font-size" . "1rem")))
      (css-rule ".btn-primary"
                `(("background" . ,(css-var "color-primary"))
                  ("color" . ,(css-var "color-surface"))))
      (css-rule ".btn-primary:hover"
                `(("filter" . "brightness(0.9)"))))))

;;; ============================================================================
;;; FORM INTROSPECTION
;;; ============================================================================

(defun inspect-form (name)
  "Return introspection data for a form."
  (let ((spec (get-form-spec name)))
    (when spec
      (list :name name
            :fields (mapcar (lambda (f)
                              (list :name (car f)
                                    :type (getf (cdr f) :type :string)
                                    :required (getf (cdr f) :required)))
                            (getf spec :fields))
            :has-submit-handler (not (null (getf spec :on-submit)))
            :has-error-handler (not (null (getf spec :on-error)))))))
