;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Component Props System
;;;; Enables React-style component composition with validated props and children

(in-package :lol-reactive)

;;; ============================================================================
;;; PROP SPEC HELPERS (needed at compile time for macro expansion)
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-prop-name (spec)
    "Extract the property name from a spec.
     SPEC can be either a symbol or a list starting with the prop name."
    (if (consp spec)
        (car spec)
        spec))

  (defun get-prop-option (spec option &optional default)
    "Get an option value from a prop spec."
    (if (consp spec)
        (getf (cdr spec) option default)
        default)))

;;; ============================================================================
;;; PROP VALIDATION (runtime)
;;; ============================================================================

(defun validate-props (specs values)
  "Validate props against specifications at runtime.
   Returns NIL if valid, or a list of error strings.

   SPECS: Property specifications from defcomponent-with-props
   VALUES: plist of keyword/value pairs"
  (let ((errors nil))
    (dolist (spec specs)
      (let* ((name (extract-prop-name spec))
             ;; Convert name to keyword for plist lookup
             (key (intern (symbol-name name) :keyword))
             (value (getf values key))
             (required (get-prop-option spec :required))
             (prop-type (get-prop-option spec :type)))
        ;; Check required
        (when (and required (null value))
          (push (format nil "~A is required" name) errors))
        ;; Check type if value provided
        (when (and value prop-type)
          (unless (typep value prop-type)
            (push (format nil "~A must be of type ~A, got ~A"
                          name prop-type (type-of value))
                  errors)))))
    (nreverse errors)))

;;; ============================================================================
;;; PROP SPEC PROCESSING (compile-time helpers)
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-prop-specs (specs)
    "Parse prop specifications into structured form.
     Returns list of (name type default required) tuples."
    (mapcar (lambda (spec)
              (if (consp spec)
                  (list (car spec)
                        (getf (cdr spec) :type t)
                        (getf (cdr spec) :default)
                        (getf (cdr spec) :required))
                  (list spec t nil nil)))
            specs))

  (defun generate-prop-keywords (specs)
    "Generate keyword arguments for defun from prop specs."
    (mapcar (lambda (spec)
              (let ((name (if (consp spec) (car spec) spec))
                    (default (when (consp spec) (getf (cdr spec) :default))))
                (if default
                    `(,name ,default)
                    name)))
            specs))

  (defun generate-inspect-pairs (prop-names)
    "Generate plist for :inspect message from prop names."
    (mapcan (lambda (name)
              `(,(intern (symbol-name name) :keyword) ,name))
            prop-names)))

;;; ============================================================================
;;; DEFCOMPONENT-WITH-PROPS
;;; ============================================================================

(defmacro! defcomponent-with-props (name (&rest prop-specs) &body body)
  "Define a component with validated props and children support.

   PROP-SPECS: List of prop specifications, each either:
     - A symbol (name only, any type, optional)
     - A list: (name :type TYPE :default VALUE :required BOOL)

   BODY: Component render code. Within the body:
     - All props are bound as local variables
     - CHILDREN is bound to rendered child content (string or nil)

   Example:
   (defcomponent-with-props card ((title :type string :required t)
                                  (variant :type keyword :default :primary))
     (htm-str
       (:div :class (classes \"card\" (format nil \"card-~A\" variant))
         (:h2 (cl-who:esc title))
         (cl-who:str (or children \"\")))))

   Usage:
   (card :title \"My Card\" :children (htm-str (:p \"Card content\")))"
  (let* ((prop-names (mapcar #'extract-prop-name prop-specs))
         (prop-keywords (generate-prop-keywords prop-specs))
         (inspect-pairs (generate-inspect-pairs prop-names)))
    `(defun ,name (&key ,@prop-keywords (children nil ,g!children-supplied))
       (declare (ignorable ,g!children-supplied))
       ;; Validate props at runtime
       (let ((,g!errors (validate-props ',prop-specs
                                        (list ,@(mapcan (lambda (n) `(,(intern (symbol-name n) :keyword) ,n))
                                                        prop-names)))))
         (when ,g!errors
           (error "Invalid props for ~A: ~{~A~^, ~}" ',name ,g!errors)))
       ;; Bind children to empty string if not provided
       (let ((children (if ,g!children-supplied children "")))
         ;; Create component using pandoriclet for Surgery compatibility
         (pandoriclet (,@(mapcar (lambda (n) `(,n ,n)) prop-names)
                       (children children))
           (dlambda
             (:render () ,@body)
             (:props () (list ,@inspect-pairs))
             (:children () children)
             (:inspect () (list :component ',name
                                :props (list ,@inspect-pairs)
                                :children-length (length children)))))))))

;;; ============================================================================
;;; WITH-PROPS - Destructuring helper
;;; ============================================================================

(defmacro with-props (bindings &body body)
  "Destructure props in component body with defaults.

   BINDINGS: List of bindings, each either:
     - A symbol (use as-is)
     - (name default) - use default if name is nil

   Example:
   (with-props ((title \"Untitled\") variant)
     (htm-str (:h1 title)))"
  `(let ,(mapcar (lambda (b)
                   (if (consp b)
                       `(,(car b) (or ,(car b) ,(cadr b)))
                       `(,b ,b)))
                 bindings)
     ,@body))
