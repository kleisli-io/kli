;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; optimization/template-validation.lisp - Compile-Time Template Validation
;;;;
;;;; PURPOSE:
;;;;   Validate HTML templates, CSS classes, and design tokens at compile time.
;;;;   Catches errors early with helpful "Did you mean?" suggestions.
;;;;
;;;; KEY MACROS:
;;;;   DEFVALIDATED-TEMPLATE - Define a template with compile-time validation
;;;;
;;;; VALIDATION TYPES:
;;;;   - HTML structure (valid nesting, required attributes)
;;;;   - CSS classes (registered in design system)
;;;;   - Design tokens (colors, spacing, typography)
;;;;   - XSS prevention (no raw interpolation in unsafe contexts)
;;;;
;;;; DESIGN:
;;;;   All validation happens at macro-expansion time.
;;;;   Runtime code is just the validated template - zero overhead.

(in-package :lol-reactive)

;;; ============================================================================
;;; HTML VALIDATION
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *void-elements*
    '(:area :base :br :col :embed :hr :img :input :link :meta
      :param :source :track :wbr)
    "HTML void elements that cannot have children.")

  (defparameter *valid-html-elements*
    '(:a :abbr :address :area :article :aside :audio :b :base :bdi :bdo
      :blockquote :body :br :button :canvas :caption :cite :code :col
      :colgroup :data :datalist :dd :del :details :dfn :dialog :div :dl
      :dt :em :embed :fieldset :figcaption :figure :footer :form :h1 :h2
      :h3 :h4 :h5 :h6 :head :header :hgroup :hr :html :i :iframe :img
      :input :ins :kbd :label :legend :li :link :main :map :mark :menu
      :meta :meter :nav :noscript :object :ol :optgroup :option :output
      :p :param :picture :pre :progress :q :rp :rt :ruby :s :samp :script
      :section :select :slot :small :source :span :strong :style :sub
      :summary :sup :table :tbody :td :template :textarea :tfoot :th :thead
      :time :title :tr :track :u :ul :var :video :wbr)
    "Valid HTML5 elements.")

  (defparameter *required-attributes*
    '((:img . (:src :alt))
      (:a . (:href))
      (:input . (:type))
      (:link . (:rel :href))
      (:script . ()))  ; src is optional for inline scripts
    "Required attributes for specific elements.")

  (defun validate-html-element (tag)
    "Check if TAG is a valid HTML element."
    (unless (member tag *valid-html-elements*)
      (let ((closest (find-closest-match tag
                       (mapcar (lambda (e) (cons e nil)) *valid-html-elements*))))
        (error "Invalid HTML element: ~A. Did you mean: ~A?" tag closest)))
    t)

  (defun validate-void-element-children (tag children)
    "Check that void elements don't have children."
    (when (and (member tag *void-elements*) children)
      (error "Void element ~A cannot have children." tag))
    t)

  (defun validate-required-attributes (tag attrs)
    "Check that required attributes are present."
    (let ((required (cdr (assoc tag *required-attributes*))))
      (dolist (attr required)
        (unless (member attr attrs)
          (error "Element ~A requires attribute ~A" tag attr))))
    t)

  (defun extract-attrs-from-sexp (sexp)
    "Extract attribute keywords from cl-who style s-expression.
     (:div :class \"foo\" :id \"bar\" \"content\") -> (:class :id)"
    (let ((attrs nil))
      (labels ((walk (rest)
                 (when (and rest (keywordp (car rest)))
                   (push (car rest) attrs)
                   (when (cdr rest)
                     (walk (cddr rest))))))
        (when (consp sexp)
          (walk (cdr sexp))))
      attrs))

  (defun validate-html-sexp (sexp)
    "Validate a cl-who style HTML s-expression at compile time.
     Returns T if valid, signals error otherwise."
    (cond
      ;; String or other atom - valid content
      ((atom sexp) t)
      ;; cl-who special forms
      ((member (car sexp) '(cl-who:str cl-who:esc cl-who:htm cl-who:fmt)) t)
      ;; Regular element
      ((keywordp (car sexp))
       (let ((tag (car sexp))
             (rest (cdr sexp)))
         ;; Validate element name
         (validate-html-element tag)
         ;; Extract children (skip attribute pairs)
         (let ((children (remove-if (lambda (x)
                                      (or (keywordp x)
                                          (and (consp x) (keywordp (car x))
                                               (not (member (car x) *valid-html-elements*)))))
                                    rest)))
           ;; Validate void elements
           (validate-void-element-children tag children)
           ;; Recursively validate children
           (dolist (child children)
             (validate-html-sexp child)))))
      ;; List starting with non-keyword - could be function call
      (t t))))

;;; ============================================================================
;;; CSS CLASS VALIDATION
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *registered-css-classes* (make-hash-table :test 'equal)
    "Registry of known CSS classes from design system.")

  (defun register-css-class (class-name &optional source)
    "Register a CSS class as valid."
    (setf (gethash class-name *registered-css-classes*) source))

  (defun validate-css-class (class-name &key (strict nil))
    "Validate CSS class exists in registry.
     STRICT: If T, error on unknown class. If NIL, just warn."
    (unless (gethash class-name *registered-css-classes*)
      (let ((all-classes nil))
        (maphash (lambda (k v) (declare (ignore v)) (push k all-classes))
                 *registered-css-classes*)
        (if (null all-classes)
            ;; No classes registered - skip validation
            t
            ;; Find closest match
            (let ((closest (find-closest-match
                            (intern (string-upcase class-name) :keyword)
                            (mapcar (lambda (c) (cons (intern (string-upcase c) :keyword) nil))
                                    all-classes))))
              (if strict
                  (error "Unknown CSS class: ~A. Did you mean: ~A?" class-name closest)
                  (warn "Unknown CSS class: ~A. Did you mean: ~A?" class-name closest))))))
    t)

  (defun extract-classes-from-sexp (sexp)
    "Extract all :class values from an HTML s-expression."
    (let ((classes nil))
      (labels ((walk (x)
                 (when (consp x)
                   (when (and (keywordp (car x))
                              (member :class x))
                     ;; Find :class value
                     (let ((pos (position :class x)))
                       (when (and pos (< (1+ pos) (length x)))
                         (let ((val (nth (1+ pos) x)))
                           (when (stringp val)
                             (dolist (c (cl-ppcre:split "\\s+" val))
                               (push c classes)))))))
                   ;; Walk children
                   (dolist (child x)
                     (walk child)))))
        (walk sexp))
      classes))

  (defun validate-css-classes-in-sexp (sexp &key (strict nil))
    "Validate all CSS classes in an HTML s-expression."
    (dolist (class (extract-classes-from-sexp sexp))
      (validate-css-class class :strict strict))
    t))

;;; ============================================================================
;;; DESIGN TOKEN VALIDATION
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun validate-token-usage (form)
    "Validate that design token functions are called with valid tokens.
     Walks the form looking for (get-color :x), (get-spacing :x), etc."
    (labels ((walk (x)
               (cond
                 ((atom x) t)
                 ;; Check for token accessor calls
                 ((and (consp x)
                       (member (car x) '(get-color get-font get-spacing get-effect)))
                  (let ((token (cadr x)))
                    (when (and (consp token) (eq (car token) 'quote)
                               (keywordp (cadr token)))
                      ;; Validate token at compile time
                      (ecase (car x)
                        (get-color (validate-token (cadr token) *colors* "color"))
                        (get-font (validate-token (cadr token) *typography* "font"))
                        (get-spacing (validate-token (cadr token) *spacing* "spacing"))
                        (get-effect (validate-token (cadr token) *effects* "effect")))))
                  t)
                 ;; Recurse into sublists
                 (t (dolist (child x t)
                      (walk child))))))
      (walk form)
      t)))

;;; ============================================================================
;;; XSS PREVENTION VALIDATION
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dangerous-contexts*
    '(:onclick :onload :onerror :onmouseover :href :src :action :style)
    "Attribute contexts where unescaped interpolation is dangerous.")

  (defun find-dangerous-interpolation (sexp)
    "Find potentially dangerous unescaped interpolation in dangerous contexts.
     Returns list of warnings."
    (let ((warnings nil))
      (labels ((walk (x context)
                 (cond
                   ((atom x) nil)
                   ;; Check if we're in a dangerous attribute
                   ((and context (member context *dangerous-contexts*)
                         (consp x)
                         (not (member (car x) '(cl-who:esc escape-html safe-str))))
                    ;; Found unescaped form in dangerous context
                    (push (format nil "Potentially unsafe: ~A in ~A context" x context)
                          warnings))
                   ;; Walk HTML element
                   ((and (consp x) (keywordp (car x)))
                    (let ((in-attr nil))
                      (dolist (item (cdr x))
                        (cond
                          ;; Keyword - entering attribute
                          ((keywordp item)
                           (setf in-attr item))
                          ;; Value after keyword - check context
                          (in-attr
                           (walk item in-attr)
                           (setf in-attr nil))
                          ;; Child element
                          (t (walk item nil))))))
                   ;; Other list - recurse
                   (t (dolist (child x)
                        (walk child nil))))))
        (walk sexp nil))
      warnings)))

;;; ============================================================================
;;; DEFVALIDATED-TEMPLATE MACRO
;;; ============================================================================

(defmacro defvalidated-template (name (&rest args) &body body)
  "Define a template function with compile-time validation.

   Validates at macro-expansion time:
   - HTML structure (element names, void elements, nesting)
   - CSS classes (if registered in design system)
   - Design tokens (colors, spacing, typography, effects)
   - XSS patterns (warns about dangerous interpolation)

   ARGS: Function arguments (like defun)
   BODY: Template body using cl-who style s-expressions

   Example:
     (defvalidated-template card (title content &key variant)
       (htm-str
         (:div :class (classes \"card\" (format nil \"card-~A\" variant))
           (:h2 (cl-who:esc title))
           (:div :class \"card-content\"
             (cl-who:str content)))))

   Compile-time checks:
   - :div, :h2 are valid elements
   - (cl-who:esc title) properly escapes user input
   - Token references are validated

   At runtime, just executes the template - zero validation overhead."
  ;; Perform compile-time validation
  (dolist (form body)
    ;; Validate HTML structure
    (when (and (consp form)
               (or (keywordp (car form))
                   (member (car form) '(htm htm-str))))
      (let ((html-form (if (member (car form) '(htm htm-str))
                           (cadr form)
                           form)))
        (validate-html-sexp html-form)
        (validate-css-classes-in-sexp html-form :strict nil)
        (let ((xss-warnings (find-dangerous-interpolation html-form)))
          (dolist (warning xss-warnings)
            (warn "~A in template ~A: ~A" "XSS Warning" name warning)))))
    ;; Validate token usage
    (validate-token-usage form))
  ;; Generate the runtime function (no validation overhead)
  `(defun ,name ,args
     ,@body))

;;; ============================================================================
;;; TEMPLATE COMPOSITION HELPERS
;;; ============================================================================

(defmacro template-fragment ((&rest args) &body body)
  "Define an inline template fragment with validation.
   Like defvalidated-template but returns a lambda."
  ;; Perform same compile-time validation
  (dolist (form body)
    (when (and (consp form)
               (or (keywordp (car form))
                   (member (car form) '(htm htm-str))))
      (let ((html-form (if (member (car form) '(htm htm-str))
                           (cadr form)
                           form)))
        (validate-html-sexp html-form))))
  `(lambda ,args ,@body))

(defmacro validate-template (&body body)
  "Validate template forms without defining a function.
   Useful for checking templates in tests or REPL."
  (dolist (form body)
    (when (and (consp form)
               (or (keywordp (car form))
                   (member (car form) '(htm htm-str))))
      (let ((html-form (if (member (car form) '(htm htm-str))
                           (cadr form)
                           form)))
        (validate-html-sexp html-form)
        (validate-css-classes-in-sexp html-form :strict nil))))
  `(progn ,@body))

;;; ============================================================================
;;; CSS CLASS REGISTRATION UTILITIES
;;; ============================================================================

(defun register-tailwind-classes ()
  "Register common Tailwind CSS classes for validation."
  (dolist (prefix '("flex" "grid" "block" "inline" "hidden"
                    "p-" "m-" "px-" "py-" "mx-" "my-"
                    "w-" "h-" "min-w-" "min-h-" "max-w-" "max-h-"
                    "text-" "font-" "bg-" "border-" "rounded-"
                    "shadow-" "hover:" "focus:" "active:"
                    "sm:" "md:" "lg:" "xl:" "2xl:"))
    (register-css-class prefix :tailwind)))

(defun register-component-classes (component-name classes)
  "Register CSS classes for a component."
  (dolist (class classes)
    (register-css-class class component-name)))

(defun list-registered-classes ()
  "List all registered CSS classes."
  (let ((classes nil))
    (maphash (lambda (k v)
               (push (cons k v) classes))
             *registered-css-classes*)
    classes))
