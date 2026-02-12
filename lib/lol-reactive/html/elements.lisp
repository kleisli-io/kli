;;;; LOL-REACTIVE HTML Elements
;;;; cl-who shorthand macros and component rendering utilities
;;;;
;;;; GENERIC INFRASTRUCTURE - NO hardcoded colors, fonts, or theme styles.

(in-package :lol-reactive)

;;; ============================================================================
;;; CL-WHO CONFIGURATION
;;; ============================================================================

(setf cl-who:*attribute-quote-char* #\"
      cl-who:*html-empty-tag-aware-p* t)

;;; ============================================================================
;;; CL-WHO SHORTHAND MACROS
;;; ============================================================================

(defmacro htm (&body body)
  "Shorthand for cl-who output to *standard-output*."
  `(cl-who:with-html-output (*standard-output*) ,@body))

(defmacro htm-str (&body body)
  "Generate HTML and return as string."
  `(cl-who:with-html-output-to-string (s) ,@body))

;;; ============================================================================
;;; COMPONENT RENDERING
;;; ============================================================================

(defun render-component (component)
  "Render a component to HTML string."
  (funcall component :render))

(defun component->html (component &key (wrapper t))
  "Convert a component to HTML, optionally wrapping in a container."
  (let ((html (render-component component))
        (id (funcall component :id)))
    (if wrapper
        (cl-who:with-html-output-to-string (s)
          (:div :id id
                :class "component-wrapper"
                :data-component-id id
            (cl-who:str html)))
        html)))

;;; ============================================================================
;;; S-EXPRESSION HIGHLIGHTING (generic utility)
;;; ============================================================================

(defun highlight-sexp (form)
  "Convert a Lisp form to syntax-highlighted HTML.
   Uses CSS classes that apps can style as needed."
  (let ((str (prin1-to-string form)))
    (setf str (cl-ppcre:regex-replace-all
               ":(\\w+)"
               str
               "<span class=\"sexp-keyword\">:\\1</span>"))
    (setf str (cl-ppcre:regex-replace-all
               "\"([^\"]*)\""
               str
               "<span class=\"sexp-string\">\"\\1\"</span>"))
    (setf str (cl-ppcre:regex-replace-all
               "\\b(\\d+)\\b"
               str
               "<span class=\"sexp-number\">\\1</span>"))
    str))
