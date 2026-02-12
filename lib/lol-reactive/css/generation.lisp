;;;; css/generation.lisp - CSS generation utilities
;;;;
;;;; PURPOSE:
;;;;   Low-level CSS generation functions for creating rules, sections,
;;;;   and keyframes from Lisp data structures.
;;;;
;;;; These functions work standalone or with css-modules:
;;;;
;;;;   ;; Standalone usage
;;;;   (css-rule ".btn" '(("padding" . "1rem")))
;;;;   => ".btn { padding: 1rem; }"
;;;;
;;;;   ;; With css-module
;;;;   (let ((module (make-css-module :buttons)))
;;;;     (funcall module :add-rule ".btn" '(("padding" . "1rem")))
;;;;     (funcall module :render))

(in-package :lol-reactive)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Rule Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-rule (selector properties)
  "Generate a CSS rule from selector and property alist.

   SELECTOR: CSS selector string (e.g., \".btn\", \"#header\", \"body\")
   PROPERTIES: Alist of (property . value) pairs

   Example:
   (css-rule \".btn\" '((\"padding\" . \"1rem\") (\"margin\" . \"0\")))
   => \".btn { padding: 1rem; margin: 0; }\""
  (format nil "~A { ~{~A: ~A;~^ ~} }"
          selector
          (mapcan (lambda (pair)
                    (list (car pair) (cdr pair)))
                  properties)))

(defun css-rules (selector &rest property-pairs)
  "Generate CSS rule with inline property pairs.

   SELECTOR: CSS selector string
   PROPERTY-PAIRS: Alternating property names and values

   Example:
   (css-rules \".btn\" \"padding\" \"1rem\" \"margin\" \"0\")
   => \".btn { padding: 1rem; margin: 0; }\""
  (css-rule selector
            (loop for (prop val) on property-pairs by #'cddr
                  collect (cons prop val))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Sections
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-section (name &rest rules)
  "Group CSS rules under a named section comment.

   NAME: Section name for the comment
   RULES: CSS rule strings to group

   Example:
   (css-section \"Buttons\"
     (css-rule \".btn\" '((\"padding\" . \"1rem\"))))
   => \"/* --- Buttons --- */
       .btn { padding: 1rem; }\""
  (format nil "/* --- ~A --- */~%~{~A~^~%~}" name rules))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Keyframes
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-keyframes (name &rest frames)
  "Generate CSS @keyframes animation.

   NAME: Animation name
   FRAMES: List of (percentage . properties-alist) pairs

   Example:
   (css-keyframes \"fade-in\"
     '(\"0%\" . ((\"opacity\" . \"0\")))
     '(\"100%\" . ((\"opacity\" . \"1\"))))
   => \"@keyframes fade-in { 0% { opacity: 0; } 100% { opacity: 1; } }\""
  (format nil "@keyframes ~A { ~{~A~^ ~} }"
          name
          (mapcar (lambda (frame)
                    (format nil "~A { ~{~A: ~A;~^ ~} }"
                            (car frame)
                            (mapcan (lambda (p) (list (car p) (cdr p)))
                                    (cdr frame))))
                  frames)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Media Queries
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-media (query &rest rules)
  "Generate CSS @media query block.

   QUERY: Media query string (e.g., \"(min-width: 768px)\")
   RULES: CSS rule strings to include

   Example:
   (css-media \"(min-width: 768px)\"
     (css-rule \".container\" '((\"max-width\" . \"1200px\"))))
   => \"@media (min-width: 768px) { .container { max-width: 1200px; } }\""
  (format nil "@media ~A { ~{~A~^ ~} }" query rules))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Variables
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-var (name)
  "Reference a CSS custom property (variable).

   NAME: Variable name (without -- prefix)
   Returns: var(--name) string

   Example:
   (css-var \"primary\") => \"var(--primary)\""
  (format nil "var(--~A)" name))

(defun css-var-definition (name value)
  "Generate a CSS custom property definition.

   NAME: Variable name (without -- prefix)
   VALUE: Variable value

   Example:
   (css-var-definition \"primary\" \"#00FF41\") => \"--primary: #00FF41\""
  (format nil "--~A: ~A" name value))
