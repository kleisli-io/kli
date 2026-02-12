;;;; css/tailwind.lisp - Tailwind CSS class generation helpers
;;;;
;;;; PURPOSE:
;;;;   Utilities for generating Tailwind CSS classes from design tokens.
;;;;   Provides clean DSL for combining classes and token-aware generators.
;;;;
;;;; USAGE:
;;;;   (classes "p-4" "bg-black" nil "text-white")  ; => "p-4 bg-black text-white"
;;;;   (tw-color "bg" :primary)                      ; => "bg-primary"
;;;;   (tw-spacing "p" :4)                           ; => "p-4"

(in-package :lol-reactive)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; String Utilities
;;; ─────────────────────────────────────────────────────────────────────────────

(defun null-or-empty-p (x)
  "Return T if X is nil or an empty string."
  (or (null x)
      (and (stringp x) (string= x ""))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Class Composition
;;; ─────────────────────────────────────────────────────────────────────────────

(defun classes (&rest class-strings)
  "Combine class strings, filtering nil/empty values.
   Flattens nested lists for convenient conditional composition.

   Examples:
   (classes \"p-4\" nil \"bg-black\")        ; => \"p-4 bg-black\"
   (classes \"base\" (when cond \"extra\"))  ; => \"base extra\" or \"base\"
   (classes (list \"a\" \"b\") \"c\")         ; => \"a b c\""
  (format nil "~{~A~^ ~}"
          (remove-if #'null-or-empty-p
                     (alexandria:flatten class-strings))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Token-Based Class Generators
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tw-color (prefix key)
  "Generate Tailwind color class from token key.
   PREFIX: Tailwind prefix (\"bg\", \"text\", \"border\", etc.)
   KEY: Color token keyword

   Examples:
   (tw-color \"bg\" :primary)     ; => \"bg-primary\"
   (tw-color \"text\" :muted)     ; => \"text-muted\"
   (tw-color \"border\" :error)   ; => \"border-error\""
  (format nil "~A-~A" prefix (string-downcase (symbol-name key))))

(defun tw-spacing (prefix key)
  "Generate Tailwind spacing class from token key.
   PREFIX: Tailwind prefix (\"p\", \"m\", \"gap\", \"px\", \"py\", etc.)
   KEY: Spacing token keyword (numeric)

   Examples:
   (tw-spacing \"p\" :4)   ; => \"p-4\"
   (tw-spacing \"mx\" :8)  ; => \"mx-8\"
   (tw-spacing \"gap\" :6) ; => \"gap-6\""
  (format nil "~A-~A" prefix (string-downcase (symbol-name key))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Convenience Generators
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tw-bg (key)
  "Generate background color class. (tw-bg :primary) => \"bg-primary\""
  (tw-color "bg" key))

(defun tw-text (key)
  "Generate text color class. (tw-text :muted) => \"text-muted\""
  (tw-color "text" key))

(defun tw-border (key)
  "Generate border color class. (tw-border :error) => \"border-error\""
  (tw-color "border" key))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Arbitrary Value Classes (for token values)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tw-arbitrary (prefix value)
  "Generate Tailwind arbitrary value class.
   PREFIX: Tailwind prefix
   VALUE: Literal CSS value

   Examples:
   (tw-arbitrary \"bg\" \"#FF0000\")    ; => \"bg-[#FF0000]\"
   (tw-arbitrary \"w\" \"clamp(1rem, 5vw, 3rem)\") ; => \"w-[clamp(1rem,5vw,3rem)]\""
  (format nil "~A-[~A]" prefix
          ;; Remove spaces for Tailwind's arbitrary value syntax
          (remove #\Space value)))

(defun tw-bg-value (key)
  "Generate background class with token value.
   (tw-bg-value :primary) => \"bg-[#00FF41]\""
  (tw-arbitrary "bg" (get-color key)))

(defun tw-text-value (key)
  "Generate text class with token value.
   (tw-text-value :muted) => \"text-[#9EB3C8]\""
  (tw-arbitrary "text" (get-color key)))

(defun tw-border-value (key)
  "Generate border class with token value.
   (tw-border-value :error) => \"border-[#FF3333]\""
  (tw-arbitrary "border" (get-color key)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tailwind Configuration Generation (Parenscript)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tailwind-config (&key (colors *colors*) (typography *typography*))
  "Generate Tailwind CDN configuration script via Parenscript.
   Extends Tailwind's theme with current design tokens.
   NO hardcoded values - all from token system."
  (let ((color-pairs (mapcan (lambda (pair)
                               (list (alexandria:make-keyword
                                      (string-downcase (symbol-name (car pair))))
                                     (cdr pair)))
                             colors))
        ;; Extract font family from typography tokens (required)
        (font-family (or (cdr (assoc :family typography))
                         (error "Typography token :family is required. Set *typography* before calling tailwind-config."))))
    (parenscript:ps*
      `(setf tailwind.config
             (ps:create
              :theme (ps:create
                      :extend (ps:create
                               :colors (ps:create ,@color-pairs)
                               :font-family (ps:create
                                             :sans (array ,font-family)))))))))
