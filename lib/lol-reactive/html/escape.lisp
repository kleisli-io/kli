;;;; LOL-REACTIVE HTML Escape
;;;; XSS prevention utilities (aliases to security.lisp functions)

(in-package :lol-reactive)

;;; ============================================================================
;;; HTML ESCAPING (XSS Prevention)
;;;
;;; These are convenient aliases for the sanitization functions in security.lisp.
;;; The actual implementations live there to avoid circular dependencies.
;;; ============================================================================

(defun escape-html (string)
  "Escape HTML special characters to prevent XSS.
   Alias for sanitize-html from security.lisp.

   Example:
     (escape-html \"<script>alert('xss')</script>\")
     => \"&lt;script&gt;alert('xss')&lt;/script&gt;\""
  (sanitize-html string))

(defmacro safe-str (expr)
  "Output escaped string in cl-who context.
   Use this when outputting user-provided content.

   Example (in cl-who):
     (:p (safe-str user-input))"
  `(cl-who:str (escape-html ,expr)))

(defmacro safe-fmt (control-string &rest args)
  "Format and escape for safe HTML output.

   Example:
     (safe-fmt \"Hello, ~A!\" username)"
  `(cl-who:str (escape-html (format nil ,control-string ,@args))))
