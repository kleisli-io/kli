;;;; css/registry.lisp - CSS module system using Let Over Lambda patterns
;;;;
;;;; PURPOSE:
;;;;   CSS modules as pandoric closures with introspectable state.
;;;;   Each module responds to messages (:render, :rules, :add-rule, :inspect).
;;;;   Global registry collects modules in load order.
;;;;
;;;; USAGE:
;;;;   ;; Create a CSS module
;;;;   (defvar *button-css*
;;;;     (make-css-module :buttons
;;;;       '(".btn" . (("padding" . "1rem") ("margin" . "0")))
;;;;       '(".btn:hover" . (("opacity" . "0.8")))))
;;;;
;;;;   (funcall *button-css* :render)   ; Generate CSS string
;;;;   (funcall *button-css* :rules)    ; Get rule list
;;;;   (funcall *button-css* :inspect)  ; Full introspection
;;;;
;;;;   (generate-all-component-css)     ; Render all registered modules

(in-package :lol-reactive)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Global Registry State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *component-css-registry* (make-hash-table :test 'eq)
  "Hash table mapping module names (keywords) to CSS module closures.")

(defvar *css-load-order* nil
  "List of module names in registration order (most recent first).
   Reversed during generation to maintain load order in output.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Module Factory (Let Over Lambda Pattern)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-css-module (name &rest initial-rules)
  "Create a CSS module with pandoric introspection.

   NAME: Keyword identifying the module (e.g., :buttons)
   INITIAL-RULES: List of (selector . properties-alist) pairs

   Returns a dlambda responding to:
     :name      - Get module name
     :rules     - Get all rules
     :add-rule  - Add (selector . properties) rule
     :render    - Generate CSS string
     :inspect   - Full state dump

   Example:
   (make-css-module :buttons
     '(\".btn\" . ((\"padding\" . \"1rem\")))
     '(\".btn:hover\" . ((\"opacity\" . \"0.8\"))))"
  (let ((module-name name)
        (rules (copy-list initial-rules))
        (created-at (get-universal-time)))

    ;; Auto-register in global registry
    (setf (gethash name *component-css-registry*)
          (lambda (message &rest args)
            (case message
              (:name module-name)

              (:rules rules)

              (:add-rule
               (destructuring-bind (selector properties) args
                 (push (cons selector properties) rules)
                 selector))

              (:remove-rule
               (let ((selector (first args)))
                 (setf rules (remove-if (lambda (r) (string= (car r) selector)) rules))
                 selector))

              (:render
               (with-output-to-string (out)
                 (dolist (rule rules)
                   (let ((selector (car rule))
                         (props (cdr rule)))
                     (cond
                       ;; Handle @keyframes specially
                       ;; Format: ("@keyframes name" (("from" . ((prop . val)...)) ("to" . ...)))
                       ((and (stringp selector)
                             (>= (length selector) 11)
                             (string= (subseq selector 0 11) "@keyframes "))
                        (format out "~A { ~{~A~^ ~} }~%"
                                selector
                                (mapcar (lambda (frame)
                                          (format nil "~A { ~{~A: ~A;~^ ~} }"
                                                  (car frame)
                                                  (mapcan (lambda (p) (list (car p) (cdr p)))
                                                          (cdr frame))))
                                        props)))
                       ;; Handle @media queries
                       ;; Format: ("@media (...)" ((".sel" (("prop" . "val")...)) ...))
                       ((and (stringp selector)
                             (>= (length selector) 6)
                             (string= (subseq selector 0 6) "@media"))
                        (format out "~A {~%" selector)
                        (dolist (inner-rule props)
                          (let ((inner-sel (first inner-rule))
                                (inner-props (second inner-rule)))
                            (format out "  ~A { ~{~A: ~A;~^ ~} }~%"
                                    inner-sel
                                    (mapcan (lambda (p) (list (car p) (cdr p)))
                                            inner-props))))
                        (format out "}~%"))
                       ;; Regular CSS rule
                       (t
                        (format out "~A { ~{~A: ~A;~^ ~} }~%"
                                selector
                                (mapcan (lambda (p) (list (car p) (cdr p)))
                                        props))))))))

              (:inspect
               (list :name module-name
                     :created-at created-at
                     :rule-count (length rules)
                     :rules rules))

              (otherwise
               (error "Unknown CSS module message: ~S" message)))))

    ;; Track load order
    (unless (member name *css-load-order*)
      (push name *css-load-order*))

    ;; Return the module
    (gethash name *component-css-registry*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Registry Functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-css-module (name)
  "Get a CSS module by name."
  (gethash name *component-css-registry*))

(defun get-component-css (name)
  "Get rendered CSS for a single module."
  (let ((module (get-css-module name)))
    (when module
      (funcall module :render))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun generate-all-component-css ()
  "Generate CSS from all registered modules in load order.

   Returns concatenated CSS string with section headers for each module."
  (with-output-to-string (out)
    (dolist (name (reverse *css-load-order*))
      (let ((module (get-css-module name)))
        (when module
          (let ((css (funcall module :render)))
            (when (and css (> (length css) 0))
              (format out "~&/* ═══ ~A ═══ */~%" name)
              (format out "~A~%" css))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Convenience Macro
;;; ─────────────────────────────────────────────────────────────────────────────

(defmacro defcss (name &body rules)
  "Define a CSS module at load time.

   NAME: Keyword identifying the module
   RULES: (selector properties-alist) pairs

   Example:
   (defcss :buttons
     (\".btn\" ((\"padding\" . \"1rem\")))
     (\".btn:hover\" ((\"opacity\" . \"0.8\"))))"
  (let ((rule-forms (mapcar (lambda (rule)
                              `(cons ,(first rule) ',(second rule)))
                            rules)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (make-css-module ,name ,@rule-forms))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Testing & Introspection
;;; ─────────────────────────────────────────────────────────────────────────────

(defun clear-css-registry ()
  "Clear the CSS registry. Used in tests."
  (clrhash *component-css-registry*)
  (setf *css-load-order* nil)
  t)

(defun list-registered-css-components ()
  "List all registered module names in load order."
  (reverse *css-load-order*))

(defun inspect-css-registry ()
  "Inspect all registered CSS modules."
  (mapcar (lambda (name)
            (let ((module (get-css-module name)))
              (when module
                (funcall module :inspect))))
          (reverse *css-load-order*)))
