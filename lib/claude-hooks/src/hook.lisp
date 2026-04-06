;;;; claude-hooks - Hook Lifecycle
;;;;
;;;; Condition type, blocking signal, run-hook entry point, and defhook macro.

(in-package :claude-hooks)

;;; ---------------------------------------------------------------------------
;;; Condition
;;; ---------------------------------------------------------------------------

(define-condition hook-block-condition (error)
  ((message :initarg :message :reader hook-block-message))
  (:report (lambda (c s)
             (format s "Hook block: ~a" (hook-block-message c)))))

(defun block! (message)
  "Signal a blocking error — caught by RUN-HOOK to exit with code 2."
  (error 'hook-block-condition :message message))

;;; ---------------------------------------------------------------------------
;;; Run Hook
;;; ---------------------------------------------------------------------------

(defun run-hook (handler)
  "Full hook lifecycle: parse stdin JSON, call HANDLER, write stdout, exit.
Exit codes: 0 = success, 1 = non-blocking error, 2 = blocking error."
  (handler-bind
      ((hook-block-condition
         (lambda (c)
           (format *error-output* "~a" (hook-block-message c))
           (finish-output *error-output*)
           (sb-ext:exit :code 2))))
    (let ((input (handler-case
                     (parse-json (read-line *standard-input* nil "{}"))
                   (error ()
                     (format *error-output* "Failed to parse hook input JSON")
                     (finish-output *error-output*)
                     (sb-ext:exit :code 1)))))
      (let ((result (handler-case
                        (funcall handler input)
                      (error (e)
                        ;; hook-block-condition is caught by handler-bind above
                        ;; before reaching here; only non-block errors land here
                        (format *error-output* "Hook error: ~a" e)
                        (finish-output *error-output*)
                        (sb-ext:exit :code 1)))))
        (when result
          (write-string (encode-json result) *standard-output*)
          (finish-output *standard-output*))
        (sb-ext:exit :code 0)))))

;;; ---------------------------------------------------------------------------
;;; Defhook Macro
;;; ---------------------------------------------------------------------------

(defmacro defhook (name (&rest bindings) &body body)
  "Define a Claude Code hook with NAME.
Generates NAME-HANDLER (pure function taking input hash-table)
and NAME (entry point calling run-hook).

BINDINGS are (var &rest keys) forms — each binds VAR to (jref input keys...).
BODY may begin with a docstring and/or declarations.

Example:
  (defhook guard-env ((path \"tool_input\" \"file_path\"))
    \"Block .env writes.\"
    (if (and path (search \".env\" path))
        (pre-tool-deny :reason \"Protected\")
        (empty-response)))"
  (let* ((handler-name (intern (format nil "~a-HANDLER" (symbol-name name))))
         (input-var (gensym "INPUT"))
         (input-sym (intern "INPUT" (symbol-package name)))
         (let-bindings (loop for (var . keys) in bindings
                             collect `(,var (jref ,input-var ,@keys))))
         ;; Extract docstring if present
         (docstring (when (and (stringp (first body))
                               (rest body))
                      (first body)))
         (real-body (if docstring (rest body) body))
         ;; Handler body: always wrap in let that binds INPUT + user bindings
         (all-bindings (cons `(,input-sym ,input-var) let-bindings))
         (handler-body `(let ,all-bindings
                           ,@(when docstring (list docstring))
                           ,@real-body)))
    `(progn
       (defun ,handler-name (,input-var)
         ,@(when docstring (list (format nil "Handler for ~a hook. ~a" name docstring)))
         ,handler-body)
       (defun ,name ()
         ,@(when docstring (list (format nil "~a hook entry point. ~a" name docstring)))
         (run-hook #',handler-name)))))
