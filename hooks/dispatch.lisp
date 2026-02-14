;;;; dispatch.lisp - dlambda hook dispatcher for kli
;;;;
;;;; O(1) message-passing dispatch from hook name string to handler function.
;;;; Each handler is a plain defun in the :kli-hook package.
;;;; The dispatcher manages stdin/stdout/exit lifecycle.

(in-package #:kli-hook)

(defvar *hook-dispatch*
  (lol:dlambda
    (:session-start      (input) (session-start-handler input))
    (:session-leave      (input) (session-leave-handler input))
    (:tool-call          (input) (tool-call-handler input))
    (:session-task-write (input) (session-task-write-handler input))
    (:file-conflict      (input) (file-conflict-handler input))
    (:playbook-activate  (input) (playbook-activate-handler input))
    (:feedback-nudge     (input) (feedback-nudge-handler input))
    (t (&rest args)      (error "Unknown hook: ~a" (car args)))))

(defun hook-main (name)
  "Read stdin JSON, dispatch to named handler, write stdout JSON.
   Exit codes: 0 = success, 1 = error, 2 = block (hook-block-condition)."
  (handler-case
      (let* ((input (parse-json (read-line *standard-input* nil "{}")))
             (key (intern (string-upcase name) :keyword))
             (result (funcall *hook-dispatch* key input)))
        (when result
          (write-string (encode-json result) *standard-output*)
          (finish-output *standard-output*)))
    (hook-block-condition (c)
      (format *error-output* "~a" c)
      (finish-output *error-output*)
      (uiop:quit 2))
    (error (e)
      (format *error-output* "Hook error: ~a" e)
      (finish-output *error-output*)
      (uiop:quit 1))))
