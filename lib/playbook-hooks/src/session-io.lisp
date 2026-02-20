;;;; playbook-hooks - Session I/O
;;;;
;;;; Read activated pattern IDs from per-session playbook state files.

(in-package :playbook-hooks)

(defun read-activated-ids (cwd session-id)
  "Read unique activated pattern IDs from the session's activated.jsonl.
Each line is a JSON array of pattern IDs. Returns a flat deduplicated list.
Returns NIL if the file does not exist or is empty."
  (let ((path (format nil "~a/.claude/sessions/~a/playbook/activated.jsonl"
                      cwd session-id))
        (ids nil))
    (dolist (line (claude-hooks:read-lines path))
      (handler-case
          (let ((parsed (yason:parse line)))
            (when (listp parsed)
              (dolist (id parsed)
                (when (stringp id)
                  (pushnew id ids :test #'string=)))))
        (error () nil)))
    (nreverse ids)))
