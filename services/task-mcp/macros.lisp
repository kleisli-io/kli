(in-package #:task-mcp)

;;; Session-Aware Tool Macros
;;;
;;; Three tiers of tool definitions for task-mcp:
;;;
;;;   define-tool         — Plain MCP tool (no session context needed)
;;;   define-session-tool — Requires session context (calls ensure-session-context)
;;;   define-task-tool    — Requires active task (calls require-current-task)
;;;
;;; These wrap mcp-framework:define-tool and inject the appropriate
;;; session management calls before the tool body executes.

(defmacro define-session-tool (name (&rest params) &body body)
  "Define an MCP tool that requires session context.
   Injects (ensure-session-context) before the tool body.
   Use for tools that read *current-task-id* but don't require it."
  (let ((docstring (when (stringp (first body)) (pop body))))
    `(define-tool ,name (,@params)
       ,@(when docstring (list docstring))
       (ensure-session-context)
       ,@body)))

(defmacro define-task-tool (name (&rest params) &body body)
  "Define an MCP tool that requires an active task.
   Injects (require-current-task) before the tool body.
   require-current-task calls ensure-session-context internally,
   then errors if *current-task-id* is still nil."
  (let ((docstring (when (stringp (first body)) (pop body))))
    `(define-tool ,name (,@params)
       ,@(when docstring (list docstring))
       (require-current-task)
       ,@body)))
