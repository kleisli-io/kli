;;; MCP Framework - Hook System
;;; Before/after hooks for tool calls

(in-package #:mcp-framework)

;;; Hook variables

(defvar *before-tool-call-hooks* nil
  "List of functions called before each tool call.
   Each function receives (tool-name args) where args is a hash-table.")

(defvar *after-tool-call-hooks* nil
  "List of functions called after each tool call.
   Each function receives (tool-name args result) where result is the tool's return value.")

;;; Hook management

(defun add-before-tool-hook (hook)
  "Add a hook function to be called before tool execution.
   HOOK should be a function of (tool-name args)."
  (pushnew hook *before-tool-call-hooks*))

(defun add-after-tool-hook (hook)
  "Add a hook function to be called after tool execution.
   HOOK should be a function of (tool-name args result)."
  (pushnew hook *after-tool-call-hooks*))

(defun remove-before-tool-hook (hook)
  "Remove a before-tool hook."
  (setf *before-tool-call-hooks* (remove hook *before-tool-call-hooks*)))

(defun remove-after-tool-hook (hook)
  "Remove an after-tool hook."
  (setf *after-tool-call-hooks* (remove hook *after-tool-call-hooks*)))

(defun clear-tool-hooks ()
  "Remove all tool hooks."
  (setf *before-tool-call-hooks* nil
        *after-tool-call-hooks* nil))

;;; Hook execution

(defun run-before-hooks (tool-name args)
  "Run all before-tool hooks. Any hook can signal to abort."
  (dolist (hook *before-tool-call-hooks*)
    (funcall hook tool-name args)))

(defun run-after-hooks (tool-name args result)
  "Run all after-tool hooks."
  (dolist (hook *after-tool-call-hooks*)
    (funcall hook tool-name args result)))

(defun call-with-hooks (tool-name args thunk)
  "Call THUNK with before/after hooks around it.
   Returns the result of THUNK."
  (run-before-hooks tool-name args)
  (let ((result (funcall thunk)))
    (run-after-hooks tool-name args result)
    result))
