;;; MCP Framework - Pandoric Tool System
;;; Self-modifying tools with external state access

(in-package #:mcp-framework)

;;; Tool registry

(defvar *tool-registry* (make-hash-table :test 'equal)
  "Global registry of MCP tools, keyed by MCP wire name.")

(defvar *tool-registry-lock* (make-lock "tool-registry")
  "Lock for thread-safe tool registry access.")

;;; Pandoric tool creation

(defun make-pandoric-tool (name behavior &key documentation schema mcp-name)
  "Create a pandoric tool with externally-accessible internals.

   The tool responds to these methods:
   - :call args... - Execute the tool
   - :inspect - Return metadata about the tool
   - :evolve new-behavior &optional reason - Hot-swap behavior
   - :devolve - Rollback to previous behavior
   - :schema - Return JSON Schema
   - :mcp-name - Return wire name
   - :export-state - Export serializable state
   - :import-state state - Import state (call-count, etc.)
   - t args... - Default: same as :call"
  (let ((wire-name (or mcp-name (lisp-name-to-mcp-name name))))
    (pandoriclet ((name name)
                  (behavior behavior)
                  (behavior-history '())
                  (schema schema)
                  (documentation documentation)
                  (wire-name wire-name)
                  (call-count 0)
                  (last-called nil)
                  (created-at (get-universal-time)))
      (dlambda
        (:call (&rest args)
         (incf call-count)
         (setf last-called (get-universal-time))
         (apply behavior args))

        (:inspect ()
         (list :name name
               :mcp-name wire-name
               :documentation documentation
               :schema schema
               :versions (1+ (length behavior-history))
               :call-count call-count
               :last-called last-called
               :created-at created-at
               :behavior-source (ignore-errors
                                  (function-lambda-expression behavior))))

        (:evolve (new-behavior &optional reason)
         (push (list (get-universal-time) reason behavior) behavior-history)
         (setf behavior new-behavior)
         (list :evolved :versions (1+ (length behavior-history))))

        (:devolve ()
         (if behavior-history
             (progn
               (setf behavior (third (pop behavior-history)))
               (list :devolved :versions (1+ (length behavior-history))))
             (list :error "No previous version")))

        (:schema () schema)

        (:mcp-name () wire-name)

        (:export-state ()
         `(("name" . ,(symbol-name name))
           ("mcp_name" . ,wire-name)
           ("call_count" . ,call-count)
           ("last_called" . ,last-called)
           ("created_at" . ,created-at)
           ("versions" . ,(1+ (length behavior-history)))))

        (:import-state (state)
         (when-let ((cc (cdr (assoc "call_count" state :test #'string=))))
           (setf call-count cc))
         (when-let ((lc (cdr (assoc "last_called" state :test #'string=))))
           (setf last-called lc))
         t)

        (t (&rest args)
         ;; Default: treat as :call
         (incf call-count)
         (setf last-called (get-universal-time))
         (apply behavior args))))))

;;; Registry operations

(defun register-tool (tool)
  "Register a pandoric tool in the global registry."
  (let ((wire-name (funcall tool :mcp-name)))
    (with-lock-held (*tool-registry-lock*)
      (setf (gethash wire-name *tool-registry*) tool))
    wire-name))

(defun unregister-tool (name)
  "Remove a tool from the registry by MCP name or Lisp symbol."
  (let ((wire-name (if (symbolp name)
                       (lisp-name-to-mcp-name name)
                       name)))
    (with-lock-held (*tool-registry-lock*)
      (remhash wire-name *tool-registry*))))

(defun get-tool (name)
  "Get a tool from the registry by MCP name or Lisp symbol."
  (let ((wire-name (if (symbolp name)
                       (lisp-name-to-mcp-name name)
                       name)))
    (with-lock-held (*tool-registry-lock*)
      (gethash wire-name *tool-registry*))))

(defun list-tools ()
  "Return list of all registered tool wire names."
  (with-lock-held (*tool-registry-lock*)
    (hash-table-keys *tool-registry*)))

(defun clear-tools ()
  "Remove all tools from the registry."
  (with-lock-held (*tool-registry-lock*)
    (clrhash *tool-registry*)))

;;; Convenience functions

(defun call-tool (name args)
  "Call a tool by name with args hash-table."
  (if-let ((tool (get-tool name)))
    (funcall tool :call args)
    (error 'tool-not-found :tool-name name)))

(defun inspect-tool (name)
  "Get inspection data for a tool."
  (if-let ((tool (get-tool name)))
    (funcall tool :inspect)
    (error 'tool-not-found :tool-name name)))

(defun evolve-tool (name new-behavior &optional reason)
  "Hot-swap a tool's behavior."
  (if-let ((tool (get-tool name)))
    (funcall tool :evolve new-behavior reason)
    (error 'tool-not-found :tool-name name)))

(defun devolve-tool (name)
  "Rollback a tool to its previous behavior."
  (if-let ((tool (get-tool name)))
    (funcall tool :devolve)
    (error 'tool-not-found :tool-name name)))

(defun export-tool-state (name)
  "Export a tool's serializable state."
  (if-let ((tool (get-tool name)))
    (funcall tool :export-state)
    (error 'tool-not-found :tool-name name)))

(defun import-tool-state (name state)
  "Import state into a tool."
  (if-let ((tool (get-tool name)))
    (funcall tool :import-state state)
    (error 'tool-not-found :tool-name name)))
