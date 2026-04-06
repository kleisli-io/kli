;;; MCP Framework - Prompt System
;;; Template prompts with arguments

(in-package #:mcp-framework)

;;; Prompt argument structure

(defstruct (prompt-argument (:conc-name prompt-arg-))
  "An argument for an MCP prompt."
  name         ; Required: argument name
  description  ; Optional: description
  required)    ; Boolean: whether argument is required

;;; Prompt structure

(defstruct (mcp-prompt (:conc-name prompt-))
  "An MCP prompt template."
  name         ; Required: unique name
  description  ; Optional: description
  arguments    ; List of prompt-argument structs
  handler)     ; Function (args-hash) -> list of messages

;;; Prompt registry

(defvar *prompt-registry* (make-hash-table :test 'equal)
  "Global registry of MCP prompts, keyed by name.")

(defvar *prompt-registry-lock* (make-lock "prompt-registry")
  "Lock for thread-safe prompt registry access.")

;;; Registry operations

(defun register-prompt (prompt)
  "Register a prompt in the global registry."
  (let ((name (prompt-name prompt)))
    (with-lock-held (*prompt-registry-lock*)
      (setf (gethash name *prompt-registry*) prompt))
    name))

(defun unregister-prompt (name)
  "Remove a prompt from the registry by name."
  (with-lock-held (*prompt-registry-lock*)
    (remhash name *prompt-registry*)))

(defun get-prompt (name)
  "Get a prompt from the registry by name."
  (with-lock-held (*prompt-registry-lock*)
    (gethash name *prompt-registry*)))

(defun list-prompts ()
  "Return list of all registered prompt names."
  (with-lock-held (*prompt-registry-lock*)
    (hash-table-keys *prompt-registry*)))

(defun clear-prompts ()
  "Remove all prompts from the registry."
  (with-lock-held (*prompt-registry-lock*)
    (clrhash *prompt-registry*)))

;;; Message construction helpers

(defun make-user-message (text)
  "Create a user message for prompt response."
  `(("role" . "user")
    ("content" . (("type" . "text")
                  ("text" . ,text)))))

(defun make-assistant-message (text)
  "Create an assistant message for prompt response."
  `(("role" . "assistant")
    ("content" . (("type" . "text")
                  ("text" . ,text)))))

;;; MCP Protocol handlers

(defun handle-prompts-list (params)
  "Handle prompts/list request."
  (declare (ignore params))
  (let ((prompts nil))
    (with-lock-held (*prompt-registry-lock*)
      (maphash (lambda (name prompt)
                 (declare (ignore name))
                 (push `(("name" . ,(prompt-name prompt))
                         ,@(when (prompt-description prompt)
                             `(("description" . ,(prompt-description prompt))))
                         ,@(when (prompt-arguments prompt)
                             `(("arguments" .
                                ,(coerce
                                  (mapcar (lambda (arg)
                                            `(("name" . ,(prompt-arg-name arg))
                                              ,@(when (prompt-arg-description arg)
                                                  `(("description" . ,(prompt-arg-description arg))))
                                              ("required" . ,(if (prompt-arg-required arg) t yason:false))))
                                          (prompt-arguments prompt))
                                  'vector)))))
                       prompts))
               *prompt-registry*))
    `(("prompts" . ,(coerce (nreverse prompts) 'vector)))))

(defun handle-prompts-get (params)
  "Handle prompts/get request."
  (let ((name (gethash "name" params))
        (arguments (gethash "arguments" params (make-hash-table :test 'equal))))
    (unless name
      (error 'invalid-params :data "Missing prompt name"))
    (let ((prompt (get-prompt name)))
      (unless prompt
        (error 'prompt-not-found :prompt-name name))
      ;; Validate required arguments
      (dolist (arg (prompt-arguments prompt))
        (when (and (prompt-arg-required arg)
                   (not (gethash (prompt-arg-name arg) arguments)))
          (error 'invalid-params
                 :data (format nil "Missing required argument: ~A" (prompt-arg-name arg)))))
      ;; Generate messages
      (let ((messages (funcall (prompt-handler prompt) arguments)))
        `(("messages" . ,(coerce messages 'vector)))))))

;;; Register MCP handlers

(register-mcp-handler "prompts/list" #'handle-prompts-list)
(register-mcp-handler "prompts/get" #'handle-prompts-get)

;;; Convenience macro

(defmacro define-prompt (name-spec (&key description) (&rest args) &body body)
  "Define and register an MCP prompt.

   NAME-SPEC is the prompt name string.
   DESCRIPTION is optional documentation.
   ARGS is a list of (name &key description required) argument specs.
   BODY receives MCP-FRAMEWORK:ARGUMENTS hash-table and should return list of messages.
   Use (gethash \"argname\" mcp-framework:arguments) to access argument values.

   Example:
     (define-prompt \"code-review\"
         (:description \"Review code for issues\")
         ((language :description \"Programming language\" :required t)
          (code :description \"Code to review\" :required t))
       (list
         (make-user-message
           (format nil \"Review this ~A code for bugs and improvements:~%~%~A\"
                   (gethash \"language\" mcp-framework:arguments)
                   (gethash \"code\" mcp-framework:arguments)))))"
  `(register-prompt
    (make-mcp-prompt
     :name ,name-spec
     :description ,description
     :arguments (list ,@(mapcar (lambda (arg-spec)
                                  (destructuring-bind (arg-name &key description required)
                                      (if (listp arg-spec) arg-spec (list arg-spec))
                                    `(make-prompt-argument
                                      :name ,(string-downcase (symbol-name arg-name))
                                      :description ,description
                                      :required ,required)))
                                args))
     :handler (lambda (mcp-framework:arguments)
                (declare (ignorable mcp-framework:arguments))
                ,@body))))
