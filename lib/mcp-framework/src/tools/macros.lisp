;;; MCP Framework - Tool Definition Macros
;;; DSL for defining MCP tools with automatic schema generation

(in-package #:mcp-framework)

;;; Parameter extraction

(defun extract-param-binding (param-spec args-var)
  "Generate binding form to extract parameter from args hash-table.
   PARAM-SPEC is (name type description) or (name type description default)."
  (let* ((name (first param-spec))
         (name-str (lisp-name-to-mcp-name name))
         (default (when (= (length param-spec) 4)
                    (fourth param-spec))))
    `(,name (gethash ,name-str ,args-var ,default))))

;;; Main macro

(defmacro define-tool (name (&rest params) &body body)
  "Define an MCP tool with automatic schema generation and registration.

   NAME is a symbol naming the tool.
   PARAMS is a list of parameter specs:
     (name type \"description\") - required parameter
     (name type \"description\" default) - optional parameter

   BODY is the tool implementation. Within BODY, parameter names are bound
   to their extracted values. The body should return content (via make-text-content,
   text-content, etc.) or a string.

   Example:
     (define-tool greet ((name string \"Person to greet\")
                         (times integer \"How many times\" 1))
       \"Greet someone multiple times.\"
       (with-output-to-string (s)
         (dotimes (i times)
           (format s \"Hello, ~A!~%\" name))))"
  (let* ((docstring (when (stringp (first body)) (pop body)))
         (args-var (gensym "ARGS"))
         (bindings (mapcar (lambda (p) (extract-param-binding p args-var))
                           params))
         (schema-form `(generate-schema ',params)))
    `(progn
       (register-tool
        (make-pandoric-tool
         ',name
         (lambda (,args-var)
           (let ,bindings
             ,@body))
         :documentation ,docstring
         :schema ,schema-form
         :mcp-name ,(lisp-name-to-mcp-name name)))
       ',name)))

;;; Utility macro for tools that just need text output

(defmacro define-simple-tool (name (&rest params) format-string &rest format-args)
  "Define a simple tool that returns formatted text.

   Example:
     (define-simple-tool greet ((name string \"Person\"))
       \"Hello, ~A!\" name)"
  (let ((docstring (when (stringp (first format-args))
                     (pop format-args))))
    `(define-tool ,name ,params
       ,@(when docstring (list docstring))
       (make-text-content ,format-string ,@format-args))))
