;;; MCP Framework - JSON Schema generation
;;; Generate JSON Schema from parameter specifications

(in-package #:mcp-framework)

;;; Type mappings

(defparameter *type-mappings*
  '((string . "string")
    (str . "string")
    (integer . "integer")
    (int . "integer")
    (number . "number")
    (num . "number")
    (boolean . "boolean")
    (bool . "boolean")
    (array . "array")
    (list . "array")
    (object . "object")
    (hash-table . "object"))
  "Mapping from Lisp type symbols to JSON Schema types.")

(defun param-to-json-type (lisp-type)
  "Convert a Lisp type symbol to JSON Schema type string."
  (or (cdr (assoc lisp-type *type-mappings* :test #'string-equal))
      "string"))

;;; Schema generation

(defun type-spec-keyword-p (type-spec keyword)
  "Check if TYPE-SPEC is a list starting with KEYWORD (cross-package safe)."
  (and (listp type-spec)
       (symbolp (car type-spec))
       (string= (symbol-name (car type-spec)) keyword)))

(defun parse-type-spec (type-spec)
  "Parse a type specification, returning (json-type . extra-props).
   Supports:
   - Simple types: string, integer, boolean, number, array, object
   - Enum: (enum \"val1\" \"val2\" ...) -> string with enum constraint
   - Typed array: (array string) -> array with items schema"
  (cond
    ;; Enum type: (enum "a" "b" "c")
    ((type-spec-keyword-p type-spec "ENUM")
     (cons "string"
           `(("enum" . ,(coerce (cdr type-spec) 'vector)))))
    ;; Typed array: (array string) or (array (enum ...))
    ((type-spec-keyword-p type-spec "ARRAY")
     (let ((item-type (if (cdr type-spec)
                          (parse-type-spec (cadr type-spec))
                          nil)))
       (cons "array"
             (when item-type
               `(("items" . (("type" . ,(car item-type))
                             ,@(cdr item-type))))))))
    ;; Simple type
    (t
     (cons (param-to-json-type type-spec) nil))))

(defun generate-param-schema (param-spec)
  "Generate JSON Schema for a single parameter.
   PARAM-SPEC is (name type description) or (name type description default).
   TYPE can be:
   - Simple: string, integer, boolean, number, array, object
   - Enum: (enum \"val1\" \"val2\")
   - Typed array: (array string) or (array (enum \"a\" \"b\"))"
  (destructuring-bind (name type description &optional default defaultp)
      (if (= (length param-spec) 4)
          (append param-spec (list t))
          (append param-spec (list nil nil)))
    (declare (ignore name default defaultp))
    (destructuring-bind (json-type . extra-props) (parse-type-spec type)
      `(("type" . ,json-type)
        ("description" . ,description)
        ,@extra-props))))

(defun generate-schema (params)
  "Generate complete JSON Schema inputSchema from parameter list.
   PARAMS is a list of (name type description) or (name type description default).
   Parameters without defaults are required."
  (let ((properties nil)
        (required nil))
    (dolist (param params)
      (let* ((name (first param))
             (name-str (lisp-name-to-mcp-name name))
             (has-default (= (length param) 4)))
        (push (cons name-str (generate-param-schema param)) properties)
        (unless has-default
          (push name-str required))))
    `(("type" . "object")
      ("properties" . ,(if properties
                           (nreverse properties)
                           (make-hash-table :test 'equal)))
      ,@(when required
          `(("required" . ,(coerce (nreverse required) 'vector)))))))

;;; Name conversion utilities

(defun lisp-name-to-mcp-name (symbol)
  "Convert Lisp symbol to MCP wire name (snake_case)."
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))

(defun mcp-name-to-lisp-name (string &optional (package *package*))
  "Convert MCP wire name (snake_case) to Lisp symbol."
  (intern (string-upcase (substitute #\- #\_ string)) package))
