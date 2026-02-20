;;; MCP Framework - Resource System
;;; Simple struct-based resources with URI scheme handlers

(in-package #:mcp-framework)

;;; Resource structure

(defstruct (mcp-resource (:conc-name resource-))
  "An MCP resource that can be read by clients."
  uri          ; Required: unique URI identifier
  name         ; Required: human-readable name
  description  ; Optional: description
  mime-type    ; Optional: MIME type hint
  handler)     ; Function (uri) -> content string

;;; Resource registry

(defvar *resource-registry* (make-hash-table :test 'equal)
  "Global registry of MCP resources, keyed by URI.")

(defvar *resource-registry-lock* (make-lock "resource-registry")
  "Lock for thread-safe resource registry access.")

;;; Registry operations

(defun register-resource (resource)
  "Register a resource in the global registry."
  (let ((uri (resource-uri resource)))
    (with-lock-held (*resource-registry-lock*)
      (setf (gethash uri *resource-registry*) resource))
    uri))

(defun unregister-resource (uri)
  "Remove a resource from the registry by URI."
  (with-lock-held (*resource-registry-lock*)
    (remhash uri *resource-registry*)))

(defun get-resource (uri)
  "Get a resource from the registry by URI."
  (with-lock-held (*resource-registry-lock*)
    (gethash uri *resource-registry*)))

(defun list-resources ()
  "Return list of all registered resource URIs."
  (with-lock-held (*resource-registry-lock*)
    (hash-table-keys *resource-registry*)))

(defun clear-resources ()
  "Remove all resources from the registry."
  (with-lock-held (*resource-registry-lock*)
    (clrhash *resource-registry*)))

;;; Resource reading

(defun read-resource (uri)
  "Read a resource's content by URI.
   Returns the content string or signals RESOURCE-NOT-FOUND."
  (let ((resource (get-resource uri)))
    (unless resource
      (error 'resource-not-found :uri uri))
    (funcall (resource-handler resource) uri)))

;;; MCP Protocol handlers

(defun handle-resources-list (params)
  "Handle resources/list request."
  (declare (ignore params))
  (let ((resources nil))
    (with-lock-held (*resource-registry-lock*)
      (maphash (lambda (uri resource)
                 (declare (ignore uri))
                 (push `(("uri" . ,(resource-uri resource))
                         ("name" . ,(resource-name resource))
                         ,@(when (resource-description resource)
                             `(("description" . ,(resource-description resource))))
                         ,@(when (resource-mime-type resource)
                             `(("mimeType" . ,(resource-mime-type resource)))))
                       resources))
               *resource-registry*))
    `(("resources" . ,(coerce (nreverse resources) 'vector)))))

(defun handle-resources-read (params)
  "Handle resources/read request."
  (let ((uri (gethash "uri" params)))
    (unless uri
      (error 'invalid-params :data "Missing uri"))
    (let ((resource (get-resource uri)))
      (unless resource
        (error 'resource-not-found :uri uri))
      (let ((content (funcall (resource-handler resource) uri)))
        `(("contents" . #((("uri" . ,uri)
                           ("mimeType" . ,(or (resource-mime-type resource) "text/plain"))
                           ("text" . ,content)))))))))

;;; Register MCP handlers

(register-mcp-handler "resources/list" #'handle-resources-list)
(register-mcp-handler "resources/read" #'handle-resources-read)

;;; Convenience macro

(defmacro define-resource (uri-spec (&key name description mime-type) &body body)
  "Define and register an MCP resource.

   URI-SPEC is the resource URI string.
   NAME is the human-readable name (defaults to URI).
   DESCRIPTION is optional documentation.
   MIME-TYPE is optional content type hint.
   BODY receives URI as argument and should return the content string.

   Example:
     (define-resource \"config://app\"
         (:name \"Application Config\" :mime-type \"application/json\")
       (json-encode *app-config*))"
  (let ((uri-var (gensym "URI")))
    `(register-resource
      (make-mcp-resource
       :uri ,uri-spec
       :name ,(or name uri-spec)
       :description ,description
       :mime-type ,mime-type
       :handler (lambda (,uri-var)
                  (declare (ignorable ,uri-var))
                  ,@body)))))
