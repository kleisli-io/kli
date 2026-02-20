;;; MCP Framework - Content types
;;; MCP content types for tool responses

(in-package #:mcp-framework)

;;; Content structures

(defstruct (text-content (:constructor %make-text-content))
  "Text content for MCP tool responses."
  (type "text" :type string :read-only t)
  (text "" :type string))

(defstruct (image-content (:constructor %make-image-content))
  "Image content for MCP tool responses."
  (type "image" :type string :read-only t)
  (data "" :type string)
  (mime-type "image/png" :type string))

(defstruct (resource-content (:constructor %make-resource-content))
  "Resource reference content for MCP tool responses."
  (type "resource" :type string :read-only t)
  (uri "" :type string)
  (mime-type nil :type (or null string)))

;;; Constructors

(defun make-text-content (text &rest args)
  "Create text content. If ARGS are provided, TEXT is used as a format control string."
  (%make-text-content :text (if args
                                (apply #'format nil text args)
                                text)))

(defun make-image-content (data &key (mime-type "image/png"))
  "Create image content with base64-encoded data."
  (%make-image-content :data data :mime-type mime-type))

(defun make-resource-content (uri &key mime-type)
  "Create resource reference content."
  (%make-resource-content :uri uri :mime-type mime-type))

;;; Convenience alias
(setf (fdefinition 'text-content) #'make-text-content)

;;; JSON serialization

(defgeneric content-to-json (content)
  (:documentation "Convert content to JSON-compatible alist."))

(defmethod content-to-json ((content text-content))
  `(("type" . "text")
    ("text" . ,(text-content-text content))))

(defmethod content-to-json ((content image-content))
  `(("type" . "image")
    ("data" . ,(image-content-data content))
    ("mimeType" . ,(image-content-mime-type content))))

(defmethod content-to-json ((content resource-content))
  (let ((result `(("type" . "resource")
                  ("uri" . ,(resource-content-uri content)))))
    (when (resource-content-mime-type content)
      (push (cons "mimeType" (resource-content-mime-type content)) result))
    result))

;;; Content normalization

(defun normalize-content (result)
  "Normalize tool result to list of content objects.
   Handles: content object, list of contents, string, or other values."
  (cond
    ;; Already a content struct
    ((or (text-content-p result)
         (image-content-p result)
         (resource-content-p result))
     (list result))
    ;; List of content structs
    ((and (listp result)
          (every (lambda (x)
                   (or (text-content-p x)
                       (image-content-p x)
                       (resource-content-p x)))
                 result))
     result)
    ;; String - wrap in text content
    ((stringp result)
     (list (make-text-content result)))
    ;; Anything else - convert to string
    (t
     (list (make-text-content (format nil "~S" result))))))
