(in-package #:kli/ext)

;;; Resource: a first-class, readable contribution kind any extension may
;;; provide. A resource mirrors a tool -- a live-object identified by its uri,
;;; carrying a reader thunk that yields content. The per-protocol registry is
;;; keyed by uri (the resource's identity), the trio find/list/read mirroring
;;; find-tool/list-tools/invoke-tool.

(defclass resource (live-object)
  ((uri
    :initarg :uri
    :reader resource-uri)
   (name
    :initarg :name
    :initform nil
    :accessor resource-name)
   (description
    :initarg :description
    :initform ""
    :accessor resource-description)
   (mime-type
    :initarg :mime-type
    :initform nil
    :accessor resource-mime-type)
   (reader
    :initarg :reader
    :accessor resource-reader)))

(defun make-resource (&key id uri name description mime-type reader)
  (make-instance 'resource
                 :id (normalize-extension-id (or id uri))
                 :uri uri
                 :name name
                 :description (or description "")
                 :mime-type mime-type
                 :reader (or reader (lambda () '()))))

(defclass resource-contribution (contribution)
  ((resource
    :initarg :resource
    :reader contribution-resource)))

(defun make-resource-contribution (&key name resource source)
  (make-instance 'resource-contribution
                 :kind :resource
                 :name (or name (resource-uri resource))
                 :resource resource
                 :source source))

(defparameter +resources-storage-key+ :kli/ext.resources
  "Storage key for the per-protocol uri-to-resource-contribution registry.")

(defun protocol-resources (protocol)
  "PROTOCOL's resource registry: a hash from uri to the contributions under it."
  (ensure-protocol-storage protocol +resources-storage-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun find-resource (protocol uri)
  (let ((contribution (first (gethash uri (protocol-resources protocol)))))
    (and contribution (contribution-resource contribution))))

(defun list-resources (protocol)
  (let ((resources '()))
    (maphash (lambda (uri contributions)
               (declare (ignore uri))
               (when contributions
                 (push (contribution-resource (first contributions)) resources)))
             (protocol-resources protocol))
    (nreverse resources)))

(defun read-resource (protocol uri)
  "Read the resource registered under URI through its reader thunk."
  (let ((resource (find-resource protocol uri)))
    (unless resource
      (error "No resource for uri ~S." uri))
    (funcall (resource-reader resource))))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution resource-contribution)
                                 context)
  (let ((resource (contribution-resource contribution)))
    (unless (typep resource 'resource)
      (error "Not a resource: ~S" resource))
    (register-live-object (context-registry context) resource)
    (push contribution
          (gethash (resource-uri resource)
                   (protocol-resources protocol)))
    (push contribution (protocol-installed-contributions protocol))
    contribution))

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution resource-contribution)
                                 context)
  (remove-live-object (context-registry context)
                      (object-id (contribution-resource contribution)))
  (let* ((uri (resource-uri (contribution-resource contribution)))
         (remaining (remove contribution
                            (gethash uri (protocol-resources protocol)))))
    (if remaining
        (setf (gethash uri (protocol-resources protocol)) remaining)
        (remhash uri (protocol-resources protocol))))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defcontribution-kind :resource (extension-id form)
  (destructuring-bind (_ name &key uri description mime-type reader) form
    (declare (ignore _))
    `(make-resource-contribution
      :name ',(normalize-extension-id name)
      :resource (make-resource
                 :id ',(normalize-extension-id name)
                 :uri ,uri
                 :name ',(normalize-extension-id name)
                 :description ,description
                 :mime-type ,mime-type
                 :reader ,reader)
      :source ',extension-id)))
