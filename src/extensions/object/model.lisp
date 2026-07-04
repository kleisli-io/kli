(in-package #:kli/object)

(defclass standard-live-object (live-object)
  ((kind
    :initarg :kind
    :initform nil
    :accessor object-kind)
   (owner
    :initarg :owner
    :initform nil
    :accessor object-owner)
   (source
    :initarg :source
    :initform nil
    :accessor object-source)
   (version
    :initarg :version
    :initform 0
    :accessor object-version)
   (state
    :initarg :state
    :initform nil
    :accessor object-state)
   (capabilities
    :initarg :capabilities
    :initform '()
    :accessor object-capabilities)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor object-metadata)))

(defun make-standard-live-object (&key id kind owner source version state
                                    capabilities metadata)
  (make-instance 'standard-live-object
                 :id id
                 :kind kind
                 :owner owner
                 :source source
                 :version (or version 0)
                 :state state
                 :capabilities capabilities
                 :metadata metadata))

(defun standard-live-object-p (object)
  (typep object 'standard-live-object))
