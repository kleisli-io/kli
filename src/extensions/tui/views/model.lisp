(in-package #:kli/tui/views)

(defvar *tui-view-counter* (make-id-counter))

(defun make-tui-view-id (kind)
  (next-keyword-id (format nil "TUI-~A" (string-upcase (symbol-name kind)))
                   '*tui-view-counter*))

(defclass tui-view (live-object) ())

(defclass tui-container-view (tui-view)
  ((children
    :initarg :children
    :initform '()
    :accessor view-children)))

(defun make-tui-container (&key id protocol children)
  (make-instance 'tui-container-view
                 :id (or id (make-tui-view-id :container))
                 :protocol protocol
                 :children children))

(defclass tui-text-view (tui-view)
  ((text
    :initarg :text
    :initform ""
    :accessor view-text)
   (padding-x
    :initarg :padding-x
    :initform 1
    :accessor view-padding-x)
   (padding-y
    :initarg :padding-y
    :initform 1
    :accessor view-padding-y)))

(defun make-tui-text (&rest args)
  (let ((text "")
        id
        protocol
        (padding-x 1)
        (padding-y 1))
    (when (and args (stringp (first args)))
      (setf text (pop args)))
    (setf id (getf args :id)
          protocol (getf args :protocol)
          padding-x (or (getf args :padding-x) padding-x)
          padding-y (or (getf args :padding-y) padding-y))
    (make-instance 'tui-text-view
                   :id (or id (make-tui-view-id :text))
                   :protocol protocol
                   :text text
                   :padding-x padding-x
                   :padding-y padding-y)))

(defclass tui-box-view (tui-container-view)
  ((padding-x
    :initarg :padding-x
    :initform 1
    :accessor view-padding-x)
   (padding-y
    :initarg :padding-y
    :initform 1
    :accessor view-padding-y)))

(defun make-tui-box (&key id protocol (padding-x 1) (padding-y 1) children)
  (make-instance 'tui-box-view
                 :id (or id (make-tui-view-id :box))
                 :protocol protocol
                 :padding-x padding-x
                 :padding-y padding-y
                 :children children))
