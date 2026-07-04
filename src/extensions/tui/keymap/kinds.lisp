(in-package #:kli/tui/keymap)

(defclass keybinding-contribution (contribution)
  ((key-id :initarg :key-id :reader contribution-key-id)
   (action :initarg :action :reader contribution-action)
   (previous-action :initform :absent :accessor contribution-previous-action)))

(defun make-keybinding-contribution (&key name key-id action source)
  (make-instance 'keybinding-contribution
                 :kind :keybinding :name name
                 :key-id key-id :action action :source source))

(defmethod install-contribution ((protocol extension-protocol)
                                 (c keybinding-contribution) context)
  (declare (ignore context))
  (let ((overrides (protocol-keymap-overrides protocol))
        (key-id (contribution-key-id c)))
    (multiple-value-bind (prev present) (gethash key-id overrides)
      (setf (contribution-previous-action c) (if present prev :absent)))
    (register-keybinding protocol key-id (contribution-action c)))
  (push c (protocol-installed-contributions protocol))
  c)

(defmethod retract-contribution ((protocol extension-protocol)
                                 (c keybinding-contribution) context)
  (declare (ignore context))
  (let ((prev (contribution-previous-action c))
        (key-id (contribution-key-id c)))
    (if (eq prev :absent)
        (unregister-keybinding protocol key-id)
        (register-keybinding protocol key-id prev)))
  (setf (protocol-installed-contributions protocol)
        (remove c (protocol-installed-contributions protocol)))
  c)

(defcontribution-kind :keybinding (extension-id form)
  (destructuring-bind (_ key-id action) form
    (declare (ignore _))
    `(make-keybinding-contribution
      :name ',(normalize-extension-id key-id)
      :key-id ,key-id
      :action ,action
      :source ',extension-id)))
