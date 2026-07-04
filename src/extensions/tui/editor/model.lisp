(in-package #:kli/tui/editor)

(defvar *editor-counter* (make-id-counter))
(defvar *paste-block-counter* (make-id-counter))

(defun make-editor-id ()
  (next-keyword-id "TUI-EDITOR" '*editor-counter*))

(defun make-paste-block-id ()
  (next-keyword-id "TUI-PASTE-BLOCK" '*paste-block-counter*))

(defclass paste-block (live-object)
  ((number
    :initarg :number
    :reader paste-block-number)
   (marker
    :initarg :marker
    :reader paste-block-marker)
   (text
    :initarg :text
    :reader paste-block-text)))

(defun make-paste-block (&key id number marker text)
  (make-instance 'paste-block
                 :id (or id (make-paste-block-id))
                 :number number
                 :marker marker
                 :text text))

(defclass editor-view (live-object)
  ((value
    :initarg :value
    :initform ""
    :accessor editor-value)
   (cursor
    :initarg :cursor
    :initform 0
    :accessor editor-cursor)
   (desired-column
    :initarg :desired-column
    :initform nil
    :accessor editor-desired-column)
   (prompt
    :initarg :prompt
    :initform "> "
    :accessor editor-prompt)
   (on-submit
    :initarg :on-submit
    :initform nil
    :accessor editor-on-submit)
   (focused
    :initarg :focused
    :initform nil
    :accessor editor-focused)
   (paste-blocks
    :initarg :paste-blocks
    :initform '()
    :accessor editor-paste-blocks)
   (paste-counter
    :initarg :paste-counter
    :initform 0
    :accessor editor-paste-counter)
   (completion
    :initform nil
    :accessor editor-completion)
   (undo-history
    :initform '()
    :accessor editor-undo-history)
   (behavior
    :initarg :behavior
    :accessor editor-behavior)))

(declaim (ftype function default-editor-behavior))

(defun make-default-editor-behavior-cell (editor-id)
  (make-behavior-cell
   :id (list editor-id :editor-behavior)
   :state '(:strategy :default-editor)
   :metadata '(:owner :tui-editor)
   :capabilities '(:editor/input :editor/paste :behavior/hotpatch :behavior/state)
   :fault-policy :continue
   :function #'default-editor-behavior))

(defun make-editor (&key id protocol (value "") (prompt "> ") on-submit focused behavior)
  (let* ((id (or id (make-editor-id)))
         (cursor (length value)))
    (make-instance 'editor-view
                   :id id
                   :protocol protocol
                   :value value
                   :cursor cursor
                   :prompt prompt
                   :on-submit on-submit
                   :focused focused
                   :behavior (or behavior
                                 (make-default-editor-behavior-cell id)))))

(defun push-undo-snapshot (editor)
  "Save the buffer and cursor so the next edit can be reverted."
  (push (cons (editor-value editor) (editor-cursor editor))
        (editor-undo-history editor)))

(defun editor-undo (editor)
  "Restore the buffer and cursor saved before the most recent edit. NIL when the
history is empty."
  (let ((snapshot (pop (editor-undo-history editor))))
    (when snapshot
      (setf (editor-value editor) (car snapshot)
            (editor-cursor editor) (cdr snapshot)
            (editor-desired-column editor) nil)
      t)))
