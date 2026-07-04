(in-package #:kli/tui/terminal)

(defvar *terminal-counter* (make-id-counter))
(defvar *screen-frame-counter* (make-id-counter))

(defun make-terminal-id (kind)
  (next-keyword-id (format nil "TUI-TERMINAL-~A"
                           (string-upcase (symbol-name kind)))
                   '*terminal-counter*))

(defun make-screen-frame-id ()
  (next-keyword-id "TUI-SCREEN-FRAME" '*screen-frame-counter*))

(defclass terminal (live-object)
  ((columns
    :initarg :columns
    :initform 80
    :accessor terminal-columns)
   (rows
    :initarg :rows
    :initform 24
    :accessor terminal-rows)))

(defclass memory-terminal (terminal)
  ((output
    :initarg :output
    :initform '()
    :accessor terminal-output-chunks)))

(defclass process-terminal (terminal)
  ((batch-depth
    :initform 0
    :accessor process-terminal-batch-depth
    :documentation "Count of open batch scopes. Writes buffer while positive, and the outermost close drains them. Nestable so an outer scope folds pre-frame writes into the frame's single write.")
   (frame-chunks
    :initform nil
    :accessor process-terminal-frame-chunks
    :documentation "Reversed list of the in-progress frame's writes, drained as one write on the outermost end-frame.")))

(defclass screen-frame (live-object)
  ((root
    :initarg :root
    :accessor frame-root)
   (focused
    :initarg :focused
    :initform nil
    :accessor frame-focused)
   (previous-lines
    :initarg :previous-lines
    :initform '()
    :accessor frame-previous-lines)
   (render-behavior
    :initarg :render-behavior
    :accessor frame-render-behavior)))

(declaim (ftype function default-frame-render))

(defun make-frame-render-behavior (frame-id)
  "Fault-policy is :escalate so a faulting render reaches the :render funnel, which owns recovery — skip frame and reset. Cell-level containment would corrupt the diff."
  (make-behavior-cell
   :id (list frame-id :frame-render-behavior)
   :state '(:strategy :diff-frame-render)
   :metadata '(:owner :tui-terminal)
   :capabilities '(:terminal/frame-render :behavior/hotpatch :behavior/state)
   :fault-policy :escalate
   :function #'default-frame-render))

(defun make-screen-frame (&key protocol id root focused render-behavior)
  (let ((id (or id (make-screen-frame-id))))
    (make-instance 'screen-frame
                   :id id
                   :protocol protocol
                   :root (or root
                             (make-tui-container :protocol protocol))
                   :focused focused
                   :render-behavior (or render-behavior
                                        (make-frame-render-behavior id)))))
