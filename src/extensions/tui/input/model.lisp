(in-package #:kli/tui/input)

(defvar *input-event-counter* (make-id-counter))
(defvar *input-decoder-counter* (make-id-counter))

(defun make-input-object-id (prefix)
  (next-keyword-id (format nil "TUI-INPUT-~A"
                           (string-upcase (symbol-name prefix)))
                   '*input-event-counter*))

(defun make-input-decoder-id ()
  (next-keyword-id "TUI-INPUT-DECODER" '*input-decoder-counter*))

(defclass input-event (live-object)
  ((kind
    :initarg :kind
    :reader input-event-kind)
   (key
    :initarg :key
    :initform nil
    :reader input-event-key)
   (key-id
    :initarg :key-id
    :initform nil
    :reader input-event-key-id)
   (text
    :initarg :text
    :initform nil
    :reader input-event-text)
   (modifiers
    :initarg :modifiers
    :initform '()
    :reader input-event-modifiers)
   (event-type
    :initarg :event-type
    :initform nil
    :reader input-event-event-type)
   (raw
    :initarg :raw
    :initform nil
    :reader input-event-raw)))

(defun make-text-input-event (text &key raw id)
  (make-instance 'input-event
                 :id (or id (make-input-object-id :text))
                 :kind :text
                 :text text
                 :raw raw))

(defun make-paste-input-event (text &key raw id)
  (make-instance 'input-event
                 :id (or id (make-input-object-id :paste))
                 :kind :paste
                 :text text
                 :raw raw))

(defun make-key-input-event (key &key raw key-id modifiers
                                  (event-type :press) id)
  (make-instance 'input-event
                 :id (or id (make-input-object-id :key))
                 :kind :key
                 :key key
                 :key-id key-id
                 :modifiers modifiers
                 :event-type event-type
                 :raw raw))

(defun make-interrupt-input-event (&key raw id)
  (make-instance 'input-event
                 :id (or id (make-input-object-id :interrupt))
                 :kind :interrupt
                 :raw raw))

(defun make-terminal-response-input-event (raw &key id)
  (make-instance 'input-event
                 :id (or id (make-input-object-id :terminal-response))
                 :kind :terminal-response
                 :raw raw))

(defclass input-decoder (live-object)
  ((buffer
    :initarg :buffer
    :initform ""
    :accessor input-decoder-buffer)
   (paste-mode
    :initarg :paste-mode
    :initform nil
    :accessor input-decoder-paste-mode)
   (paste-buffer
    :initarg :paste-buffer
    :initform ""
    :accessor input-decoder-paste-buffer)
   (behavior
    :initarg :behavior
    :accessor input-decoder-behavior)))

(defclass input-route-context (live-object)
  ((interrupt-handler
    :initarg :interrupt-handler
    :initform nil
    :accessor route-interrupt-handler)
   (abort-handler
    :initarg :abort-handler
    :initform nil
    :accessor route-abort-handler)
   (clear-screen-handler
    :initarg :clear-screen-handler
    :initform nil
    :accessor route-clear-screen-handler)
   (tool-output-handler
    :initarg :tool-output-handler
    :initform nil
    :accessor route-tool-output-handler)
   (next-surface-handler
    :initarg :next-surface-handler
    :initform nil
    :accessor route-next-surface-handler)))

(defun make-input-route-context (&key (id :input-route-context)
                                      protocol
                                      interrupt-handler
                                      abort-handler
                                      clear-screen-handler
                                      tool-output-handler
                                      next-surface-handler)
  (make-instance 'input-route-context
                 :id id
                 :protocol protocol
                 :interrupt-handler interrupt-handler
                 :abort-handler abort-handler
                 :clear-screen-handler clear-screen-handler
                 :tool-output-handler tool-output-handler
                 :next-surface-handler next-surface-handler))
