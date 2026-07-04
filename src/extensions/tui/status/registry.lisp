(in-package #:kli/tui/status)

(defparameter +slots-key+ :kli/tui/status/slots)
(defparameter +slot-order-key+ :kli/tui/status/order)

(defun protocol-status-slots (protocol)
  (ensure-protocol-storage protocol +slots-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun register-status-slot (protocol id &key (initial ""))
  (unless (nth-value 1 (gethash id (protocol-status-slots protocol)))
    (setf (protocol-storage protocol +slot-order-key+)
          (append (protocol-storage protocol +slot-order-key+ '()) (list id))))
  (setf (gethash id (protocol-status-slots protocol)) initial)
  id)

(defun unregister-status-slot (protocol id)
  (remhash id (protocol-status-slots protocol))
  (setf (protocol-storage protocol +slot-order-key+)
        (remove id (protocol-storage protocol +slot-order-key+ '()) :test #'equal))
  id)

(defun set-status (protocol id text)
  (setf (gethash id (protocol-status-slots protocol)) text))

(defun status-text (protocol id)
  (gethash id (protocol-status-slots protocol)))

(defun list-status-slots (protocol)
  (protocol-storage protocol +slot-order-key+ '()))

(defun render-status-line (protocol theme width)
  "No text in any slot yields no line, so the footer stays invisible until set."
  (declare (ignore theme))
  (let ((parts (loop for id in (list-status-slots protocol)
                     for txt = (status-text protocol id)
                     when (and txt (plusp (length txt))) collect txt)))
    (when parts
      (list (pad-right (format nil "~{~A~^ | ~}" parts) width)))))

(defparameter +widgets-key+ :kli/tui/status/widgets)
(defparameter +widget-order-key+ :kli/tui/status/widget-order)

(defun protocol-widgets (protocol)
  (ensure-protocol-storage protocol +widgets-key+
                           (lambda () (make-hash-table :test #'equal))))

(defun register-widget (protocol id factory &key (placement :footer))
  "Register FACTORY under ID. PLACEMENT routes its lines: :footer renders
below the prompt box, :above-input directly above it."
  (unless (nth-value 1 (gethash id (protocol-widgets protocol)))
    (setf (protocol-storage protocol +widget-order-key+)
          (append (protocol-storage protocol +widget-order-key+ '()) (list id))))
  (setf (gethash id (protocol-widgets protocol)) (cons factory placement))
  id)

(defun unregister-widget (protocol id)
  (remhash id (protocol-widgets protocol))
  (setf (protocol-storage protocol +widget-order-key+)
        (remove id (protocol-storage protocol +widget-order-key+ '()) :test #'equal))
  id)

(defun list-widgets (protocol)
  (protocol-storage protocol +widget-order-key+ '()))

(defun safely-render-widget (factory widget-id protocol theme width)
  "Run FACTORY behind the fault barrier: a widget that errors or returns a
non-list contributes no footer lines. The non-list case is a contract
violation, not a signal — noted unconditionally, never escalated."
  (let ((lines (with-extension-fault-barrier (:seam :widget-render :id widget-id)
                 (funcall factory protocol theme width))))
    (if (listp lines)
        lines
        (progn
          (note-fault :widget-render widget-id
                      (make-condition 'simple-error
                                      :format-control "widget returned ~S, expected a list of lines"
                                      :format-arguments (list lines)))
          nil))))

(defun render-widgets (protocol theme width &key (placement :footer))
  (loop for id in (list-widgets protocol)
        for (factory . slot) = (gethash id (protocol-widgets protocol))
        when (eq slot placement)
          append (safely-render-widget factory id protocol theme width)))

(defun render-footer (protocol theme width)
  (append (render-status-line protocol theme width)
          (render-widgets protocol theme width)))
