(in-package #:kli/tui/views)

(defun render-container-lines (view width)
  (loop for child in (view-children view)
        append (render-lines child width)))

(defun render-text-lines (view width)
  (if (blank-string-p (view-text view))
      '()
      (let* ((padding-x (view-padding-x view))
             (padding-y (view-padding-y view))
             (content-width (max 1 (- width (* 2 padding-x))))
             (left (make-string padding-x :initial-element #\Space))
             (right left)
             (empty (make-string width :initial-element #\Space))
             (body (loop for line in (wrap-text
                                      (normalize-text (view-text view))
                                      content-width)
                         collect (pad-right
                                  (concatenate 'string left line right)
                                  width)))
             (vertical-pad (loop repeat padding-y collect empty)))
        (append vertical-pad body vertical-pad))))

(defun render-box-lines (view width)
  (let ((children (view-children view)))
    (when children
      (let* ((padding-x (view-padding-x view))
             (padding-y (view-padding-y view))
             (content-width (max 1 (- width (* 2 padding-x))))
             (left (make-string padding-x :initial-element #\Space))
             (empty (make-string width :initial-element #\Space))
             (body (loop for child in children
                         append (loop for line in (render-lines
                                                   child
                                                   content-width)
                                      collect
                                      (pad-right
                                       (concatenate 'string left line)
                                       width))))
             (vertical-pad (loop repeat padding-y collect empty)))
        (append vertical-pad body vertical-pad)))))

(defun invalidate-container (view)
  (dolist (child (view-children view))
    (invalidate child))
  nil)

(defun container-children (view)
  (copy-list (view-children view)))

(defun container-add-child (view child)
  (setf (view-children view)
        (append (view-children view) (list child)))
  child)

(defun container-remove-child (view child)
  (setf (view-children view)
        (remove child (view-children view) :test #'eq))
  child)

(defun container-clear-children (view)
  (setf (view-children view) '())
  view)
