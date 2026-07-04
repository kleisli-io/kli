(in-package #:kli/tui/terminal)

(defun write-diff (terminal previous lines)
  (loop with last-index = (1- (length lines))
        for index from 0
        for line in lines
        for old = (nth index previous)
        unless (string= line (or old ""))
          do (progn
               (write-terminal terminal line)
               (unless (= index last-index)
                 (write-terminal terminal (string #\Newline)))))
  (when (> (length previous) (length lines))
    (loop repeat (- (length previous) (length lines))
          do (terminal-clear-line terminal))))

(defun frame-render-lines (frame width)
  (render-lines (frame-root frame) width))

(defun default-frame-render (frame terminal &key force)
  (multiple-value-bind (width height)
      (terminal-size terminal)
    (declare (ignore height))
    (let ((lines (frame-render-lines frame width)))
      (when force
        (terminal-clear-screen terminal)
        (setf (frame-previous-lines frame) '()))
      (write-diff terminal (frame-previous-lines frame) lines)
      (setf (frame-previous-lines frame) lines)
      (let ((cursor (cursor-position (frame-root frame) width)))
        (when cursor
          (terminal-move-cursor terminal (car cursor) (cdr cursor))))
      lines)))

(defun render-screen-frame (frame terminal &key force)
  (call-behavior (frame-render-behavior frame)
                 frame
                 terminal
                 :force force))

(defun recode-screen-frame (frame
                            &key function version (state nil state-p)
                              (metadata nil metadata-p)
                              (capabilities nil capabilities-p))
  (apply #'recode-tui-behavior
         (frame-render-behavior frame)
         (append (when function
                   (list :function function))
                 (when version
                   (list :version version))
                 (when state-p
                   (list :state state))
                 (when metadata-p
                   (list :metadata metadata))
                 (when capabilities-p
                   (list :capabilities capabilities))))
  frame)

(defun recode-frame-renderer (frame &key
                                      (function #'default-frame-render)
                                      version (state nil state-p)
                                      (metadata nil metadata-p)
                                      (capabilities nil capabilities-p))
  (apply #'recode-screen-frame
         frame
         (append (list :function function)
                 (when version
                   (list :version version))
                 (when state-p
                   (list :state state))
                 (when metadata-p
                   (list :metadata metadata))
                 (when capabilities-p
                   (list :capabilities capabilities)))))

(defun set-frame-focus (frame view)
  (when (frame-focused frame)
    (set-focused (frame-focused frame) nil))
  (setf (frame-focused frame) view)
  (when view
    (set-focused view t))
  view)

(defun handle-frame-input (frame input)
  (when (frame-focused frame)
    (handle-input (frame-focused frame) input)))

(defun invalidate-frame (frame)
  (invalidate (frame-root frame)))
