(in-package #:kli/tui/editor)

(defun recognized-command-p (editor data)
  (keymap-recognized-p (object-protocol editor) data))

(defun line-start-position (editor &optional (position (editor-cursor editor)))
  (let ((newline (position #\Newline
                           (editor-value editor)
                           :end position
                           :from-end t)))
    (if newline (1+ newline) 0)))

(defun line-end-position (editor &optional (position (editor-cursor editor)))
  (or (position #\Newline (editor-value editor) :start position)
      (length (editor-value editor))))

(defun cursor-row (editor)
  (count #\Newline (editor-value editor) :end (editor-cursor editor)))

(defun cursor-column (editor)
  (- (editor-cursor editor) (line-start-position editor)))

(defun move-cursor (editor position)
  (setf (editor-cursor editor)
        (max 0 (min position (length (editor-value editor))))
        (editor-desired-column editor) nil)
  t)

(defun move-cursor-vertical (editor position column)
  (setf (editor-cursor editor)
        (max 0 (min position (length (editor-value editor))))
        (editor-desired-column editor) column)
  t)

(defun next-cursor-position (editor)
  (let ((range (paste-marker-starting-at editor (editor-cursor editor))))
    (if range
        (cdr range)
        (1+ (editor-cursor editor)))))

(defun previous-cursor-position (editor)
  (let ((range (paste-marker-ending-at editor (editor-cursor editor))))
    (if range
        (car range)
        (1- (editor-cursor editor)))))

(defun move-up (editor)
  (let ((current-start (line-start-position editor)))
    (unless (zerop current-start)
      (let* ((column (or (editor-desired-column editor)
                         (cursor-column editor)))
             (previous-end (1- current-start))
             (previous-start (line-start-position editor previous-end)))
        (move-cursor-vertical editor
                              (min (+ previous-start column)
                                   previous-end)
                              column)))))

(defun move-down (editor)
  (let ((current-end (line-end-position editor)))
    (unless (>= current-end (length (editor-value editor)))
      (let* ((column (or (editor-desired-column editor)
                         (cursor-column editor)))
             (next-start (1+ current-end))
             (next-end (line-end-position editor next-start)))
        (move-cursor-vertical editor
                              (min (+ next-start column)
                                   next-end)
                              column)))))

(defun word-start-before (editor &optional (position (editor-cursor editor)))
  (let ((index position)
        (value (editor-value editor)))
    (loop while (and (plusp index)
                     (whitespace-char-p (char value (1- index))))
          do (decf index))
    (loop while (and (plusp index)
                     (not (whitespace-char-p
                           (char value (1- index)))))
          do (decf index))
    index))

(defun word-end-after (editor &optional (position (editor-cursor editor)))
  (let ((index position)
        (value (editor-value editor)))
    (loop while (and (< index (length value))
                     (whitespace-char-p (char value index)))
          do (incf index))
    (loop while (and (< index (length value))
                     (not (whitespace-char-p (char value index))))
          do (incf index))
    index))

(defun delete-range (editor start end)
  (let* ((value (editor-value editor))
         (start (max 0 (min start (length value))))
         (end (max 0 (min end (length value)))))
    (when (< start end)
      (push-undo-snapshot editor)
      (setf (editor-value editor)
            (concatenate 'string
                         (subseq value 0 start)
                         (subseq value end))
            (editor-cursor editor) start
            (editor-desired-column editor) nil)
      t)))

(defun insert-text (editor text)
  (when (plusp (length text))
    (push-undo-snapshot editor))
  (let ((value (editor-value editor))
        (cursor (editor-cursor editor)))
    (setf (editor-value editor)
          (concatenate 'string
                       (subseq value 0 cursor)
                       text
                       (subseq value cursor))
          (editor-cursor editor) (+ cursor (length text))
          (editor-desired-column editor) nil)
    t))

(defun insert-paste (editor text)
  (let ((text (insertable-input-string text)))
    (when text
      (insert-text editor
                   (if (large-paste-p text)
                       (store-paste-block editor text)
                       text)))))

(defun delete-to-line-end (editor)
  (let ((end (line-end-position editor)))
    (if (< (editor-cursor editor) end)
        (delete-range editor (editor-cursor editor) end)
        (when (and (< (editor-cursor editor) (length (editor-value editor)))
                   (char= #\Newline
                          (char (editor-value editor)
                                (editor-cursor editor))))
          (delete-range editor (editor-cursor editor)
                        (1+ (editor-cursor editor)))))))

(defun handle-editor-command (editor data)
  "Dispatch DATA's keymap action against EDITOR. App keys that reach the editor
with no route handler (:swallow, :clear-screen, :tool-output) stay inert."
  (case (keymap-action (object-protocol editor) data)
    (:move-char-left  (move-cursor editor (previous-cursor-position editor)))
    (:move-char-right (move-cursor editor (next-cursor-position editor)))
    (:move-line-up    (move-up editor))
    (:move-line-down  (move-down editor))
    (:move-line-start (move-cursor editor (line-start-position editor)))
    (:move-line-end   (move-cursor editor (line-end-position editor)))
    (:move-word-left  (move-cursor editor (word-start-before editor)))
    (:move-word-right (move-cursor editor (word-end-after editor)))
    (:delete-char-forward
     (let ((range (paste-marker-starting-at editor (editor-cursor editor))))
       (if range
           (delete-range editor (car range) (cdr range))
           (delete-range editor (editor-cursor editor)
                         (1+ (editor-cursor editor)))))
     t)
    (:delete-to-line-start
     (delete-range editor (line-start-position editor) (editor-cursor editor))
     t)
    (:delete-to-line-end (delete-to-line-end editor) t)
    (:delete-word-backward
     (delete-range editor (word-start-before editor) (editor-cursor editor))
     t)
    (:delete-word-forward
     (delete-range editor (editor-cursor editor) (word-end-after editor))
     t)
    (:insert-tab   (insert-text editor "    ") t)
    (:insert-space (insert-text editor " ") t)
    (:undo (editor-undo editor))
    ((:swallow :clear-screen :tool-output) t)
    (:ignore nil)
    (t nil)))

(defun handle-plain-editor-input (editor data)
  (cond
    ((submit-input-p data)
     (submit-editor* editor))
    ((newline-input-p data)
     (insert-text editor (string #\Newline)))
    ((backspace-input-p data)
     (when (plusp (editor-cursor editor))
       (let ((range (paste-marker-ending-at editor
                                            (editor-cursor editor))))
         (if range
             (delete-range editor (car range) (cdr range))
             (delete-range editor
                           (1- (editor-cursor editor))
                           (editor-cursor editor))))))
    ((handle-editor-command editor data))
    ((recognized-command-p editor data)
     nil)
    ((insertable-input-string data)
     (insert-text editor (insertable-input-string data)))))

(defun default-editor-behavior (editor operation &optional data)
  (case operation
    (:handle-input
     (if (handle-completion-input editor data)
         t
         ;; An input that itself changed the popup owns it: a submitted
         ;; command may have opened a selection menu that the token-derived
         ;; buffer refresh must not clobber.
         (let ((popup (editor-completion editor)))
           (prog1 (handle-plain-editor-input editor data)
             (when (eq popup (editor-completion editor))
               (refresh-editor-completion editor))))))
    (:handle-paste
     (prog1 (insert-paste editor data)
       (refresh-editor-completion editor)))
    (otherwise
     nil)))

(defun handle-editor-input (editor input)
  (call-behavior (editor-behavior editor) editor :handle-input input))

(defun handle-editor-paste (editor text)
  (call-behavior (editor-behavior editor) editor :handle-paste text))

(defun invalidate-editor (editor)
  (declare (ignore editor))
  nil)

(defun set-editor-focused (editor state)
  (setf (editor-focused editor) state)
  editor)

(defun submit-editor* (editor)
  (let ((handler (editor-on-submit editor)))
    (when handler
      (funcall handler (editor-expanded-value editor))))
  t)

(defun set-editor-value (editor value)
  (setf (editor-value editor) value
        (editor-cursor editor) (length value)
        (editor-desired-column editor) nil
        (editor-paste-blocks editor) '()
        (editor-paste-counter editor) 0
        (editor-completion editor) nil
        (editor-undo-history editor) '())
  editor)

(defun recode-editor (editor
                      &key function version (state nil state-p)
                        (metadata nil metadata-p)
                        (capabilities nil capabilities-p))
  (apply #'recode-tui-behavior
         (editor-behavior editor)
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
  editor)

(defun recode-editor-behavior (editor &key
                                        (function #'default-editor-behavior)
                                        version (state nil state-p)
                                        (metadata nil metadata-p)
                                        (capabilities nil capabilities-p))
  (apply #'recode-editor
         editor
         (append (list :function function)
                 (when version
                   (list :version version))
                 (when state-p
                   (list :state state))
                 (when metadata-p
                   (list :metadata metadata))
                 (when capabilities-p
                   (list :capabilities capabilities)))))
