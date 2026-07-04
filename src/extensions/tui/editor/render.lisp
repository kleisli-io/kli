(in-package #:kli/tui/editor)

(defun editor-whitespace-char-p (char)
  (or (char= char #\Space) (char= char #\Tab)))

(defun wrap-editor-logical-line (line max-width)
  "Word-aware wrap of LINE into a list of (text start end) chunks that cover
   LINE exactly (every character preserved). Breaks at the last whitespace
   before an overflowing word, force-breaking words longer than MAX-WIDTH.
   Trailing whitespace before a break stays at the end of the preceding chunk,
   so concatenating the chunk texts reconstructs LINE."
  (let ((max-width (max 1 max-width))
        (length (length line)))
    (if (or (zerop length) (<= (visible-width line) max-width))
        (list (list line 0 length))
        (let ((chunks '())
              (chunk-start 0)
              (current-width 0)
              (wrap-opportunity -1)
              (wrap-opportunity-width 0))
          (dotimes (i length)
            (let* ((char (char line i))
                   (char-width (character-width char))
                   (whitespacep (editor-whitespace-char-p char)))
              (when (> (+ current-width char-width) max-width)
                (cond
                  ((and (>= wrap-opportunity 0)
                        (<= (+ (- current-width wrap-opportunity-width) char-width)
                            max-width))
                   (push (list (subseq line chunk-start wrap-opportunity)
                               chunk-start wrap-opportunity)
                         chunks)
                   (setf chunk-start wrap-opportunity
                         current-width (- current-width wrap-opportunity-width)))
                  ((< chunk-start i)
                   (push (list (subseq line chunk-start i) chunk-start i) chunks)
                   (setf chunk-start i
                         current-width 0)))
                (setf wrap-opportunity -1))
              (incf current-width char-width)
              (let ((next (when (< (1+ i) length) (char line (1+ i)))))
                (when (and whitespacep next (not (editor-whitespace-char-p next)))
                  (setf wrap-opportunity (1+ i)
                        wrap-opportunity-width current-width)))))
          (push (list (subseq line chunk-start) chunk-start length) chunks)
          (nreverse chunks)))))

(defun editor-layout (editor width)
  "Single source of truth for the editor's visual layout. Returns a list of
   (text . cursor-column) rows, where cursor-column is the visual column of the
   text cursor on that row or NIL when the cursor is not on it. The first visual
   row carries the prompt. Every other row (soft-wrapped or from an explicit
   newline) carries an equal-width continuation prefix, so the content width is
   uniform across rows and the cursor remaps by chunk containment."
  (let* ((prompt (editor-prompt editor))
         (prefix-width (visible-width prompt))
         (continuation (make-string prefix-width :initial-element #\Space))
         (content-width (max 1 (- width prefix-width)))
         (cursor-line (cursor-row editor))
         (cursor-column (cursor-column editor))
         (rows '()))
    (loop for line in (split-lines (editor-value editor))
          for logical-index from 0
          do (let* ((chunks (wrap-editor-logical-line line content-width))
                    (last-index (1- (length chunks))))
               (loop for (text start end) in chunks
                     for chunk-index from 0
                     for prefix = (if (and (zerop logical-index) (zerop chunk-index))
                                      prompt
                                      continuation)
                     for in-chunk = (and (= logical-index cursor-line)
                                         (if (= chunk-index last-index)
                                             (>= cursor-column start)
                                             (and (>= cursor-column start)
                                                  (< cursor-column end))))
                     for column = (when in-chunk
                                    (min (+ prefix-width
                                            (visible-width
                                             (subseq text 0
                                                     (min (- cursor-column start)
                                                          (length text)))))
                                         (max 0 (1- width))))
                     do (push (cons (truncate-to-width
                                     (concatenate 'string prefix text) width)
                                    column)
                              rows))))
    (nreverse rows)))

(defun layout-cursor-position (layout)
  (loop for (nil . column) in layout
        for row from 0
        when column return (cons row column)))

(defun editor-rows-and-cursor (editor width)
  "The editor's visual rows and cursor position from one layout pass, as
   (values rows cursor). Cursor is NIL when the editor is unfocused. For
   per-frame renderers that need both, sparing the second layout pass."
  (let ((layout (editor-layout editor width)))
    (values (mapcar #'car layout)
            (and (editor-focused editor)
                 (layout-cursor-position layout)))))

(defun editor-row-lines (editor width)
  "The editor's visual rows alone, without the completion popup block below
   them. Lets a renderer frame the input separately from the popup."
  (mapcar #'car (editor-layout editor width)))

(defun render-editor-lines (editor width)
  (append (editor-row-lines editor width)
          (render-completion-lines editor width)))

(defun editor-cursor-position (editor width)
  (if (editor-focused editor)
      (layout-cursor-position (editor-layout editor width))
      nil))
