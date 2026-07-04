(in-package #:kli/tui/markdown)

(defstruct (seg (:constructor make-seg (text &optional fg attrs)))
  "A styled run of text. Inline prose, highlighted code, and table cells are all
   seg lists, folded to a column by one visible-width-aware wrapper that carries
   open SGR state across the break. Width is always counted on the plain text,
   never the ANSI, so style never inflates the layout."
  text fg (attrs nil))

(defun md (theme token)
  (theme-token theme token))

(defun render-seg-line (segs)
  "Concatenate SEGS into one styled string. A seg with no fg/attrs emits its
   text verbatim (no escapes)."
  (with-output-to-string (out)
    (dolist (s segs)
      (let ((fg (seg-fg s)) (attrs (seg-attrs s)) (text (seg-text s)))
        (write-string (if (or fg attrs) (style-span text :fg fg :attrs attrs) text) out)))))

(defun seg-line-width (segs)
  (reduce #'+ segs :key (lambda (s) (visible-width (seg-text s))) :initial-value 0))

(defun inline-segments (nodes theme &optional fg attrs)
  "Walk inline NODES into a flat seg list preserving exact source spacing.
   Nested emph/strong/link/code style their span without fabricating separators
   between adjacent nodes."
  (let ((out '()))
    (labels ((walk (nodes fg attrs)
               (dolist (node nodes)
                 (cond
                   ((stringp node) (push (make-seg node fg attrs) out))
                   ((consp node)
                    (case (car node)
                      (:emph (walk (cdr node) fg (cons :italic attrs)))
                      (:strong (walk (cdr node) fg (cons :bold attrs)))
                      (:code (push (make-seg (second node) (md theme "mdCode") attrs) out))
                      ((:explicit-link :reference-link)
                       (walk (getf (cdr node) :label) (md theme "mdLink") (cons :underline attrs))
                       (let ((src (getf (cdr node) :source)))
                         (when src
                           (push (make-seg (format nil " (~A)" src) (md theme "mdLinkUrl") attrs)
                                 out))))
                      (:line-break (push (make-seg (string #\Newline)) out))
                      (:entity (push (make-seg (format nil "~A" (second node)) fg attrs) out))
                      (t (walk (cdr node) fg attrs))))))))
      (walk nodes fg attrs))
    (nreverse out)))

(defun segments->runs (segs)
  "Flatten SEGS into alternating runs: (:word seglist width), (:space n), and
   (:break). A word merges adjacent non-space text across segs so styled
   neighbours stay one unit. Spaces and newlines are only ever reproduced from
   the source, never fabricated."
  (let ((runs '()) (word '()) (wwidth 0))
    (labels ((flush ()
               (when word
                 (push (list :word (nreverse word) wwidth) runs)
                 (setf word '() wwidth 0))))
      (dolist (s segs)
        (let* ((text (seg-text s)) (fg (seg-fg s)) (attrs (seg-attrs s))
               (n (length text)) (i 0))
          (loop while (< i n) do
            (let ((c (char text i)))
              (cond
                ((char= c #\Newline) (flush) (push (list :break) runs) (incf i))
                ((char= c #\Space)
                 (flush)
                 (let ((j i))
                   (loop while (and (< j n) (char= (char text j) #\Space)) do (incf j))
                   (push (list :space (- j i) fg attrs) runs)
                   (setf i j)))
                (t
                 (let ((j i))
                   (loop while (and (< j n)
                                    (not (char= (char text j) #\Space))
                                    (not (char= (char text j) #\Newline)))
                         do (incf j))
                   (let ((piece (subseq text i j)))
                     (push (make-seg piece fg attrs) word)
                     (incf wwidth (visible-width piece)))
                   (setf i j))))))))
      (flush))
    (nreverse runs)))

(defun char-wrap-segs (segs width)
  "Break SEGS into seg-lines of at most WIDTH visible columns, preserving every
   character (including leading spaces), splitting within a seg when needed.
   Wide characters consume two columns of the budget, zero-width characters
   stay with their preceding base character, and a character wider than the
   remaining room moves whole to the next line. An empty seg list yields a
   single empty line so blank source lines survive."
  (when (null segs)
    (return-from char-wrap-segs (list '())))
  (let ((width (max 1 width)) (lines '()) (line '()) (col 0))
    (flet ((finish () (push (nreverse line) lines) (setf line '() col 0)))
      (dolist (s segs)
        (let* ((text (seg-text s)) (fg (seg-fg s)) (attrs (seg-attrs s))
               (n (length text)) (start 0) (i 0))
          (flet ((emit (end)
                   (when (> end start)
                     (push (make-seg (subseq text start end) fg attrs) line))
                   (setf start end)))
            (loop while (< i n)
                  do (let ((cw (character-width (char text i))))
                       (when (and (plusp col) (> (+ col cw) width))
                         (emit i)
                         (finish))
                       (incf col cw)
                       (incf i)))
            (emit n))))
      (when (or line (null lines)) (finish)))
    (nreverse lines)))

(defun prefix-wrap-segs (prefix prefix-width segs width)
  "Render the already-styled PREFIX string (PREFIX-WIDTH visible columns) ahead
   of SEGS, soft-wrapping the content to WIDTH under a blank PREFIX-WIDTH-column
   hanging indent. SEGS keep their own fg/attrs and PREFIX its own styling, so a
   muted line gutter can sit before syntax-highlighted source. An empty SEGS list
   yields a single prefix-only line."
  (let ((pieces (char-wrap-segs segs (max 1 (- width prefix-width))))
        (pad (make-string (max 0 prefix-width) :initial-element #\Space)))
    (loop for piece in pieces
          for first = t then nil
          collect (concatenate 'string
                               (if first prefix pad)
                               (render-seg-line piece)))))

(defun wrap-runs (runs width)
  "Greedy word-wrap RUNS to WIDTH, returning a list of seg-lines. Separators are
   reproduced mid-line carrying the style of their source text (so an underlined
   heading or link stays continuous across its spaces) and dropped at a break. A
   word longer than WIDTH is char-split so no content is lost."
  (let ((width (max 1 width)) (lines '()) (line '()) (col 0)
        (pending 0) (pending-segs '()))
    (flet ((finish ()
             (push (nreverse line) lines)
             (setf line '() col 0 pending 0 pending-segs '())))
      (dolist (run runs)
        (ecase (car run)
          (:break (finish))
          (:space (destructuring-bind (n &optional fg attrs) (cdr run)
                    (incf pending n)
                    (push (make-seg (make-string n :initial-element #\Space) fg attrs)
                          pending-segs)))
          (:word
           (destructuring-bind (segs w) (cdr run)
             (when (and (plusp col) (> (+ col pending w) width))
               (finish))
             (if (and (zerop col) (> w width))
                 (loop for rest on (char-wrap-segs segs width)
                       do (setf line (reverse (car rest))
                                col (seg-line-width (car rest))
                                pending 0 pending-segs '())
                          (when (cdr rest) (finish)))
                 (progn
                   (when (plusp pending)
                     (dolist (s (nreverse pending-segs)) (push s line))
                     (incf col pending)
                     (setf pending 0 pending-segs '()))
                   (dolist (s segs) (push s line))
                   (incf col w)))))))
      (when line (finish)))
    (nreverse lines)))

(defun para->lines (inlines theme width &key base-fg base-attrs)
  "Render inline NODES as word-wrapped styled lines at WIDTH."
  (mapcar #'render-seg-line
          (wrap-runs (segments->runs (inline-segments inlines theme base-fg base-attrs))
                     width)))
