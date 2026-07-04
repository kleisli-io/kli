(in-package #:kli/tui/markdown)

(defun heading-attrs (level)
  (if (<= level 2) '(:bold :underline) '(:bold)))

(defun heading-lines (node theme width)
  (let ((level (getf (cdr node) :level))
        (inlines (getf (cdr node) :contents)))
    (para->lines inlines theme width
                 :base-fg (md theme "mdHeading")
                 :base-attrs (heading-attrs level))))

(defun code-block-lines (node theme width)
  (let* ((lang (getf (cdr node) :lang))
         (content (or (getf (cdr node) :content) ""))
         (border (md theme "mdCodeBlockBorder")))
    (when (and (plusp (length content))
               (char= (char content (1- (length content))) #\Newline))
      (setf content (subseq content 0 (1- (length content)))))
    (let ((top (let* ((label (if (and lang (plusp (length lang)))
                                 (format nil "─ ~A " lang)
                                 ""))
                      (fill (max 0 (- width (visible-width label)))))
                 (style-span (concatenate 'string label
                                          (make-string fill :initial-element #\─))
                             :fg border)))
          (bottom (style-span (make-string (max 1 width) :initial-element #\─) :fg border)))
      (append (list top)
              (loop for segline in (highlight-code-lines content lang theme)
                    append (mapcar #'render-seg-line (char-wrap-segs segline width)))
              (list bottom)))))

(defun blockquote-lines (node theme width)
  (let ((gutter (style-span "│ " :fg (md theme "mdQuoteBorder"))))
    (mapcar (lambda (l) (concatenate 'string gutter l))
            (blocks->lines (cdr node) theme (max 1 (- width 2))
                           :base-fg (md theme "mdQuote")
                           :base-attrs '(:italic)))))

(defun list-lines (node theme width &key base-fg base-attrs)
  (let ((ordered (member (car node) '(:ordered-list :counted-list))) (n 0) (out '()))
    (dolist (item (cdr node) (nreverse out))
      (when (consp item)
        (incf n)
        (let* ((marker (if ordered (format nil "~D. " n) "- "))
               (mw (length marker))
               (bullet (style-span marker :fg (md theme "mdListBullet")))
               (inner (blocks->lines (cdr item) theme (max 1 (- width mw))
                                     :base-fg base-fg :base-attrs base-attrs)))
          (loop for line in (or inner (list ""))
                for first = t then nil
                do (push (concatenate 'string
                                      (if first bullet (make-string mw :initial-element #\Space))
                                      line)
                         out)))))))

(defun clamp-columns (natural ncols width)
  "Per-column content widths that fit WIDTH once borders and 1-space cell
   padding are accounted for, shrinking the widest columns proportionally and
   never below 1 so the box always closes."
  (let* ((overhead (+ (1+ ncols) (* 2 ncols)))
         (avail (- width overhead))
         (total (reduce #'+ natural :initial-value 0)))
    (cond
      ((<= avail ncols) (make-list ncols :initial-element 1))
      ((<= total avail) natural)
      (t (let* ((scaled (mapcar (lambda (w) (max 1 (floor (* w avail) (max 1 total)))) natural))
                (slack (- avail (reduce #'+ scaled :initial-value 0)))
                (order (sort (loop for i below ncols collect i) #'>
                             :key (lambda (i) (nth i natural)))))
           (loop for i in order while (plusp slack)
                 do (incf (nth i scaled)) (decf slack))
           scaled)))))

(defun cell-content-segs (cell theme base-fg base-attrs)
  "CELL = (th|td (:plain . inlines) align) → seg list for its content."
  (inline-segments (and (consp (second cell)) (cdr (second cell)))
                   theme base-fg base-attrs))

(defun table-lines (node theme width)
  (destructuring-bind (&key head body) (cdr node)
    (let* ((hf (md theme "mdHeading"))
           (head-rows (mapcar (lambda (r)
                                (mapcar (lambda (c) (cell-content-segs c theme hf '(:bold))) r))
                              head))
           (body-rows (mapcar (lambda (r)
                                (mapcar (lambda (c) (cell-content-segs c theme nil nil)) r))
                              body))
           (all (append head-rows body-rows))
           (ncols (reduce #'max all :key #'length :initial-value 0)))
      (when (zerop ncols)
        (return-from table-lines nil))
      (let* ((natural (loop for c below ncols
                            collect (loop for row in all
                                          maximize (if (< c (length row))
                                                       (seg-line-width (nth c row))
                                                       0))))
             (border (md theme "mdQuoteBorder"))
             (colw (clamp-columns natural ncols width)))
        (labels ((bar (l m r)
                   (style-span
                    (with-output-to-string (s)
                      (write-string l s)
                      (loop for c below ncols for w in colw
                            do (write-string (make-string (+ w 2) :initial-element #\─) s)
                               (write-string (if (= c (1- ncols)) r m) s)))
                    :fg border))
                 (vbar () (style-span "│" :fg border))
                 (row-lines (row)
                   (let* ((wrapped (loop for c below ncols
                                         collect (wrap-runs
                                                  (segments->runs (if (< c (length row))
                                                                      (nth c row)
                                                                      '()))
                                                  (nth c colw))))
                          (height (reduce #'max wrapped :key #'length :initial-value 1)))
                     (loop for li below height
                           collect (with-output-to-string (s)
                                     (write-string (vbar) s)
                                     (loop for c below ncols for w in colw
                                           for cl = (let ((ls (nth c wrapped)))
                                                      (if (< li (length ls)) (nth li ls) '()))
                                           do (write-string " " s)
                                              (write-string (render-seg-line cl) s)
                                              (write-string
                                               (make-string (max 0 (- w (seg-line-width cl)))
                                                            :initial-element #\Space)
                                               s)
                                              (write-string " " s)
                                              (write-string (vbar) s)))))))
          (append (list (bar "┌" "┬" "┐"))
                  (loop for r in head-rows append (row-lines r))
                  (when head-rows (list (bar "├" "┼" "┤")))
                  (loop for r in body-rows append (row-lines r))
                  (list (bar "└" "┴" "┘"))))))))

(defun block->lines (node theme width &key base-fg base-attrs)
  (cond
    ((stringp node) (para->lines (list node) theme width
                                 :base-fg base-fg :base-attrs base-attrs))
    ((not (consp node)) nil)
    ((eq (car node) 'code-block) (code-block-lines node theme width))
    ((eq (car node) 'table) (table-lines node theme width))
    (t (case (car node)
         (:heading (heading-lines node theme width))
         ((:paragraph :plain) (para->lines (cdr node) theme width
                                           :base-fg base-fg :base-attrs base-attrs))
         (:block-quote (blockquote-lines node theme width))
         ((:bullet-list :ordered-list :counted-list)
          (list-lines node theme width :base-fg base-fg :base-attrs base-attrs))
         (:horizontal-rule
          (list (style-span (make-string (max 1 width) :initial-element #\─)
                            :fg (md theme "mdHr"))))
         ((:reference) nil)
         ((:html :raw-html)
          (para->lines (list (format nil "~A" (second node))) theme width
                       :base-fg base-fg :base-attrs base-attrs))
         (t (para->lines (cdr node) theme width
                         :base-fg base-fg :base-attrs base-attrs))))))

(defun blocks->lines (blocks theme width &key base-fg base-attrs)
  "Render a block sequence with one blank line between adjacent rendered blocks."
  (let ((out '()) (first t))
    (dolist (block blocks (nreverse out))
      (let ((lines (block->lines block theme width
                                 :base-fg base-fg :base-attrs base-attrs)))
        (when lines
          (unless first (push "" out))
          (setf first nil)
          (dolist (l lines) (push l out)))))))

(defun %markdown->lines-full (markdown theme width)
  "Parse MARKDOWN (3bmd, into a node tree with no ANSI printer) and walk each
   block to a list of width-wrapped styled lines via the inline machinery. The
   extension flags gate the parser, not just a printer. Bound to T they yield
   structured code-block and table nodes (unbound, fences and tables degrade). A
   trailing newline is appended so a table (or other terminator-sensitive block)
   at end of input parses as structure rather than literal text."
  (let ((doc (let ((*code-blocks* t) (*tables* t))
               (parse-doc (concatenate 'string (or markdown "") (string #\Newline))))))
    (blocks->lines doc theme width)))

;;; Incremental render for a growing reply (the streaming assistant path).
;;; Re-parsing the whole accumulated text on every delta is superlinear in the
;;; parser, so a long reply costs O(reply^k) per delta. Instead split the text at
;;; provably-safe blank-line boundaries into a committed stable prefix and an open
;;; tail: each newly-closed segment parses+renders exactly once into cached lines,
;;; and only the open tail re-renders per delta. The output is byte-identical to a
;;; full render because, AT A SAFE BOUNDARY,
;;;   full(prefix ++ tail) == full(prefix) ++ ("") ++ full(tail)
;;; (the same single "" inter-block separator blocks->lines uses, and only when
;;; both sides render to lines). The predicate below is conservative: when in
;;; doubt it does not advance, so the tail simply re-renders as before.

(defun %blank-line-p (text start end)
  "T when TEXT[START:END) is empty or all space/tab."
  (loop for i from start below end
        always (let ((c (char text i))) (or (char= c #\Space) (char= c #\Tab)))))

(defun %line-indent (text start end)
  "Count of leading spaces in TEXT[START:END); a leading tab counts as >=4 so it
   reads as an indented-code / continuation indent."
  (let ((n 0))
    (loop for i from start below end
          for c = (char text i)
          do (cond ((char= c #\Space) (incf n))
                   ((char= c #\Tab) (return (+ n 4)))
                   (t (return n)))
          finally (return n))))

(defun %fence-line-p (text start end)
  "When TEXT[START:END), after <=3 leading spaces, opens/closes a fenced code
   block, return the fence char (#\\` or #\\~); else NIL. Three or more of the same
   fence char is the toggle; closer length-matching is ignored, which only loosens
   detection conservatively -- a stray closer never creates a false boundary
   because boundaries also require the preceding line to be top-level."
  (let ((i start) (spaces 0))
    (loop while (and (< i end) (char= (char text i) #\Space) (< spaces 3))
          do (incf i) (incf spaces))
    (when (and (< i end) (member (char text i) '(#\` #\~)))
      (let ((fc (char text i)) (run 0))
        (loop while (and (< i end) (char= (char text i) fc)) do (incf i) (incf run))
        (when (>= run 3) fc)))))

(defun %container-line-p (text start end)
  "T when TEXT[START:END), after <=3 leading spaces, begins a list item
   (-,+,* then space/tab/EOL; or digits then .,) then space/tab/EOL) or a
   blockquote (>). Such a line's block can merge with following content across a
   blank line, so a boundary must never sit right after one."
  (let ((i start) (spaces 0))
    (loop while (and (< i end) (char= (char text i) #\Space) (< spaces 3))
          do (incf i) (incf spaces))
    (when (< i end)
      (let ((c (char text i)))
        (cond
          ((char= c #\>) t)
          ((member c '(#\- #\+ #\*))
           (let ((n (1+ i)))
             (or (>= n end) (char= (char text n) #\Space) (char= (char text n) #\Tab))))
          ((digit-char-p c)
           (let ((j i))
             (loop while (and (< j end) (digit-char-p (char text j))) do (incf j))
             (and (< j end) (member (char text j) '(#\. #\)))
                  (let ((n (1+ j)))
                    (or (>= n end) (char= (char text n) #\Space) (char= (char text n) #\Tab))))))
          (t nil))))))

(defun %lines-with-offsets (text from)
  "List of (start end has-newline next-start) for the lines of TEXT from FROM."
  (let ((len (length text)) (out '()) (i from))
    (loop while (<= i len)
          do (let ((nl (position #\Newline text :start i)))
               (if nl
                   (progn (push (list i nl t (1+ nl)) out) (setf i (1+ nl)))
                   (progn (when (< i len) (push (list i len nil len) out))
                          (return)))))
    (nreverse out)))

(defun next-safe-boundary (text from)
  "Largest offset B (FROM <= B <= len) such that TEXT[0:B] is a safe stable prefix
   per the rule above. Fence state at FROM is closed (FROM is 0 or a prior safe
   boundary). Returns FROM when no safe boundary is found."
  (let ((lines (%lines-with-offsets text from))
        (in-fence nil) (fence-char nil)
        (last-nonblank nil)
        (best from))
    (loop for cell on lines
          for (start end has-nl next) = (car cell)
          do (let ((fc (%fence-line-p text start end)))
               (cond
                 (in-fence (when (and fc (char= fc fence-char))
                             (setf in-fence nil fence-char nil))
                           (setf last-nonblank (cons start end)))
                 (fc (setf in-fence t fence-char fc)
                     (setf last-nonblank (cons start end)))
                 ((%blank-line-p text start end)
                  (when (and has-nl last-nonblank
                             (not (%container-line-p text (car last-nonblank) (cdr last-nonblank)))
                             (< (%line-indent text (car last-nonblank) (cdr last-nonblank)) 4))
                    (let ((follow (loop for c2 in (cdr cell)
                                        for (s2 e2) = c2
                                        unless (%blank-line-p text s2 e2)
                                          return c2)))
                      (when (and follow (zerop (%line-indent text (first follow) (second follow))))
                        (setf best next)))))
                 (t (setf last-nonblank (cons start end))))))
    best))

(defstruct (mdstream (:constructor make-mdstream))
  "Incremental render state for one growing reply. STABLE-LINES is the cached
   render of the committed prefix; STABLE-TAIL is its last cons, so a newly-closed
   segment splices on in O(segment) without re-copying the prefix."
  (stable-offset 0) (stable-lines '()) (stable-tail nil) (theme :unset) (width -1))

(defun mdstream-reset (st theme width)
  (setf (mdstream-stable-offset st) 0 (mdstream-stable-lines st) '()
        (mdstream-stable-tail st) nil (mdstream-theme st) theme (mdstream-width st) width))

(defun %blank-suffix-p (text from)
  (loop for i from from below (length text)
        always (let ((c (char text i)))
                 (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))))

(defun mdstream-render (st text theme width)
  "Incremental analogue of (markdown->lines TEXT THEME WIDTH) for a growing TEXT.
   Resets when THEME or WIDTH changes; otherwise assumes TEXT only grew."
  (unless (and (eql theme (mdstream-theme st)) (= width (mdstream-width st)))
    (mdstream-reset st theme width))
  (let ((b (next-safe-boundary text (mdstream-stable-offset st))))
    (when (> b (mdstream-stable-offset st))
      ;; A committed segment renders once and is final, so its (different) code
      ;; block must not feed the open block's *HL-STREAM*; the open tail below
      ;; keeps the binding. Without this a closing segment would thrash the
      ;; stream's anchor against an unrelated block.
      (let ((seg (let ((*hl-stream* nil))
                   (%markdown->lines-full
                    (subseq text (mdstream-stable-offset st) b) theme width))))
        ;; An empty segment (a lone reference definition / HTML comment) advances
        ;; the offset but adds no lines and no separator, matching blocks->lines.
        (when seg
          (let ((chunk (if (mdstream-stable-lines st) (cons "" seg) seg)))
            (if (mdstream-stable-tail st)
                (setf (cdr (mdstream-stable-tail st)) chunk)
                (setf (mdstream-stable-lines st) chunk))
            (setf (mdstream-stable-tail st) (last chunk)))))
      (setf (mdstream-stable-offset st) b)))
  ;; Stitch the cached prefix with a fresh render of the open tail. The append
  ;; copies only the prefix spine (no parse) into an immutable result that never
  ;; aliases STABLE-LINES; a tail that renders to nothing contributes no separator.
  (let ((off (mdstream-stable-offset st)))
    (cond
      ((%blank-suffix-p text off) (mdstream-stable-lines st))
      ((null (mdstream-stable-lines st)) (%markdown->lines-full (subseq text off) theme width))
      (t (let ((tail (%markdown->lines-full (subseq text off) theme width)))
           (if tail
               (append (mdstream-stable-lines st) (list "") tail)
               (mdstream-stable-lines st)))))))

(defvar *md-stream* nil
  "When bound to an MDSTREAM, MARKDOWN->LINES renders the growing TEXT
   incrementally against it; otherwise it does a full parse+render. The streaming
   transcript path binds it per open reply so each delta avoids re-parsing the
   whole accumulated text.")

(defun markdown->lines (markdown theme width)
  "Render MARKDOWN to width-wrapped styled lines. Incremental against *MD-STREAM*
   when bound (the streaming reply), a full parse+render otherwise."
  (if *md-stream*
      (mdstream-render *md-stream* (or markdown "") theme width)
      (%markdown->lines-full markdown theme width)))
