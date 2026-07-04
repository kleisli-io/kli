(in-package #:kli/tui/markdown)

(defun diff-opcodes (a b)
  (let ((m (make-instance 'sequence-matcher :test-function #'equal)))
    (set-sequences m a b)
    (get-opcodes m)))

(defun number-width (n)
  "Decimal digit count of line count N, floored at 1 so an empty side still
   reserves a column."
  (max 1 (length (princ-to-string (max 1 n)))))

(defun diff-gutter-width (ow nw)
  "Visible columns of the two-column gutter: OW + NW number columns, two
   separators, the marker, and a trailing space."
  (+ ow nw 4))

(defun diff-gutter (old-no new-no marker token theme ow nw)
  "Styled gutter for one diff row: OLD-NO in OW columns then NEW-NO in NW columns
   (NIL renders blank) then MARKER, all in TOKEN's colour."
  (style-span (format nil "~v@a ~v@a ~C " ow (or old-no "") nw (or new-no "") marker)
              :fg (md theme token)))

(defun diff-row-lines (old-no new-no marker token theme segs width ow nw)
  "SEGS as styled diff rows behind a numeric OLD-NO/NEW-NO gutter, soft-wrapping
   to WIDTH with a blank gutter on continuations. SEGS keep their own fg/attrs."
  (prefix-wrap-segs (diff-gutter old-no new-no marker token theme ow nw)
                    (diff-gutter-width ow nw) segs width))

(defun plain-row-lines (old-no new-no marker token theme text width ow nw)
  (diff-row-lines old-no new-no marker token theme
                  (list (make-seg (normalize-text text) (md theme token)))
                  width ow nw))

(defun replace-segs (string ops side fg)
  "Segs for one side of a single-line replacement: changed character runs carry
   inverse video, every run carries FG. SIDE is :old or :new."
  (let ((out '()))
    (dolist (op ops (nreverse out))
      (let ((tag (opcode-tag op)))
        (multiple-value-bind (lo hi changed)
            (ecase side
              (:old (values (opcode-i1 op) (opcode-i2 op) (member tag '(:delete :replace))))
              (:new (values (opcode-j1 op) (opcode-j2 op) (member tag '(:insert :replace)))))
          (when (and (< lo hi) (or changed (eq tag :equal)))
            (push (make-seg (subseq string lo hi) fg (and changed '(:inverse))) out)))))))

(defun render-diff-segments (segments a b a-hl theme width ow nw)
  "Render already-clipped diff SEGMENTS over line vectors A and B behind a
two-column numeric gutter OW/NW columns wide. Line numbers are true file indices
(opcode i/j + 1). A-HL, when non-nil, is A's syntax-highlighted segment vector
for equal/context lines. The second value is the rendered-line index of the first
changed row."
  (let ((out '())
        (first-change nil))
    (flet ((emit (lines) (dolist (l lines) (push l out))))
      (dolist (segment segments)
        (destructuring-bind (tag i1 i2 j1 j2) segment
          (unless (or first-change (eq tag :equal))
            (setf first-change (length out)))
          (ecase tag
            (:equal
             (loop for i from i1 below i2
                   for j from j1 below j2
                   do (emit (diff-row-lines
                             (1+ i) (1+ j) #\Space "toolDiffContext" theme
                             (if a-hl
                                 (aref a-hl i)
                                 (list (make-seg (normalize-text (aref a i))
                                                 (md theme "toolDiffContext"))))
                             width ow nw))))
            (:delete
             (loop for i from i1 below i2
                   do (emit (plain-row-lines (1+ i) nil #\- "toolDiffRemoved" theme
                                             (aref a i) width ow nw))))
            (:insert
             (loop for j from j1 below j2
                   do (emit (plain-row-lines nil (1+ j) #\+ "toolDiffAdded" theme
                                             (aref b j) width ow nw))))
            (:replace
             (if (and (= (- i2 i1) 1) (= (- j2 j1) 1))
                 (let ((ops (diff-opcodes (coerce (aref a i1) 'list)
                                          (coerce (aref b j1) 'list))))
                   (emit (diff-row-lines (1+ i1) nil #\- "toolDiffRemoved" theme
                                         (replace-segs (aref a i1) ops :old
                                                       (md theme "toolDiffRemoved"))
                                         width ow nw))
                   (emit (diff-row-lines nil (1+ j1) #\+ "toolDiffAdded" theme
                                         (replace-segs (aref b j1) ops :new
                                                       (md theme "toolDiffAdded"))
                                         width ow nw)))
                 (progn
                   (loop for i from i1 below i2
                         do (emit (plain-row-lines (1+ i) nil #\- "toolDiffRemoved" theme
                                                   (aref a i) width ow nw)))
                   (loop for j from j1 below j2
                         do (emit (plain-row-lines nil (1+ j) #\+ "toolDiffAdded" theme
                                                   (aref b j) width ow nw))))))))))
    (values (nreverse out) first-change)))

(defun diff-line-state (old new theme lang)
  "Normalize OLD/NEW and return line vectors plus highlighted old lines."
  (let* ((coloring (and lang (lang->coloring lang)))
         (old (if coloring (normalize-text old) old))
         (new (if coloring (normalize-text new) new))
         (a (if (zerop (length old)) #() (coerce (split-lines old) 'vector)))
         (b (if (zerop (length new)) #() (coerce (split-lines new) 'vector)))
         (a-hl (and coloring (coerce (highlight-code-lines old lang theme) 'vector))))
    (values a b a-hl)))

(defun opcode-segment (op)
  (list (opcode-tag op) (opcode-i1 op) (opcode-i2 op)
        (opcode-j1 op) (opcode-j2 op)))

(defun render-diff (old new theme width &key lang gutter-width)
  "Line diff of OLD→NEW with +/-/space gutters in the diff colours. A single-
line replacement gets character-level inverse video. All lines soft-wrap.
Empty content diffs as zero lines rather than one empty line, so a new
file renders all-added with no phantom removed line. Second value is the
rendered-line index of the first -/+ row, nil when the contents are equal.
When LANG names a known colorize type, unchanged context lines are syntax-
highlighted for that language; changed -/+ lines keep their diff colours so
the add/remove signal stays legible. GUTTER-WIDTH, when non-nil, sets both
number columns to that fixed width instead of this file's own digit count."
  (multiple-value-bind (a b a-hl) (diff-line-state old new theme lang)
    (render-diff-segments (mapcar #'opcode-segment (diff-opcodes a b))
                          a b a-hl theme width
                          (or gutter-width (number-width (length a)))
                          (or gutter-width (number-width (length b))))))

(defun opcode-hunk-window (op context-lines)
  "Raw line-coordinate preview window for one changed opcode."
  (list :i-start (max 0 (- (opcode-i1 op) context-lines))
        :i-end (+ (opcode-i2 op) context-lines)
        :j-start (max 0 (- (opcode-j1 op) context-lines))
        :j-end (+ (opcode-j2 op) context-lines)
        :hunks 1))

(defun merge-hunk-windows (windows)
  "Merge ordered preview WINDOWS whose old and new ranges overlap or touch."
  (let ((out '()))
    (dolist (window windows (nreverse out))
      (let ((last (first out)))
        (if (and last
                 (<= (getf window :i-start) (getf last :i-end))
                 (<= (getf window :j-start) (getf last :j-end)))
            (setf (getf last :i-end) (max (getf last :i-end) (getf window :i-end))
                  (getf last :j-end) (max (getf last :j-end) (getf window :j-end))
                  (getf last :hunks) (+ (getf last :hunks) (getf window :hunks)))
            (push (copy-list window) out))))))

(defun clip-segment-to-window (segment window)
  "Return SEGMENT clipped to WINDOW, or NIL when it falls outside. Changed
segments are kept whole once they intersect so hunk previews never slice a
delete/insert/replace block in half."
  (destructuring-bind (tag i1 i2 j1 j2) segment
    (let ((wi1 (getf window :i-start))
          (wi2 (getf window :i-end))
          (wj1 (getf window :j-start))
          (wj2 (getf window :j-end)))
      (ecase tag
        (:equal
         (let* ((lo (max i1 wi1 (+ i1 (- wj1 j1))))
                (hi (min i2 wi2 (+ i1 (- wj2 j1)))))
           (when (< lo hi)
             (list :equal lo hi (+ j1 (- lo i1)) (+ j1 (- hi i1))))))
        (:delete
         (when (< (max i1 wi1) (min i2 wi2))
           segment))
        (:insert
         (when (< (max j1 wj1) (min j2 wj2))
           segment))
        (:replace
         (when (or (< (max i1 wi1) (min i2 wi2))
                   (< (max j1 wj1) (min j2 wj2)))
           segment))))))

(defun render-diff-hunk-blocks (old new theme width &key lang (context-lines 1)
                                                      gutter-width)
  "Collapsed-preview blocks for every changed hunk in OLD→NEW.
Each returned block is a plist (:LINES rendered-lines :HUNKS changed-hunk-count).
Hunk windows are selected from raw diff opcodes before rendering, then rendered
with the same diff styling as RENDER-DIFF. Adjacent/overlapping windows are
merged before rendering. GUTTER-WIDTH, when non-nil, sets both number columns to
that fixed width instead of this file's own digit count."
  (multiple-value-bind (a b a-hl) (diff-line-state old new theme lang)
    (let* ((opcodes (diff-opcodes a b))
           (segments (mapcar #'opcode-segment opcodes))
           (ow (or gutter-width (number-width (length a))))
           (nw (or gutter-width (number-width (length b))))
           (windows (merge-hunk-windows
                     (loop for op in opcodes
                           unless (eq (opcode-tag op) :equal)
                             collect (opcode-hunk-window op context-lines)))))
      (loop for window in windows
            for clipped = (remove nil
                                  (mapcar (lambda (segment)
                                            (clip-segment-to-window segment window))
                                          segments))
            collect (list :lines (render-diff-segments clipped a b a-hl theme width ow nw)
                          :hunks (getf window :hunks))))))

(defun unified-diff-token (line)
  "Theme token + attrs for a unified-diff LINE keyed by its leading marker."
  (cond ((or (string-prefix-p "+++" line) (string-prefix-p "---" line)) (values "accent" '(:bold)))
        ((string-prefix-p "@@" line) (values "accent" nil))
        ((string-prefix-p "+" line) (values "toolDiffAdded" nil))
        ((string-prefix-p "-" line) (values "toolDiffRemoved" nil))
        (t (values "toolDiffContext" nil))))

(defun render-unified-diff (text theme width)
  "Colourise an already-unified-diff TEXT by its line markers. Soft-wrap long
   lines."
  (loop for line in (split-lines text)
        append (multiple-value-bind (token attrs) (unified-diff-token line)
                 (let ((fg (md theme token)))
                   (loop for piece in (char-wrap-segs (list (make-seg (normalize-text line)))
                                                      (max 1 width))
                         collect (style-span (render-seg-line piece) :fg fg :attrs attrs))))))

(defun hunk-header-p (line)
  (and (string-prefix-p "@@ -" line)
       (search " +" line :start2 4)
       (search " @@" line)))

(defun unified-diff-p (text)
  "True only when TEXT is structurally a unified diff — a hunk header, or both
   file headers — so it never fires on prose that merely starts a line with +/-."
  (when (and text (plusp (length text)))
    (let ((lines (split-lines text)))
      (or (some #'hunk-header-p lines)
          (and (some (lambda (l) (string-prefix-p "--- " l)) lines)
               (some (lambda (l) (string-prefix-p "+++ " l)) lines))))))
