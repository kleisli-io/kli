(in-package #:kli/text)

(defun character-width (char)
  "Visible terminal columns CHAR occupies, wcwidth-style. Tab is 3 (the
   NORMALIZE-TEXT expansion), combining and enclosing marks, format characters,
   and Hangul jamo medial vowels and finals are 0 (they compose into a
   preceding base), East-Asian wide and fullwidth characters are 2, everything
   else 1. Codepoints below U+0300 short-circuit to 1, the dominant case."
  (let ((code (char-code char)))
    (cond ((char= char #\Tab) 3)
          ((< code #x300) 1)
          ((<= #x1160 code #x11FF) 0)
          ((member (sb-unicode:general-category char) '(:mn :me :cf)) 0)
          ((member (sb-unicode:east-asian-width char) '(:w :f)) 2)
          (t 1))))

(defun visible-width (string)
  (loop for char across string
        sum (character-width char)))

(defun truncate-to-width (string width)
  (let ((width (max 0 width)))
    (with-output-to-string (out)
      (loop with used = 0
            for char across string
            for char-width = (character-width char)
            while (<= (+ used char-width) width)
            do (write-char char out)
               (incf used char-width)))))

(defun wrap-text (string width)
  (let ((width (max 1 width)))
    (loop for logical-line in (split-lines string)
          append (wrap-logical-line logical-line width))))

(defun pad-right (string width)
  (let ((missing (- width (visible-width string))))
    (if (plusp missing)
        (concatenate 'string string (make-string missing
                                                 :initial-element #\Space))
        (truncate-to-width string width))))

(defun wrap-logical-line (line width)
  (cond
    ((zerop (length line)) (list ""))
    (t
     ;; Track the accumulated line's width incrementally and join its words once
     ;; per flush. Recomputing (visible-width current) and rebuilding the string
     ;; per word is O(W) per word -> O(W * width) over the line. VISIBLE-WIDTH is
     ;; strictly additive (no neighbor context), so the incremental width equals
     ;; the per-word recompute and the join reproduces the same separators.
     (let ((lines '())
           (current-words '())
           (current-width 0))
       (flet ((flush ()
                (when current-words
                  (push (cons (nreverse current-words) current-width) lines)
                  (setf current-words '() current-width 0))))
         (dolist (word (split-words line))
           (let ((word-width (visible-width word)))
             (cond
               ((null current-words)
                (setf current-words (list word)
                      current-width word-width))
               ((<= (+ current-width 1 word-width) width)
                (push word current-words)
                (incf current-width (+ 1 word-width)))
               (t
                (flush)
                (setf current-words (list word)
                      current-width word-width)))))
         (flush))
       (mapcan (lambda (entry)
                 (let ((candidate (format nil "~{~A~^ ~}" (car entry))))
                   (if (<= (cdr entry) width)
                       (list candidate)
                       (hard-wrap candidate width))))
               (nreverse lines))))))

(defun hard-wrap (string width)
  "Slice STRING into pieces of at most WIDTH visible columns with no regard for
   word boundaries. A character wider than the remaining room starts a new
   piece, zero-width characters stay with their preceding base character, and a
   single character wider than WIDTH gets an over-wide piece so no content is
   lost."
  (let ((width (max 1 width))
        (pieces '())
        (start 0)
        (used 0))
    (loop for index from 0 below (length string)
          for char-width = (character-width (char string index))
          do (when (and (> (+ used char-width) width) (> index start))
               (push (subseq string start index) pieces)
               (setf start index
                     used 0))
             (incf used char-width))
    (when (< start (length string))
      (push (subseq string start) pieces))
    (nreverse pieces)))

(defun split-lines (string)
  (let ((lines '())
        (start 0))
    (loop for index from 0 below (length string)
          when (char= (char string index) #\Newline)
            do (push (subseq string start index) lines)
               (setf start (1+ index)))
    (push (subseq string start) lines)
    (nreverse lines)))

(defparameter *render-line-limit* 500
  "Most characters of one rendered line a tool emits into the model's context.
One structurally-long line (a minified bundle, a giant log record) would
otherwise flood the window. Render-only: truncation never alters stored state,
hashes, or offsets.")

(defun render-truncate-front (line)
  "LINE's head within *render-line-limit*, with a [+N chars] elision marker."
  (if (<= (length line) *render-line-limit*)
      line
      (format nil "~A[+~D chars]"
              (subseq line 0 *render-line-limit*)
              (- (length line) *render-line-limit*))))

(defun render-bounded-lines (text)
  "Front-truncate each line of TEXT past *render-line-limit* and rejoin, so one
structurally-long line cannot flood a multi-line render."
  (format nil "~{~A~^~%~}" (mapcar #'render-truncate-front (split-lines text))))

(defun split-words (string)
  (let ((words '())
        (start nil))
    (labels ((finish (index)
               (when start
                 (push (subseq string start index) words)
                 (setf start nil))))
      (loop for index from 0 below (length string)
            for char = (char string index)
            do (if (char= char #\Space)
                   (finish index)
                   (unless start
                     (setf start index))))
      (finish (length string)))
    (nreverse words)))

(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun split-on-whitespace (string)
  (let ((words '())
        (start nil)
        (length (length string)))
    (labels ((emit (end)
               (when start
                 (push (subseq string start end) words)
                 (setf start nil))))
      (loop for index below length
            for char = (char string index)
            do (if (whitespace-char-p char)
                   (emit index)
                   (unless start
                     (setf start index))))
      (emit length))
    (nreverse words)))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or string "")))

(defun blank-string-p (string)
  (or (null string)
      (zerop (length string))
      (every #'whitespace-char-p string)))

(defun normalize-text (string)
  (with-output-to-string (out)
    (loop for char across string
          do (if (char= char #\Tab)
                 (write-string "   " out)
                 (write-char char out)))))

(defun printable-string-p (string)
  (and (plusp (length string))
       (loop for char across string
             always (let ((code (char-code char)))
                      (and (>= code 32) (/= code 127))))))

(defun parse-positive-integer (string)
  (when string
    (ignore-errors
      (let ((value (parse-integer string :junk-allowed t)))
        (when (and value (plusp value))
          value)))))

(defun string-prefix-p (prefix string)
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string= prefix string :end2 prefix-length))))

(defun string-suffix-p (suffix string)
  (let ((suffix-length (length suffix))
        (string-length (length string)))
    (and (<= suffix-length string-length)
         (string= suffix string :start2 (- string-length suffix-length)))))
