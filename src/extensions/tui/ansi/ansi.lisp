(in-package #:kli/tui/ansi)

;;; Parse raw terminal output (SGR colour/attr sequences + tabs) into the seg
;;; model shared with the markdown renderer. Not a terminal emulator: cursor,
;;; erase, and other non-SGR control sequences are stripped, since tool output
;;; is captured to a file rather than a live PTY.

(defparameter +bel+ (code-char 7))

(defparameter +attr-order+ '(:bold :dim :italic :underline :inverse :strike)
  "Canonical attr order, so a given style always yields the same seg.")

(defparameter +basic-colors+
  #(#x000000 #x800000 #x008000 #x808000 #x000080 #x800080 #x008080 #xc0c0c0
    #x808080 #xff0000 #x00ff00 #xffff00 #x0000ff #xff00ff #x00ffff #xffffff)
  "xterm palette for SGR indices 0-15 (30-37/90-97 and 38;5;0..15).")

(defun clamp8 (v) (max 0 (min 255 (or v 0))))

(defun rgb (hex)
  (make-color (ldb (byte 8 16) hex) (ldb (byte 8 8) hex) (ldb (byte 8 0) hex)))

(defun palette-color (n)
  "xterm-256 index N -> colour: 0-15 basic, 16-231 the 6x6x6 cube, 232-255 gray."
  (let ((n (clamp8 n)))
    (cond ((< n 16) (rgb (aref +basic-colors+ n)))
          ((< n 232)
           (flet ((lvl (c) (if (zerop c) 0 (+ 55 (* c 40)))))
             (let ((i (- n 16)))
               (make-color (lvl (floor i 36)) (lvl (mod (floor i 6) 6)) (lvl (mod i 6))))))
          (t (let ((v (+ 8 (* 10 (- n 232))))) (make-color v v v))))))

(defun sgr-attr (code)
  (case code (1 :bold) (2 :dim) (3 :italic) (4 :underline) (7 :inverse) (9 :strike)))

(defun order-attrs (attrs)
  (remove-if-not (lambda (a) (member a attrs)) +attr-order+))

(defun take-extended-color (rest)
  "REST follows a 38/48 selector. Returns (values colour rest'), consuming the
   5;n (256) or 2;r;g;b (truecolor) form."
  (case (car rest)
    (5 (values (palette-color (cadr rest)) (cddr rest)))
    (2 (destructuring-bind (&optional r g b &rest more) (cdr rest)
         (values (make-color (clamp8 r) (clamp8 g) (clamp8 b)) more)))
    (t (values nil (cdr rest)))))

(defun apply-sgr (params fg attrs)
  "Fold SGR PARAMS over the current FG (colour or nil) and ATTRS (keyword list).
   Returns (values fg attrs). Background codes are consumed but not carried — the
   seg model has no background; the transcript paints one as a continuous layer."
  (loop with rest = (or params '(0))
        while rest
        for code = (pop rest)
        do (cond
             ((zerop code) (setf fg nil attrs nil))
             ((member code '(1 2 3 4 7 9)) (pushnew (sgr-attr code) attrs))
             ((= code 22) (setf attrs (remove :dim (remove :bold attrs))))
             ((= code 23) (setf attrs (remove :italic attrs)))
             ((= code 24) (setf attrs (remove :underline attrs)))
             ((= code 27) (setf attrs (remove :inverse attrs)))
             ((= code 29) (setf attrs (remove :strike attrs)))
             ((<= 30 code 37) (setf fg (palette-color (- code 30))))
             ((<= 90 code 97) (setf fg (palette-color (+ 8 (- code 90)))))
             ((= code 39) (setf fg nil))
             ((= code 38) (multiple-value-setq (fg rest) (take-extended-color rest)))
             ((= code 48) (setf rest (nth-value 1 (take-extended-color rest))))
             (t nil)))
  (values fg (order-attrs attrs)))

(defun style-key (fg attrs)
  (cons (and fg (list (color-r fg) (color-g fg) (color-b fg))) attrs))

(defun split-params (pstr)
  (let ((out '()) (start 0))
    (dotimes (k (length pstr))
      (when (char= (char pstr k) #\;)
        (push (subseq pstr start k) out)
        (setf start (1+ k))))
    (push (subseq pstr start) out)
    (nreverse (mapcar (lambda (tok)
                        (if (zerop (length tok)) 0 (or (parse-integer tok :junk-allowed t) 0)))
                      out))))

(defun scan-csi (string i n)
  "STRING[i]=ESC, STRING[i+1]=#\\[. Returns (values final params-string end):
   FINAL is the final byte (NIL if unterminated), END the index past it."
  (let ((pstart (+ i 2)) (j (+ i 2)))
    (loop while (and (< j n) (<= #x30 (char-code (char string j)) #x3F)) do (incf j))
    (let ((pend j))
      (loop while (and (< j n) (<= #x20 (char-code (char string j)) #x2F)) do (incf j))
      (if (and (< j n) (<= #x40 (char-code (char string j)) #x7E))
          (values (char string j) (subseq string pstart pend) (1+ j))
          (values nil "" n)))))

(defun skip-osc (string i n)
  "STRING[i]=ESC, STRING[i+1]=#\\]. Skip to just past the BEL or ST terminator."
  (loop for j from (+ i 2) below n
        for c = (char string j)
        do (cond ((char= c +bel+) (return (1+ j)))
                 ((and (char= c #\Esc) (< (1+ j) n) (char= (char string (1+ j)) #\\))
                  (return (+ j 2))))
        finally (return n)))

(defun ansi->segs (string)
  "Parse raw terminal output STRING into a flat seg list. Each seg's text is
   visible content only (tabs expanded via NORMALIZE-TEXT); its fg/attrs carry
   the active SGR style. Adjacent runs of identical style are one seg."
  (let ((segs '())
        (buf (make-string-output-stream))
        (fg nil) (attrs '())
        (n (length string)) (i 0))
    (flet ((flush ()
             (let ((text (get-output-stream-string buf)))
               (when (plusp (length text))
                 (push (make-seg (normalize-text text) fg attrs) segs)))))
      (loop while (< i n)
            for c = (char string i)
            do (cond
                 ((char/= c #\Esc) (write-char c buf) (incf i))
                 ((and (< (1+ i) n) (char= (char string (1+ i)) #\[))
                  (multiple-value-bind (final pstr end) (scan-csi string i n)
                    (setf i end)
                    (when (and final (char= final #\m))
                      (multiple-value-bind (nfg nattrs) (apply-sgr (split-params pstr) fg attrs)
                        (unless (equal (style-key fg attrs) (style-key nfg nattrs))
                          (flush)
                          (setf fg nfg attrs nattrs))))))
                 ((and (< (1+ i) n) (char= (char string (1+ i)) #\]))
                  (setf i (skip-osc string i n)))
                 (t (setf i (min n (+ i 2))))))
      (flush))
    (nreverse segs)))
