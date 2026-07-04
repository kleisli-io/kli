(in-package #:kli/tui/terminal)

(defun split-on-char (char string)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start (or pos (length string)))
        while pos
        do (setf start (1+ pos))))

(defun parse-hex-channel (hex)
  "Scale a hex color field to a double in [0,1], width-independent, or NIL."
  (let ((len (length hex)))
    (when (plusp len)
      (multiple-value-bind (value end)
          (parse-integer hex :radix 16 :junk-allowed t)
        (when (and value (= end len))
          (/ (coerce value 'double-float)
             (coerce (1- (expt 2 (* 4 len))) 'double-float)))))))

(defun parse-osc-color-report (response)
  "Parse an OSC 10/11 color report into (R G B) doubles in [0,1], or NIL.
Handles 16-bit and 8-bit channels and both BEL and ST terminators."
  (let ((start (search "rgb:" response)))
    (when start
      (let* ((body (string-right-trim (list #\Bel #\Esc #\\)
                                      (subseq response (+ start 4))))
             (fields (split-on-char #\/ body)))
        (when (= 3 (length fields))
          (let ((channels (mapcar #'parse-hex-channel fields)))
            (when (every #'identity channels)
              channels)))))))

(defun linearize-channel (c)
  "Undo sRGB gamma for one channel."
  (declare (type double-float c))
  (if (<= c 0.04045d0)
      (/ c 12.92d0)
      (expt (/ (+ c 0.055d0) 1.055d0) 2.4d0)))

(defun relative-luminance (rgb)
  "WCAG relative luminance of an (R G B) triple of doubles in [0,1]."
  (destructuring-bind (r g b) rgb
    (+ (* 0.2126d0 (linearize-channel (coerce r 'double-float)))
       (* 0.7152d0 (linearize-channel (coerce g 'double-float)))
       (* 0.0722d0 (linearize-channel (coerce b 'double-float))))))

(defun classify-background (rgb)
  "Classify an (R G B) triple as :dark below mid luminance, else :light."
  (if (< (relative-luminance rgb) 0.5d0) :dark :light))

(defun parse-colorfgbg (value)
  "Classify a COLORFGBG value by its trailing background index, or NIL.
Indices 0-6 and 8 read dark, 7 and 9-15 read light, anything else NIL."
  (when (and value (plusp (length value)))
    (let ((index (parse-integer (car (last (split-on-char #\; value)))
                                :junk-allowed t)))
      (cond ((null index) nil)
            ((or (<= 0 index 6) (= index 8)) :dark)
            ((or (= index 7) (<= 9 index 15)) :light)
            (t nil)))))

(defun da1-reply-position (buffer)
  "Position just past a complete DA1 reply (ESC [ ? ... c) in BUFFER, or NIL."
  (let* ((marker (format nil "~C[?" #\Esc))
         (start (search marker buffer)))
    (when start
      (let ((terminator (position #\c buffer :start (+ start (length marker)))))
        (when terminator
          (1+ terminator))))))

(defun scan-osc-color-report (buffer)
  "Return a complete OSC 11 report substring in BUFFER, or NIL if incomplete.
Closes on the nearest BEL or ST terminator after the introducer."
  (let ((start (search (format nil "~C]11;" #\Esc) buffer)))
    (when start
      (let ((bel (position #\Bel buffer :start start))
            (st (search (format nil "~C\\" #\Esc) buffer :start2 start)))
        (cond ((and st (or (null bel) (< st bel)))
               (subseq buffer start (+ st 2)))
              (bel (subseq buffer start (1+ bel))))))))

(defun resolve-background-mode (status colorfgbg)
  "Resolve a :dark or :light background mode from a handshake STATUS and a
COLORFGBG value. An (R G B) STATUS classifies directly, otherwise COLORFGBG
decides, falling back to :dark."
  (if (consp status)
      (classify-background status)
      (or (parse-colorfgbg colorfgbg) :dark)))

(defun query-terminal-background (input write-control &key (timeout 0.15))
  "Query the terminal background colour over INPUT, writing via WRITE-CONTROL.
Returns (values STATUS RESIDUAL): STATUS is an (R G B) triple, :no-support,
or :timeout. RESIDUAL is the raw accumulated input, fed back verbatim so the
decoder discards the replies and preserves any typed-ahead keystrokes."
  (funcall write-control (format nil "~C]11;?~C\\~C[c" #\Esc #\Esc #\Esc))
  (let ((buffer (make-array 0 :element-type 'character
                              :adjustable t :fill-pointer 0))
        (deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop
      (loop for char = (read-char-no-hang input nil nil)
            while char do (vector-push-extend char buffer))
      (let ((report (scan-osc-color-report buffer)))
        (cond (report
               (return (values (parse-osc-color-report report) buffer)))
              ((da1-reply-position buffer)
               (return (values :no-support buffer)))
              ((>= (get-internal-real-time) deadline)
               (return (values :timeout buffer)))
              (t (sleep 0.005)))))))
