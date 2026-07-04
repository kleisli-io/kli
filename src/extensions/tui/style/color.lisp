(in-package #:kli/tui/style)

(defstruct (color (:constructor make-color (r g b)))
  (r 0 :type (unsigned-byte 8))
  (g 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8)))

(defun hex->color (hex)
  "#rrggbb or #rgb -> color. NIL/empty/non-# -> NIL (terminal default)."
  (when (and (stringp hex) (plusp (length hex)) (char= (char hex 0) #\#))
    (let ((h (subseq hex 1)))
      (flet ((b (s) (parse-integer s :radix 16)))
        (cond ((= (length h) 6)
               (make-color (b (subseq h 0 2)) (b (subseq h 2 4)) (b (subseq h 4 6))))
              ((= (length h) 3)
               (make-color (* 17 (b (subseq h 0 1)))
                           (* 17 (b (subseq h 1 2)))
                           (* 17 (b (subseq h 2 3))))))))))

(defparameter +cube-levels+ #(0 95 135 175 215 255))

(defun nearest-cube-axis (v)
  (let ((best 0) (bestd most-positive-fixnum))
    (dotimes (i 6 best)
      (let ((d (abs (- v (aref +cube-levels+ i)))))
        (when (< d bestd) (setf bestd d best i))))))

(defun wdist (r1 g1 b1 r2 g2 b2)
  "Fixed-weight redmean approximation of perceptual color distance."
  (+ (* 2 (expt (- r1 r2) 2)) (* 4 (expt (- g1 g2) 2)) (* 3 (expt (- b1 b2) 2))))

(defun color->256 (c)
  "Nearest xterm-256 index: best of the 6x6x6 cube and the 24-step gray ramp."
  (let* ((r (color-r c)) (g (color-g c)) (b (color-b c))
         (ri (nearest-cube-axis r)) (gi (nearest-cube-axis g)) (bi (nearest-cube-axis b))
         (cr (aref +cube-levels+ ri)) (cg (aref +cube-levels+ gi)) (cb (aref +cube-levels+ bi))
         (cube-idx (+ 16 (* 36 ri) (* 6 gi) bi))
         (cube-d   (wdist r g b cr cg cb))
         (gray-avg (round (+ r g b) 3))
         (gi2 (max 0 (min 23 (round (- gray-avg 8) 10))))
         (gray-level (+ 8 (* 10 gi2)))
         (gray-idx   (+ 232 gi2))
         (gray-d (wdist r g b gray-level gray-level gray-level)))
    (if (<= cube-d gray-d) cube-idx gray-idx)))

(defun detect-color-mode (&optional (colorterm (uiop:getenv "COLORTERM"))
                                     (term      (uiop:getenv "TERM")))
  (cond ((member colorterm '("truecolor" "24bit") :test #'equal) :truecolor)
        ((and term (search "256color" term)) :256)
        (t :none)))
