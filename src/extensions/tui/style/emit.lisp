(in-package #:kli/tui/style)

(defvar *color-mode* :truecolor
  ":truecolor | :256 | :none. The app sets it from detect-color-mode at startup.
Tests bind it for determinism.")

(defun fg-truecolor (c) (format nil "~C[38;2;~D;~D;~Dm" #\Esc (color-r c) (color-g c) (color-b c)))
(defun bg-truecolor (c) (format nil "~C[48;2;~D;~D;~Dm" #\Esc (color-r c) (color-g c) (color-b c)))
(defun fg-256 (c) (format nil "~C[38;5;~Dm" #\Esc (color->256 c)))
(defun bg-256 (c) (format nil "~C[48;5;~Dm" #\Esc (color->256 c)))

(defparameter +fg-reset+ (format nil "~C[39m" #\Esc))
(defparameter +bg-reset+ (format nil "~C[49m" #\Esc))

(defun sgr (&rest codes) (format nil "~C[~{~A~^;~}m" #\Esc codes))

(defparameter *attr-set*   '(:bold "1" :dim "2" :italic "3" :underline "4" :inverse "7" :strike "9"))
(defparameter *attr-reset* '(:bold "22" :dim "22" :italic "23" :underline "24" :inverse "27" :strike "29"))

(defun style-span (text &key fg bg attrs (mode *color-mode*))
  "Wrap TEXT in fg/bg/attr SGR, closing with SELECTIVE resets only. :none -> plain.
Attrs open in order and close in reverse, then bg, then fg, so a span nests
inside another without leaking either layer. Never emits ESC[0m."
  (if (eq mode :none)
      text
      (with-output-to-string (s)
        (when fg (write-string (ecase mode (:truecolor (fg-truecolor fg)) (:256 (fg-256 fg))) s))
        (when bg (write-string (ecase mode (:truecolor (bg-truecolor bg)) (:256 (bg-256 bg))) s))
        (dolist (a attrs) (write-string (sgr (getf *attr-set* a)) s))
        (write-string text s)
        (dolist (a (reverse attrs)) (write-string (sgr (getf *attr-reset* a)) s))
        (when bg (write-string +bg-reset+ s))
        (when fg (write-string +fg-reset+ s)))))
