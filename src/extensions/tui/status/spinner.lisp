(in-package #:kli/tui/status)

(defparameter +spinner-frames+
  #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille rotation frames the working indicator cycles through, one cell each.")

(defparameter +spinner-label+ "working"
  "Word shown after the glyph while a turn is in flight.")

(defun spinner-glyph (phase)
  "The braille frame for PHASE, wrapping around +SPINNER-FRAMES+ so any counter
   maps to a frame."
  (aref +spinner-frames+ (mod phase (length +spinner-frames+))))

(defun render-spinner-line (active phase width &optional detail)
  "One line -- glyph, label, then any tool progress DETAIL -- while
   ACTIVE, else NIL so the indicator reserves no row at an idle prompt.
   Clipped to WIDTH."
  (when active
    (let ((text (format nil "~A ~A~@[ -- ~A~]"
                        (spinner-glyph phase)
                        +spinner-label+
                        (and detail (plusp (length detail)) detail))))
      (list (if (> (length text) width)
                (subseq text 0 width)
                text)))))
