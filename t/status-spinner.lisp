(in-package #:kli/tests)

(in-suite all)

(test status-spinner-glyph-cycles-through-frames
  "Each phase maps to its frame and the index wraps, so a free-running counter
   never indexes past the ramp."
  (is (string= "⠋" (tui-status:spinner-glyph 0)))
  (is (string= "⠙" (tui-status:spinner-glyph 1)))
  (is (string= "⠏" (tui-status:spinner-glyph 9)))
  (is (string= (tui-status:spinner-glyph 0) (tui-status:spinner-glyph 10)))
  (is (string= (tui-status:spinner-glyph 3) (tui-status:spinner-glyph 13))))

(test status-spinner-line-shows-glyph-and-label-only-while-active
  "An active spinner is one line of glyph then label. Idle reserves no row."
  (is (null (tui-status:render-spinner-line nil 0 40)))
  (let ((lines (tui-status:render-spinner-line t 1 40)))
    (is (= 1 (length lines)))
    (is (string= "⠙ working" (first lines)))))

(test status-spinner-line-clips-to-width
  "The line never overflows its width budget."
  (is (string= "⠋ w" (first (tui-status:render-spinner-line t 0 3)))))

(test status-spinner-line-appends-tool-update-detail
  "A tool progress detail rides after the label, still one line and still
   clipped to width. A nil or empty detail leaves the line unchanged."
  (let ((lines (tui-status:render-spinner-line t 0 60 "compiling core.lisp")))
    (is (= 1 (length lines)))
    (is (string= "⠋ working -- compiling core.lisp" (first lines))))
  (is (string= "⠋ working"
               (first (tui-status:render-spinner-line t 0 60 nil))))
  (is (string= "⠋ working"
               (first (tui-status:render-spinner-line t 0 60 ""))))
  (is (string= "⠋ working -- comp"
               (first (tui-status:render-spinner-line t 0 17 "compiling")))))
