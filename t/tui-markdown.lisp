(in-package #:kli/tests)
(in-suite all)

(defun md-visible (s)
  "Strip SGR escapes, leaving the visible characters."
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n)
            do (if (char= (char s i) #\Esc)
                   (let ((m (position #\m s :start i)))
                     (setf i (if m (1+ m) n)))
                   (progn (write-char (char s i) out) (incf i)))))))

(defun md-underline-mask (s)
  "Return (values visible-text bits) where bits[i] is T iff visible char i was emitted
   with SGR underline (4) active. The style layer emits each attr as a standalone
   ESC[<code>m, so a bare \"4\"/\"24\" toggles underline. Color codes carry an inner
   separator and never match."
  (let ((vis (make-string-output-stream)) (bits '()) (under nil)
        (i 0) (n (length s)))
    (loop while (< i n)
          do (if (char= (char s i) #\Esc)
                 (let* ((m (position #\m s :start i))
                        (code (subseq s (+ i 2) (or m n))))
                   (cond ((string= code "4") (setf under t))
                         ((string= code "24") (setf under nil)))
                   (setf i (if m (1+ m) n)))
                 (progn (write-char (char s i) vis) (push under bits) (incf i))))
    (values (get-output-stream-string vis) (nreverse bits))))

(defun md-fg (theme token)
  (tui-style:fg-truecolor (tui-style:theme-token theme token)))

(defun md-join (lines)
  (format nil "~{~A~}" lines))

(defmacro with-dark-theme ((theme) &body body)
  `(let* ((tui-style:*color-mode* :truecolor)
          (,theme (builtin-theme "dark.json")))
     ,@body))

(test markdown-heading-styled
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:markdown->lines "# Title" theme 40)))
      (is (= 1 (length lines)))
      (is (string= "Title" (md-visible (first lines))) "the # marker is dropped")
      (is (search (md-fg theme "mdHeading") (first lines)))
      (is (search (format nil "~C[1m" #\Esc) (first lines)) "headings are bold")
      (is (search (format nil "~C[4m" #\Esc) (first lines)) "level<=2 headings underline"))))

(test markdown-heading-underline-spans-spaces
  "A level<=2 heading underlines each word and the inter-word spaces, so the rule does not break into disconnected underlined segments. Plain prose never gains underline on its spaces."
  (with-dark-theme (theme)
    (let ((line (first (tui-markdown:markdown->lines "## The Quiet Power" theme 40))))
      (multiple-value-bind (vis bits) (md-underline-mask line)
        (is (string= "The Quiet Power" vis))
        (is (find #\Space vis) "sanity: the heading has inter-word spaces")
        (is (every #'identity bits)
            "every heading char, including the spaces, is underlined")))
    (let ((line (first (tui-markdown:markdown->lines "one two" theme 40))))
      (multiple-value-bind (vis bits) (md-underline-mask line)
        (is (string= "one two" vis))
        (is (notany #'identity bits) "plain prose stays un-underlined")))))

(test markdown-inline-spacing-preserved
  "Adjacent inline nodes keep source spacing. No space is inserted before the comma, and styled runs do not bleed separators."
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:markdown->lines "**bold**, plain" theme 40)))
      (is (= 1 (length lines)))
      (is (string= "bold, plain" (md-visible (first lines))))
      (is (search (format nil "~C[1m" #\Esc) (first lines)) "the bold run is bold"))))

(test markdown-inline-code-and-link
  (with-dark-theme (theme)
    (let* ((lines (tui-markdown:markdown->lines "`x` [a](http://h)" theme 60))
           (out (md-join lines)))
      (is (string= "x a (http://h)" (md-visible (first lines))))
      (is (search (md-fg theme "mdCode") out) "inline code carries mdCode")
      (is (search (md-fg theme "mdLink") out) "link label carries mdLink")
      (is (search (md-fg theme "mdLinkUrl") out) "link url carries mdLinkUrl")
      (is (search (format nil "~C[4m" #\Esc) out) "link label underlines"))))

(test markdown-bullet-list
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:markdown->lines (format nil "- one~%- two") theme 40)))
      (is (= 2 (length lines)))
      (is (string= "- one" (md-visible (first lines))))
      (is (string= "- two" (md-visible (second lines))))
      (is (search (md-fg theme "mdListBullet") (first lines))))))

(test markdown-ordered-list
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:markdown->lines (format nil "1. a~%2. b") theme 40)))
      (is (= 2 (length lines)))
      (is (string= "1. a" (md-visible (first lines))))
      (is (string= "2. b" (md-visible (second lines)))))))

(test markdown-blockquote
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:markdown->lines "> quoted" theme 40)))
      (is (string= "│ quoted" (md-visible (first lines))))
      (is (search (md-fg theme "mdQuoteBorder") (first lines)) "gutter is mdQuoteBorder")
      (is (search (format nil "~C[3m" #\Esc) (first lines)) "quote text is italic"))))

(test markdown-horizontal-rule
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:markdown->lines (format nil "a~%~%---~%~%b") theme 20)))
      (is (some (lambda (l) (string= (make-string 20 :initial-element #\─) (md-visible l)))
                lines)
          "a full-width rule is emitted"))))

(test markdown-code-block-lisp-highlighted
  (with-dark-theme (theme)
    (let* ((src (format nil "```lisp~%(defun f (x) x)~%```"))
           (lines (tui-markdown:markdown->lines src theme 40))
           (out (md-join lines)))
      (is (text:string-prefix-p "─ lisp " (md-visible (first lines))) "fenced header names the lang")
      (is (string= (make-string 40 :initial-element #\─) (md-visible (car (last lines))))
          "bottom fence is a full-width rule")
      (is (search (md-fg theme "syntaxKeyword") out) "defun highlights as a keyword"))))

(test markdown-code-block-plain-unknown-lang
  (with-dark-theme (theme)
    (let* ((src (format nil "```text~%plain code~%```"))
           (lines (tui-markdown:markdown->lines src theme 40))
           (out (md-join lines)))
      (is (some (lambda (l) (string= "plain code" (md-visible l))) lines))
      (is (not (search (md-fg theme "syntaxKeyword") out))
          "an unknown language is not highlighted"))))

(test markdown-code-block-soft-wraps
  "A code line wider than the column folds, losing no characters."
  (with-dark-theme (theme)
    (let* ((src (format nil "```text~%~A~%```" (make-string 30 :initial-element #\a)))
           (lines (tui-markdown:markdown->lines src theme 12))
           (code (remove-if (lambda (l) (search "─" (md-visible l))) lines))
           (joined (apply #'concatenate 'string (mapcar #'md-visible code))))
      (is (every (lambda (l) (<= (text:visible-width (md-visible l)) 12)) lines)
          "no line exceeds the width")
      (is (string= (make-string 30 :initial-element #\a) joined)
          "every character survives the wrap"))))

(test markdown-table-box-drawn
  (with-dark-theme (theme)
    (let* ((src (format nil "| A | B |~%| - | - |~%| 1 | 2 |"))
           (lines (tui-markdown:markdown->lines src theme 40))
           (out (md-join lines)))
      (is (text:string-prefix-p "┌" (md-visible (first lines))))
      (is (text:string-suffix-p "┐" (md-visible (first lines))))
      (is (search "┬" (md-visible (first lines))))
      (is (text:string-prefix-p "└" (md-visible (car (last lines)))))
      (is (some (lambda (l) (search "│" (md-visible l))) lines))
      (is (search (md-fg theme "mdHeading") out) "header cells are mdHeading"))))

(test syntax-lisp-token-mapping
  (with-dark-theme (theme)
    (let* ((src (format nil "```lisp~%(defun f (x) ;; doc~%  (+ 1 \"s\"))~%```"))
           (out (md-join (tui-markdown:markdown->lines src theme 60))))
      (is (search (md-fg theme "syntaxKeyword") out) "defun -> keyword")
      (is (search (md-fg theme "syntaxComment") out) ";; doc -> comment")
      (is (search (md-fg theme "syntaxNumber") out) "1 -> number")
      (is (search (md-fg theme "syntaxString") out) "\"s\" -> string")
      (is (search (md-fg theme "syntaxFunction") out) "+ -> function"))))

(test markdown-code-block-no-library-chatter
  "The renderer runs on the main thread where *standard-output* and *trace-output* are the real TTY, so a highlighter must never scribble there. With colorize's debug default forced ON and a cold cache, the render stays silent."
  (with-dark-theme (theme)
    (let* ((tui-markdown::*highlight-cache* (make-hash-table :test 'equal))
           (colorize::*debug* t)
           (sink (make-string-output-stream))
           (lines (let ((*standard-output* sink) (*trace-output* sink))
                    (tui-markdown:markdown->lines
                     (format nil "```lisp~%(defun chatter (x) (+ x 1))~%```") theme 40))))
      (is (string= "" (get-output-stream-string sink))
          "highlighting a lisp block writes nothing to stdout/trace-output")
      (is (not (search "Scan was called" (md-join lines)))
          "no library chatter leaks into the rendered lines"))))

(test markdown-code-block-highlight-is-memoized
  "Re-rendering the same block reuses cached seg-lines rather than re-scanning."
  (with-dark-theme (theme)
    (let ((tui-markdown::*highlight-cache* (make-hash-table :test 'equal))
          (src (format nil "```lisp~%(defun memo (x) x)~%```")))
      (let ((a (tui-markdown:markdown->lines src theme 40))
            (b (tui-markdown:markdown->lines src theme 40)))
        (is (= 1 (hash-table-count tui-markdown::*highlight-cache*))
            "a repeated block produces one cache entry")
        (is (equal (mapcar #'md-visible a) (mapcar #'md-visible b))
            "the second render matches the first")))))

(test markdown-char-wrap-segs-counts-columns
  "Wide characters consume two columns of the wrap budget, and a zero-width
combining mark stays with its base character at the width boundary."
  (let* ((wide (code-char #x4E2D))
         (lines (tui-markdown::char-wrap-segs
                 (list (tui-markdown::make-seg
                        (make-string 3 :initial-element wide)))
                 4)))
    (is (= 2 (length lines)) "three wide characters split two then one")
    (is (string= (make-string 2 :initial-element wide)
                 (tui-markdown::seg-text (first (first lines)))))
    (is (string= (string wide)
                 (tui-markdown::seg-text (first (second lines))))))
  (let ((lines (tui-markdown::char-wrap-segs
                (list (tui-markdown::make-seg
                       (format nil "ab~C" (code-char #x0301))))
                2)))
    (is (= 1 (length lines))
        "a trailing combining mark fits the exact-width line")
    (is (string= (format nil "ab~C" (code-char #x0301))
                 (tui-markdown::seg-text (first (first lines)))))))

(test markdown-highlight-cache-evicts-least-recently-used
  "The cache is bounded. Past the limit the least recently touched entry is
evicted, so a long session cannot retain seg-lines without bound."
  (with-dark-theme (theme)
    (let ((tui-markdown::*highlight-cache* (make-hash-table :test 'equal))
          (tui-markdown::*highlight-cache-limit* 2))
      (tui-markdown:highlight-code-lines "(a)" "lisp" theme)
      (tui-markdown:highlight-code-lines "(b)" "lisp" theme)
      (tui-markdown:highlight-code-lines "(a)" "lisp" theme)
      (tui-markdown:highlight-code-lines "(c)" "lisp" theme)
      (is (= 2 (hash-table-count tui-markdown::*highlight-cache*))
          "the cache never exceeds the limit")
      (is (not (null (gethash (list "(a)" "lisp" theme)
                              tui-markdown::*highlight-cache*)))
          "the recently touched entry survives eviction")
      (is (null (gethash (list "(b)" "lisp" theme)
                         tui-markdown::*highlight-cache*))
          "the least recently used entry is evicted")
      (is (not (null (gethash (list "(c)" "lisp" theme)
                              tui-markdown::*highlight-cache*)))
          "the entry that triggered eviction is cached"))))

(test markdown-highlight-memoize-off-reads-but-never-inserts
  "With *HIGHLIGHT-MEMOIZE-P* off a miss computes without inserting, while a
hit still reuses cached seg-lines."
  (with-dark-theme (theme)
    (let ((tui-markdown::*highlight-cache* (make-hash-table :test 'equal)))
      (let ((tui-markdown:*highlight-memoize-p* nil))
        (tui-markdown:highlight-code-lines "(miss)" "lisp" theme)
        (is (zerop (hash-table-count tui-markdown::*highlight-cache*))
            "a miss with memoization off inserts nothing"))
      (let ((cached (tui-markdown:highlight-code-lines "(hit)" "lisp" theme)))
        (let ((tui-markdown:*highlight-memoize-p* nil))
          (is (eq cached
                  (tui-markdown:highlight-code-lines "(hit)" "lisp" theme))
              "a hit with memoization off reuses the cached seg-lines"))))))

(test markdown-hl-stream-matches-full-render
  "An hl-stream fed every growing prefix of a code block yields, at each step,
exactly what a full re-scan would: committing completed lines up to a safe
scan-restart boundary is byte-identical to re-scanning the whole block."
  (with-dark-theme (theme)
    (let ((code (format nil "(defun f (x)~%  \"doc~%spanning\"~%  (+ x 1))~%~%;; tail~%(defvar *y* 10)~%"))
          (st (tui-markdown::make-hl-stream))
          (bad nil))
      (loop for i from 1 to (length code)
            for prefix = (subseq code 0 i)
            for want = (tui-markdown::compute-highlight-code-lines prefix "lisp" theme)
            for got = (tui-markdown::hlstream-render st prefix "lisp" theme)
            unless (equalp got want) do (setf bad i) (loop-finish))
      (is (null bad)
          "incremental highlight diverged from full render at prefix length ~A" bad))))

(test markdown-hl-stream-resets-on-block-switch
  "One hl-stream reused across two different same-language blocks must never
splice the first block's committed lines onto the second. A code that does not
extend the committed prefix forces a reset, so the open tail re-scans clean."
  (with-dark-theme (theme)
    (let ((st (tui-markdown::make-hl-stream))
          (a (format nil "(alpha)~%(beta)~%(gamma)~%"))
          (b (format nil "(delta)~%(epsilon)~%(eta)~%")))
      (loop for i from 1 to (length a)
            do (tui-markdown::hlstream-render st (subseq a 0 i) "lisp" theme))
      (let ((bad nil))
        (loop for i from 1 to (length b)
              for prefix = (subseq b 0 i)
              for want = (tui-markdown::compute-highlight-code-lines prefix "lisp" theme)
              for shared = (tui-markdown::hlstream-render st prefix "lisp" theme)
              unless (equalp shared want) do (setf bad i) (loop-finish))
        (is (null bad)
            "switching blocks on a shared hl-stream gave wrong highlight at length ~A" bad)))))

(test markdown-streaming-code-block-equals-full-render
  "Streaming a fenced code block in deltas through markdown->lines — with the
per-reply mdstream and hl-stream bound as the transcript binds them — ends
byte-identical to a single cold full render."
  (with-dark-theme (theme)
    (let* ((body (with-output-to-string (s)
                   (loop for i below 30
                         do (format s "(defun frob-~D (x y)~%  \"doc ~D\" (+ x y))~%" i i))))
           (src (format nil "Intro paragraph.~%~%```lisp~%~A```" body))
           (md (tui-markdown::make-mdstream))
           (hl (tui-markdown::make-hl-stream))
           (streamed
             (let ((tui-markdown:*highlight-memoize-p* nil)
                   (tui-markdown:*md-stream* md)
                   (tui-markdown:*hl-stream* hl))
               (loop for i from 1 below (length src) by 7
                     do (tui-markdown:markdown->lines (subseq src 0 i) theme 60))
               (tui-markdown:markdown->lines src theme 60)))
           (full (let ((tui-markdown::*highlight-cache* (make-hash-table :test 'equal)))
                   (tui-markdown:markdown->lines src theme 60))))
      (is (equal (mapcar #'md-visible streamed) (mapcar #'md-visible full))
          "streamed visible text matches the full render")
      (is (equalp streamed full)
          "streamed styled lines match the full render byte-for-byte"))))

(test markdown-highlight-survives-partial-escape
  "A code prefix ending mid-escape (a trailing backslash inside an open string)
over-reads colorize's lexer. The highlighter must degrade to plain lines for that
frame rather than fault the render, losing no characters."
  (with-dark-theme (theme)
    (let* ((tui-markdown::*highlight-cache* (make-hash-table :test 'equal))
           (partial "char *s = \"hi\\")
           (lines (tui-markdown:highlight-code-lines partial "c" theme)))
      (is (consp lines) "a partial-escape prefix yields lines, not a fault")
      (is (string= partial
                   (md-join (mapcar (lambda (sl)
                                      (md-join (mapcar #'tui-markdown::seg-text sl)))
                                    lines)))
          "every character survives as plain text"))))

(test diff-line-gutters-and-colors
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:render-diff (format nil "a~%b~%c")
                                           (format nil "a~%B~%c")
                                           theme 40)))
      (is (= 4 (length lines)))
      (is (string= "1 1   a" (md-visible (first lines))) "context numbers old and new")
      (is (string= "2   - b" (md-visible (second lines))) "delete numbers the old side")
      (is (string= "  2 + B" (md-visible (third lines))) "insert numbers the new side")
      (is (string= "3 3   c" (md-visible (fourth lines))))
      (is (search (md-fg theme "toolDiffRemoved") (second lines)))
      (is (search (md-fg theme "toolDiffAdded") (third lines)))
      (is (search (format nil "~C[7m" #\Esc) (second lines))
          "a 1x1 replace gets character-level inverse video"))))

(test diff-multiline-replace-has-no-inverse
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:render-diff (format nil "x~%y")
                                           (format nil "p~%q")
                                           theme 40)))
      (is (equal '("1   - x" "2   - y" "  1 + p" "  2 + q") (mapcar #'md-visible lines)))
      (is (not (search (format nil "~C[7m" #\Esc) (md-join lines)))
          "a multi-line replace falls back to plain -/+ rows"))))

(test diff-soft-wraps-long-line
  (with-dark-theme (theme)
    (let* ((long (make-string 30 :initial-element #\z))
           (lines (tui-markdown:render-diff "x" (format nil "x~%~A" long) theme 12))
           (added (remove-if-not (lambda (l) (find #\z (md-visible l))) lines)))
      (is (every (lambda (l) (<= (text:visible-width (md-visible l)) 12)) lines)
          "no line exceeds the width")
      (is (= 30 (length (apply #'concatenate 'string
                               (mapcar (lambda (l)
                                         (remove-if-not (lambda (c) (char= c #\z)) (md-visible l)))
                                       added))))
          "every added character survives the wrap"))))

(test diff-reports-first-changed-line-index
  "The second value of render-diff is the rendered-line index where the first
-/+ row appears, nil when the contents are equal."
  (with-dark-theme (theme)
    (multiple-value-bind (lines first-change)
        (tui-markdown:render-diff (format nil "a~%b~%c")
                                  (format nil "a~%B~%c")
                                  theme 40)
      (is (= 1 first-change))
      (is (string= "2   - b" (md-visible (nth first-change lines)))))
    (multiple-value-bind (lines first-change)
        (tui-markdown:render-diff "same" "same" theme 40)
      (is (= 1 (length lines)))
      (is (null first-change)))))

(test diff-gutter-two-column-branches
  "The gutter carries true old/new file line numbers per branch: equal shows both,
delete the old side only, insert the new side only."
  (with-dark-theme (theme)
    (is (equal '("1 1   a" "2   - b" "  2 + B" "3 3   c")
               (mapcar #'md-visible
                       (tui-markdown:render-diff (format nil "a~%b~%c")
                                                 (format nil "a~%B~%c") theme 40)))
        "equal numbers both sides; a 1x1 replace puts old on - and new on +")
    (is (equal '("1 1   a" "2   - b" "3 2   c")
               (mapcar #'md-visible
                       (tui-markdown:render-diff (format nil "a~%b~%c")
                                                 (format nil "a~%c") theme 40)))
        "a pure delete numbers the old side only")
    (is (equal '("1 1   a" "  2 + b" "2 3   c")
               (mapcar #'md-visible
                       (tui-markdown:render-diff (format nil "a~%c")
                                                 (format nil "a~%b~%c") theme 40)))
        "a pure insert numbers the new side only")))

(test diff-gutter-blank-on-soft-wrap-continuation
  "A soft-wrapped row keeps its numeric gutter on the first piece and a blank
gutter of the same width on every continuation."
  (with-dark-theme (theme)
    (let* ((long (make-string 30 :initial-element #\z))
           (lines (mapcar #'md-visible
                          (tui-markdown:render-diff "x" (format nil "x~%~A" long) theme 12))))
      (is (string= "1 1   x" (first lines)))
      (is (string= "  2 + zzzzzz" (second lines)) "the first wrapped piece carries the gutter")
      (is (every (lambda (l) (string= "      " (subseq l 0 6))) (cddr lines))
          "continuations get a blank gutter as wide as the gutter"))))

(test diff-hunk-blocks-three-line-context-and-clamp
  "Collapsed hunk blocks keep three context lines around each change, clamped at
file bounds, with a uniform gutter width across every hunk in the card."
  (with-dark-theme (theme)
    (let* ((old (format nil "~{l~2,'0D~^~%~}" (loop for i from 1 to 12 collect i)))
           (new (with-output-to-string (o)
                  (loop for i from 1 to 12 for first = t then nil do
                    (unless first (terpri o))
                    (cond ((= i 2) (write-string "X02" o))
                          ((= i 11) (write-string "X11" o))
                          (t (format o "l~2,'0D" i))))))
           (blocks (tui-markdown:render-diff-hunk-blocks old new theme 40 :context-lines 3)))
      (is (= 2 (length blocks)) "distant changes stay separate hunks")
      (is (equal '(" 1  1   l01" " 2    - l02" "    2 + X02"
                   " 3  3   l03" " 4  4   l04" " 5  5   l05")
                 (mapcar #'md-visible (getf (first blocks) :lines)))
          "top clamps at line 1; three lines of trailing context")
      (is (equal '(" 8  8   l08" " 9  9   l09" "10 10   l10"
                   "11    - l11" "   11 + X11" "12 12   l12")
                 (mapcar #'md-visible (getf (second blocks) :lines)))
          "bottom clamps at the last line; gutter width matches the first hunk"))))

(test diff-hunk-blocks-merge-adjacent
  "Changes within the context window merge into a single hunk block that reports
both changed hunks."
  (with-dark-theme (theme)
    (let* ((old (format nil "~{l~2,'0D~^~%~}" (loop for i from 1 to 9 collect i)))
           (new (with-output-to-string (o)
                  (loop for i from 1 to 9 for first = t then nil do
                    (unless first (terpri o))
                    (cond ((= i 4) (write-string "X4" o))
                          ((= i 6) (write-string "X6" o))
                          (t (format o "l~2,'0D" i))))))
           (blocks (tui-markdown:render-diff-hunk-blocks old new theme 40 :context-lines 3)))
      (is (= 1 (length blocks)) "nearby changes collapse into one block")
      (is (= 2 (getf (first blocks) :hunks)) "the merged block reports both hunks"))))

(test unified-diff-detection-is-strict
  (is (tui-markdown:unified-diff-p (format nil "@@ -1,2 +1,3 @@~% ctx~%-a~%+b")))
  (is (tui-markdown:unified-diff-p (format nil "--- a~%+++ b~%+x")))
  (is (not (tui-markdown:unified-diff-p (format nil "hello~%world"))))
  (is (not (tui-markdown:unified-diff-p "+ a bullet-ish prose line")))
  (is (not (tui-markdown:unified-diff-p ""))))

(test render-unified-diff-colors-by-marker
  (with-dark-theme (theme)
    (let ((lines (tui-markdown:render-unified-diff
                  (format nil "@@ -1 +1 @@~%-old~%+new~% ctx") theme 40)))
      (is (= 4 (length lines)))
      (is (search (md-fg theme "accent") (first lines)) "hunk header in accent")
      (is (search (md-fg theme "toolDiffRemoved") (second lines)))
      (is (search (md-fg theme "toolDiffAdded") (third lines)))
      (is (search (md-fg theme "toolDiffContext") (fourth lines))))))

(test assistant-renders-as-markdown
  (with-dark-theme (theme)
    (let* ((proto (make-tui-rendering-fixture))
           (ev (tui-transcript:make-transcript-event :message :assistant "# Title")))
      (is (equal (tui-markdown:markdown->lines "# Title" theme 40)
                 (tui-core:render-transcript-event :message proto ev theme 40))))))

(test tool-result-diff-renders-colored
  (with-dark-theme (theme)
    (let* ((proto (make-tui-rendering-fixture))
           (ev (tui-transcript:make-transcript-event
                :tool-result nil "Edited /tmp/x (+1 -1)" :name "edit" :status :ok
                :presentation
                (ext:result-diff
                 :updates (list (tools-filesystem:file-diff-presentation-update
                                 "/tmp/x" "a" "b")))))
           (lines (tui-core:render-transcript-event :tool-result proto ev theme 40)))
      (is (string= " edit /tmp/x (+1 -1)" (md-visible (first lines)))
          "diff result keeps a bold tool heading")
      (is (search (md-fg theme "toolDiffAdded") (md-join lines)))
      (is (not (search (tui-style:bg-truecolor (tui-style:theme-token theme "toolSuccessBg"))
                       (md-join lines)))
          "a diff result is not wrapped in the success bg block"))))
