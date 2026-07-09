(in-package #:kli/tui/transcript)

(defun label (value)
  (string-downcase (string value)))

(defparameter *tool-result-preview-line-limit* 10
  "Number of result lines shown inline before the tail is collapsed into a
   \"… (+N more lines)\" indicator while tool output is collapsed.")

(defparameter *file-update-hunk-context-lines* 3
  "Unchanged old/new lines kept before and after each collapsed file-update hunk.")

(defparameter *file-update-gutter-width* 3
  "Fixed line-number column count every file-update diff card uses for both the
   old and new gutters, so content indentation is identical across cards no
   matter how long each file is. NIL restores the per-file digit-count width.")

(defparameter *file-update-hunk-preview-line-cap* 200
  "Maximum rendered diff lines shown in a collapsed file-update card before a
   hidden-hunk indicator advertises Ctrl+O for the full diff.")

(defparameter +tool-output-expanded-key+ :kli/tui/transcript/tool-output-expanded
  "protocol-storage key for the per-protocol tool-output expansion toggle.")

(defvar *tool-output-expanded* nil
  "Dynamic seam read by the render path. Bound per render pass by RENDER-EVENT
   from the active protocol's stored toggle, never set at top level. The
   per-protocol source of truth is protocol-storage under
   +TOOL-OUTPUT-EXPANDED-KEY+.")

(defun tool-output-expanded-p (protocol)
  (protocol-storage protocol +tool-output-expanded-key+ nil))

(defun toggle-tool-output-expansion (protocol)
  "Flip the active protocol's tool-output expansion toggle and return the new
   value. The caller must reprint the transcript (committed events are written
   once) for the change to take effect on screen."
  (setf (protocol-storage protocol +tool-output-expanded-key+)
        (not (tool-output-expanded-p protocol))))

(declaim (ftype (function (t) string) read-summary-tail filesystem-summary-tail))

(defun tool-result-preview (text)
  "Collapsed display of a tool result: the first *TOOL-RESULT-PREVIEW-LINE-LIMIT*
   lines, with any remaining lines collapsed into a count indicator. The full
   text is retained on the event and shown when *TOOL-OUTPUT-EXPANDED* is set."
  (let* ((text (or text ""))
         (lines (split-lines text))
         (limit *tool-result-preview-line-limit*))
    (if (<= (length lines) limit)
        text
        (format nil "~{~A~^~%~}~%… (+~D more lines · Ctrl+O)"
                (subseq lines 0 limit)
                (- (length lines) limit)))))

(defun tool-result-display-text (text)
  (if *tool-output-expanded*
      (or text "")
      (tool-result-preview text)))

(defun format-event (event)
  (let ((kind (event-kind event))
        (role (event-role event))
        (text (or (event-text event) ""))
        (name (event-name event))
        (status (event-status event)))
    (case kind
      (:message (format nil "[~A] ~A" (label role) text))
      (:tool-call
       (if (eq (presentation-kind (event-presentation event)) :command)
           (format nil "$ ~A" text)
           (format nil "[tool] ~@[~A ~]~A" name text)))
      (:tool-result
       (let ((term (event-presentation event)))
         (case (presentation-kind term)
           (:summary (format nil "[read]~A" (read-summary-tail term)))
           (:filesystem-summary (format nil "[~A]~A"
                                        (or name (getf term :summary-kind))
                                        (filesystem-summary-tail term)))
           (otherwise (format nil "[result] ~A" (tool-result-display-text text))))))
      (:response (if (eq status :error)
                     (format nil "~C ~A" #\▎ text)
                     (format nil "~C ~A" #\› text)))
      (otherwise (if (eq status :error)
                     (format nil "~C ~A" #\▎ text)
                     (format nil "~C ~A" #\· text))))))

(defun bg-block-lines (theme bg-token fg-token text width)
  "A bg block hugging its content. Inter-entry spacing is one plain blank line
   (RENDER-EVENTS-LINES), so the block carries no coloured vertical halo. Styled
   rendering applies when a theme is active. pad-right counts ANSI bytes, so plain
   text is always padded before it is wrapped in a span, and mixed-style lines
   compute their pad from the plain width."
  (let ((bg (theme-token theme bg-token))
        (fg (theme-token theme fg-token)))
    (flet ((line (plain) (style-span (pad-right plain width) :bg bg :fg fg)))
      (loop for w in (wrap-text text (max 1 (- width 2)))
            collect (line (concatenate 'string " " w))))))

(defun render-user (text theme width)
  (bg-block-lines theme "userMessageBg" "userMessageText" text width))

(defun render-assistant (text theme width)
  "Assistant replies render as markdown. A parse failure on untrusted model
   output falls back to plain wrapped text rather than breaking the frame."
  (or (ignore-errors (markdown->lines text theme width))
      (loop for w in (wrap-text text (max 1 (1- width)))
            collect (pad-right (concatenate 'string " " w) width))))

(defun render-system-notice (text theme width)
  "Ambient notice: leading dim dot, muted body, no slab, no bracket. The dot
   marks only the first line so continuations indent under the body."
  (loop for w in (wrap-text text (max 1 (- width 2)))
        for i from 0
        for pad = (max 0 (- width (+ 2 (visible-width w))))
        collect (concatenate 'string
                             (if (zerop i) (style theme "dim" (string #\·)) " ")
                             " " (style theme "muted" w)
                             (make-string pad :initial-element #\Space))))

(defun render-command-response (text theme width)
  "Direct command output: a subtle reply marker then a muted body, no ambient
   dot and no /cmd: prefix -- the command is echoed just above as the user line,
   so this reads as its answer rather than as background activity."
  (loop for w in (wrap-text text (max 1 (- width 2)))
        for i from 0
        for pad = (max 0 (- width (+ 2 (visible-width w))))
        collect (concatenate 'string
                             (if (zerop i) (style theme "muted" (string #\›)) " ")
                             " " (style theme "muted" w)
                             (make-string pad :initial-element #\Space))))

(defun render-system-error (text theme width)
  "Error notice: left bar in the error colour, body at full weight (a real
   failure, not a routine note)."
  (let ((bar (theme-token theme "error")))
    (loop for w in (wrap-text text (max 1 (- width 2)))
          for pad = (max 0 (- width (+ 2 (visible-width w))))
          collect (concatenate 'string
                               (style-span (string #\▎) :fg bar) " " w
                               (make-string pad :initial-element #\Space)))))

(defun file-update-list (term)
  "Private diff updates from a :diff presentation term. Public result details
   are model-facing metadata, not UI diff storage."
  (when (and (consp term) (eq (presentation-kind term) :diff))
    (remove-if-not (lambda (update)
                     (and (consp update)
                          (stringp (getf update :path))))
                   (getf term :updates))))

(defun file-update-heading (name update theme)
  "Card heading: bold tool name in toolTitle, then the path and change counts
   in toolOutput."
  (let* ((head (string-downcase (princ-to-string (or name ""))))
         (path (getf update :path))
         (added (getf update :added))
         (removed (getf update :removed))
         (tail (with-output-to-string (out)
                 (when path (format out " ~A" path))
                 (when (and added removed)
                   (format out " (+~D -~D)" added removed)))))
    (concatenate 'string " "
                 (style theme "toolTitle" head :attrs '(:bold))
                 (style theme "toolOutput" tail))))

(defun file-update-hunk-separator-line (theme)
  (concatenate 'string " " (style theme "toolOutput" "…")))

(defun file-update-hidden-hunks-line (hidden theme)
  (concatenate 'string " "
               (style theme "toolOutput"
                      (format nil "… (+~D changed hunk~:P omitted; re-read for full context)" hidden))))

(defun file-update-notice-lines (update theme width)
  (let ((notice (getf update :notice)))
    (when (and (stringp notice) (plusp (length notice)))
      (loop for w in (wrap-text notice (max 1 (- width 2)))
            for pad = (max 0 (- width (+ 2 (visible-width w))))
            collect (concatenate 'string "  " (style theme "toolOutput" w)
                                 (make-string pad :initial-element #\Space))))))

(defun file-update-row-token (kind)
  (case kind
    (:context "toolDiffContext")
    (:remove "toolDiffRemoved")
    (:add "toolDiffAdded")
    (otherwise "toolDiffContext")))

(defun file-update-row-marker (kind)
  (case kind
    (:remove #\-)
    (:add #\+)
    (otherwise #\Space)))

(defun file-update-gutter (row theme)
  (let* ((kind (getf row :kind))
         (token (file-update-row-token kind))
         (old (or (getf row :old-line) ""))
         (new (or (getf row :new-line) "")))
    (style-span
     (format nil "~v@a ~v@a ~C "
             *file-update-gutter-width* old
             *file-update-gutter-width* new
             (file-update-row-marker kind))
     :fg (theme-token theme token))))

(defun file-update-row-lines (row theme width)
  (let* ((kind (getf row :kind))
         (token (file-update-row-token kind))
         (text (normalize-text (or (getf row :text) ""))))
    (prefix-wrap-segs
     (file-update-gutter row theme)
     (+ (* 2 *file-update-gutter-width*) 4)
     (list (make-seg text (theme-token theme token)))
     width)))

(defun file-update-hunk-lines (hunk theme width)
  (loop for row in (getf hunk :rows)
        append (file-update-row-lines row theme width)))

(defun file-update-presentation-lines (update theme width)
  (let ((out '())
        (first-hunk-p t))
    (dolist (hunk (getf update :hunks))
      (unless first-hunk-p
        (push (file-update-hunk-separator-line theme) out))
      (dolist (line (file-update-hunk-lines hunk theme width))
        (push line out))
      (setf first-hunk-p nil))
    (append (nreverse out)
            (when (and (getf update :truncated-p)
                       (getf update :hidden-hunks))
              (list (file-update-hidden-hunks-line
                     (getf update :hidden-hunks) theme)))
            (file-update-notice-lines update theme width))))

(defun capped-hunk-preview-lines (blocks theme)
  "Render hunk BLOCKS up to *FILE-UPDATE-HUNK-PREVIEW-LINE-CAP* lines.
Separated blocks receive a muted separator. Once the cap is reached, remaining
changed hunks are counted in a Ctrl+O indicator."
  (let ((out '())
        (used 0)
        (hidden 0)
        (first-block-p t)
        (cap *file-update-hunk-preview-line-cap*)
        (remaining-blocks blocks))
    (loop while remaining-blocks
          for block = (pop remaining-blocks)
          for lines = (getf block :lines)
          for hunks = (or (getf block :hunks) 1)
          for separator = (and (not first-block-p)
                               (file-update-hunk-separator-line theme))
          for needed = (+ (length lines) (if separator 1 0))
          do (cond
               ((<= (+ used needed) cap)
                (when separator
                  (push separator out)
                  (incf used))
                (dolist (line lines)
                  (push line out)
                  (incf used))
                (setf first-block-p nil))
               (t
                (let ((space (- cap used)))
                  (when (and separator (plusp space))
                    (push separator out)
                    (decf space)
                    (incf used))
                  (when (plusp space)
                    (dolist (line (subseq lines 0 (min space (length lines))))
                      (push line out)
                      (incf used)))
                  (incf hidden hunks)
                  (dolist (rest remaining-blocks)
                    (incf hidden (or (getf rest :hunks) 1)))
                  (setf remaining-blocks nil)))))
    (let ((lines (nreverse out)))
      (if (plusp hidden)
          (append lines (list (file-update-hidden-hunks-line hidden theme)))
          lines))))

(defun diff-card-lines (update theme width)
  "Diff body of one file-update card. The term already carries bounded private
   hunk rows; expanded mode never reveals a hidden full before/after body."
  (or (file-update-presentation-lines update theme width)
      (file-update-notice-lines
       (list :notice "No changed lines in private diff presentation.")
       theme width)))

(defun render-file-update (name updates theme width)
  "One diff card per updated file: heading line, then the old→new line diff.
   A nil old means a new file and renders all-added."
  (loop for update in updates
        append (cons (file-update-heading name update theme)
                     (diff-card-lines update theme width))))

(defun path->lang (path)
  "File PATH's extension → a colorize language tag string for HIGHLIGHT-CODE-LINES,
   or NIL when the extension names no highlightable language."
  (let* ((p (and (stringp path) path))
         (dot (and p (position #\. p :from-end t)))
         (ext (and dot (string-downcase (subseq p (1+ dot))))))
    (cond ((null ext) nil)
          ((member ext '("lisp" "lsp" "cl" "asd" "asdf") :test #'string=) "lisp")
          ((string= ext "el") "elisp")
          ((member ext '("scm" "ss" "rkt") :test #'string=) "scheme")
          ((member ext '("clj" "cljs" "cljc" "edn") :test #'string=) "clojure")
          ((string= ext "py") "python")
          ((string= ext "c") "c")
          ((member ext '("cpp" "cc" "cxx" "hpp" "hh" "hxx" "h") :test #'string=) "c++")
          ((string= ext "java") "java")
          ((member ext '("hs" "lhs") :test #'string=) "haskell")
          (t nil))))

(defun search-line-parts (line)
  "Parse a search body LINE `‹marker›‹lineno›:‹hash›|‹content›`. Returns
   (values marker prefix content): MARKER is #\\* for a match line or #\\Space for
   context, PREFIX is the source span up to and including the first #\\|, and
   CONTENT is the source text after it. NIL when LINE is not a grep match line —
   a file-path header, a (no matches)/(skipped …)/(stopped …) notice, or the
   collapsed-preview tail — so those render as plain muted text."
  (let ((bar (position #\| line)))
    (when (and bar (>= bar 3) (member (char line 0) '(#\Space #\*)))
      (let ((colon (position #\: line :end bar)))
        (when (and colon (> colon 1) (< (1+ colon) bar)
                   (loop for k from 1 below colon always (digit-char-p (char line k))))
          (values (char line 0)
                  (subseq line 0 (1+ bar))
                  (subseq line (1+ bar))))))))

(defun search-prefix-style (marker prefix theme)
  "Style a search line PREFIX: the MARKER in accent for a match / muted for
   context, then the lineno:hash| gutter in muted tool-output colour."
  (concatenate 'string
               (style-span (subseq prefix 0 1)
                           :fg (theme-token theme (if (char= marker #\*)
                                                      "accent" "toolDiffContext")))
               (style-span (subseq prefix 1)
                           :fg (theme-token theme "toolOutput"))))

(defun search-heading (name details theme)
  "Card heading: bold tool name in toolTitle, then the pattern and match count
   in muted toolOutput, mirroring FILE-UPDATE-HEADING."
  (let* ((head (string-downcase (princ-to-string (or name ""))))
         (pattern (getf details :pattern))
         (matches (getf details :matches))
         (tail (with-output-to-string (out)
                 (when pattern (format out " ~A" pattern))
                 (when (and matches (plusp matches))
                   (format out " (~:D match~:[es~;~])" matches (= matches 1))))))
    (concatenate 'string " "
                 (style theme "toolTitle" head :attrs '(:bold))
                 (style theme "toolOutput" tail))))

(defun render-search (name text theme width details)
  "Search result as a clean syntax-highlighted code listing: a heading, then one
   block per matching file. Grep lines show a muted lineno:hash| gutter — the
   match marker in accent — ahead of the source content highlighted for the
   file's language, soft-wrapped under a hanging indent. The language is tracked
   per file block from its path header, falling back to the search :path. Path
   headers and notices render muted. No background block, matching the diff card;
   the collapsed-preview tail passes through untouched."
  (let ((out '())
        (cur-lang (path->lang (getf details :path))))
    (push (search-heading name details theme) out)
    (dolist (line (split-lines text))
      (multiple-value-bind (marker prefix content) (search-line-parts line)
        (if prefix
            (let ((segline (first (highlight-code-lines content cur-lang theme))))
              (dolist (l (prefix-wrap-segs (search-prefix-style marker prefix theme)
                                           (visible-width prefix) segline width))
                (push l out)))
            (progn
              (when (and (plusp (length line)) (not (find #\Space line)))
                (let ((l (path->lang line)))
                  (when l (setf cur-lang l))))
              (push (concatenate 'string " " (style theme "toolOutput" line)) out)))))
    (nreverse out)))

(defun ansi-seg-lines (text)
  "Parse raw tool output TEXT (SGR colour/attrs, tabs, newlines) into one seg
   list per logical line. SGR state carries across newlines and tabs are
   expanded; no escape survives in seg text, so a rendered seg-line can sit under
   an enclosing background without an interior reset punching a gap in it."
  (let ((lines '()) (cur '()))
    (dolist (s (ansi->segs text))
      (let ((fg (seg-fg s)) (attrs (seg-attrs s)) (str (seg-text s)) (start 0))
        (loop for nl = (position #\Newline str :start start)
              for end = (or nl (length str))
              do (when (< start end)
                   (push (make-seg (subseq str start end) fg attrs) cur))
                 (when (null nl) (return))
                 (push (nreverse cur) lines)
                 (setf cur '() start (1+ nl)))))
    (push (nreverse cur) lines)
    (nreverse lines)))

(defun seg-bg-rows (segs bg width)
  "Char-wrap SEGS under a one-column left margin, each row right-padded to WIDTH
   inside one continuous BG layer. The segs emit only selective resets, so the
   background fills edge to edge across content and padding."
  (loop for piece in (char-wrap-segs segs (max 1 (- width 2)))
        for used = (1+ (seg-line-width piece))
        collect (style-span
                 (concatenate 'string " " (render-seg-line piece)
                              (make-string (max 0 (- width used)) :initial-element #\Space))
                 :bg bg)))

(defun render-tool-box (name text status theme width)
  "Tool result box: a status-tinted background hugging the bold tool name and its
   output. The output is parsed from raw terminal bytes into segs, so the
   background is one continuous layer -- an embedded reset in the tool's own
   output cannot punch a gap, width is counted on visible text, and tabs align."
  (let* ((bg (theme-token theme (case status
                                  (:ok "toolSuccessBg")
                                  (:error "toolErrorBg")
                                  (t "toolPendingBg"))))
         (head (string-downcase (princ-to-string (or name ""))))
         (head-segs (if (plusp (length text))
                        (list (make-seg head nil '(:bold)) (make-seg " "))
                        (list (make-seg head nil '(:bold)))))
         (lines (ansi-seg-lines text)))
    (loop for segs in (cons (append head-segs (first lines)) (rest lines))
          append (seg-bg-rows segs bg width))))

(defun bash-command-lines (text width)
  "Char-wrap a shell command into (WIDTH-2)-column chunks (preserving explicit
   newlines) so a long command flows continuously like a real prompt instead of
   word-wrapping into orphaned tokens."
  (let ((budget (max 1 (- width 2))))
    (loop for logical in (split-lines text)
          append (if (zerop (length logical))
                     (list "")
                     (loop for start from 0 below (length logical) by budget
                           collect (subseq logical start
                                           (min (length logical) (+ start budget))))))))

(defun render-bash (text theme width)
  "$ cmd in bold green between two full-width green rules. The command flows
   continuously across soft-wraps. Only the first line carries the $ prompt, and
   continuations align under it rather than reading as a second command."
  (let* ((green (theme-token theme "bashMode"))
         (rule (style-span (make-string width :initial-element #\─) :fg green)))
    (append (list rule)
            (loop for w in (bash-command-lines text width)
                  for first = t then nil
                  collect (style-span
                           (pad-right (concatenate 'string (if first "$ " "  ") w) width)
                           :fg green :attrs '(:bold)))
            (list rule))))

(defun thinking-token (status)
  (let ((s (and status (string-downcase (princ-to-string status)))))
    (cond ((null s) "thinkingOff")
          ((string= s "minimal") "thinkingMinimal")
          ((string= s "low") "thinkingLow")
          ((string= s "medium") "thinkingMedium")
          ((string= s "high") "thinkingHigh")
          ((string= s "xhigh") "thinkingXhigh")
          (t "thinkingOff"))))

(defun strip-html-comments (text)
  (let ((input (or text "")))
    (with-output-to-string (out)
      (loop with start = 0
            with length = (length input)
            while (< start length)
            for open = (search "<!--" input :start2 start)
            do (cond
                 ((null open)
                  (write-string input out :start start)
                  (return))
                 (t
                  (write-string input out :start start :end open)
                  (let ((close (search "-->" input :start2 (+ open 4))))
                    (if close
                        (setf start (+ close 3))
                        (progn
                          (write-string input out :start open)
                          (return))))))))))

(defun normalize-thinking-text (text)
  (format nil "~{~A~^~%~}"
          (remove-if #'blank-string-p
                     (split-lines (strip-html-comments text)))))

(defun render-thinking (text status theme width)
  "Left │ gutter in the reasoning-effort ramp colour, italic dim text body."
  (let ((ramp (theme-token theme (thinking-token status)))
        (text (normalize-thinking-text text)))
    (loop for w in (wrap-text text (max 1 (- width 2)))
          for pad = (max 0 (- width (+ 2 (visible-width w))))
          collect (concatenate 'string
                               (style-span (string #\│) :fg ramp) " "
                               (style theme "thinkingText" w :attrs '(:italic))
                               (make-string pad :initial-element #\Space)))))

(defun read-summary-tail (term)
  "Trailing detail of a read summary line: the path with its line count, a
   clamped span when the read was truncated, and a raw marker when unanchored.
   Leads with a space so it appends directly after the tool name."
  (let ((path (getf term :path))
        (lines (getf term :lines))
        (start (getf term :start))
        (end (getf term :end))
        (raw (getf term :raw))
        (truncated (getf term :truncated)))
    (with-output-to-string (out)
      (when path (format out " ~A" path))
      (cond ((and truncated start end lines)
             (format out " (lines ~D-~D of ~D)" start end lines))
            (lines (format out " (~D line~:P)" lines)))
      (when raw (format out " raw")))))

(defun render-read-summary (name term theme width)
  "A read result as one line: bold tool name, then a muted path/line-count tail.
   The model still receives the full body; the view shows only this line. Never
   collapses -- it is already a single line."
  (declare (ignore width))
  (let ((head (string-downcase (princ-to-string (or name "")))))
    (list (concatenate 'string " "
                       (style theme "toolTitle" head :attrs '(:bold))
                       (style theme "toolOutput" (read-summary-tail term))))))

(defun plural-word (count singular plural)
  (if (= (or count 0) 1) singular plural))

(defun filesystem-summary-tail (term)
  "Trailing detail for compact find/search summaries. Leads with a space."
  (let ((kind (getf term :summary-kind))
        (pattern (getf term :pattern))
        (path (getf term :path))
        (count (getf term :count))
        (matches (getf term :matches))
        (file-count (getf term :file-count))
        (skipped (getf term :skipped))
        (timed-out (getf term :timed-out))
        (handle (getf term :result-handle))
        (truncated (getf term :truncated)))
    (with-output-to-string (out)
      (ecase kind
        (:find
         (format out " ~A (~D ~A)"
                 (or pattern "")
                 (or count 0)
                 (plural-word count "path" "paths")))
        (:search
         (format out " ~A in ~A (~D ~A in ~D ~A)"
                 (or pattern "")
                 (or path "")
                 (or matches 0)
                 (plural-word matches "match" "matches")
                 (or file-count 0)
                 (plural-word file-count "file" "files"))))
      (when truncated (format out " · truncated"))
      (when (and skipped (plusp skipped))
        (format out " · skipped ~D" skipped))
      (when (and timed-out (plusp timed-out))
        (format out " · timed out ~D" timed-out))
      (when handle (format out " · handle ~A" handle)))))

(defun render-filesystem-summary (name term theme width)
  "A find/search result as one compact line. The model still gets the full body."
  (declare (ignore width))
  (let ((head (string-downcase (princ-to-string (or name (getf term :summary-kind) "")))))
    (list (concatenate 'string " "
                       (style theme "toolTitle" head :attrs '(:bold))
                       (style theme "toolOutput" (filesystem-summary-tail term))))))

(defun diff-note-lines (term theme width)
  "A muted trailing note on a diff result -- e.g. a paren-repair the runner
   applied -- so a silent structural fix is visible. NIL when the term has none."
  (let ((note (getf term :note)))
    (when (and (stringp note) (plusp (length note)))
      (loop for w in (wrap-text note (max 1 (- width 2)))
            for pad = (max 0 (- width (+ 2 (visible-width w))))
            collect (concatenate 'string "  " (style theme "toolOutput" w)
                                 (make-string pad :initial-element #\Space))))))

(defun tool-call-display-text (event)
  (let* ((term (event-presentation event))
         (text (or (event-text event) ""))
         (preview (and (consp term) (getf term :preview))))
    (cond (*tool-output-expanded*
           (cond ((and (plusp (length text)) preview)
                  (format nil "~A~%~A" text preview))
                 (preview preview)
                 (t text)))
          ((and preview (zerop (length text)))
           "… (arguments · Ctrl+O)")
          (preview
           (format nil "~A … (arguments · Ctrl+O)" text))
          (t text))))

(defun render-tool-call-event (event theme width)
  "A tool-call line: a `$ cmd` for a command presentation, else a name+args
   header box. A :hidden call never reaches here -- the projector drops it."
  (let ((text (tool-call-display-text event)))
    (if (eq (presentation-kind (event-presentation event)) :command)
        (render-bash text theme width)
        (render-tool-box (event-name event) text nil theme width))))

(defun render-tool-result-event (event theme width)
  "A tool-result, dispatched on its presentation kind: a read summary, a
   file-update diff (with any note), a search listing, or a generic box. A
   pre-presentation record (kind NIL) degrades to the box."
  (let* ((term (event-presentation event))
         (name (event-name event))
         (status (event-status event))
         (details (event-details event))
         (text (tool-result-display-text (event-text event))))
    (if (and (eq status :error)
             (eq (presentation-kind term) :filesystem-summary))
        (render-tool-box name text status theme width)
        (case (presentation-kind term)
          (:summary (render-read-summary name term theme width))
          (:filesystem-summary (render-filesystem-summary name term theme width))
          (:diff (let ((updates (file-update-list term)))
                   (if updates
                       (append (render-file-update name updates theme width)
                               (diff-note-lines term theme width))
                       (render-tool-box name text status theme width))))
          (:listing (render-search name text theme width details))
          (otherwise (render-tool-box name text status theme width))))))

(defun render-styled-event (event theme width)
  (let ((kind (event-kind event))
        (role (event-role event))
        (text (or (event-text event) ""))
        (status (event-status event)))
    (case kind
      (:message (if (eq role :assistant)
                    (render-assistant text theme width)
                    (render-user text theme width)))
      (:tool-call (render-tool-call-event event theme width))
      (:tool-result (render-tool-result-event event theme width))
      (:thinking (render-thinking text status theme width))
      (:response (if (eq status :error)
                     (render-system-error text theme width)
                     (render-command-response text theme width)))
      (otherwise (if (eq status :error)
                     (render-system-error text theme width)
                     (render-system-notice text theme width))))))

(defparameter *aborted-reply-annotation* "· Interrupted"
  "Trailing marker appended to an assistant reply the user aborted (pi-style:
   the abort shows on the reply itself, not as a separate \"Interrupted.\" row).")

(defparameter *truncated-reply-annotation*
  "· Truncated -- max output tokens reached"
  "Trailing marker appended to an assistant reply the provider cut at the
   output-token limit, so the mid-sentence stop is attributed.")

(defun aborted-reply-p (event)
  (and (eq (event-kind event) :message)
       (eq (event-role event) :assistant)
       (eq (event-status event) :aborted)))

(defun truncated-reply-p (event)
  (and (eq (event-kind event) :message)
       (eq (event-role event) :assistant)
       (eq (event-status event) :truncated)))

(defun reply-annotation-lines (annotation theme width)
  "A blank spacer then a dim trailing ANNOTATION line, so the marker stands
   off the reply text above it. Plain when no theme."
  (let ((text (concatenate 'string " " annotation)))
    (list (pad-right "" width)
          (if theme
              (style theme "muted" (pad-right text width) :attrs '(:italic))
              (pad-right text width)))))

(defun render-transcript-event-default (kind protocol event theme width)
  (declare (ignore kind protocol))
  (let ((lines (if (null theme)
                   (loop for wrapped in (wrap-text (format-event event) width)
                         collect (pad-right wrapped width))
                   (render-styled-event event theme width))))
    (cond
      ((aborted-reply-p event)
       (append lines (reply-annotation-lines *aborted-reply-annotation*
                                             theme width)))
      ((truncated-reply-p event)
       (append lines (reply-annotation-lines *truncated-reply-annotation*
                                             theme width)))
      (t lines))))

(defun render-event (protocol event width)
  (let ((*tool-output-expanded* (tool-output-expanded-p protocol)))
    (render-transcript-event (event-kind event)
                             protocol event
                             (ignore-errors (active-theme protocol))
                             width)))

(defun previous-history-input-p (data)
  (and (stringp data)
       (member data '("up" "ctrl+p") :test #'string=)))

(defun next-history-input-p (data)
  (and (stringp data)
       (member data '("down" "ctrl+n") :test #'string=)))

(defun redisplay-input-p (data)
  (and (stringp data)
       (string= data "ctrl+l")))

(defun history-entry (value)
  (let ((entry (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
    (unless (blank-string-p entry)
      entry)))

(defun record-history (view value)
  (let ((entry (history-entry value))
        (tail (transcript-view-history-tail view)))
    (when (and entry
               (or (null tail)
                   (not (string= entry (car tail)))))
      (let ((cell (list entry)))
        (if tail
            (setf (cdr tail) cell)
            (setf (transcript-view-history view) cell))
        (setf (transcript-view-history-tail view) cell))))
  (setf (transcript-view-history-position view) nil
        (transcript-view-history-draft view) ""))

(defun previous-history (view)
  (let ((history (transcript-view-history view))
        (editor (transcript-view-editor view)))
    (when history
      (unless (transcript-view-history-position view)
        (setf (transcript-view-history-draft view)
              (editor-value editor)
              (transcript-view-history-position view) (length history)))
      (when (plusp (transcript-view-history-position view))
        (decf (transcript-view-history-position view)))
      (set-editor-value editor
                        (nth (transcript-view-history-position view)
                             history))
      t)))

(defun next-history (view)
  (let ((history (transcript-view-history view))
        (editor (transcript-view-editor view)))
    (when (transcript-view-history-position view)
      (if (< (transcript-view-history-position view)
             (1- (length history)))
          (progn
            (incf (transcript-view-history-position view))
            (set-editor-value
             editor
             (nth (transcript-view-history-position view) history)))
          (progn
            (setf (transcript-view-history-position view) nil)
            (set-editor-value editor
                              (transcript-view-history-draft view))))
      t)))

(defun reset-history-position (view)
  (setf (transcript-view-history-position view) nil
        (transcript-view-history-draft view) ""))

(defun append-submission-events (view events)
  (cond
    ((null events)
     nil)
    ((typep events 'transcript-event)
     (transcript-add-event (transcript-view-transcript view) events))
    ((listp events)
     (dolist (event events)
       (when (typep event 'transcript-event)
         (transcript-add-event (transcript-view-transcript view) event)))))
  events)

(defun submit-transcript-input (view input)
  (let ((entry (history-entry input)))
    (when entry
      (append-submission-events
       view
       (when (transcript-view-on-submit view)
         (funcall (transcript-view-on-submit view) entry)))))
  t)

(defun handle-transcript-input (view data)
  (cond
    ((submit-input-p data)
     (let ((editor (transcript-view-editor view)))
       (unless (editor-completion editor)
         (record-history view (editor-expanded-value editor)))
       (handle-input editor data))
     t)
    ((handle-input (transcript-view-editor view) data)
     (unless (or (previous-history-input-p data)
                 (next-history-input-p data)
                 (redisplay-input-p data))
       (reset-history-position view))
     t)
    ((previous-history-input-p data)
     (previous-history view))
    ((next-history-input-p data)
     (next-history view))))

(defun handle-transcript-paste (view text)
  (when (handle-paste (transcript-view-editor view) text)
    (reset-history-position view)
    t))

(defun invalidate-transcript-view (view)
  (invalidate (transcript-view-editor view)))

(defun set-transcript-focused (view state)
  (set-focused (transcript-view-editor view) state)
  view)
