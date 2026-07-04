(in-package #:kli/tui/transcript)

(defun terminal-cursor-up (terminal count)
  (when (plusp count)
    (write-terminal terminal (format nil "~C[~DA" #\Esc count))))

(defun terminal-cursor-down (terminal count)
  (when (plusp count)
    (write-terminal terminal (format nil "~C[~DB" #\Esc count))))

(defun terminal-cursor-forward (terminal count)
  (when (plusp count)
    (write-terminal terminal (format nil "~C[~DC" #\Esc count))))

(defun scrollback-anchor-bottom (renderer)
  "Arm the next frame to land its content against the bottom of the screen.
   Every screen-clear path sets this so the prompt does not draw at the home
   position."
  (setf (scrollback-renderer-bottom-anchor-pending-p renderer) t)
  renderer)

(defun scrollback-clear-screen (renderer)
  (terminal-clear-screen (scrollback-renderer-terminal renderer))
  (setf (scrollback-renderer-frozen-stream-lines renderer) 0
        (scrollback-renderer-region-lines renderer) nil
        (scrollback-renderer-region-cursor-row renderer) 0
        (scrollback-renderer-region-cursor-column renderer) 0
        (scrollback-renderer-notice renderer) nil)
  (scrollback-anchor-bottom renderer))

(defun render-events-lines (protocol events width)
  "One blank line after each committed event gives inter-entry breathing room and
   a pre-prompt gap — the last event's trailing blank sits above the region."
  (loop for event in events
        append (render-event protocol event width)
        collect ""))

(defun render-notice-lines (protocol notice width)
  "Render the transient NOTICE hint as dim region lines wrapped to WIDTH (the
   post-margin content width). Plain when no theme is active. Empty when NOTICE
   is nil or blank, reserving no rows."
  (when (and notice (plusp (length notice)))
    (let ((theme (ignore-errors (active-theme protocol))))
      (loop for w in (wrap-text notice (max 1 width))
            collect (if theme
                        (style theme "dim" (pad-right w width) :attrs '(:italic))
                        (pad-right w width))))))

(defun region-first-change (old new)
  "First line index at which OLD and NEW diverge, or NIL when identical. For
   equal common prefixes of differing length, returns the index a redraw must
   start from (clamped so a pure shrink rewrites its last surviving line)."
  (let ((min (min (length old) (length new))))
    (or (loop for i from 0 below min
              for o in old
              for n in new
              unless (string= o n) do (return i))
        (cond ((= (length old) (length new)) nil)
              ((< (length new) (length old)) (max 0 (1- (length new))))
              (t min)))))

(defun write-region-line (terminal line)
  (terminal-clear-line terminal)
  (write-terminal terminal line))

(defun clear-region-remnants (terminal extra)
  "Clear EXTRA stale lines below the cursor (sitting at the end of the last drawn
   line) and return it to that row."
  (when (plusp extra)
    (loop repeat extra
          do (write-terminal terminal (string #\Newline))
             (terminal-clear-line terminal))
    (terminal-cursor-up terminal extra)))

(defun place-region-cursor (renderer region-lines cursor-row cursor-col)
  "With the cursor at the end of the last region line, move it to the prompt
   cursor position and persist the region geometry."
  (let ((terminal (scrollback-renderer-terminal renderer))
        (count (length region-lines)))
    (terminal-cursor-up terminal (- (1- count) cursor-row))
    (write-terminal terminal (string #\Return))
    (terminal-cursor-forward terminal cursor-col)
    (setf (scrollback-renderer-region-lines renderer) region-lines
          (scrollback-renderer-region-cursor-row renderer) cursor-row
          (scrollback-renderer-region-cursor-column renderer) cursor-col)))

(defparameter *transcript-left-margin* 1
  "Columns of plain left gutter applied to every committed and region line.
   Content is wrapped to (width - margin) so the indent never overflows.")

(defun leading-bg-sgr (line)
  "The background-setting SGR escape (the ESC[48 form) within LINE's leading SGR
   run, or NIL. The gutter inherits ONLY the background, so a coloured block
   reaches column 0 without dragging fg/underline/bold into the gutter."
  (let ((n (length line)) (i 0))
    (loop while (and (< (1+ i) n)
                     (char= (char line i) #\Esc)
                     (char= (char line (1+ i)) #\[))
          do (let ((m (position #\m line :start (+ i 2)))
                   (p (+ i 2)))
               (unless m (return nil))
               (when (and (<= (+ p 2) m)
                          (char= (char line p) #\4)
                          (char= (char line (1+ p)) #\8))
                 (return-from leading-bg-sgr (subseq line i (1+ m))))
               (setf i (1+ m))))
    nil))

(defun indent-lines (lines margin)
  "Inset content by MARGIN columns. A coloured background bleeds through the
   gutter to the left edge — the gutter is painted that same bg — while other
   styles (fg, bold, underline) never leak into it. Plain lines just shift right.
   Blank separators stay empty so they carry no trailing whitespace."
  (if (zerop margin)
      lines
      (let ((spaces (make-string margin :initial-element #\Space)))
        (mapcar (lambda (l)
                  (cond ((string= l "") l)
                        (t (let ((bg (leading-bg-sgr l)))
                             (if bg
                                 (concatenate 'string bg spaces +bg-reset+ l)
                                 (concatenate 'string spaces l))))))
                lines))))

(defun render-streaming-event-cached (renderer protocol event width)
  "Render the open streaming EVENT, reusing the previous frame's lines when
   nothing observable changed. Spinner ticks repaint at ~10 Hz between delta
   drains, and without the cache every repaint re-parses and re-wraps the whole
   accumulated reply -- O(reply^2) over a stream. The key covers each input
   render-event reads that can change mid-stream: the event itself, its text
   length (deltas only append, so length identifies content), its status (the
   abort annotation), width, theme, and tool-output expansion. Highlight
   memoization is off for this render: a growing fenced block would otherwise
   insert a highlight-cache entry for every partial prefix, one per delta.
   On a cache miss the markdown render runs incrementally against a per-reply
   mdstream (bound into *MD-STREAM*), so even a real content change re-parses only
   the open tail rather than the whole accumulated reply. An open fenced code
   block — which mdstream cannot advance past — re-highlights incrementally against
   a per-reply hl-stream (bound into *HL-STREAM*), re-scanning only its open tail."
  (let ((key (list event
                   (length (event-text event))
                   (event-status event)
                   width
                   (ignore-errors (active-theme protocol))
                   (tool-output-expanded-p protocol)))
        (cache (scrollback-renderer-stream-render-cache renderer)))
    (if (and cache (equal (car cache) key))
        (cdr cache)
        (let* ((md (or (scrollback-renderer-stream-md renderer)
                       (setf (scrollback-renderer-stream-md renderer) (make-mdstream))))
               (hl (or (scrollback-renderer-stream-hl renderer)
                       (setf (scrollback-renderer-stream-hl renderer) (make-hl-stream))))
               (lines (let ((*highlight-memoize-p* nil)
                            (*md-stream* md)
                            (*hl-stream* hl))
                        (render-event protocol event width))))
          (setf (scrollback-renderer-stream-render-cache renderer)
                (cons key lines))
          lines))))

(defun finalized-commit-lines (protocol events frozen width)
  "Committed lines for finalized EVENTS, dropping the FROZEN prefix already in
   scrollback while keeping each event's trailing inter-entry blank — so a
   finalized streamed reply stays separated from the next entry."
  (let ((all (render-events-lines protocol events width)))
    (if (plusp frozen) (nthcdr frozen all) all)))

(defparameter *footer-left-margin* 4
  "Columns of empty space to the left of the footer.")
(defparameter *footer-right-margin* 4
  "Columns kept clear to the right of the footer.")
(defparameter *footer-top-gap* 1
  "Blank lines between the prompt and the footer.")
(defparameter *footer-bottom-gap* 1
  "Blank lines kept below the footer so it floats off the bottom edge.")

(defun footer-block (protocol width theme)
  "The footer's region lines: a *FOOTER-TOP-GAP* blank gap above, each footer line
   inset by *FOOTER-LEFT-MARGIN* and rendered to the width left after both side
   margins, then a *FOOTER-BOTTOM-GAP* blank gap below. Empty (reserving no rows)
   when the footer has no content."
  (let ((content (render-footer protocol theme
                                (max 1 (- width *footer-left-margin*
                                          *footer-right-margin*)))))
    (when content
      (append (make-list *footer-top-gap* :initial-element "")
              (indent-lines content *footer-left-margin*)
              (make-list *footer-bottom-gap* :initial-element "")))))

(defun render-input-rule (protocol width)
  "One horizontal rule at WIDTH framing the input box. Dim when a theme is
   active, plain otherwise."
  (let ((line (make-string (max 1 width) :initial-element #\─))
        (theme (ignore-errors (active-theme protocol))))
    (if theme (style theme "dim" line) line)))

(defun clamp-region-blocks (rows stream-lines trailing-lines gap-lines
                            notice-lines above-lines prompt-lines popup-lines
                            footer-lines)
  "Fit the live-region blocks into the ROWS viewport. The stream overflow
   budget freezes stream lines only, so on a short terminal the remaining
   blocks can still exceed the screen, and a region taller than the viewport
   desyncs the relative cursor walk that anchors every frame. Whole blocks
   drop, most expendable first -- footer, above-input widgets, completion
   popup, notice, gap, trailing events -- and any remainder comes off the top
   of the assembled region. Returns (values trailing gap notice above popup
   footer top-drop) with the dropped blocks emptied."
  (let ((overflow (- (+ (length stream-lines) (length trailing-lines)
                        (length gap-lines) (length notice-lines)
                        (length above-lines)
                        (length prompt-lines) (length popup-lines)
                        (length footer-lines))
                     rows)))
    (flet ((drop (lines)
             (cond ((not (plusp overflow)) lines)
                   ((null lines) lines)
                   (t (decf overflow (length lines)) nil))))
      (let* ((footer (drop footer-lines))
             (above (drop above-lines))
             (popup (drop popup-lines))
             (notice (drop notice-lines))
             (gap (drop gap-lines))
             (trailing (drop trailing-lines)))
        (values trailing gap notice above popup footer (max 0 overflow))))))

(defun draw-scrollback-frame (renderer force)
  "Commit finalized events (and overflow-frozen open-event lines) into native
   scrollback, then line-diff the bottom region (open-event tail ++ prompt)
   against the cache, rewriting only changed lines.

   When the transcript shrank below the committed count, or the committed count
   has passed the open stream's commit boundary, drop everything and restart.
   Each render anchors to the old region top, with committed history
   above and off-cursor. On finalize the open event's already-frozen prefix is
   dropped so it is not committed twice, while the unfrozen tail keeps its
   inter-entry blank. Events appended after the open streaming event (the abort
   hint and Interrupted row) render live below the stream tail, else they fall
   past the commit boundary and stay invisible until the stream finalizes.
   An open stream keeps the idle prompt's one-blank gap: a blank row sits
   between the stream tail and the notice/prompt block unless trailing events
   already supply their inter-entry blank. That row is part of the overflow
   budget and is given up when freezing every stream line still cannot fit
   the region.
   A frame armed by a screen clear jumps the cursor down first so its content
   ends on the bottom row - the prompt reads as a chat input with history
   growing above. The jump clamps at the top when the content overflows the
   viewport, and it is a cursor move rather than newline scrolling, so a short
   frame pushes nothing into native scrollback.
   The prompt renders as an input box: a dim full-width rule above and below
   the editor rows, with the completion popup dropping in under the bottom
   rule so the frame stays stable while completions cycle. The prompt cursor
   row shifts by the top rule.
   The transient notice hint sits just above the prompt box and below the stream
   tail as renderer state, not a transcript event, so it leaves no permanent row.
   Above-input widgets (the working indicator) render directly above the prompt
   box's top rule, inside the gutter-indented region, reserving no row while
   they contribute nothing.
   The footer pins to the bottom, inset by its own margins with a blank gap above
   the prompt, absent when no slot or widget has content. Stable top stream lines
   that would push the prompt past the viewport are frozen, with trailing notices
   and footer rows reserved out of that budget. When the non-stream rows alone
   exceed the viewport, whole blocks drop -- footer, above-input widgets, popup,
   notice, gap, trailing -- then the region top is shaved as a last resort, so
   the region never outgrows the screen. A left gutter applies to every
   committed and prompt line, with content wrapped to CWIDTH so the indent never
   overflows while the footer keeps full width."
  (let* ((protocol (object-protocol renderer))
         (terminal (scrollback-renderer-terminal renderer))
         (view (scrollback-renderer-view renderer))
         (editor (transcript-view-editor view))
         (transcript (scrollback-renderer-transcript renderer))
         (events (transcript-events transcript))
         (width (terminal-columns terminal))
         (rows (terminal-rows terminal))
         (streaming (scrollback-renderer-streaming-event renderer))
         (printed (scrollback-renderer-printed-events renderer))
         (frozen (scrollback-renderer-frozen-stream-lines renderer))
         (stream-index (and streaming (position streaming events)))
         (commit-limit (if streaming (or stream-index printed) (length events))))
    (when (or (> printed (length events)) (> printed commit-limit))
      (terminal-clear-screen terminal)
      (terminal-clear-scrollback terminal)
      (setf printed 0
            frozen 0
            commit-limit (if streaming (or stream-index 0) (length events))
            (scrollback-renderer-printed-events renderer) 0
            (scrollback-renderer-frozen-stream-lines renderer) 0
            (scrollback-renderer-region-lines renderer) nil
            (scrollback-renderer-region-cursor-row renderer) 0
            (scrollback-renderer-region-cursor-column renderer) 0
            (scrollback-renderer-bottom-anchor-pending-p renderer) t))
    (terminal-cursor-up terminal (scrollback-renderer-region-cursor-row renderer))
    (write-terminal terminal (string #\Return))
    (let* ((margin *transcript-left-margin*)
           (cwidth (max 1 (- width margin)))
           (fin-events (subseq events printed commit-limit))
           (fin-lines (finalized-commit-lines protocol fin-events frozen cwidth))
           (stream-all (and streaming
                            (render-streaming-event-cached
                             renderer protocol streaming cwidth)))
           (stream-lines (nthcdr frozen stream-all))
           (trailing-lines (and stream-index
                                (render-events-lines
                                 protocol (nthcdr (1+ stream-index) events) cwidth)))
           (gap-wanted (and stream-all (null trailing-lines)))
           (notice-lines (render-notice-lines
                          protocol (scrollback-renderer-notice renderer) cwidth))
           (above-lines (render-widgets protocol
                                        (ignore-errors (active-theme protocol))
                                        cwidth :placement :above-input))
           (rule (render-input-rule protocol cwidth))
           (editor-rows-cursor (multiple-value-list
                                (editor-rows-and-cursor editor cwidth)))
           (prompt-lines (append (list rule)
                                 (first editor-rows-cursor)
                                 (list rule)))
           (popup-lines (render-completion-lines editor cwidth))
           (prompt-cursor (or (second editor-rows-cursor) (cons 0 0)))
           (footer-lines (footer-block protocol width
                                       (ignore-errors (active-theme protocol))))
           (spill (- (+ (length stream-lines) (length trailing-lines)
                        (if gap-wanted 1 0)
                        (length notice-lines) (length above-lines)
                        (length prompt-lines) (length popup-lines))
                     (- rows (length footer-lines))))
           (excess (max 0 (min (length stream-lines) spill)))
           (gap-lines (and gap-wanted
                           (<= spill (length stream-lines))
                           (list "")))
           (onscreen-stream (nthcdr excess stream-lines)))
      (multiple-value-bind (trailing-lines gap-lines notice-lines above-lines
                            popup-lines footer-lines top-drop)
          (clamp-region-blocks rows onscreen-stream trailing-lines gap-lines
                               notice-lines above-lines prompt-lines
                               popup-lines footer-lines)
        (let* ((region (append (indent-lines
                                (nthcdr top-drop
                                        (append onscreen-stream trailing-lines
                                                gap-lines notice-lines
                                                above-lines prompt-lines
                                                popup-lines))
                                margin)
                               footer-lines))
               (region-crow (min (max 0 (- (+ (length onscreen-stream)
                                              (length trailing-lines)
                                              (length gap-lines)
                                              (length notice-lines)
                                              (length above-lines)
                                              (1+ (car prompt-cursor)))
                                           top-drop))
                                 (1- (length region))))
               (region-ccol (+ margin (cdr prompt-cursor)))
               (commit-block (indent-lines
                              (append fin-lines (subseq stream-lines 0 excess))
                              margin))
               (cache (scrollback-renderer-region-lines renderer))
               (old-count (length cache))
               (first-change (and cache (not force)
                                  (region-first-change cache region))))
          (when (shiftf (scrollback-renderer-bottom-anchor-pending-p renderer)
                        nil)
            (terminal-move-cursor terminal
                                  (max 0 (- rows
                                            (length commit-block)
                                            (length region)))
                                  0))
          (cond
            (commit-block
             (dolist (line commit-block)
               (write-region-line terminal line)
               (write-terminal terminal (string #\Newline)))
             (loop for line in region
                   for rest on region
                   do (write-region-line terminal line)
                      (when (cdr rest)
                        (write-terminal terminal (string #\Newline))))
             (clear-region-remnants
              terminal (- old-count (length commit-block) (length region)))
             (setf (scrollback-renderer-printed-events renderer) commit-limit
                   (scrollback-renderer-frozen-stream-lines renderer)
                   (if streaming (+ frozen excess) 0))
             (place-region-cursor renderer region region-crow region-ccol))
            ((and cache (not force) (null first-change))
             (terminal-cursor-down terminal region-crow)
             (write-terminal terminal (string #\Return))
             (terminal-cursor-forward terminal region-ccol)
             (setf (scrollback-renderer-region-cursor-row renderer) region-crow
                   (scrollback-renderer-region-cursor-column renderer)
                   region-ccol))
            ((and cache (not force))
             (terminal-cursor-down terminal first-change)
             (write-terminal terminal (string #\Return))
             (loop for i from first-change below (length region)
                   for line in (nthcdr first-change region)
                   do (when (> i first-change)
                        (write-terminal terminal (string #\Newline)))
                      (write-region-line terminal line))
             (clear-region-remnants terminal (- old-count (length region)))
             (place-region-cursor renderer region region-crow region-ccol))
            (t
             (loop for line in region
                   for rest on region
                   do (write-region-line terminal line)
                      (when (cdr rest)
                        (write-terminal terminal (string #\Newline))))
             (clear-region-remnants terminal (- old-count (length region)))
             (place-region-cursor renderer region region-crow region-ccol)))))))
  renderer)

(defparameter +autowrap-off+ (format nil "~C[?7l" #\Esc)
  "DECAWM reset — the terminal stops auto-wrapping at the right margin.")
(defparameter +autowrap-on+ (format nil "~C[?7h" #\Esc)
  "DECAWM set — restore the terminal's default auto-wrap.")

(defun default-scrollback-render (renderer &key force)
  "Render a frame as one batched write. The whole frame buffers and reaches the
   tty in a single write, so a multiplexer that ignores synchronized output
   (mode 2026) still sees the frame in one read and cannot redraw the pane part
   way through the cursor walk - the flash of the cursor at a stale spot. The
   batch is the load-bearing guarantee, with mode 2026 a bonus on terminals
   that honor it. The cursor stays hidden until placed at its final prompt
   position, and autowrap is off so a line that exactly fills the terminal (an
   edge-to-edge bg block, or a soft-wrapped input row at the width boundary)
   cannot trigger the terminal's own wrap or scroll - the renderer owns every
   line break. Every mode and the batch restore in mirror order on exit, fault
   or not, so the surrounding shell never inherits a hidden cursor, a wrapless
   terminal, or a half-drained frame."
  (let ((terminal (scrollback-renderer-terminal renderer)))
    (terminal-begin-frame terminal)
    (unwind-protect
         (progn
           (terminal-begin-synchronized-update terminal)
           (terminal-hide-cursor terminal)
           (write-terminal terminal +autowrap-off+)
           (unwind-protect (draw-scrollback-frame renderer force)
             (write-terminal terminal +autowrap-on+)
             (terminal-show-cursor terminal)
             (terminal-end-synchronized-update terminal)))
      (terminal-end-frame terminal)))
  renderer)

(defun begin-scrollback-stream (renderer event)
  "Open the live region on EVENT. Renders draw it diffed, out of scrollback.
   Reset frozen-stream-lines and the render cache so overflow geometry from
   a prior streaming event (e.g. thinking) does not carry over to the new
   one (e.g. assistant) and drop its first lines via a stale nthcdr."
  (unless (eq (scrollback-renderer-streaming-event renderer) event)
    (setf (scrollback-renderer-streaming-event renderer) event
          (scrollback-renderer-region-lines renderer) nil
          (scrollback-renderer-frozen-stream-lines renderer) 0
          (scrollback-renderer-stream-render-cache renderer) nil
          (scrollback-renderer-stream-md renderer) nil
          (scrollback-renderer-stream-hl renderer) nil))
  renderer)

(defun finalize-scrollback-stream (renderer)
  "Close the commit boundary so the next render freezes the finalized event into
   scrollback. Region geometry stays so that frame can clear on-screen remnants.
   The render cache drops so the finalized reply's lines are not retained."
  (setf (scrollback-renderer-streaming-event renderer) nil
        (scrollback-renderer-stream-render-cache renderer) nil
        (scrollback-renderer-stream-md renderer) nil
        (scrollback-renderer-stream-hl renderer) nil)
  renderer)

(defun render-scrollback-frame (renderer terminal &key force)
  (declare (ignore terminal))
  (call-behavior (scrollback-renderer-behavior renderer)
                 renderer
                 :force force))

(defun scrollback-reset (renderer)
  (setf (scrollback-renderer-printed-events renderer) 0
        (scrollback-renderer-frozen-stream-lines renderer) 0
        (scrollback-renderer-streaming-event renderer) nil
        (scrollback-renderer-stream-render-cache renderer) nil
        (scrollback-renderer-stream-md renderer) nil
        (scrollback-renderer-stream-hl renderer) nil
        (scrollback-renderer-region-lines renderer) nil
        (scrollback-renderer-region-cursor-row renderer) 0
        (scrollback-renderer-region-cursor-column renderer) 0
        (scrollback-renderer-notice renderer) nil
        (scrollback-renderer-bottom-anchor-pending-p renderer) nil)
  renderer)

(defun recode-scrollback (renderer
                          &key function version (state nil state-p)
                            (metadata nil metadata-p)
                            (capabilities nil capabilities-p))
  (apply #'recode-tui-behavior
         (scrollback-renderer-behavior renderer)
         (append (when function
                   (list :function function))
                 (when version
                   (list :version version))
                 (when state-p
                   (list :state state))
                 (when metadata-p
                   (list :metadata metadata))
                 (when capabilities-p
                   (list :capabilities capabilities))))
  renderer)

(defun recode-scrollback-renderer (renderer &key
                                              (function #'default-scrollback-render)
                                              version (state nil state-p)
                                              (metadata nil metadata-p)
                                              (capabilities nil capabilities-p))
  (apply #'recode-scrollback
         renderer
         (append (list :function function)
                 (when version
                   (list :version version))
                 (when state-p
                   (list :state state))
                 (when metadata-p
                   (list :metadata metadata))
                 (when capabilities-p
                   (list :capabilities capabilities)))))
