(in-package #:kli/tests)

(defun make-tui-transcript-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tui-views:*tui-views-extension-manifest*
                        tui-input:*tui-input-extension-manifest*
                        tui-editor:*tui-editor-extension-manifest*
                        tui-terminal:*tui-terminal-extension-manifest*
                        tui-transcript:*tui-transcript-extension-manifest*)
    (values protocol context)))

(test tui-transcript-extension-registers-service
  (multiple-value-bind (protocol context) (make-tui-transcript-fixture)
    (declare (ignore context))
    (is (ext:extension-loaded-p protocol :tui-transcript))))

(test tui-transcript-events-render-with-labels
  (let ((message (tui-transcript:make-transcript-event
                  :message :assistant "hello"))
        (tool (tui-transcript:make-transcript-event
               :tool-call nil "(+ 1 2)" :name "eval"))
        (result (tui-transcript:make-transcript-event
                 :tool-result nil "3" :name "eval"))
        (system (tui-transcript:make-transcript-event
                 :system nil "ready")))
    (is (string= "[assistant] hello"
                 (tui-transcript:format-event message)))
    (is (string= "[tool] eval (+ 1 2)"
                 (tui-transcript:format-event tool)))
    (is (string= "[result] 3"
                 (tui-transcript:format-event result))
        "result render drops the tool name (the [tool] line above shows it)")
    (is (string= "· ready"
                 (tui-transcript:format-event system)))))

(test transcript-add-event-appends-in-order
  "transcript-add-event keeps events in submission order via a constant-time
   tail pointer. transcript-clear empties so a later append starts a fresh
   ordered list, and transcript-remove-last-user-event drops the latest user
   row so the next append still lands at the end."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol)))
    (flet ((sys (text) (tui-transcript:make-transcript-event :system nil text))
           (usr (text) (tui-transcript:make-transcript-event :message :user text))
           (texts () (mapcar #'tui-transcript:event-text
                             (tui-transcript:transcript-events transcript))))
      (let ((events (list (sys "a") (usr "b") (sys "c"))))
        (dolist (e events)
          (tui-transcript:transcript-add-event transcript e))
        (is (equal events (tui-transcript:transcript-events transcript))
            "events are returned in submission order")
        (is (equal '("a" "b" "c") (texts))))
      (tui-transcript:transcript-clear transcript)
      (is (null (tui-transcript:transcript-events transcript)))
      (tui-transcript:transcript-add-event transcript (sys "d"))
      (is (equal '("d") (texts))
          "a fresh append after clear lands at the head and the tail tracks it")
      (tui-transcript:transcript-add-event transcript (usr "e"))
      (tui-transcript:transcript-add-event transcript (sys "f"))
      (is (equal "e"
                 (tui-transcript:transcript-remove-last-user-event transcript))
          "the latest user row is dropped and its text returned")
      (is (equal '("d" "f") (texts)))
      (tui-transcript:transcript-add-event transcript (sys "g"))
      (is (equal '("d" "f" "g") (texts))
          "after a removal the next append still lands at the end"))))

(test tui-transcript-ambient-format-event-glyphs-by-status
  (is (string= "· note"
               (tui-transcript:format-event
                (tui-transcript:make-transcript-event :system nil "note")))
      "a notice drops the bracket for a leading dot")
  (is (string= "▎ boom"
               (tui-transcript:format-event
                (tui-transcript:make-transcript-event :system nil "boom" :status :error)))
      "an error carries the left bar glyph"))

(test tui-transcript-truncated-reply-renders-annotation
  "A :truncated assistant reply renders the max-output annotation under the
reply text, mirroring the aborted-reply marker; an unmarked reply renders
none."
  (let* ((protocol (make-tui-transcript-fixture))
         (te (tui-transcript:make-transcript-event
              :message :assistant "Cut mid-sen" :status :truncated))
         (joined (format nil "~{~A~^ ~}"
                         (tui-transcript::render-event protocol te 60))))
    (is (search "Truncated" joined))
    (is (search "max output tokens" joined))
    (is (not (search "Truncated"
                     (format nil "~{~A~^ ~}"
                             (tui-transcript::render-event
                              protocol
                              (tui-transcript:make-transcript-event
                               :message :assistant "whole reply")
                              60)))))))

(test tui-transcript-tool-output-expansion-toggles-result-preview
  (let* ((lines (format nil "~{line~D~^~%~}" '(1 2 3 4 5 6 7 8 9 10 11 12 13)))
         (result (tui-transcript:make-transcript-event
                  :tool-result nil lines :name "bash")))
    (let ((tui-transcript:*tool-output-expanded* nil))
      (let ((rendered (tui-transcript:format-event result)))
        (is (search "… (+3 more lines · Ctrl+O)" rendered)
            "collapsed result shows a head preview with a remainder indicator")
        (is (not (search "line11" rendered))
            "collapsed result hides lines past the preview limit")))
    (let ((tui-transcript:*tool-output-expanded* t))
      (let ((rendered (tui-transcript:format-event result)))
        (is (search "line13" rendered)
            "expanded result shows the full stored text")
        (is (not (search "more lines" rendered))
            "expanded result has no truncation indicator")))
    (let ((protocol (make-tui-transcript-fixture)))
      (is (eq t (tui-transcript:toggle-tool-output-expansion protocol))
          "toggle flips collapsed -> expanded")
      (is (eq t (tui-transcript:tool-output-expanded-p protocol)))
      (is (null (tui-transcript:toggle-tool-output-expansion protocol))
          "toggle flips expanded -> collapsed"))))

(test tui-transcript-view-submits-user-input-and-callback-events
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript
                :on-submit (lambda (input)
                             (list (tui-transcript:make-transcript-event
                                    :message :assistant
                                    (format nil "received: ~A" input)))))))
    (tui-core:handle-input view "hello")
    (tui-core:handle-input view "enter")
    (is (= 1 (length (tui-transcript:transcript-events transcript))))
    (is (string= "[assistant] received: hello"
                 (tui-transcript:format-event
                  (first (tui-transcript:transcript-events transcript)))))
    (is (string= "" (tui-editor:editor-value
                     (tui-transcript:transcript-view-editor view))))))

(test tui-transcript-view-recalls-input-history
  (let* ((protocol (make-tui-transcript-fixture))
         (view (tui-transcript:make-transcript-view :protocol protocol))
         (editor (tui-transcript:transcript-view-editor view)))
    (tui-core:handle-input view "one")
    (tui-core:handle-input view "enter")
    (tui-core:handle-input view "two")
    (tui-core:handle-input view "enter")
    (is (string= "" (tui-editor:editor-value editor)))
    (tui-core:handle-input view "up")
    (is (string= "two" (tui-editor:editor-value editor)))
    (tui-core:handle-input view "up")
    (is (string= "one" (tui-editor:editor-value editor)))
    (tui-core:handle-input view "down")
    (is (string= "two" (tui-editor:editor-value editor)))
    (tui-core:handle-input view "down")
    (is (string= "" (tui-editor:editor-value editor)))))

(test tui-transcript-keeps-up-and-down-inside-multiline-input-before-history
  (let* ((protocol (make-tui-transcript-fixture))
         (view (tui-transcript:make-transcript-view :protocol protocol))
         (editor (tui-transcript:transcript-view-editor view)))
    (tui-core:handle-input view "one")
    (tui-core:handle-input view "enter")
    (tui-core:handle-input view (format nil "ab~%cd"))
    (is (tui-core:handle-input view "up"))
    (tui-core:handle-input view "X")
    (is (string= (format nil "abX~%cd")
                 (tui-editor:editor-value editor)))))

(test tui-scrollback-renderer-appends-events-without-clearing-screen
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         output)
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "ready"))
    (tui-core:render-frame renderer terminal)
    (setf output (tui-terminal:terminal-output terminal))
    (is (not (search (format nil "~C[2J" #\Esc) output)))
    (is (search "· ready" output))
    (is (search "> " output))))

(test tui-scrollback-renderer-redraws-prompt-and-appends-submitted-events
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript
                :on-submit (lambda (input)
                             (list (tui-transcript:make-transcript-event
                                    :message :assistant
                                    (format nil "received: ~A" input))))))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         output)
    (tui-core:set-focused view t)
    (tui-core:render-frame renderer terminal)
    (tui-core:handle-input view "hello")
    (tui-core:render-frame renderer terminal)
    (tui-core:handle-input view "enter")
    (tui-core:render-frame renderer terminal)
    (setf output (tui-terminal:terminal-output terminal))
    (is (search "> hello" output))
    (is (search "[assistant] received: hello" output))
    (is (not (search (format nil "~C[2J" #\Esc) output)))))

(test tui-scrollback-streaming-render-cache-reuses-until-inputs-change
  "Frames between delta drains (the ~10 Hz spinner ticks) reuse the open
event's rendered lines; a delta append, status change, or width change
re-renders; finalize drops the cache."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol
                                                      :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (te (tui-transcript:make-transcript-event
              :message :assistant (tui-transcript::make-streaming-text "Hello"))))
    (let ((first-render (tui-transcript:render-streaming-event-cached
                         renderer protocol te 40)))
      (is (eq first-render (tui-transcript:render-streaming-event-cached
                            renderer protocol te 40))
          "an unchanged event reuses the cached lines")
      (tui-transcript::append-streaming-text
       (tui-transcript:event-text te) " world")
      (let ((second-render (tui-transcript:render-streaming-event-cached
                            renderer protocol te 40)))
        (is (not (eq first-render second-render))
            "an appended delta re-renders")
        (is (not (eq second-render (tui-transcript:render-streaming-event-cached
                                    renderer protocol te 30)))
            "a width change re-renders"))
      (setf (tui-transcript:event-status te) :aborted)
      (is (search "Interrupted"
                  (format nil "~{~A~^ ~}"
                          (tui-transcript:render-streaming-event-cached
                           renderer protocol te 40)))
          "a status change re-renders, picking up the abort annotation")
      (tui-transcript:finalize-scrollback-stream renderer)
      (is (null (tui-transcript::scrollback-renderer-stream-render-cache
                 renderer))
          "finalize drops the cache"))))

(test tui-scrollback-renderer-force-repaints-region-without-clearing-screen
  "A forced render (the ctrl+l redisplay) repaints the region in place with clear-line plus prompt, never a clear-screen."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         output)
    (tui-core:set-focused view t)
    (tui-core:handle-input view "hello")
    (tui-core:render-frame renderer terminal)
    (tui-terminal:terminal-clear terminal)
    (tui-core:render-frame renderer terminal :force t)
    (setf output (tui-terminal:terminal-output terminal))
    (is (search (format nil "~C[K" #\Esc) output))
    (is (search "> hello" output))
    (is (not (search (format nil "~C[2J" #\Esc) output)))))

(test tui-scrollback-frame-batches-update-with-hidden-cursor
  "A frame opens by entering synchronized update, hiding the cursor and
   disabling autowrap, and closes in mirror order - the cursor is only ever
   visible at its final placed position, never mid-repaint."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol)))
    (tui-core:set-focused view t)
    (tui-core:handle-input view "hello")
    (tui-core:render-frame renderer terminal)
    (let ((output (tui-terminal:terminal-output terminal))
          (prefix (format nil "~C[?2026h~C[?25l~C[?7l" #\Esc #\Esc #\Esc))
          (suffix (format nil "~C[?7h~C[?25h~C[?2026l" #\Esc #\Esc #\Esc))
          (hide (format nil "~C[?25l" #\Esc))
          (show (format nil "~C[?25h" #\Esc)))
      (flet ((occurrences (needle)
               (loop with step = (length needle)
                     for start = (search needle output)
                       then (search needle output :start2 (+ start step))
                     while start count 1)))
        (is (eql 0 (search prefix output)))
        (is (eql (- (length output) (length suffix))
                 (search suffix output :from-end t)))
        (is (= 1 (occurrences hide)))
        (is (= 1 (occurrences show)))
        (is (search "> hello" output))))))

(test tui-scrollback-frame-restores-terminal-modes-when-draw-faults
  "A faulting draw still restores autowrap, reveals the cursor and leaves
   synchronized update - the shell never inherits a hidden cursor."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (original (fdefinition 'tui-transcript::draw-scrollback-frame)))
    (tui-core:set-focused view t)
    (setf (fdefinition 'tui-transcript::draw-scrollback-frame)
          (lambda (renderer force)
            (declare (ignore renderer force))
            (error "draw boom")))
    (unwind-protect
         (signals error (tui-transcript::default-scrollback-render renderer))
      (setf (fdefinition 'tui-transcript::draw-scrollback-frame) original))
    (let ((output (tui-terminal:terminal-output terminal))
          (suffix (format nil "~C[?7h~C[?25h~C[?2026l" #\Esc #\Esc #\Esc)))
      (is (eql (- (length output) (length suffix))
               (search suffix output :from-end t))))))

(test tui-scrollback-clear-screen-preserves-transcript-events
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         output)
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "ready"))
    (tui-core:render-frame renderer terminal)
    (tui-terminal:terminal-clear terminal)
    (tui-transcript:scrollback-clear-screen renderer)
    (tui-core:render-frame renderer terminal)
    (setf output (tui-terminal:terminal-output terminal))
    (is (= 1 (length (tui-transcript:transcript-events transcript))))
    (is (search (format nil "~C[2J" #\Esc) output))
    (is (not (search (format nil "~C[3J" #\Esc) output)))
    (is (not (search "· ready" output)))
    (is (search "> " output))))

(test tui-scrollback-clears-display-when-events-reset
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         output)
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "ready"))
    (tui-core:render-frame renderer terminal)
    (tui-terminal:terminal-clear terminal)
    (tui-transcript:transcript-clear transcript)
    (tui-core:render-frame renderer terminal)
    (setf output (tui-terminal:terminal-output terminal))
    (is (null (tui-transcript:transcript-events transcript)))
    (is (search (format nil "~C[2J" #\Esc) output))
    (is (search (format nil "~C[3J" #\Esc) output))
    (is (search "> " output))))

(test (tui-scrollback-renderer-can-be-recoded-without-losing-state :fixture interactive-authority)
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol)))
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "ready"))
    (tui-core:render-frame renderer terminal)
    (is (= 1 (tui-transcript:scrollback-renderer-printed-events renderer)))
    (is (eq renderer
            (tui-transcript:recode-scrollback-renderer renderer)))
    (is (= 1 (tui-core:behavior-version
              (tui-transcript:scrollback-renderer-behavior renderer))))
    (is (= 1 (tui-transcript:scrollback-renderer-printed-events renderer)))))

(test (tui-scrollback-recode-preserves-terminal-and-session-state :fixture interactive-authority)
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol)))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "ready"))
    (tui-core:handle-input view "draft")
    (tui-core:render-frame renderer terminal)
    (is (search "· ready" (tui-terminal:terminal-output terminal)))
    (is (search "> draft" (tui-terminal:terminal-output terminal)))
    (is (eq renderer
            (tui-transcript:recode-scrollback-renderer renderer)))
    (is (= 1 (length (tui-transcript:transcript-events transcript))))
    (is (string= "draft"
                 (tui-editor:editor-value
                  (tui-transcript:transcript-view-editor view))))
    (is (search "· ready" (tui-terminal:terminal-output terminal)))
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "after"))
    (tui-core:render-frame renderer terminal)
    (let ((output (tui-terminal:terminal-output terminal)))
      (is (= 2 (length (tui-transcript:transcript-events transcript))))
      (is (= 2 (tui-transcript:scrollback-renderer-printed-events renderer)))
      (is (search "· ready" output))
      (is (search "· after" output))
      (is (search "> draft" output)))))

(test tui-scrollback-renderer-streams-tail-without-re-emitting-history
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 40))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "Hel")))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :message :user "hi"))
    (tui-core:render-frame renderer terminal)
    (is (= 1 (tui-transcript:scrollback-renderer-printed-events renderer)))
    (tui-transcript:transcript-add-event transcript live)
    (tui-transcript:begin-scrollback-stream renderer live)
    (tui-core:render-frame renderer terminal)
    (tui-terminal:terminal-clear terminal)
    (setf (tui-transcript:event-text live) "Hello")
    (tui-core:render-frame renderer terminal)
    (let ((frame (tui-terminal:terminal-output terminal)))
      (is (search "Hello" frame))
      (is (not (search "[user] hi" frame)))
      (is (not (search (format nil "~C[2J" #\Esc) frame))))
    (tui-terminal:terminal-clear terminal)
    (setf (tui-transcript:event-text live) "Hello world")
    (tui-core:render-frame renderer terminal)
    (let ((frame (tui-terminal:terminal-output terminal)))
      (is (search "Hello world" frame))
      (is (not (search "[user] hi" frame))))
    (tui-transcript:finalize-scrollback-stream renderer)
    (tui-core:render-frame renderer terminal)
    (is (= 2 (tui-transcript:scrollback-renderer-printed-events renderer)))
    (is (null (tui-transcript:scrollback-renderer-streaming-event renderer)))
    (is (string= "Hello world" (tui-transcript:event-text live)))))

(test tui-scrollback-renderer-shows-events-appended-after-the-stream
  "Events appended after the open streaming event (e.g. the abort hint and
   \"Interrupted.\") render in the live region below the stream tail, not hidden
   past the commit boundary until the stream finalizes."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "answering")))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event transcript live)
    (tui-transcript:begin-scrollback-stream renderer live)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "Press Esc again to interrupt."))
    (tui-core:render-frame renderer terminal)
    (let ((frame (tui-terminal:terminal-output terminal)))
      (is (search "answering" frame) "the streaming tail still renders")
      (is (search "Press Esc again to interrupt." frame)
          "a notice appended after the open stream renders live, not after finalize"))
    (tui-terminal:terminal-clear terminal)
    (tui-transcript:finalize-scrollback-stream renderer)
    (tui-core:render-frame renderer terminal)
    (is (= 2 (tui-transcript:scrollback-renderer-printed-events renderer))
        "finalize commits the assistant message and the trailing notice")))

(test tui-scrollback-renders-the-prompt-as-an-input-box
  "The prompt is framed by a full-width rule above and below so it reads as an
   input box. The completion popup drops in under the bottom rule, keeping the
   frame stable while completions are open. The cursor stays on the prompt row
   inside the box."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 32))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol))
         (editor (tui-transcript:transcript-view-editor view))
         (rule (make-string 31 :initial-element #\─)))
    (tui-core:set-focused view t)
    (tui-core:render-frame renderer terminal)
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer))
          (crow (tui-transcript:scrollback-renderer-region-cursor-row renderer)))
      (is (= 3 (length region)) "an idle prompt is rule, editor row, rule")
      (is (search rule (first region)) "a full-width rule opens the box")
      (is (text:string-prefix-p " > " (second region))
          "the prompt row sits between the rules")
      (is (search rule (third region)) "a full-width rule closes the box")
      (is (= 1 crow) "the cursor row is the prompt row inside the box"))
    (setf (tui-editor:editor-completion editor)
          (tui-editor::make-completion-popup
           :kind :command
           :candidates (list (tui-editor:make-completion-candidate
                              :insert "/research" :match "/research")
                             (tui-editor:make-completion-candidate
                              :insert "/resume" :match "/resume"))))
    (tui-core:render-frame renderer terminal)
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer)))
      (is (search rule (third region))
          "the bottom rule stays directly under the prompt row")
      (is (search "/research" (fourth region))
          "the completion popup renders below the bottom rule"))))

(test tui-scrollback-clear-anchors-the-next-frame-to-the-bottom
  "After a screen clear the next frame jumps the cursor down so its content
   ends on the bottom row - the prompt reads as a chat input with history
   growing above - instead of drawing from the home position. The jump is a
   cursor move, not newline scrolling, so nothing is pushed into native
   scrollback, and it arms once: the following frame diffs in place."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 32 :rows 10))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol)))
    (tui-core:set-focused view t)
    (tui-transcript:scrollback-clear-screen renderer)
    (tui-terminal:terminal-clear terminal)
    (tui-core:render-frame renderer terminal)
    (let* ((frame (tui-terminal:terminal-output terminal))
           (region (tui-transcript:scrollback-renderer-region-lines renderer))
           (move (format nil "~C[~D;1H" #\Esc (1+ (- 10 (length region)))))
           (jump (search move frame)))
      (is (= 3 (length region)) "an idle prompt region is rule, editor row, rule")
      (is-true jump "the frame jumps to rows minus region height before drawing")
      (is (< jump (search " > " frame)) "the prompt paints after the jump"))
    (tui-terminal:terminal-clear terminal)
    (tui-core:render-frame renderer terminal)
    (is (not (search (format nil "~C[8;1H" #\Esc)
                     (tui-terminal:terminal-output terminal)))
        "the anchor arms once - the next frame diffs in place")))

(test tui-scrollback-bottom-anchor-clamps-when-content-overflows
  "When the reprint after a clear is taller than the viewport (the Ctrl+O
   reprint of a long transcript) the anchor clamps to the home row and the
   overflow scrolls into scrollback as before - real content, never blank
   padding."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 32 :rows 10))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol)))
    (tui-core:set-focused view t)
    (dotimes (i 12)
      (tui-transcript:transcript-add-event
       transcript
       (tui-transcript:make-transcript-event
        :message :assistant (format nil "reply ~D" i))))
    (tui-transcript:scrollback-anchor-bottom renderer)
    (tui-core:render-frame renderer terminal)
    (let ((frame (tui-terminal:terminal-output terminal)))
      (is (search (format nil "~C[1;1H" #\Esc) frame)
          "the jump clamps at the home row when content overflows")
      (is (search "reply 11" frame) "the reprint commits the transcript")
      (is (search " > " frame) "the prompt still paints"))))

(test tui-scrollback-stream-keeps-the-pre-prompt-gap
  "While a stream is open the prompt keeps the same one-blank gap it has at an
   idle prompt instead of sitting flush against the stream tail. Trailing
   events appended after the open stream already end with their inter-entry
   blank, so the gap is not doubled."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 40))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "Hello")))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event transcript live)
    (tui-transcript:begin-scrollback-stream renderer live)
    (tui-core:render-frame renderer terminal)
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer))
          (crow (tui-transcript:scrollback-renderer-region-cursor-row renderer)))
      (is (search "Hello" (first region)) "the stream tail renders first")
      (is (string= "" (second region))
          "one blank separates the stream tail from the prompt box")
      (is (search "──" (third region))
          "the box's top rule sits below the gap")
      (is (text:string-prefix-p " > " (fourth region))
          "the prompt sits inside the box")
      (is (= 3 crow) "the cursor row accounts for the gap and the top rule"))
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "Press Esc again to interrupt."))
    (tui-core:render-frame renderer terminal)
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer)))
      (is (= 1 (count "" region :test #'string=))
          "a trailing event carries its own blank, so the gap is not doubled"))))

(test tui-scrollback-stream-gap-budgets-into-the-viewport-overflow
  "The pre-prompt gap is part of the overflow budget, so a stream tail that
   fills the viewport freezes one more line instead of pushing the prompt off
   the bottom edge."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 20 :rows 5))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "")))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event transcript live)
    (tui-transcript:begin-scrollback-stream renderer live)
    (setf (tui-transcript:event-text live)
          "alpha bravo charlie delta echo foxtrot golf hotel")
    (tui-core:render-frame renderer terminal)
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer)))
      (is (= 5 (length region)) "the region fills but never exceeds the viewport")
      (is (string= "" (second region)) "the gap survives the overflow trim")
      (is (text:string-prefix-p " > " (fourth region))
          "the prompt keeps its boxed row")
      (is (plusp (tui-transcript:scrollback-renderer-frozen-stream-lines renderer))
          "the gap and box cost more frozen stream lines"))))

(test tui-scrollback-renderer-restarts-when-stream-opens-behind-commit-boundary
  "Re-opening a stream on an event already committed (printed past its index)
   self-heals with a full clear and restart instead of a subseq fault. Live
   repro: a hard redraw during an open stream dropped the stream, the reprint
   committed the half-streamed event, and the next delta re-opened it behind
   the commit boundary, faulting every frame."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 40))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "partial")))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :message :user "hi"))
    (tui-transcript:transcript-add-event transcript live)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "notice"))
    (tui-core:render-frame renderer terminal)
    (is (= 3 (tui-transcript:scrollback-renderer-printed-events renderer)))
    (tui-transcript:begin-scrollback-stream renderer live)
    (tui-terminal:terminal-clear terminal)
    (finishes (tui-core:render-frame renderer terminal))
    (is (= 1 (tui-transcript:scrollback-renderer-printed-events renderer))
        "the restart recommits only the events before the open stream")
    (is (search (format nil "~C[2J" #\Esc) (tui-terminal:terminal-output terminal))
        "the inconsistent geometry forces a full clear and reprint")))

(defun max-cursor-up (output)
  "Largest N across all ESC[NA cursor-up sequences in OUTPUT, or 0."
  (let ((max 0)
        (esc (format nil "~C[" #\Esc))
        (pos 0))
    (loop for hit = (search esc output :start2 pos)
          while hit
          do (let ((digits-end (position #\A output :start (+ hit 2))))
               (when (and digits-end
                          (every #'digit-char-p
                                 (subseq output (+ hit 2) digits-end)))
                 (setf max (max max (parse-integer output
                                                   :start (+ hit 2)
                                                   :end digits-end))))
               (setf pos (+ hit 2))))
    max))

(test tui-scrollback-renderer-freezes-overflow-into-scrollback
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 20 :rows 4))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "")))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event transcript live)
    (tui-transcript:begin-scrollback-stream renderer live)
    (setf (tui-transcript:event-text live)
          "alpha bravo charlie delta echo foxtrot golf hotel")
    (tui-core:render-frame renderer terminal)
    (is (<= (length (tui-transcript:scrollback-renderer-region-lines renderer)) 4)
        "on-screen region never exceeds the viewport")
    (is (plusp (tui-transcript:scrollback-renderer-frozen-stream-lines renderer))
        "overflow freezes top stream lines into scrollback")
    (let ((frozen-1 (tui-transcript:scrollback-renderer-frozen-stream-lines renderer)))
      (tui-terminal:terminal-clear terminal)
      (setf (tui-transcript:event-text live)
            "alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo lima")
      (tui-core:render-frame renderer terminal)
      (is (<= (length (tui-transcript:scrollback-renderer-region-lines renderer)) 4))
      (is (>= (tui-transcript:scrollback-renderer-frozen-stream-lines renderer) frozen-1)
          "frozen count grows as more lines roll off")
      (is (<= (max-cursor-up (tui-terminal:terminal-output terminal)) 3)
          "relative cursor anchor never moves past rows-1"))
    (tui-terminal:terminal-clear terminal)
    (tui-transcript:finalize-scrollback-stream renderer)
    (tui-core:render-frame renderer terminal)
    (is (= 1 (tui-transcript:scrollback-renderer-printed-events renderer)))
    (is (zerop (tui-transcript:scrollback-renderer-frozen-stream-lines renderer)))
    (is (not (search "[assistant]" (tui-terminal:terminal-output terminal)))
        "finalize commits only the unfrozen tail, leaving the frozen prefix unduplicated")))
(test tui-scrollback-stream-switch-resets-frozen-lines
  "When the streaming event switches (e.g. thinking -> assistant) the frozen
   line count from the prior event must not carry over: draw-scrollback-frame
   computes stream-lines via (nthcdr frozen stream-all), so a stale count
   drops the new event's first N lines -- they render neither in the region
   nor in scrollback. This only manifests when the thinking text was long
   enough to trigger overflow freezing, hence the 'sometimes' in the bug
   report."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 48 :rows 4))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (thinking (tui-transcript:make-transcript-event
                    :thinking nil "alpha bravo charlie delta echo foxtrot golf hotel"))
         (assistant (tui-transcript:make-transcript-event
                     :message :assistant "Hello world")))
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event transcript thinking)
    (tui-transcript:begin-scrollback-stream renderer thinking)
    (tui-core:render-frame renderer terminal)
    (is (plusp (tui-transcript:scrollback-renderer-frozen-stream-lines renderer))
        "thinking overflow freezes lines into scrollback")
    (tui-transcript:transcript-add-event transcript assistant)
    (tui-transcript:begin-scrollback-stream renderer assistant)
    (is (zerop (tui-transcript:scrollback-renderer-frozen-stream-lines renderer))
        "switching the streaming event resets frozen-stream-lines")
    (is (null (tui-transcript::scrollback-renderer-stream-render-cache renderer))
        "switching the streaming event drops the stale render cache")
    (tui-terminal:terminal-clear terminal)
    (tui-core:render-frame renderer terminal)
    (is (search "Hello world" (tui-terminal:terminal-output terminal))
        "the assistant text renders after the switch, not dropped by a stale nthcdr")))

(test tui-transcript-clamp-region-blocks-drops-expendable-first
  "Overflow drops whole blocks decoration-first (footer, above-input, popup,
notice, gap, trailing) and only the remainder comes off the region top."
  (multiple-value-bind (trailing gap notice above popup footer top-drop)
      (tui-transcript::clamp-region-blocks
       6 '("s") '("t") '("") '("n") '("a") '("r" "e" "r") '("p" "p") '("f" "f"))
    (is (null footer) "the footer drops first")
    (is (null above) "the above-input block drops second")
    (is (null popup) "the completion popup drops third")
    (is (null notice) "the notice drops fourth")
    (is (equal '("") gap) "the gap survives once the overflow is covered")
    (is (equal '("t") trailing) "trailing events survive")
    (is (zerop top-drop) "block drops covered the overflow"))
  (multiple-value-bind (trailing gap notice above popup footer top-drop)
      (tui-transcript::clamp-region-blocks
       12 '("s") '("t") '("") '("n") '("a") '("r" "e" "r") '("p" "p") '("f" "f"))
    (is (and (equal '("t") trailing) (equal '("") gap) (equal '("n") notice)
             (equal '("a") above)
             (equal '("p" "p") popup) (equal '("f" "f") footer))
        "a fitting region keeps every block")
    (is (zerop top-drop)))
  (multiple-value-bind (trailing gap notice above popup footer top-drop)
      (tui-transcript::clamp-region-blocks
       2 '() '() '() '() '() '("r" "a" "b" "r") '() '())
    (declare (ignore trailing gap notice above popup footer))
    (is (= 2 top-drop)
        "rows beyond every droppable block come off the region top")))

(test tui-scrollback-clamps-non-stream-rows-to-the-viewport
  "With no stream lines to freeze, a tall prompt plus a notice on a short
terminal drops decoration blocks and shaves the region top, so the on-screen
region never outgrows the viewport and the cursor anchor stays inside it."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 20 :rows 4))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol)))
    (tui-core:set-focused view t)
    (setf (tui-transcript:scrollback-renderer-notice renderer) "hint text")
    (tui-editor:set-editor-value
     (tui-transcript:transcript-view-editor view)
     "alpha bravo charlie delta echo foxtrot golf hotel india")
    (finishes (tui-core:render-frame renderer terminal))
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer))
          (crow (tui-transcript:scrollback-renderer-region-cursor-row
                 renderer)))
      (is (<= (length region) 4) "the region never exceeds the viewport")
      (is (< crow (length region)) "the cursor row stays inside the region")
      (is (notany (lambda (l) (search "hint text" l)) region)
          "the notice dropped to make room")
      (is (<= (max-cursor-up (tui-terminal:terminal-output terminal)) 3)
          "the cursor walk never moves past rows-1"))))

(test tui-scrollback-streaming-render-skips-highlight-memoization
  "The open streaming event renders with highlight memoization off, so a
growing fenced block does not insert a cache entry per delta. The finalized
commit render memoizes as usual."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 60))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "")))
    (tui-style:register-theme protocol (builtin-theme "dark.json"))
    (tui-core:set-focused view t)
    (let ((tui-markdown::*highlight-cache* (make-hash-table :test 'equal)))
      (tui-transcript:transcript-add-event transcript live)
      (tui-transcript:begin-scrollback-stream renderer live)
      (setf (tui-transcript:event-text live)
            (format nil "```lisp~%(defun f (x) x)~%```"))
      (tui-core:render-frame renderer terminal)
      (is (zerop (hash-table-count tui-markdown::*highlight-cache*))
          "the streaming render inserts no highlight-cache entry")
      (tui-transcript:finalize-scrollback-stream renderer)
      (tui-terminal:terminal-clear terminal)
      (tui-core:render-frame renderer terminal)
      (is (plusp (hash-table-count tui-markdown::*highlight-cache*))
          "the finalized commit render memoizes the closed block"))))

(test tui-transcript-indent-lines-adds-left-gutter
  (is (equal '("  a" "" "  b")
             (tui-transcript:indent-lines '("a" "" "b") 2))
      "non-blank lines gain the gutter, and blank separators stay empty")
  (is (equal '("a" "b") (tui-transcript:indent-lines '("a" "b") 0))
      "a zero margin is a no-op"))

(test tui-transcript-indent-lines-bleeds-only-background-into-gutter
  "A coloured-bg line keeps its colour at the left edge because the gutter is painted the same bg. fg- or underline-only lines get a plain gutter so the underline never stretches into it."
  (let* ((bg (format nil "~C[48;2;1;2;3m" #\Esc))
         (rst (format nil "~C[49m" #\Esc))
         (ul (format nil "~C[4m" #\Esc))
         (ul-off (format nil "~C[24m" #\Esc)))
    (is (string= (concatenate 'string bg "  " rst bg "x" rst)
                 (first (tui-transcript:indent-lines
                         (list (concatenate 'string bg "x" rst)) 2)))
        "bg is painted across the gutter so the colour reaches column 0")
    (is (string= (concatenate 'string "  " ul "h" ul-off)
                 (first (tui-transcript:indent-lines
                         (list (concatenate 'string ul "h" ul-off)) 2)))
        "an underline (no bg) gets a plain gutter, and the underline stays on the text")
    (is (string= bg (tui-transcript::leading-bg-sgr
                     (concatenate 'string bg "x" rst)))
        "leading-bg-sgr extracts the bg-set escape from the leading run")
    (is (null (tui-transcript::leading-bg-sgr (concatenate 'string ul "h")))
        "no bg in the leading run -> nil (plain gutter)")))

(test tui-transcript-finalized-commit-keeps-inter-entry-blank
  "The finalize path preserves each event's trailing blank even when the streamed body was frozen into scrollback, so a long reply does not touch the next entry with no vertical gap."
  (let* ((protocol (make-tui-transcript-fixture))
         (a (tui-transcript:make-transcript-event :message :assistant "abc"))
         (u (tui-transcript:make-transcript-event :message :user "y")))
    (let ((whole (tui-transcript::finalized-commit-lines protocol (list a) 0 40))
          (frozen (tui-transcript::finalized-commit-lines protocol (list a) 1 40))
          (pair (tui-transcript::finalized-commit-lines protocol (list a u) 0 40)))
      (is (string= "" (car (last whole))) "an unfrozen event ends with a blank")
      (is (find "" frozen :test #'string=)
          "the blank survives when the event body was frozen")
      (is (= 2 (count "" pair :test #'string=))
          "each finalized event keeps its own trailing blank"))))

(test tui-scrollback-renderer-applies-left-margin
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol
                :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal
                    :protocol protocol))
         (tui-transcript::*transcript-left-margin* 2)
         output)
    (tui-core:set-focused view t)
    (tui-transcript:transcript-add-event
     transcript
     (tui-transcript:make-transcript-event :system nil "ready"))
    (tui-core:render-frame renderer terminal)
    (setf output (tui-terminal:terminal-output terminal))
    (is (search "  · ready" output) "committed history carries the left gutter")
    (is (search "  > " output) "the prompt carries the left gutter")
    (is (every (lambda (l) (or (string= l "") (text:string-prefix-p "  " l)))
               (tui-transcript:scrollback-renderer-region-lines renderer))
        "every non-blank region line starts at the margin")))

(test tui-scrollback-disables-autowrap-around-a-frame
  "A line that exactly fills the terminal must not trigger the terminal's own wrap or scroll, so the frame draws with autowrap off and restores it after."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 48))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol))
         (off (format nil "~C[?7l" #\Esc))
         (on (format nil "~C[?7h" #\Esc)))
    (tui-core:set-focused view t)
    (tui-core:render-frame renderer terminal)
    (let* ((out (tui-terminal:terminal-output terminal))
           (off-at (search off out))
           (on-at (search on out)))
      (is-true off-at "the frame disables autowrap before drawing")
      (is-true on-at "the frame restores autowrap after drawing")
      (is (< off-at on-at) "autowrap is disabled before it is restored"))))

(test tui-scrollback-renders-footer-at-bottom-below-prompt
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 24))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol)))
    (tui-core:set-focused view t)
    (tui-status:register-status-slot protocol :ctx)
    (tui-status:set-status protocol :ctx "ctx 5%")
    (tui-core:render-frame renderer terminal)
    (let* ((region (tui-transcript:scrollback-renderer-region-lines renderer))
           (crow (tui-transcript:scrollback-renderer-region-cursor-row renderer))
           (footer-line (find-if (lambda (l) (search "ctx 5%" l)) region))
           (footer-idx (position footer-line region :test #'eq)))
      (is-true footer-line "the footer renders below the prompt")
      (is (text:string-prefix-p " > " (nth crow region))
          "the cursor row stays on the prompt, never the footer")
      (is (> footer-idx crow) "the footer sits below the cursor row")
      (is (member "" region :test #'string=)
          "blank gaps frame the footer above and below")
      (is (string= "" (car (last region)))
          "a bottom gap keeps the footer off the bottom edge")
      (is (text:string-prefix-p
           (make-string tui-transcript::*footer-left-margin* :initial-element #\Space)
           footer-line)
          "the footer is inset from the left edge")
      (is (search "ctx 5%" (tui-terminal:terminal-output terminal))))))

(test tui-scrollback-renders-above-input-widget-above-prompt
  "An above-input widget (the working indicator's slot) renders inside the
   region directly above the prompt box, never in the footer below it, and
   carries the transcript left gutter."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 24))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol)))
    (tui-core:set-focused view t)
    (tui-status:register-widget protocol :spin
                                (lambda (p theme width)
                                  (declare (ignore p theme width))
                                  (list "* working"))
                                :placement :above-input)
    (tui-status:register-status-slot protocol :ctx)
    (tui-status:set-status protocol :ctx "ctx 5%")
    (tui-core:render-frame renderer terminal)
    (let* ((region (tui-transcript:scrollback-renderer-region-lines renderer))
           (crow (tui-transcript:scrollback-renderer-region-cursor-row renderer))
           (spin-idx (position-if (lambda (l) (search "* working" l)) region))
           (prompt-idx (position-if (lambda (l) (text:string-prefix-p " > " l))
                                    region))
           (footer-idx (position-if (lambda (l) (search "ctx 5%" l)) region)))
      (is-true spin-idx "the above-input widget renders in the region")
      (is (= spin-idx (- prompt-idx 2))
          "the widget sits directly above the input box top rule")
      (is (> footer-idx prompt-idx) "the footer stays below the prompt")
      (is (text:string-prefix-p " > " (nth crow region))
          "the cursor row stays on the prompt, never the widget line")
      (is (text:string-prefix-p " *" (nth spin-idx region))
          "the widget line carries the transcript left gutter"))))

(test tui-scrollback-footer-absent-until-status-set
  "With no slot text and no widgets the footer contributes no line at all, so the region is the prompt box alone."
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 24))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol)))
    (tui-core:set-focused view t)
    (tui-core:render-frame renderer terminal)
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer)))
      (is (= 3 (length region)) "the region is the prompt box alone")
      (is (text:string-prefix-p " > " (second region))))))

(test tui-scrollback-footer-reserves-rows-so-prompt-survives-overflow
  (let* ((protocol (make-tui-transcript-fixture))
         (transcript (tui-transcript:make-transcript :protocol protocol))
         (view (tui-transcript:make-transcript-view
                :protocol protocol :transcript transcript))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol :columns 20 :rows 6))
         (renderer (tui-transcript:make-scrollback-renderer
                    transcript view terminal :protocol protocol))
         (live (tui-transcript:make-transcript-event :message :assistant "")))
    (tui-core:set-focused view t)
    (tui-status:register-status-slot protocol :ctx)
    (tui-status:set-status protocol :ctx "ctx 9%")
    (tui-transcript:transcript-add-event transcript live)
    (tui-transcript:begin-scrollback-stream renderer live)
    (setf (tui-transcript:event-text live)
          "alpha bravo charlie delta echo foxtrot golf hotel")
    (tui-core:render-frame renderer terminal)
    (let ((region (tui-transcript:scrollback-renderer-region-lines renderer)))
      (is (<= (length region) 6)
          "stream tail + prompt + footer block never exceed the viewport")
      (is (find-if (lambda (l) (search "ctx 9%" l)) region)
          "the footer stays pinned at the bottom under overflow")
      (is (find-if (lambda (l) (text:string-prefix-p " > " l)) region)
          "the prompt is never pushed off-screen by the footer")
      (is (plusp (tui-transcript:scrollback-renderer-frozen-stream-lines renderer))
          "overflow freezes stream lines to make room for prompt + footer"))))


(test transcript-remove-last-user-event-drops-only-the-last-user-row
  "Removes the most recent :message/:user row, returns its text, and keeps the rest.
   Returns NIL when there is no user row."
  (let ((transcript (tui-transcript:make-transcript)))
    (is (null (tui-transcript:transcript-remove-last-user-event transcript)))
    (dolist (event (list (tui-transcript:make-transcript-event :message :user "first")
                         (tui-transcript:make-transcript-event :message :assistant "reply")
                         (tui-transcript:make-transcript-event :message :user "second")))
      (tui-transcript:transcript-add-event transcript event))
    (is (string= "second"
                 (tui-transcript:transcript-remove-last-user-event transcript)))
    (let ((events (tui-transcript:transcript-events transcript)))
      (is (equal '(:user :assistant)
                 (mapcar #'tui-transcript:event-role events)))
      (is (string= "first" (tui-transcript:event-text (first events)))))))
