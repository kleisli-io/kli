(in-package #:kli/tests)

(defun make-tui-rendering-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tui-style:*tui-style-extension-manifest*
                        tui-status:*tui-status-extension-manifest*
                        tui-views:*tui-views-extension-manifest*
                        tui-input:*tui-input-extension-manifest*
                        tui-editor:*tui-editor-extension-manifest*
                        tui-terminal:*tui-terminal-extension-manifest*
                        tui-transcript:*tui-transcript-extension-manifest*)
    (values protocol context)))

(defun styled-bg-block (theme bg-token fg-token text width)
  (let ((bg (tui-style:theme-token theme bg-token))
        (fg (tui-style:theme-token theme fg-token)))
    (flet ((line (s) (tui-style:style-span (text:pad-right s width) :bg bg :fg fg)))
      (loop for w in (text:wrap-text text (max 1 (- width 2)))
            collect (line (concatenate 'string " " w))))))

(defun styled-tool-lines (theme bg-token name text width)
  (let* ((bg (tui-style:theme-token theme bg-token))
         (head (string-downcase name))
         (body (concatenate 'string head " " text))
         (rest (subseq body (length head)))
         (content (concatenate 'string " " (tui-style:style-span head :attrs '(:bold)) rest))
         (pad (max 0 (- width (1+ (text:visible-width body))))))
    (list (tui-style:style-span
           (concatenate 'string content (make-string pad :initial-element #\Space)) :bg bg))))

(defun rendering-find-system-renderer ()
  (find-method (fdefinition 'tui-core:render-transcript-event)
               '()
               (list (sb-mop:intern-eql-specializer :system)
                     (find-class 't) (find-class 't)
                     (find-class 't) (find-class 't))
               nil))

(ext:defextension rendering-system-renderer
  (:provides
   (message-renderer :system
     (lambda (event theme width)
       (declare (ignore theme width))
       (list (format nil "D:~A" (tui-transcript:event-text event)))))))

(ext:defextension rendering-combo-ui
  (:provides
   (message-renderer :system
     (lambda (event theme width)
       (declare (ignore theme width))
       (list (format nil "C:~A" (tui-transcript:event-text event)))))
   (status-slot :branch :initial "")
   (theme :combo-theme
     (tui-style:load-theme "{\"name\":\"combo-theme\",\"colors\":{},\"vars\":{}}"))))

(test styled-theme-nil-is-plain
  (let ((proto (make-tui-rendering-fixture)))
    (dolist (event (list
                    (tui-transcript:make-transcript-event
                     :message :user "hello world this is a long-ish message to wrap")
                    (tui-transcript:make-transcript-event :message :assistant "hi")
                    (tui-transcript:make-transcript-event
                     :tool-call nil "ls -la /some/very/long/path/that/wraps/around"
                     :name "bash")
                    (tui-transcript:make-transcript-event
                     :tool-result nil (format nil "line1~%line2") :name "x")
                    (tui-transcript:make-transcript-event :system nil "a system note")
                    (tui-transcript:make-transcript-event
                     :thinking nil "pondering deeply" :status "HIGH")
                    (tui-transcript:make-transcript-event :custom nil "x")))
      (is (equal (loop for w in (text:wrap-text (tui-transcript:format-event event) 40)
                       collect (text:pad-right w 40))
                 (tui-core:render-transcript-event
                  (tui-transcript:event-kind event) proto event nil 40))
          "theme nil renders the plain format-event path for kind ~A"
          (tui-transcript:event-kind event)))))

(test message-renderer-overrides-one-kind
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (let ((contrib (tui-status:message-renderer-contribution
                    :system
                    (lambda (event theme width)
                      (declare (ignore theme width))
                      (list (format nil "CUSTOM:~A" (tui-transcript:event-text event))))
                    :rendering-test))
          (sys (tui-transcript:make-transcript-event :system nil "note"))
          (usr (tui-transcript:make-transcript-event :message :user "hey")))
      (ext:install-contribution proto contrib context)
      (is (equal (list "CUSTOM:note")
                 (tui-core:render-transcript-event :system proto sys nil 20)))
      (is (equal (styled-bg-block (tui-style:active-theme proto)
                                  "userMessageBg" "userMessageText" "hey" 20)
                 (tui-transcript:render-event proto usr 20))
          "non-overridden kinds still hit the default styled renderer")
      (ext:retract-contribution proto contrib context))))

(test message-renderer-retract-is-clean
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (let ((contrib (tui-status:message-renderer-contribution
                    :system
                    (lambda (event theme width)
                      (declare (ignore event theme width))
                      (list "x"))
                    :rendering-test)))
      (is (null (rendering-find-system-renderer)))
      (ext:install-contribution proto contrib context)
      (is (not (null (rendering-find-system-renderer))))
      (ext:retract-contribution proto contrib context)
      (is (null (rendering-find-system-renderer))
          "retract removes the method (refcount back to 0)"))))

(test (message-renderer-declarative-clause :fixture interactive-authority)
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (let ((handle (install-extension
                   context *rendering-system-renderer-extension-manifest*))
          (sys (tui-transcript:make-transcript-event :system nil "z")))
      (is (equal (list "D:z")
                 (tui-core:render-transcript-event :system proto sys nil 20)))
      (ext:deactivate-extension proto handle context)
      (is (null (rendering-find-system-renderer))))))

(test status-slot-set-get-clear
  (let ((proto (make-tui-rendering-fixture)))
    (tui-status:register-status-slot proto :branch)
    (tui-status:register-status-slot proto :model)
    (tui-status:set-status proto :branch "main")
    (tui-status:set-status proto :model "gpt")
    (is (string= "main" (tui-status:status-text proto :branch)))
    (is (equal '(:branch :model) (tui-status:list-status-slots proto)))
    (let ((line (first (tui-status:render-status-line proto nil 20))))
      (is (= 20 (length line)))
      (is (string= "main | gpt" (string-right-trim " " line))))
    (tui-status:unregister-status-slot proto :branch)
    (is (equal '(:model) (tui-status:list-status-slots proto)))
    (is (null (tui-status:status-text proto :branch)))))

(test status-line-empty-renders-nothing
  (let ((proto (make-tui-rendering-fixture)))
    (is (null (tui-status:render-status-line proto nil 20)))
    (tui-status:register-status-slot proto :branch)
    (is (null (tui-status:render-status-line proto nil 20))
        "a registered but blank slot renders nothing")
    (tui-status:set-status proto :branch "main")
    (is (not (null (tui-status:render-status-line proto nil 20))))))

(test status-slot-contribution-roundtrip
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (let ((contrib (tui-status:make-status-slot-contribution
                    :name :branch :slot-id :branch :initial ""
                    :source :rendering-test)))
      (ext:install-contribution proto contrib context)
      (is (string= "" (tui-status:status-text proto :branch)))
      (tui-status:set-status proto :branch "main")
      (is (string= "main"
                   (string-right-trim " " (first (tui-status:render-footer proto nil 20)))))
      (ext:retract-contribution proto contrib context)
      (is (null (tui-status:status-text proto :branch)))
      (is (not (member contrib (ext:protocol-installed-contributions proto)))
          "installed-contributions clean after retract"))))

(test widget-registry-roundtrip
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (tui-status:register-widget proto :clock
                                (lambda (p theme width)
                                  (declare (ignore p theme width))
                                  (list "tick")))
    (is (equal (list "tick") (tui-status:render-widgets proto nil 20)))
    (tui-status:unregister-widget proto :clock)
    (is (null (tui-status:render-widgets proto nil 20)))
    (let ((contrib (tui-status:make-widget-contribution
                    :name :clock :widget-id :clock
                    :widget (lambda (p theme width)
                              (declare (ignore p theme width))
                              (list "tock"))
                    :source :rendering-test)))
      (ext:install-contribution proto contrib context)
      (is (equal (list "tock") (tui-status:render-widgets proto nil 20)))
      (ext:retract-contribution proto contrib context)
      (is (null (tui-status:render-widgets proto nil 20))))))

(test widget-placement-routes-above-input-out-of-the-footer
  "A widget registered with :above-input placement renders only through the
   placement-filtered render -- the footer keeps default-placed widgets and
   never shows above-input ones."
  (let ((proto (make-tui-rendering-fixture)))
    (tui-status:register-widget proto :clock
                                (lambda (p theme width)
                                  (declare (ignore p theme width))
                                  (list "tick")))
    (tui-status:register-widget proto :spin
                                (lambda (p theme width)
                                  (declare (ignore p theme width))
                                  (list "spin"))
                                :placement :above-input)
    (is (equal (list "tick") (tui-status:render-widgets proto nil 20))
        "the default render keeps footer placement")
    (is (equal (list "spin")
               (tui-status:render-widgets proto nil 20 :placement :above-input))
        "the placement-filtered render shows only above-input widgets")
    (is (equal (list "tick") (tui-status:render-footer proto nil 20))
        "the footer block excludes above-input widgets")))

(defun widget-render-error-log-lines ()
  (let ((path (ext:fault-log-path :widget-render)))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line collect line)))))

(test render-widgets-isolates-a-failing-widget-and-records-id
  "A status widget that errors (or returns a non-list) must not crash the render
loop: healthy widgets still render and the failure is logged with the widget id.
Guards the fatal-exit path a hot-patched widget tripped -- a funcall NIL in the
render path took the whole image down."
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((ext:*extension-fault-policy* nil)
           (proto (make-tui-rendering-fixture)))
      (tui-status:register-widget proto :boom
                                  (lambda (p theme width)
                                    (declare (ignore p theme width))
                                    (error "widget boom")))
      (tui-status:register-widget proto :good
                                  (lambda (p theme width)
                                    (declare (ignore p theme width))
                                    (list "still here")))
      (tui-status:register-widget proto :not-a-list
                                  (lambda (p theme width)
                                    (declare (ignore p theme width))
                                    "oops-not-a-list"))
      (is (equal (list "still here")
                 (tui-status:render-widgets proto nil 20)))
      (is (equal (list "still here")
                 (tui-status:render-footer proto nil 20)))
      (let ((lines (widget-render-error-log-lines)))
        (is (not (null lines))
            "the failing widgets must be logged to the fault sink")
        (is (some (lambda (l) (search "id=:BOOM" l)) lines)
            "the erroring widget's id must be recorded")
        (is (some (lambda (l) (search "id=:NOT-A-LIST" l)) lines)
            "the non-list-returning widget's id must be recorded")))))

(test (ui-extension-install-assert-retract :fixture interactive-authority)
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (let ((handle (install-extension
                   context *rendering-combo-ui-extension-manifest*)))
      (is (equal (list "C:hi")
                 (tui-core:render-transcript-event
                  :system proto
                  (tui-transcript:make-transcript-event :system nil "hi")
                  nil 10)))
      (is (string= "" (tui-status:status-text proto :branch)))
      (is (not (null (tui-style:find-theme proto "combo-theme"))))
      (ext:deactivate-extension proto handle context)
      (is (null (rendering-find-system-renderer)))
      (is (null (tui-status:status-text proto :branch)))
      (is (null (tui-style:find-theme proto "combo-theme"))))))

(test (ui-builder-verbs-contribute :fixture interactive-authority)
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (let* ((manifest (ext:kli-extension (b :rendering-builder-ui)
                       (tui-status:message-renderer b :system
                         (lambda (event theme width)
                           (declare (ignore theme width))
                           (list (format nil "B:~A" (tui-transcript:event-text event)))))
                       (tui-status:status-slot b :branch :initial "x")
                       (tui-status:widget b :clock
                         (lambda (p theme width)
                           (declare (ignore p theme width))
                           (list "tick")))))
           (handle (with-interactive-authority
                     (ext:install-manifest manifest proto context)))
           (sys (tui-transcript:make-transcript-event :system nil "hi")))
      (is (equal (list "B:hi")
                 (tui-core:render-transcript-event :system proto sys nil 10)))
      (is (string= "x" (tui-status:status-text proto :branch)))
      (is (equal (list "tick") (tui-status:render-widgets proto nil 10)))
      (ext:deactivate-extension proto handle context)
      (is (null (rendering-find-system-renderer)))
      (is (null (tui-status:status-text proto :branch)))
      (is (null (tui-status:render-widgets proto nil 10))))))

(test styled-user-bg-block
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (ev (tui-transcript:make-transcript-event :message :user "hi there"))
         (lines (tui-core:render-transcript-event :message proto ev theme 24)))
    (is (= 1 (length lines)) "a single hugged bg line, no vertical halo")
    (is (equal (styled-bg-block theme "userMessageBg" "userMessageText" "hi there" 24)
               lines))
    (is (search (format nil "~C[48;2;52;53;65m" #\Esc) (first lines))
        "userMessageBg background is applied")))

(test styled-assistant-markdown
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (ev (tui-transcript:make-transcript-event :message :assistant "hello"))
         (lines (tui-core:render-transcript-event :message proto ev theme 24)))
    (is (equal (tui-markdown:markdown->lines "hello" theme 24) lines)
        "assistant replies render through the markdown renderer")
    (is (equal (list "hello") lines)
        "plain prose stays plain (no leading pad, no ANSI)")))

(test styled-tool-status-bg
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json")))
    (is (equal (styled-tool-lines theme "toolPendingBg" "grep" "x" 24)
               (tui-core:render-transcript-event
                :tool-call proto
                (tui-transcript:make-transcript-event :tool-call nil "x" :name "grep")
                theme 24))
        "a tool-call tints with toolPendingBg")
    (is (equal (styled-tool-lines theme "toolSuccessBg" "grep" "found" 24)
               (tui-core:render-transcript-event
                :tool-result proto
                (tui-transcript:make-transcript-event
                 :tool-result nil "found" :name "grep" :status :ok)
                theme 24))
        "an :ok result tints with toolSuccessBg")
    (is (equal (styled-tool-lines theme "toolErrorBg" "grep" "nope" 24)
               (tui-core:render-transcript-event
                :tool-result proto
                (tui-transcript:make-transcript-event
                 :tool-result nil "nope" :name "grep" :status :error)
                theme 24))
        "an :error result tints with toolErrorBg")))

(test styled-tool-box-ansi-output-continuous-background
  "Tool output carrying its own SGR (an embedded full reset, a coloured run) or a
raw tab renders through the seg pipeline under one continuous background. Mirrors
the three live-reproduced gap modes: (A) an interior ESC[0m/ESC[49m no longer
survives to punch the fill -- the sole bg reset is the trailing one; (B) the
tool's foreground colour is preserved and width is counted on visible text, so
the row is padded to exactly WIDTH; (C) a raw tab is normalised out."
  (let* ((tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (esc (code-char 27))
         (reset-all (format nil "~C[0m" esc))
         (bg-reset (format nil "~C[49m" esc))
         (fg-reset (format nil "~C[39m" esc)))
    (labels ((box (text) (tui-transcript::render-tool-box "bash" text :ok theme 24))
             (row-width (line) (text:visible-width (rendering-visible-line line)))
             (trailing-bg-only-p (line)
               (let ((p (search bg-reset line)))
                 (and p (= p (- (length line) (length bg-reset)))))))
      (let ((line (first (box (format nil "red ~C[0m tail" esc)))))
        (is (not (search reset-all line)) "no ESC[0m survives from the tool output")
        (is (trailing-bg-only-p line)
            "the sole ESC[49m is the trailing token -- no interior reset punches the fill")
        (is (= 24 (row-width line)) "the row fills exactly WIDTH visible columns"))
      (let ((line (first (box (format nil "~C[31mERROR~C[0m ok" esc esc)))))
        (is (not (search reset-all line)) "no ESC[0m from the coloured run")
        (is (trailing-bg-only-p line) "no interior bg reset in the coloured row")
        (is (search (format nil "~C[38;2;128;0;0m" esc) line)
            "the tool's red is preserved as a seg foreground")
        (is (search fg-reset line) "the coloured run closes with a fg-only reset")
        (is (= 24 (row-width line))
            "width is counted on visible text only, so the row is not under-padded"))
      (let ((line (first (box (format nil "a~Cb" #\Tab)))))
        (is (not (find #\Tab line)) "the raw tab is normalised out")
        (is (= 24 (row-width line)) "the tab-normalised row still fills WIDTH")))))

(test styled-tool-result-render-ignores-non-file-details
  "Details without file content leave the render untouched, so bash and eval results are byte-identical with and without their details. An integer :files (a count, not an update list) must not crash the file-update dispatch."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (plain (tui-transcript:make-transcript-event
                 :tool-result nil "found" :name "grep" :status :ok))
         (expected (tui-core:render-transcript-event
                    :tool-result proto plain theme 24)))
    (dolist (details '((:exit-code 0 :timed-out-p nil)
                       (:pattern "x" :path "y" :matches 2 :files 2)
                       (:files :not-a-list)))
      (is (equal expected
                 (tui-core:render-transcript-event
                  :tool-result proto
                  (tui-transcript:make-transcript-event
                   :tool-result nil "found" :name "grep" :status :ok
                   :details details)
                  theme 24))
          "details ~S must render as a plain box" details))))

(defun file-update-heading-line (theme name tail)
  (concatenate 'string " "
               (tui-style:style theme "toolTitle" name :attrs '(:bold))
               (tui-style:style theme "toolOutput" tail)))

(defun rendering-visible-line (line)
  "Strip SGR escapes, leaving only visible characters for assertions."
  (with-output-to-string (out)
    (loop with i = 0 with n = (length line)
          while (< i n)
          do (if (char= (char line i) #\Esc)
                 (setf i (1+ (or (position #\m line :start i) (1- n))))
                 (progn (write-char (char line i) out) (incf i))))))

(defun rendering-visible-lines (lines)
  (mapcar #'rendering-visible-line lines))

(defun test-diff-update (path old new &key added removed old-known-p)
  (tools-filesystem:file-diff-presentation-update
   path old new :added added :removed removed :old-known-p old-known-p))

(defun test-diff-presentation (&rest updates)
  (ext:result-diff :updates updates))

(test styled-tool-result-diff-ignores-public-file-details
  "Public file details are model-facing metadata. A :diff term without private
updates falls back instead of rendering any public old/new bodies."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (new-content (format nil "alpha~%beta~%gamma"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil "Edited /tmp/x (+1 -0)" :name "edit" :status :ok
                 :presentation (ext:result-diff)
                 :details (list :files (list (list :path "/tmp/x" :new new-content)))))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 80))
         (visible (mapcar (lambda (line)
                            (string-right-trim " " line))
                          (rendering-visible-lines lines))))
    (is (equal '(" edit Edited /tmp/x (+1 -0)") visible))
    (is (notany (lambda (line) (search "+ alpha" line)) visible))
    (is (notany (lambda (line) (search "+ beta" line)) visible))))

(test styled-tool-result-file-update-renders-diff-card
  "Private presentation updates render one diff card per file: a toolTitle
heading with path and counts, then bounded remove/add rows."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil "Edited /tmp/x (+1 -1)" :name "edit" :status :ok
                 :presentation
                 (test-diff-presentation
                  (test-diff-update "/tmp/x" "alpha beta" "alpha gamma"))))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (equal '(" edit /tmp/x (+1 -1)"
                 "  1     - alpha beta"
                 "      1 + alpha gamma")
               visible))
    (is (search (tui-style:fg-truecolor
                 (tui-style:theme-token theme "toolDiffAdded"))
                (third lines)))))

(test styled-tool-result-multi-file-update-renders-card-per-file
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil "Edited a, b" :name "edit" :status :ok
                 :presentation
                 (test-diff-presentation
                  (test-diff-update "a" "x" "y")
                  (test-diff-update "b" "p" "q"))))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (equal '(" edit a (+1 -1)"
                 "  1     - x"
                 "      1 + y"
                 " edit b (+1 -1)"
                 "  1     - p"
                 "      1 + q")
               visible))))

(test styled-tool-result-multi-file-update-unifies-gutter-across-differing-lengths
  "Cards for files of different lengths share one gutter width, so content lines
up across cards instead of jumping by each file's own digit count. The one-line
file renders behind the same three-column gutter as the twenty-line file."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (short-old "x")
         (short-new "y")
         (long-old (format nil "~{l~2,'0D~^~%~}" (loop for i from 1 to 20 collect i)))
         (long-new (format nil "~{~A~^~%~}"
                           (loop for i from 1 to 20
                                 collect (if (= i 1) "L01" (format nil "l~2,'0D" i)))))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil "Edited short, long" :name "edit" :status :ok
                 :presentation
                 (test-diff-presentation
                  (test-diff-update "short" short-old short-new)
                  (test-diff-update "long" long-old long-new))))
         (tui-transcript:*tool-output-expanded* t)
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (member "  1     - x" visible :test #'string=)
        "the one-line file still renders behind the shared three-column gutter")
    (is (member "  1     - l01" visible :test #'string=)
        "the twenty-line file uses the same gutter, so content aligns across cards")))

(test styled-tool-result-write-new-file-renders-all-added
  "Write details with a nil old render as an all-added diff with no phantom removed line for the empty old content."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (new-content (format nil "hello~%world"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil "Wrote 11 characters to /tmp/new.txt."
                 :name "write" :status :ok
                 :presentation
                 (test-diff-presentation
                  (test-diff-update "/tmp/new.txt" nil new-content
                                    :old-known-p t))))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (equal '(" write /tmp/new.txt (+2 -0)"
                 "      1 + hello"
                 "      2 + world")
               visible))
    (is (= 3 (length lines)) "heading plus one added line per content line")
    (is (every (lambda (l) (search "+ " l)) (rest lines)))
    (is (notany (lambda (l) (search "- " l)) (rest lines))
        "no phantom removed line from the empty old content")))

(defun file-update-fixture-event (old new)
  (tui-transcript:make-transcript-event
   :tool-result nil "Edited /tmp/x (+1 -1)" :name "edit" :status :ok
   :presentation (test-diff-presentation
                  (test-diff-update "/tmp/x" old new
                                    :added 1 :removed 1))))

(test styled-file-update-collapsed-shows-every-hunk
  "Collapsed file-update cards show each separated changed hunk with three context
lines before and after under a numeric old/new gutter, instead of only the first
changed window."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (numbers (loop for i from 1 to 20 collect i))
         (old (format nil "~{l~2,'0D~^~%~}" numbers))
         (new (format nil "~{~A~^~%~}"
                      (loop for i in numbers
                            collect (case i
                                      (5 "B05")
                                      (15 "B15")
                                      (t (format nil "l~2,'0D" i))))))
         (event (file-update-fixture-event old new))
         (tui-transcript:*tool-output-expanded* nil)
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (equal (file-update-heading-line theme "edit" " /tmp/x (+1 -1)")
               (first lines)))
    (is (equal '(" edit /tmp/x (+1 -1)"
                 "  2   2   l02" "  3   3   l03" "  4   4   l04"
                 "  5     - l05" "      5 + B05"
                 "  6   6   l06" "  7   7   l07" "  8   8   l08"
                 " …"
                 " 12  12   l12" " 13  13   l13" " 14  14   l14"
                 " 15     - l15" "     15 + B15"
                 " 16  16   l16" " 17  17   l17" " 18  18   l18")
               visible))
    (is (some (lambda (line) (string= " …" line)) visible)
        "separated hunks get a muted separator")
    (is (notany (lambda (line) (search "l01" line)) visible)
        "only hunk-local context is shown")))

(test styled-file-update-collapsed-merges-adjacent-hunks
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (old (format nil "a~%b~%c~%d~%e"))
         (new (format nil "a~%B~%C~%d~%e"))
         (event (file-update-fixture-event old new))
         (tui-transcript:*tool-output-expanded* nil)
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (equal '(" edit /tmp/x (+1 -1)"
                 "  1   1   a" "  2     - b" "  3     - c" "      2 + B" "      3 + C"
                 "  4   4   d" "  5   5   e")
               visible))
    (is (notany (lambda (line) (string= " …" line)) visible)
        "adjacent hunks merge into one preview block")))

(test styled-file-update-collapsed-cap-shows-hidden-hunks
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (old (format nil "~{~A~^~%~}"
                      (loop for i from 1 to 12 collect (format nil "l~2,'0D" i))))
         (new (format nil "~{~A~^~%~}"
                      (loop for i from 1 to 12
                            collect (if (member i '(2 6 10))
                                        (format nil "B~2,'0D" i)
                                        (format nil "l~2,'0D" i)))))
         (tools-filesystem::*file-diff-presentation-line-cap* 5)
         (event (file-update-fixture-event old new))
         (tui-transcript:*tool-output-expanded* nil)
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (equal '(" edit /tmp/x (+1 -1)"
                 "  1   1   l01" "  2     - l02" "      2 + B02" "  3   3   l03" "  4   4   l04"
                 " … (+3 changed hunks omitted; re-read for full context)")
               (subseq visible 0 7)))
    (is (notany (lambda (line) (search "B06" line)) visible))
    (is (some (lambda (line) (search "re-read" line)) visible))))

(test styled-file-update-expanded-keeps-bounded-presentation
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (old (format nil "~{~A~^~%~}"
                      (loop for i from 1 to 12 collect (format nil "l~2,'0D" i))))
         (new (format nil "~{~A~^~%~}"
                      (loop for i from 1 to 12
                            collect (if (member i '(2 6 10))
                                        (format nil "B~2,'0D" i)
                                        (format nil "l~2,'0D" i)))))
         (tools-filesystem::*file-diff-presentation-line-cap* 5)
         (event (file-update-fixture-event old new))
         (tui-transcript:*tool-output-expanded* t)
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (notany (lambda (line) (search "B06" line)) visible)
        "expanded mode does not recover a hidden full diff from session data")
    (is (some (lambda (line) (search "re-read" line)) visible))))

(test styled-session-replay-file-update-uses-private-presentation
  "Session replay keeps model-facing details compact while the TUI renders the
file diff from the private presentation term stored alongside the result."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (details '(:files ((:path "/tmp/x" :added 1 :removed 1
                             :changed-ranges ((:start 1 :end 1))
                             :old-sha256 "old-hash"
                             :new-sha256 "new-hash"))))
         (presentation
           (test-diff-presentation (test-diff-update "/tmp/x" "a" "b")))
         (event (first
                 (tui-transcript:session-entry-transcript-events
                  (sess:make-message-entry
                   (sess:make-tool-result-message
                    "Edited /tmp/x (+1 -1)"
                    :tool-name "edit"
                    :metadata (list :details details
                                    :presentation presentation))))))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (eq details (tui-transcript:event-details event)))
    (is (eq presentation (tui-transcript:event-presentation event)))
    (is (not (tree-contains-key-p details :old)))
    (is (not (tree-contains-key-p details :new)))
    (is (equal '(" edit /tmp/x (+1 -1)"
                 "  1     - a"
                 "      1 + b")
               visible))))

(test (styled-system-notice-dim-dot :fixture interactive-authority)
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (ev (tui-transcript:make-transcript-event :system nil "ready"))
         (lines (tui-core:render-transcript-event :system proto ev theme 24))
         (pad (max 0 (- 24 (+ 2 (text:visible-width "ready")))))
         (expected (concatenate 'string
                     (tui-style:style theme "dim" (string #\·))
                     " " (tui-style:style theme "muted" "ready")
                     (make-string pad :initial-element #\Space))))
    (is (= 1 (length lines)) "one ambient notice line, no slab")
    (is (string= expected (first lines)))
    (is (not (search (format nil "~C[48" #\Esc) (first lines))) "no background slab")
    (is (not (search "[system]" (first lines))) "no bracket label")))

(test (styled-system-error-bar :fixture interactive-authority)
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (ev (tui-transcript:make-transcript-event :system nil "boom" :status :error))
         (lines (tui-core:render-transcript-event :system proto ev theme 24))
         (bar (tui-style:theme-token theme "error"))
         (pad (max 0 (- 24 (+ 2 (text:visible-width "boom")))))
         (expected (concatenate 'string
                     (tui-style:style-span (string #\▎) :fg bar)
                     " boom"
                     (make-string pad :initial-element #\Space))))
    (is (= 1 (length lines)))
    (is (string= expected (first lines)) "error bar, full-weight body, no slab")
    (is (not (search (format nil "~C[48" #\Esc) (first lines))) "no background slab")))

(test styled-tool-result-read-renders-one-line-summary
  "A read result renders as a single summary line from its presentation term:
the bold tool name then the muted path and line count. The model still gets the
full body, but the view never shows the file contents."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil (format nil "1:ab|alpha~%2:cd|beta")
                 :name "read" :status :ok
                 :presentation (ext:result-summary :path "/tmp/x.lisp" :lines 2)))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40)))
    (is (= 1 (length lines)) "a read summary is exactly one line, never expands")
    (is (search "read" (first lines)))
    (is (search "/tmp/x.lisp" (first lines)))
    (is (search "(2 lines)" (first lines)))
    (is (notany (lambda (l) (search "alpha" l)) lines)
        "the file body never appears in the view")))

(test styled-tool-result-find-renders-one-line-summary
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil (format nil "src/a.lisp~%src/b.lisp")
                 :name "find" :status :ok
                 :presentation (ext:result-filesystem-summary
                                :find :pattern "src/**/*.lisp" :count 2
                                :result-handle "H1" :truncated t)))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 80))
         (visible (rendering-visible-lines lines)))
    (is (equal '(" find src/**/*.lisp (2 paths) · truncated · handle H1") visible))
    (is (notany (lambda (line) (search "src/a.lisp" line)) visible)
        "the listed path body is not rendered")))

(test styled-tool-result-search-renders-one-line-summary
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil (format nil "/tmp/x.lisp~%*1:ab|needle")
                 :name "search" :status :ok
                 :presentation (ext:result-filesystem-summary
                                :search :pattern "needle" :path "src/**/*.lisp"
                                :matches 3 :file-count 2 :skipped 4 :timed-out 1
                                :result-handle "S1" :truncated t)))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 100))
         (visible (rendering-visible-lines lines)))
    (is (equal '(" search needle in src/**/*.lisp (3 matches in 2 files) · truncated · skipped 4 · timed out 1 · handle S1")
               visible))
    (is (notany (lambda (line) (search "*1:ab" line)) visible)
        "matched source lines are not rendered")))

(test filesystem-summary-presenters-preserve-result-payload
  (let* ((details (list :pattern "*.lisp" :count 2 :result-handle "H"))
         (result (ext:tool-text-result (format nil "a.lisp~%b.lisp")
                                       :details details))
         (content (ext:tool-result-content result)))
    (is (equal '(:kind :filesystem-summary :summary-kind :find
                 :pattern "*.lisp" :count 2 :result-handle "H" :truncated nil)
               (ext:find-summary-result-presenter nil result)))
    (is (eq content (ext:tool-result-content result)))
    (is (eq details (ext:tool-result-details result)))
    (is (string= (format nil "a.lisp~%b.lisp")
                 (getf (first (ext:tool-result-content result)) :text)))))

(test filesystem-tool-calls-are-hidden
  (multiple-value-bind (proto context) (make-tui-rendering-fixture)
    (install-extensions context
                        tools-filesystem:*find-tool-extension-manifest*
                        tools-filesystem:*search-tool-extension-manifest*)
    (is (eq :hidden
            (ext:presentation-kind
             (ext:present-call (ext:find-tool proto :find)
                               '(:pattern "*.lisp")))))
    (is (eq :hidden
            (ext:presentation-kind
             (ext:present-call (ext:find-tool proto :search)
                               '(:pattern "needle" :path "src/**/*.lisp")))))))

(test filesystem-summary-errors-render-as-error-box
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (event (tui-transcript:make-transcript-event
                 :tool-result nil "permission denied"
                 :name "search" :status :error
                 :presentation (ext:result-filesystem-summary
                                :search :pattern "needle" :path "/root"
                                :matches 0 :file-count 0)))
         (lines (tui-core:render-transcript-event
                 :tool-result proto event theme 40))
         (visible (rendering-visible-lines lines)))
    (is (string= " search permission denied"
                 (string-right-trim " " (first visible))))
    (is (search (format nil "~C[48" #\Esc) (first lines))
        "error result uses the boxed error renderer, not the quiet summary")))

(test styled-command-response-uses-reply-marker
  "Direct command output renders as a :response: a › reply marker and muted body,
no ambient · and no /cmd: prefix, so it reads as the command's answer rather than
background activity. An error response keeps the ▎ bar via status."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (resp (tui-transcript:make-transcript-event :response nil "the answer"))
         (lines (tui-core:render-transcript-event :response proto resp theme 24))
         (err (tui-transcript:make-transcript-event
               :response nil "boom" :status :error))
         (err-lines (tui-core:render-transcript-event :response proto err theme 24)))
    (is (= 1 (length lines)))
    (is (search "›" (first lines)) "the response carries the reply marker")
    (is (not (search (string #\·) (first lines))) "no ambient dot")
    (is (string= "› the answer" (tui-transcript:format-event resp)))
    (is (search "▎" (first err-lines)) "an error response keeps the error bar")))

(test styled-tool-call-omitted-arguments-expand-with-ctrl-o
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (ev (tui-transcript:make-transcript-event
              :tool-call nil "" :name "edit"
              :presentation '(:kind :header :text "" :preview "input:\n@@ x\n~new"))))
    (let ((collapsed (tui-core:render-transcript-event :tool-call proto ev theme 40)))
      (is (some (lambda (line) (search "arguments" line)) collapsed)
          "collapsed tool call advertises omitted arguments")
      (is (notany (lambda (line) (search "@@ x" line)) collapsed)
          "collapsed tool call does not inline the patch preview"))
    (let* ((tui-transcript:*tool-output-expanded* t)
           (expanded (tui-core:render-transcript-event :tool-call proto ev theme 40)))
      (is (some (lambda (line) (search "input:" line)) expanded))
      (is (some (lambda (line) (search "@@ x" line)) expanded)))))

(test styled-bash-green-rules
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (green (tui-style:theme-token theme "bashMode"))
         (ev (tui-transcript:make-transcript-event
              :tool-call nil "ls -la" :name "bash"
              :presentation (ext:call-command "ls -la")))
         (lines (tui-core:render-transcript-event :tool-call proto ev theme 24))
         (rule (tui-style:style-span (make-string 24 :initial-element #\─) :fg green)))
    (is (= 3 (length lines)))
    (is (equal rule (first lines)))
    (is (equal rule (third lines)))
    (is (equal (tui-style:style-span (text:pad-right "$ ls -la" 24) :fg green :attrs '(:bold))
               (second lines)))
    (is (search (format nil "~C[38;2;181;189;104m" #\Esc) (first lines))
        "rules render in bashMode green")))

(test styled-bash-wrapped-command-prompts-once
  "A single command that wraps must not read as two commands. Only the first line carries the \"$ \" prompt, and continuations align under it with two spaces."
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json"))
         (green (tui-style:theme-token theme "bashMode"))
         (ev (tui-transcript:make-transcript-event
              :tool-call nil "aaaa bbbb cccc dddd eeee ffff" :name "bash"
              :presentation (ext:call-command "aaaa bbbb cccc dddd eeee ffff")))
         (lines (tui-core:render-transcript-event :tool-call proto ev theme 16))
         (body (butlast (rest lines))))
    (is (> (length body) 1) "the command wraps to more than one line")
    (flet ((plain (l)
             (with-output-to-string (s)
               (loop with i = 0 with n = (length l)
                     while (< i n)
                     do (if (char= (char l i) #\Esc)
                            (setf i (1+ (or (position #\m l :start i) (1- n))))
                            (progn (write-char (char l i) s) (incf i)))))))
      (is (text:string-prefix-p "$ " (plain (first body)))
          "first wrapped line keeps the $ prompt")
      (is (every (lambda (l) (not (text:string-prefix-p "$ " (plain l))))
                 (rest body))
          "continuation lines do not repeat the $ prompt"))))

(test styled-thinking-ramp
  (let* ((proto (make-tui-rendering-fixture))
         (tui-style:*color-mode* :truecolor)
         (theme (builtin-theme "dark.json")))
    (dolist (pair '((nil "thinkingOff")
                    ("minimal" "thinkingMinimal")
                    ("low" "thinkingLow")
                    ("medium" "thinkingMedium")
                    ("high" "thinkingHigh")
                    ("xhigh" "thinkingXhigh")))
      (destructuring-bind (status token) pair
        (let* ((ev (tui-transcript:make-transcript-event :thinking nil "musing" :status status))
               (lines (tui-core:render-transcript-event :thinking proto ev theme 24))
               (ramp (tui-style:theme-token theme token))
               (expected (concatenate 'string
                           (tui-style:style-span (string #\│) :fg ramp) " "
                           (tui-style:style theme "thinkingText" "musing" :attrs '(:italic))
                           (make-string (- 24 (+ 2 (text:visible-width "musing")))
                                        :initial-element #\Space))))
          (is (= 1 (length lines)))
          (is (equal expected (first lines))
              "thinking status ~A maps to ~A in italic dim" status token))))))
