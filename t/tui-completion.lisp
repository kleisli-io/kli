(in-package #:kli/tests)

(defun register-completion-fixture-commands (context protocol)
  (let ((provider (ext:require-capability-provider protocol
                                                   :commands
                                                   :contract :commands/v1)))
    (flet ((add (name description &optional arguments)
             (ext:provider-call provider :register-command context name
                                (commands:make-command
                                 :name name
                                 :description description
                                 :arguments arguments))))
      (add "commands" "List registered commands.")
      (add "compact" "Compact the session log.")
      (add "clear-other" "Clear the other thing.")
      (add "echo" "Echo arguments back." '(:tail :words))
      (add "review" "Review a file." "<path> [notes...]")
      (add "sync" "Synchronize the working tree with the newest upstream revision and rebuild artifacts.")
      (add "skill:pdf-tools" "Work with PDF files.")
      (add "skill:debugging" "Debug systematically."))
    (ext:provider-call provider :register-command context "deploy"
                       (commands:make-command
                        :name "deploy"
                        :description "Deploy a target."
                        :completer
                        (lambda (command tail)
                          (declare (ignore command))
                          (if (find #\Space tail)
                              (when (text:string-prefix-p "staging " tail)
                                '(:hint "<version>"))
                              '(:candidates (("staging" . "the staging cluster")
                                             "production")
                                :hint "<target>")))))))

(defun make-tui-completion-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tui-views:*tui-views-extension-manifest*
                        tui-input:*tui-input-extension-manifest*
                        tui-editor:*tui-editor-extension-manifest*
                        tui-completion:*tui-completion-extension-manifest*)
    (register-completion-fixture-commands context protocol)
    (values protocol context)))

(defun feed-editor-keys (editor keys)
  (dolist (key keys editor)
    (tui-core:handle-input editor key)))

(defun popup-matches (editor)
  (let ((popup (tui-editor:editor-completion editor)))
    (and popup
         (mapcar #'tui-editor:completion-candidate-match
                 (tui-editor:completion-popup-candidates popup)))))

(defvar *completion-tree-counter* 0)

(defun make-completion-file-tree ()
  (let ((root (uiop:ensure-directory-pathname
               (format nil "/tmp/kli-completion-tree-~D/"
                       (incf *completion-tree-counter*)))))
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist (merge-pathnames "src/" root))
    (ensure-directories-exist (merge-pathnames "build/" root))
    (dolist (file '("src/main.lisp" "src/util.lisp" "README.md"
                    ".hidden" "build/out.txt"))
      (with-open-file (stream (merge-pathnames file root)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string "content" stream)))
    (with-open-file (stream (merge-pathnames ".gitignore" root)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-line "build/" stream))
    root))

(test completion-trigger-rules
  (is (equal '(:command "co" 0 3) (tui-editor:completion-trigger "/co" 3)))
  (is (equal '(:command "co" 0 5) (tui-editor:completion-trigger "/comm" 3)))
  (is (equal '(:skill "d" 4 6) (tui-editor:completion-trigger "sum $d" 6)))
  (is (equal '(:file "ma" 4 7) (tui-editor:completion-trigger "see @ma" 7)))
  (is (equal '(:file "f" 0 2) (tui-editor:completion-trigger "@f" 2)))
  (is (equal '(:argument ("bash" . "ls") 6 8)
             (tui-editor:completion-trigger "/bash ls" 8)))
  (is (equal '(:argument ("auth" . "") 6 6)
             (tui-editor:completion-trigger "/auth " 6)))
  (is (null (tui-editor:completion-trigger "/co" 0)))
  (is (null (tui-editor:completion-trigger "US$pdf" 6)))
  (is (null (tui-editor:completion-trigger "a$$d" 4)))
  (is (null (tui-editor:completion-trigger "@@f" 3)))
  (is (null (tui-editor:completion-trigger "plain text" 10))))

(test completion-trigger-keeps-qualified-command-token
  (is (equal '(:command "cairn:" 0 7)
             (tui-editor:completion-trigger "/cairn:" 7))))

(test completion-fuzzy-scoring
  (is (= 0 (tui-editor:fuzzy-score "" "anything")))
  (is (null (tui-editor:fuzzy-score "xz" "echo")))
  (is (> (tui-editor:fuzzy-score "co" "compact")
         (tui-editor:fuzzy-score "co" "commands")))
  (is (> (tui-editor:fuzzy-score "co" "commands")
         (tui-editor:fuzzy-score "co" "clear-other")))
  (is (> (tui-editor:fuzzy-score "co" "clear-other")
         (tui-editor:fuzzy-score "co" "echo"))))

(test completion-slash-popup-ranks-and-tab-accepts
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "c" "o"))
    (is (equal '("compact" "commands" "clear-other" "echo")
               (popup-matches editor)))
    (tui-core:handle-input editor "tab")
    (is (string= "/compact " (tui-editor:editor-value editor)))
    (is (= 9 (tui-editor:editor-cursor editor)))
    (is (null (tui-editor:editor-completion editor)))))

(test completion-enter-accepts-without-submitting
  (let* ((protocol (make-tui-completion-fixture))
         (submitted nil)
         (editor (tui-editor:make-editor
                  :protocol protocol
                  :on-submit (lambda (value) (setf submitted value)))))
    (feed-editor-keys editor '("/" "c" "o"))
    (is (not (null (tui-editor:editor-completion editor))))
    (tui-core:handle-input editor "enter")
    (is (string= "/compact " (tui-editor:editor-value editor)))
    (is (null submitted))
    (tui-core:handle-input editor "enter")
    (is (string= "/compact " submitted))))

(test completion-skill-sigil-replaces-anchor-token
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("s" "u" "m" " " "$" "d"))
    (is (equal '("debugging" "pdf-tools") (popup-matches editor)))
    (tui-core:handle-input editor "down")
    (is (= 1 (tui-editor:completion-popup-selected
              (tui-editor:editor-completion editor))))
    (tui-core:handle-input editor "tab")
    (is (string= "sum $pdf-tools " (tui-editor:editor-value editor)))
    (is (= 15 (tui-editor:editor-cursor editor)))))

(test completion-arrow-selection-wraps
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("$" "d"))
    (tui-core:handle-input editor "up")
    (is (= 1 (tui-editor:completion-popup-selected
              (tui-editor:editor-completion editor))))
    (tui-core:handle-input editor "down")
    (is (= 0 (tui-editor:completion-popup-selected
              (tui-editor:editor-completion editor))))))

(test completion-hint-renders-argument-specs
  (let ((protocol (make-tui-completion-fixture))
        (tui-style:*color-mode* :none))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (feed-editor-keys editor '("/" "r" "e"))
      (is (equal '("> /re"
                   "  /review <path> [notes...]  Review a file."
                   "  /clear-other               Clear the other thing.")
                 (tui-core:render-lines editor 60))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (feed-editor-keys editor '("/" "e" "c"))
      (is (equal '("> /ec" "  /echo <words>  Echo arguments back.")
                 (tui-core:render-lines editor 60))))))

(test completion-argument-popup-offers-completer-candidates
  (let* ((protocol (make-tui-completion-fixture))
         (tui-style:*color-mode* :none)
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "d" "e" "p" "l" "o" "y" " "))
    (is (equal '("staging" "production") (popup-matches editor)))
    (is (equal '("> /deploy "
                 "  staging     the staging cluster"
                 "  production")
               (tui-core:render-lines editor 60)))
    (feed-editor-keys editor '("s"))
    (is (equal '("staging") (popup-matches editor)))
    (tui-core:handle-input editor "tab")
    (is (string= "/deploy staging " (tui-editor:editor-value editor)))
    (is (null (tui-editor:editor-completion editor)))
    (is (equal '("> /deploy staging " "  <version>")
               (tui-core:render-lines editor 60)))))

(test completion-command-accept-opens-argument-menu
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "d" "e" "p"))
    (tui-core:handle-input editor "tab")
    (is (string= "/deploy " (tui-editor:editor-value editor)))
    (is (equal '("staging" "production") (popup-matches editor))
        "accepting the command name opens its argument menu at once")))

(test completion-argument-hint-suppressed-when-menu-exists
  (let* ((protocol (make-tui-completion-fixture))
         (tui-style:*color-mode* :none)
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "d" "e" "p" "l" "o" "y" " "))
    (tui-editor::dismiss-editor-completion editor)
    (is (equal '("> /deploy ") (tui-core:render-lines editor 60))
        "no hint line for a command with a menu, even with the popup dismissed")))

(test completion-qualified-shadowed-commands-are-distinct
  (let* ((protocol (make-tui-completion-fixture))
         (provider (ext:require-capability-provider protocol
                                                    :commands
                                                    :contract :commands/v1)))
    (flet ((register (source tier description)
             (ext:provider-call provider :register-command nil "resume"
                                (commands:make-command
                                 :name "resume"
                                 :description description
                                 :arguments '(:tail :task)
                                 :runner #'run-test-command)
                                :source source :tier tier)))
      (register :session-commands :extension "Resume the current session.")
      (register :cairn :extension "Resume a cairn task.")
      (ext:provider-call provider :register-command nil "resume"
                         (commands:clone-command
                          (ext:provider-call provider :find-command
                                             "session-commands:resume")
                          :name "resume")
                         :source :user :tier :alias)
      (let* ((candidates (remove-if-not
                          (lambda (candidate)
                            (member (tui-editor:completion-candidate-match candidate)
                                    '("resume" "user:resume" "session-commands:resume"
                                      "cairn:resume")
                                    :test #'string=))
                          (tui-completion:command-candidates protocol)))
             (matches (mapcar #'tui-editor:completion-candidate-match candidates)))
        (is (equal '("resume" "cairn:resume") matches))
        (is (equal '("Resume the current session." "Resume a cairn task.")
                   (mapcar #'tui-editor:completion-candidate-description
                           candidates)))))))

(test completion-offers-qualified-shadow-when-winner-registered-first
  "When the precedence winner is registered before a losing source (native :core
resume ahead of cairn :extension resume), completion still lists the bare winner
and offers the loser as a qualified shadow. Regression: LIST-COMMANDS once
returned the push-order head (the loser), so the bare entry carried the loser and
SAME-COMPLETION-COMMAND-P dropped cairn:resume from the menu entirely."
  (let* ((protocol (make-tui-completion-fixture))
         (provider (ext:require-capability-provider protocol
                                                    :commands
                                                    :contract :commands/v1)))
    (flet ((register (source tier description)
             (ext:provider-call provider :register-command nil "resume"
                                (commands:make-command
                                 :name "resume"
                                 :description description
                                 :arguments '(:tail :task)
                                 :runner #'run-test-command)
                                :source source :tier tier)))
      (register :session-commands :core "Resume the current session.")
      (register :cairn :extension "Resume a cairn task."))
    (let* ((candidates (remove-if-not
                        (lambda (candidate)
                          (member (tui-editor:completion-candidate-match candidate)
                                  '("resume" "session-commands:resume" "cairn:resume")
                                  :test #'string=))
                        (tui-completion:command-candidates protocol)))
           (matches (mapcar #'tui-editor:completion-candidate-match candidates)))
      (is (equal '("resume" "cairn:resume") matches)
          "the bare winner plus the losing source as a qualified shadow")
      (is (equal '("Resume the current session." "Resume a cairn task.")
                 (mapcar #'tui-editor:completion-candidate-description
                         candidates))
          "the bare entry shows the winner's description, not the push-order head's"))))

(test completion-popup-wraps-long-descriptions
  (let* ((protocol (make-tui-completion-fixture))
         (tui-style:*color-mode* :none)
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "s" "y"))
    (is (equal '("> /sy"
                 "  /sync  Synchronize the working tree"
                 "         with the newest upstream"
                 "         revision and rebuild artifacts.")
               (tui-core:render-lines editor 40)))))

(test completion-argument-hint-line-renders-spec-while-tail-blank
  (let* ((protocol (make-tui-completion-fixture))
         (tui-style:*color-mode* :none)
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "e" "c" "h" "o" " "))
    (is (null (tui-editor:editor-completion editor)))
    (is (equal '("> /echo " "  <words>")
               (tui-core:render-lines editor 60)))
    (feed-editor-keys editor '("h" "i"))
    (is (equal '("> /echo hi")
               (tui-core:render-lines editor 60)))))

(test completion-selected-row-renders-inverse
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (tui-style:*color-mode* :truecolor))
    (feed-editor-keys editor '("/" "c" "o"))
    (let ((lines (tui-core:render-lines editor 60))
          (inverse (format nil "~C[7m" #\Esc)))
      (is (text:string-prefix-p inverse (second lines)))
      (is (not (text:string-prefix-p inverse (third lines)))))))

(test completion-degrades-without-provider
  (multiple-value-bind (protocol context) (make-tui-editor-fixture)
    (declare (ignore context))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (feed-editor-keys editor '("/" "r" "e"))
      (is (null (tui-editor:editor-completion editor)))
      (is (equal '("> /re") (tui-core:render-lines editor 40))))))

(test completion-file-walk-honors-ignores-and-hidden
  (let ((protocol (make-tui-completion-fixture))
        (root (make-completion-file-tree)))
    (uiop:with-current-directory (root)
      (let ((editor (tui-editor:make-editor :protocol protocol)))
        (feed-editor-keys editor '("s" "e" "e" " " "@"))
        (is (equal '("src/" "README.md" "src/main.lisp" "src/util.lisp")
                   (popup-matches editor)))
        (feed-editor-keys editor '("m" "a"))
        (is (equal '("src/main.lisp") (popup-matches editor)))
        (tui-core:handle-input editor "tab")
        (is (string= "see @src/main.lisp " (tui-editor:editor-value editor)))))))

(test completion-tab-path-auto-accepts-and-chains
  (let ((protocol (make-tui-completion-fixture))
        (root (make-completion-file-tree)))
    (uiop:with-current-directory (root)
      (let ((editor (tui-editor:make-editor :protocol protocol)))
        (feed-editor-keys editor '("s" "r"))
        (tui-core:handle-input editor "tab")
        (is (string= "src/" (tui-editor:editor-value editor)))
        (is (null (tui-editor:editor-completion editor)))
        (tui-core:handle-input editor "m")
        (tui-core:handle-input editor "tab")
        (is (string= "src/main.lisp " (tui-editor:editor-value editor)))))))

(test completion-tab-without-token-inserts-spaces
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor "tab")
    (is (string= "    " (tui-editor:editor-value editor)))))

(test completion-escape-dismisses-the-popup
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "c" "o"))
    (is (not (null (tui-editor:editor-completion editor))))
    (is-true (tui-core:dismiss-overlay editor))
    (is (null (tui-editor:editor-completion editor)))
    (is (string= "/co" (tui-editor:editor-value editor))
        "dismissal leaves the buffer untouched")
    (is (null (tui-core:dismiss-overlay editor))
        "a second dismiss declines so the router can route abort")))

(test completion-escape-routes-dismiss-before-abort
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (aborted 0)
         (route-context (tui-input:make-input-route-context
                         :abort-handler (lambda () (incf aborted))))
         (escape (tui-input:make-key-input-event :escape :key-id "escape")))
    (feed-editor-keys editor '("/" "c" "o"))
    (tui-input:route-input-event editor escape route-context)
    (is (null (tui-editor:editor-completion editor))
        "the first escape closes the popup")
    (is (= 0 aborted)
        "closing the popup swallows the escape ahead of abort")
    (tui-input:route-input-event editor escape route-context)
    (is (= 1 aborted)
        "with no popup the escape routes to the abort handler")))

(test completion-escape-dismisses-through-transcript-view
  (multiple-value-bind (protocol context) (make-tui-completion-fixture)
    (install-extensions context
                        tui-terminal:*tui-terminal-extension-manifest*
                        tui-transcript:*tui-transcript-extension-manifest*)
    (let* ((view (tui-transcript:make-transcript-view :protocol protocol))
           (editor (tui-transcript:transcript-view-editor view)))
      (feed-editor-keys editor '("/" "c" "o"))
      (is (not (null (tui-editor:editor-completion editor))))
      (is-true (tui-core:dismiss-overlay view))
      (is (null (tui-editor:editor-completion editor)))
      (is (null (tui-core:dismiss-overlay view))))))

(test completion-clears-on-set-editor-value
  (let* ((protocol (make-tui-completion-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (feed-editor-keys editor '("/" "c" "o"))
    (is (not (null (tui-editor:editor-completion editor))))
    (tui-editor:set-editor-value editor "")
    (is (null (tui-editor:editor-completion editor)))))

(test completion-rank-keeps-all-and-window-slides
  "Ranking keeps every match instead of capping at the render limit, and the
render window slides to keep the selection in view, so the menu scrolls
through all of them."
  (let* ((candidates (loop for i from 0 below 10
                           for name = (format nil "item-~2,'0D" i)
                           collect (tui-editor:make-completion-candidate
                                    :insert name :match name)))
         (ranked (tui-editor::rank-candidates "item" candidates)))
    (is (= 10 (length ranked))
        "all matches survive ranking, none dropped at the render limit")
    (is (equal (mapcar #'tui-editor:completion-candidate-match candidates)
               (mapcar #'tui-editor:completion-candidate-match ranked))
        "stable sort preserves source order on equal-prefix ties")
    (multiple-value-bind (slice offset)
        (tui-editor::completion-window ranked 9 tui-editor::+completion-limit+)
      (is (= tui-editor::+completion-limit+ (length slice)))
      (is (<= offset 9 (+ offset (1- (length slice))))
          "the window slid to keep the last selection visible")
      (is (eq (nth 9 ranked) (nth (- 9 offset) slice))
          "the selected candidate sits inside the visible slice"))))
