(in-package #:kli/tests)

(defun make-tui-editor-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tui-views:*tui-views-extension-manifest*
                        tui-input:*tui-input-extension-manifest*
                        tui-editor:*tui-editor-extension-manifest*)
    (values protocol context)))

(defun large-paste-text (&optional (line-count 20))
  (format nil "~{~A~^~%~}"
          (loop for index from 1 to line-count
                collect (format nil "line ~D" index))))

(test tui-editor-extension-registers-service
  (multiple-value-bind (protocol context) (make-tui-editor-fixture)
    (declare (ignore context))
    (is (ext:extension-loaded-p protocol :tui-editor))))

(test tui-editor-is-live-object-with-hotpatchable-behavior
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :id :editor-test :protocol protocol)))
    (is (typep editor 'kli:live-object))
    (is (eq :editor-test (kli:object-id editor)))
    (is (= 0 (tui-core:behavior-version
              (tui-editor:editor-behavior editor))))))

(test tui-editor-edits-printable-text
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor "a")
    (tui-core:handle-input editor "b")
    (tui-core:handle-input editor "backspace")
    (is (string= "a" (tui-editor:editor-value editor)))))

(test tui-editor-inserts-terminal-whitespace
  (let ((protocol (make-tui-editor-fixture)))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hello")
      (tui-core:handle-input editor " ")
      (tui-core:handle-input editor "world")
      (is (string= "hello world" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "tab")
      (is (string= "    " (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "space")
      (is (string= " " (tui-editor:editor-value editor))))))

(test tui-editor-preserves-pasted-newlines
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor (format nil "hello~%world"))
    (is (string= (format nil "hello~%world")
                 (tui-editor:editor-value editor)))
    (is (equal (tui-core:render-lines editor 16)
               '("> hello" "  world")))))

(test tui-editor-represents-large-paste-with-marker
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (paste (large-paste-text)))
    (is (tui-core:handle-paste editor paste))
    (is (string= "[paste #1 +20 lines]"
                 (tui-editor:editor-value editor)))
    (is (string= paste (tui-editor:editor-expanded-value editor)))
    (is (= 1 (length (tui-editor:editor-paste-blocks editor))))
    (is (typep (first (tui-editor:editor-paste-blocks editor))
               'kli:live-object))
    (is (equal (tui-core:render-lines editor 80)
               '("> [paste #1 +20 lines]")))))

(test tui-editor-represents-long-single-line-paste-with-marker
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (paste (make-string 1001 :initial-element #\a)))
    (is (tui-core:handle-paste editor paste))
    (is (string= "[paste #1 1001 chars]"
                 (tui-editor:editor-value editor)))
    (is (string= paste (tui-editor:editor-expanded-value editor)))))

(test tui-editor-submits-expanded-large-paste
  (let ((protocol (make-tui-editor-fixture))
        (submitted nil)
        (paste (large-paste-text)))
    (let ((editor (tui-editor:make-editor
                   :protocol protocol
                   :on-submit (lambda (value)
                                (setf submitted value)))))
      (tui-core:handle-paste editor paste)
      (tui-core:handle-input editor "enter")
      (is (string= paste submitted)))))

(test tui-editor-treats-paste-marker-as-editing-unit
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (paste (large-paste-text)))
    (tui-core:handle-input editor "A")
    (tui-core:handle-paste editor paste)
    (tui-core:handle-input editor "B")
    (tui-core:handle-input editor "ctrl+a")
    (tui-core:handle-input editor "right")
    (tui-core:handle-input editor "right")
    (tui-core:handle-input editor "backspace")
    (is (string= "AB" (tui-editor:editor-value editor)))
    (tui-core:handle-paste editor paste)
    (tui-core:handle-input editor "ctrl+a")
    (tui-core:handle-input editor "right")
    (tui-core:handle-input editor "delete")
    (is (string= "AB" (tui-editor:editor-value editor)))))

(test tui-editor-focused-cursor-position
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor "ab")
    (setf (tui-editor:editor-focused editor) t)
    (tui-core:handle-input editor "left")
    (is (equal '("> ab") (tui-core:render-lines editor 16)))
    (is (equal (cons 0 3)
               (tui-core:cursor-position editor 16)))))

(test tui-editor-wraps-long-input-and-remaps-cursor
  "At width 16 the prompt \"> \" leaves content width 14, so a 20-char word force-breaks into two rows with a two-space indent on the continuation. The cursor at end maps to the second visual row rather than clamping to row 0."
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor (make-string 20 :initial-element #\a))
    (setf (tui-editor:editor-focused editor) t)
    (is (equal (list (format nil "> ~A" (make-string 14 :initial-element #\a))
                     (format nil "  ~A" (make-string 6 :initial-element #\a)))
               (tui-core:render-lines editor 16)))
    (is (equal (cons 1 8) (tui-core:cursor-position editor 16)))
    (tui-core:handle-input editor "ctrl+a")
    (is (equal (cons 0 2) (tui-core:cursor-position editor 16)))))

(test tui-editor-cursor-and-wrap-count-wide-characters
  "A CJK character occupies two columns, so the cursor lands two columns past
the prompt and wrapping budgets columns rather than characters."
  (let* ((protocol (make-tui-editor-fixture))
         (wide (code-char #x4E2D))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor (string wide))
    (setf (tui-editor:editor-focused editor) t)
    (is (equal (cons 0 4) (tui-core:cursor-position editor 16))
        "prompt width 2 plus one wide character is column 4")
    (tui-core:handle-input editor "left")
    (is (equal (cons 0 2) (tui-core:cursor-position editor 16)))
    (tui-core:handle-input editor "ctrl+e")
    (tui-core:handle-input editor (make-string 7 :initial-element wide))
    (is (equal (list (format nil "> ~A" (make-string 7 :initial-element wide))
                     (format nil "  ~A" (string wide)))
               (tui-core:render-lines editor 16))
        "content width 14 fits seven wide characters per row")
    (is (equal (cons 1 4) (tui-core:cursor-position editor 16)))))

(test tui-editor-supports-readline-style-editing
  (let ((protocol (make-tui-editor-fixture)))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hello world")
      (tui-core:handle-input editor "ctrl+w")
      (is (string= "hello " (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hello world")
      (tui-core:handle-input editor "ctrl+u")
      (is (string= "" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hello world")
      (tui-core:handle-input editor "ctrl+a")
      (tui-core:handle-input editor "ctrl+k")
      (is (string= "" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "ab")
      (tui-core:handle-input editor "ctrl+b")
      (tui-core:handle-input editor "X")
      (is (string= "aXb" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "ab")
      (tui-core:handle-input editor "left")
      (tui-core:handle-input editor "delete")
      (is (string= "a" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hello world")
      (tui-core:handle-input editor "alt+b")
      (tui-core:handle-input editor "X")
      (is (string= "hello Xworld" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hello world")
      (tui-core:handle-input editor "ctrl+a")
      (tui-core:handle-input editor "alt+d")
      (is (string= " world" (tui-editor:editor-value editor))))))

(test tui-editor-ctrl-l-requests-redraw-without-changing-value
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor "hello")
    (is (tui-core:handle-input editor "ctrl+l"))
    (is (string= "hello" (tui-editor:editor-value editor)))))

(test (tui-editor-behavior-can-be-recoded-without-losing-state :fixture interactive-authority)
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor "hello")
    (is (eq editor (tui-editor:recode-editor-behavior editor)))
    (is (= 1 (tui-core:behavior-version
              (tui-editor:editor-behavior editor))))
    (is (string= "hello" (tui-editor:editor-value editor)))
    (is (tui-core:handle-input editor "ctrl+l"))
    (is (string= "hello" (tui-editor:editor-value editor)))))

(test (tui-editor-recode-preserves-buffer-cursor-and-paste-blocks :fixture interactive-authority)
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (paste (large-paste-text)))
    (tui-core:handle-input editor "A")
    (tui-core:handle-paste editor paste)
    (tui-core:handle-input editor "B")
    (tui-core:handle-input editor "ctrl+a")
    (tui-core:handle-input editor "right")
    (let ((cursor (tui-editor:editor-cursor editor))
          (blocks (copy-list (tui-editor:editor-paste-blocks editor)))
          (expanded (tui-editor:editor-expanded-value editor)))
      (is (eq editor (tui-editor:recode-editor-behavior editor)))
      (is (= cursor (tui-editor:editor-cursor editor)))
      (is (equal blocks (tui-editor:editor-paste-blocks editor)))
      (is (string= expanded (tui-editor:editor-expanded-value editor)))
      (is (= 1 (length (tui-editor:editor-paste-blocks editor))))
      (tui-core:handle-input editor "X")
      (is (string= "AX[paste #1 +20 lines]B"
                   (tui-editor:editor-value editor)))
      (is (string= (format nil "AX~AB" paste)
                   (tui-editor:editor-expanded-value editor))))))

(test tui-editor-up-and-down-move-within-multiline-text
  (let ((protocol (make-tui-editor-fixture)))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor (format nil "ab~%cd"))
      (is (tui-core:handle-input editor "up"))
      (tui-core:handle-input editor "X")
      (is (string= (format nil "abX~%cd")
                   (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor (format nil "ab~%cd"))
      (tui-core:handle-input editor "ctrl+a")
      (tui-core:handle-input editor "up")
      (is (tui-core:handle-input editor "down"))
      (tui-core:handle-input editor "X")
      (is (string= (format nil "ab~%Xcd")
                   (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "ab")
      (tui-core:handle-input editor "ctrl+a")
      (is (not (tui-core:handle-input editor "up")))
      (tui-core:handle-input editor "ctrl+e")
      (is (not (tui-core:handle-input editor "down"))))))

(test tui-editor-routes-decoded-events
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (dolist (event (tui-input:input-decoder-feed
                    (tui-input:make-input-decoder :protocol protocol)
                    "hello"))
      (tui-input:route-input-event editor event nil))
    (tui-input:route-input-event
     editor
     (first (tui-input:decode-input-sequence protocol (string #\Return)))
     nil)
    (is (string= "hello" (tui-editor:editor-value editor)))))

(test tui-editor-undo-reverts-edits-one-step-at-a-time
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-keymap:register-keybinding protocol "ctrl+z" :undo)
    (tui-core:handle-input editor "hello")
    (tui-core:handle-input editor " world")
    (is (string= "hello world" (tui-editor:editor-value editor)))
    (is (tui-core:handle-input editor "ctrl+z"))
    (is (string= "hello" (tui-editor:editor-value editor)))
    (is (tui-core:handle-input editor "ctrl+z"))
    (is (string= "" (tui-editor:editor-value editor)))
    (is (not (tui-core:handle-input editor "ctrl+z")))
    (is (string= "" (tui-editor:editor-value editor)))))

(test tui-editor-undo-reverts-paste-as-one-unit
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (paste (large-paste-text)))
    (tui-keymap:register-keybinding protocol "ctrl+z" :undo)
    (tui-core:handle-input editor "draft ")
    (tui-core:handle-paste editor paste)
    (is (string= "draft [paste #1 +20 lines]"
                 (tui-editor:editor-value editor)))
    (is (tui-core:handle-input editor "ctrl+z"))
    (is (string= "draft " (tui-editor:editor-value editor)))))

(test tui-editor-undo-restores-cursor-after-delete
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-keymap:register-keybinding protocol "ctrl+z" :undo)
    (tui-core:handle-input editor "hello world")
    (tui-core:handle-input editor "ctrl+w")
    (is (string= "hello " (tui-editor:editor-value editor)))
    (is (tui-core:handle-input editor "ctrl+z"))
    (is (string= "hello world" (tui-editor:editor-value editor)))
    (is (= 11 (tui-editor:editor-cursor editor)))))

(test tui-editor-undo-history-clears-on-set-value
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-keymap:register-keybinding protocol "ctrl+z" :undo)
    (tui-core:handle-input editor "abc")
    (tui-editor:set-editor-value editor "xyz")
    (is (not (tui-core:handle-input editor "ctrl+z")))
    (is (string= "xyz" (tui-editor:editor-value editor)))))


(test tui-editor-rows-and-cursor-single-pass-matches-two-pass
  "editor-rows-and-cursor agrees with the separate editor-row-lines and
cursor-position calls, and reports a NIL cursor while the editor is
unfocused."
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor (format nil "hello~%world"))
    (setf (tui-editor:editor-focused editor) t)
    (tui-core:handle-input editor "left")
    (multiple-value-bind (rows cursor)
        (tui-editor:editor-rows-and-cursor editor 16)
      (is (equal rows (tui-editor:editor-row-lines editor 16)))
      (is (equal cursor (tui-core:cursor-position editor 16)))
      (is (consp cursor)))
    (setf (tui-editor:editor-focused editor) nil)
    (is (null (nth-value 1 (tui-editor:editor-rows-and-cursor editor 16))))))
