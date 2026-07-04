(in-package #:kli/tests)

(defun make-tui-keymap-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context tui-keymap:*tui-keymap-extension-manifest*)
    (values protocol context)))

(test tui-keymap-extension-registers-service
  (multiple-value-bind (protocol context) (make-tui-keymap-fixture)
    (declare (ignore context))
    (is (ext:extension-loaded-p protocol :tui-keymap))))

(test tui-keymap-resolves-base-bindings
  (let ((protocol (make-tui-keymap-fixture)))
    (is (eq :move-line-start (tui-keymap:keymap-action protocol "ctrl+a")))
    (is (eq :move-line-end   (tui-keymap:keymap-action protocol "ctrl+e")))
    (is (eq :newline         (tui-keymap:keymap-action protocol "shift+enter")))
    (is (eq :submit          (tui-keymap:keymap-action protocol "enter")))
    (is (null (tui-keymap:keymap-action protocol "f5")))
    (is (null (tui-keymap:keymap-action protocol nil)))))

(test tui-keymap-recognized-p
  (let ((protocol (make-tui-keymap-fixture)))
    (is-true (tui-keymap:keymap-recognized-p protocol "escape"))
    (is-true (tui-keymap:keymap-recognized-p protocol "page-up"))
    (is (not (tui-keymap:keymap-recognized-p protocol "f5")))))

(test tui-keymap-override-wins-then-retract-restores-base
  (let ((protocol (make-tui-keymap-fixture)))
    (is (eq :move-line-start (tui-keymap:keymap-action protocol "ctrl+a")))
    (tui-keymap:register-keybinding protocol "ctrl+a" :move-char-left)
    (is (eq :move-char-left (tui-keymap:keymap-action protocol "ctrl+a")))
    (tui-keymap:unregister-keybinding protocol "ctrl+a")
    (is (eq :move-line-start (tui-keymap:keymap-action protocol "ctrl+a")))))

(test tui-keymap-contribution-roundtrip-restores-base
  (multiple-value-bind (protocol context) (make-tui-keymap-fixture)
    (let ((contrib (tui-keymap:make-keybinding-contribution
                    :name :ctrl+a :key-id "ctrl+a"
                    :action :move-char-left :source :test)))
      (is (eq :move-line-start (tui-keymap:keymap-action protocol "ctrl+a")))
      (ext:install-contribution protocol contrib context)
      (is (eq :move-char-left (tui-keymap:keymap-action protocol "ctrl+a")))
      (ext:retract-contribution protocol contrib context)
      (is (eq :move-line-start (tui-keymap:keymap-action protocol "ctrl+a"))))))

(test tui-keymap-contribution-over-existing-override-restores-prior
  (multiple-value-bind (protocol context) (make-tui-keymap-fixture)
    (tui-keymap:register-keybinding protocol "ctrl+a" :undo)
    (let ((contrib (tui-keymap:make-keybinding-contribution
                    :name :ctrl+a :key-id "ctrl+a" :action :redo :source :test)))
      (ext:install-contribution protocol contrib context)
      (is (eq :redo (tui-keymap:keymap-action protocol "ctrl+a")))
      (ext:retract-contribution protocol contrib context)
      (is (eq :undo (tui-keymap:keymap-action protocol "ctrl+a"))
          "retract restores the prior override, not the base"))))

(test tui-keymap-load-keymap-applies-valid-and-skips-unknown
  (let ((protocol (make-tui-keymap-fixture)))
    (handler-bind ((warning #'muffle-warning))
      (is (= 2 (tui-keymap:load-keymap
                protocol
                "{\"keybindings\":{\"ctrl+a\":\"move-char-left\",\"ctrl+z\":\"undo\",\"bogus\":\"frobnicate\"}}"))))
    (is (eq :move-char-left (tui-keymap:keymap-action protocol "ctrl+a")))
    (is (eq :undo (tui-keymap:keymap-action protocol "ctrl+z")))
    (is (null (tui-keymap:keymap-action protocol "bogus")))))

(defun route-probe-key (protocol key-id &optional (key :enter) modifiers)
  "Route a key event with KEY-ID through a route-probe-view and return the input the view received."
  (let ((view (make-instance 'route-probe-view :protocol protocol))
        (event (tui-input:make-key-input-event key :key-id key-id
                                                   :modifiers modifiers)))
    (tui-input:route-input-event view event nil)
    (first (route-probe-inputs view))))

(test tui-keymap-router-normalizes-submit-newline-backspace
  "The router normalizes submit, newline, and backspace key-ids. A motion key passes through to the view as its raw key-id."
  (let ((protocol (make-tui-keymap-fixture)))
    (is (string= "enter"    (route-probe-key protocol "enter")))
    (is (string= "newline"  (route-probe-key protocol "shift+enter" :enter '(:shift))))
    (is (string= "newline"  (route-probe-key protocol "ctrl+j" :j '(:ctrl))))
    (is (string= "backspace" (route-probe-key protocol "backspace" :backspace)))
    (is (string= "ctrl+a"   (route-probe-key protocol "ctrl+a" :a '(:ctrl))))))

(test tui-keymap-router-intercepts-app-keys
  (let ((protocol (make-tui-keymap-fixture))
        (cleared nil)
        (tool nil))
    (let ((view (make-instance 'route-probe-view :protocol protocol)))
      (is (tui-input:route-input-event
           view
           (tui-input:make-key-input-event :l :key-id "ctrl+l" :modifiers '(:ctrl))
           (tui-input:make-input-route-context
            :clear-screen-handler (lambda () (setf cleared t)))))
      (is-true cleared)
      (is (null (route-probe-inputs view))))
    (let ((view (make-instance 'route-probe-view :protocol protocol)))
      (is (tui-input:route-input-event
           view
           (tui-input:make-key-input-event :o :key-id "ctrl+o" :modifiers '(:ctrl))
           (list :tool-output-handler (lambda () (setf tool t)))))
      (is-true tool)
      (is (null (route-probe-inputs view))))))

(test tui-keymap-editor-dispatch-goldens
  "Golden editor dispatch through the keymap. Newline (shift+enter) inserts a newline rather than submitting, and up at the top line declines (nil) so history can take over."
  (let ((protocol (make-tui-editor-fixture)))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hello")
      (tui-core:handle-input editor "ctrl+a")
      (tui-core:handle-input editor "X")
      (is (string= "Xhello" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "hi")
      (tui-core:handle-input editor "ctrl+a")
      (tui-core:handle-input editor "ctrl+e")
      (tui-core:handle-input editor "Z")
      (is (string= "hiZ" (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "foo bar")
      (tui-core:handle-input editor "ctrl+w")
      (is (string= "foo " (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "a")
      (tui-core:handle-input editor "newline")
      (tui-core:handle-input editor "b")
      (is (string= (format nil "a~%b") (tui-editor:editor-value editor))))
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "ab")
      (tui-core:handle-input editor "ctrl+a")
      (is (not (tui-core:handle-input editor "up"))))))

(test tui-keymap-binds-ctrl-z-to-undo
  (let ((protocol (make-tui-keymap-fixture)))
    (is (eq :undo (tui-keymap:keymap-action protocol "ctrl+z")))))

(test tui-keymap-advertises-only-implemented-actions
  (is (member :undo tui-keymap:+keymap-actions+))
  (is (not (member :redo tui-keymap:+keymap-actions+)))
  (is (not (member :yank tui-keymap:+keymap-actions+)))
  (is (not (member :yank-pop tui-keymap:+keymap-actions+))))

(test tui-keymap-ctrl-z-undoes-the-last-edit
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-core:handle-input editor "hello")
    (tui-core:handle-input editor " world")
    (is (string= "hello world" (tui-editor:editor-value editor)))
    (tui-core:handle-input editor "ctrl+z")
    (is (string= "hello" (tui-editor:editor-value editor)))
    (tui-core:handle-input editor "ctrl+z")
    (is (string= "" (tui-editor:editor-value editor)))))

(test tui-keymap-editor-respects-override
  (let* ((protocol (make-tui-editor-fixture))
         (editor (tui-editor:make-editor :protocol protocol)))
    (tui-keymap:register-keybinding protocol "ctrl+a" :move-char-left)
    (tui-core:handle-input editor "ab")
    (tui-core:handle-input editor "ctrl+a")
    (tui-core:handle-input editor "X")
    (is (string= "aXb" (tui-editor:editor-value editor)))
    (tui-keymap:unregister-keybinding protocol "ctrl+a")
    (let ((editor (tui-editor:make-editor :protocol protocol)))
      (tui-core:handle-input editor "ab")
      (tui-core:handle-input editor "ctrl+a")
      (tui-core:handle-input editor "Y")
      (is (string= "Yab" (tui-editor:editor-value editor))))))
