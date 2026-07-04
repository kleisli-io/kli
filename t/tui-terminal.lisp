(in-package #:kli/tests)

(defun make-tui-terminal-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tui-views:*tui-views-extension-manifest*
                        tui-input:*tui-input-extension-manifest*
                        tui-editor:*tui-editor-extension-manifest*
                        tui-terminal:*tui-terminal-extension-manifest*)
    (values protocol context)))

(test tui-terminal-extension-registers-service
  (multiple-value-bind (protocol context) (make-tui-terminal-fixture)
    (declare (ignore context))
    (is (ext:extension-loaded-p protocol :tui-terminal))))

(test tui-memory-terminal-records-output-and-size
  (let* ((protocol (make-tui-terminal-fixture))
         (terminal (tui-terminal:make-memory-terminal
                    :protocol protocol
                    :id :memory-terminal-test
                    :columns 12
                    :rows 7)))
    (is (typep terminal 'kli:live-object))
    (is (eq :memory-terminal-test (kli:object-id terminal)))
    (multiple-value-bind (columns rows)
        (tui-core:terminal-size terminal)
      (is (= 12 columns))
      (is (= 7 rows)))
    (is (string= "hi"
                 (tui-core:write-terminal terminal "hi")))
    (is (string= " there"
                 (tui-core:write-terminal
                                                   terminal " there")))
    (is (string= "hi there" (tui-terminal:terminal-output terminal)))
    (is (eq terminal (tui-terminal:terminal-clear terminal)))
    (is (string= "" (tui-terminal:terminal-output terminal)))))

(test tui-terminal-control-sequences-render-to-memory-terminal
  (let* ((protocol (make-tui-terminal-fixture))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol)))
    (tui-terminal:terminal-clear-screen terminal)
    (tui-terminal:terminal-clear-line terminal)
    (tui-terminal:terminal-clear-scrollback terminal)
    (tui-terminal:terminal-move-cursor terminal 0 3)
    (tui-terminal:terminal-hide-cursor terminal)
    (tui-terminal:terminal-show-cursor terminal)
    (tui-terminal:terminal-enable-bracketed-paste terminal)
    (tui-terminal:terminal-disable-bracketed-paste terminal)
    (let ((output (tui-terminal:terminal-output terminal)))
      (is (search (format nil "~C[2J~C[H" #\Esc #\Esc) output))
      (is (search (format nil "~C[K" #\Esc) output))
      (is (search (format nil "~C[3J" #\Esc) output))
      (is (search (format nil "~C[1;4H" #\Esc) output))
      (is (search (format nil "~C[?25l" #\Esc) output))
      (is (search (format nil "~C[?25h" #\Esc) output))
      (is (search (format nil "~C[?2004h" #\Esc) output))
      (is (search (format nil "~C[?2004l" #\Esc) output)))))

(test tui-frame-renders-root-to-memory-terminal
  (let* ((protocol (make-tui-terminal-fixture))
         (root (tui-views:make-tui-container :protocol protocol))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 12))
         (frame (tui-terminal:make-screen-frame :protocol protocol :root root)))
    (tui-core:add-child root
                                 (tui-views:make-tui-text "hello"
                                                          :protocol protocol
                                                          :padding-x 0
                                                          :padding-y 0))
    (tui-core:render-frame frame terminal :force t)
    (is (search "hello" (tui-terminal:terminal-output terminal)))
    (is (equal '("hello       ") (tui-terminal:frame-previous-lines frame)))))

(test tui-frame-leaves-final-line-open
  (let* ((protocol (make-tui-terminal-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 12))
         (frame (tui-terminal:make-screen-frame :protocol protocol :root editor))
         output)
    (tui-core:render-frame frame terminal :force t)
    (setf output (tui-terminal:terminal-output terminal))
    (is (char/= #\Newline (char output (1- (length output)))))))

(test tui-frame-focus-moves-terminal-cursor
  (let* ((protocol (make-tui-terminal-fixture))
         (editor (tui-editor:make-editor :protocol protocol))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 12))
         (frame (tui-terminal:make-screen-frame :protocol protocol :root editor))
         output)
    (is (eq editor (tui-terminal:set-frame-focus frame editor)))
    (tui-core:handle-input editor "ab")
    (tui-core:handle-input editor "left")
    (tui-core:render-frame frame terminal :force t)
    (setf output (tui-terminal:terminal-output terminal))
    (is (not (search "|" output)))
    (is (search (format nil "~C[1;4H" #\Esc) output))))

(test tui-frame-diff-skips-unchanged-lines
  (let* ((protocol (make-tui-terminal-fixture))
         (root (tui-views:make-tui-container :protocol protocol))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 12))
         (frame (tui-terminal:make-screen-frame :protocol protocol :root root)))
    (tui-core:add-child root
                                 (tui-views:make-tui-text "hello"
                                                          :protocol protocol
                                                          :padding-x 0
                                                          :padding-y 0))
    (tui-core:render-frame frame terminal :force t)
    (tui-terminal:terminal-clear terminal)
    (tui-core:render-frame frame terminal)
    (is (string= "" (tui-terminal:terminal-output terminal)))))

(test tui-frame-diff-clears-removed-lines
  (let* ((protocol (make-tui-terminal-fixture))
         (root (tui-views:make-tui-container :protocol protocol))
         (first (tui-views:make-tui-text "a" :protocol protocol :padding-x 0 :padding-y 0))
         (second (tui-views:make-tui-text "b" :protocol protocol :padding-x 0 :padding-y 0))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 4))
         (frame (tui-terminal:make-screen-frame :protocol protocol :root root)))
    (tui-core:add-child root first)
    (tui-core:add-child root second)
    (tui-core:render-frame frame terminal :force t)
    (tui-terminal:terminal-clear terminal)
    (tui-core:remove-child root second)
    (tui-core:render-frame frame terminal)
    (is (search (format nil "~C[K" #\Esc)
                (tui-terminal:terminal-output terminal)))))

(test tui-process-terminal-folds-nested-batches-into-one-write
  "A nested batch scope drains only at the outermost close, so a reset clear that
   wraps the frame's own batch reaches the tty in the same single write as the
   frame it precedes, and an unbatched write still passes straight through."
  (let* ((protocol (make-tui-terminal-fixture))
         (terminal (tui-terminal:make-process-terminal :protocol protocol))
         (out (make-string-output-stream))
         after-clear after-inner-end after-outer-end passthrough)
    (let ((*standard-output* out))
      (flet ((drained () (get-output-stream-string out)))
        (tui-terminal:terminal-begin-frame terminal)
        (tui-core:write-terminal terminal "CLR")
        (setf after-clear (drained))
        (tui-terminal:terminal-begin-frame terminal)
        (tui-core:write-terminal terminal "FRAME")
        (tui-terminal:terminal-end-frame terminal)
        (setf after-inner-end (drained))
        (tui-terminal:terminal-end-frame terminal)
        (setf after-outer-end (drained))
        (tui-core:write-terminal terminal "Z")
        (setf passthrough (drained))))
    (is (string= "" after-clear))
    (is (string= "" after-inner-end))
    (is (string= "CLRFRAME" after-outer-end))
    (is (string= "Z" passthrough))))

(test tui-process-terminal-drains-faulted-frame
  "A fault inside the inner frame still drains the buffered bytes at the outer
   close and leaves batching off, so a faulted render never strands bytes or
   wedges the terminal in batching mode."
  (let* ((protocol (make-tui-terminal-fixture))
         (terminal (tui-terminal:make-process-terminal :protocol protocol))
         (out (make-string-output-stream))
         drained-after-fault passthrough)
    (let ((*standard-output* out))
      (flet ((drained () (get-output-stream-string out)))
        (ignore-errors
          (tui-terminal:terminal-begin-frame terminal)
          (unwind-protect
               (progn
                 (tui-core:write-terminal terminal "CLR")
                 (tui-terminal:terminal-begin-frame terminal)
                 (unwind-protect
                      (progn (tui-core:write-terminal terminal "PART")
                             (error "boom"))
                   (tui-terminal:terminal-end-frame terminal))
                 (tui-core:write-terminal terminal "UNREACHED"))
            (tui-terminal:terminal-end-frame terminal)))
        (setf drained-after-fault (drained))
        (tui-core:write-terminal terminal "Z")
        (setf passthrough (drained))))
    (is (string= "CLRPART" drained-after-fault))
    (is (string= "Z" passthrough))))

(test (tui-frame-renderer-can-be-recoded-without-losing-state :fixture interactive-authority)
  (let* ((protocol (make-tui-terminal-fixture))
         (root (tui-views:make-tui-container :protocol protocol))
         (terminal (tui-terminal:make-memory-terminal :protocol protocol :columns 12))
         (frame (tui-terminal:make-screen-frame :protocol protocol :root root)))
    (tui-core:add-child root
                                 (tui-views:make-tui-text "hello"
                                                          :protocol protocol
                                                          :padding-x 0
                                                          :padding-y 0))
    (tui-core:render-frame frame terminal :force t)
    (is (equal '("hello       ") (tui-terminal:frame-previous-lines frame)))
    (is (eq frame (tui-terminal:recode-frame-renderer frame)))
    (is (= 1 (tui-core:behavior-version
              (tui-terminal:frame-render-behavior frame))))
    (is (equal '("hello       ") (tui-terminal:frame-previous-lines frame)))))

