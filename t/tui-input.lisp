(in-package #:kli/tests)

(defclass route-probe-view (kli:live-object)
  ((inputs
    :initform '()
    :accessor route-probe-inputs)
   (pastes
    :initform '()
    :accessor route-probe-pastes)
   (accept-paste
    :initarg :accept-paste
    :initform t
    :accessor route-probe-accept-paste)))

(defmethod tui-core:handle-input ((view route-probe-view) input)
  (push input (route-probe-inputs view))
  input)

(defmethod tui-core:handle-paste ((view route-probe-view) text)
  (push text (route-probe-pastes view))
  (when (route-probe-accept-paste view)
    text))

(defun make-tui-input-fixture ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tui-input:*tui-input-extension-manifest*)
    (values protocol context)))

(test tui-input-extension-registers-service
  (multiple-value-bind (protocol context) (make-tui-input-fixture)
    (declare (ignore context))
    (is (ext:extension-loaded-p protocol :tui-input))))

(test tui-input-events-are-clos-live-objects
  (let ((event (tui-input:make-text-input-event "a" :raw "a")))
    (is (typep event 'kli:live-object))
    (is (kli:object-id event))
    (is (eq :text (tui-input:input-event-kind event)))
    (is (string= "a" (tui-input:input-event-text event)))
    (is (string= "a" (tui-input:input-event-raw event)))))

(test tui-input-classifiers
  (is (tui-input:printable-character-p #\a))
  (is (not (tui-input:printable-character-p (code-char 7))))
  (is (string= (format nil "a~%b    c")
               (tui-input:normalize-insertable-text
                (format nil "a~%b~Cc" #\Tab))))
  (is (string= "abc" (tui-input:insertable-input-string "abc")))
  (is (null (tui-input:insertable-input-string (string (code-char 7)))))
  (is (tui-input:submit-input-p "enter"))
  (is (tui-input:newline-input-p "newline"))
  (is (tui-input:backspace-input-p "backspace")))

(test tui-input-decoder-emits-text-and-basic-keys
  (let* ((protocol (make-tui-input-fixture))
         (text (first (tui-input:decode-input-sequence protocol "a")))
         (enter (first (tui-input:decode-input-sequence protocol (string #\Return))))
         (backspace (first (tui-input:decode-input-sequence
                            protocol (string #\Rubout)))))
    (is (eq :text (tui-input:input-event-kind text)))
    (is (string= "a" (tui-input:input-event-text text)))
    (is (eq :key (tui-input:input-event-kind enter)))
    (is (eq :enter (tui-input:input-event-key enter)))
    (is (eq :backspace (tui-input:input-event-key backspace)))))

(test tui-input-decoder-emits-escape-tab-and-shift-tab
  (let* ((protocol (make-tui-input-fixture))
         (escape (first (tui-input:decode-input-sequence protocol (string #\Esc))))
         (tab (first (tui-input:decode-input-sequence protocol (string #\Tab))))
         (shift-tab (first (tui-input:decode-input-sequence
                            protocol (format nil "~C[Z" #\Esc)))))
    (is (eq :key (tui-input:input-event-kind escape)))
    (is (eq :escape (tui-input:input-event-key escape)))
    (is (string= "escape" (tui-input:input-event-key-id escape)))
    (is (eq :tab (tui-input:input-event-key tab)))
    (is (string= "tab" (tui-input:input-event-key-id tab)))
    (is (eq :tab (tui-input:input-event-key shift-tab)))
    (is (equal '(:shift) (tui-input:input-event-modifiers shift-tab)))
    (is (string= "shift+tab" (tui-input:input-event-key-id shift-tab)))))

(test tui-input-decoder-emits-control-and-alt-keys
  (let* ((protocol (make-tui-input-fixture))
         (ctrl-j (first (tui-input:decode-input-sequence
                         protocol (string #\Newline))))
         (ctrl-l (first (tui-input:decode-input-sequence
                         protocol (string (code-char 12)))))
         (alt-d (first (tui-input:decode-input-sequence
                        protocol (format nil "~Cd" #\Esc))))
         (alt-backspace (first (tui-input:decode-input-sequence
                                protocol (format nil "~C~C" #\Esc #\Rubout)))))
    (is (eq :j (tui-input:input-event-key ctrl-j)))
    (is (equal '(:ctrl) (tui-input:input-event-modifiers ctrl-j)))
    (is (string= "ctrl+j" (tui-input:input-event-key-id ctrl-j)))
    (is (eq :l (tui-input:input-event-key ctrl-l)))
    (is (string= "ctrl+l" (tui-input:input-event-key-id ctrl-l)))
    (is (eq :d (tui-input:input-event-key alt-d)))
    (is (equal '(:alt) (tui-input:input-event-modifiers alt-d)))
    (is (string= "alt+d" (tui-input:input-event-key-id alt-d)))
    (is (eq :backspace (tui-input:input-event-key alt-backspace)))
    (is (equal '(:alt)
               (tui-input:input-event-modifiers alt-backspace)))))

(test tui-input-decoder-emits-kitty-modified-keys
  "With the kitty keyboard protocol active, Shift+Enter arrives as a distinct CSI u sequence and ctrl+<letter> is re-encoded the same way. Both resolve to the key-ids the keymap binds."
  (let* ((protocol (make-tui-input-fixture))
         (shift-enter (first (tui-input:decode-input-sequence
                              protocol (format nil "~C[13;2u" #\Esc))))
         (ctrl-j (first (tui-input:decode-input-sequence
                         protocol (format nil "~C[106;5u" #\Esc)))))
    (is (eq :enter (tui-input:input-event-key shift-enter)))
    (is (equal '(:shift) (tui-input:input-event-modifiers shift-enter)))
    (is (string= "shift+enter" (tui-input:input-event-key-id shift-enter)))
    (is (string= "ctrl+j" (tui-input:input-event-key-id ctrl-j)))
    (is (equal '(:ctrl) (tui-input:input-event-modifiers ctrl-j)))))

(test tui-input-decoder-buffers-partial-escape-sequences
  (let* ((protocol (make-tui-input-fixture))
         (decoder (tui-input:make-input-decoder :protocol protocol)))
    (is (null (tui-input:input-decoder-feed decoder (string #\Esc))))
    (is (string= (string #\Esc)
                 (tui-input:input-decoder-buffer decoder)))
    (let ((events (tui-input:input-decoder-feed decoder "[A")))
      (is (= 1 (length events)))
      (is (eq :key (tui-input:input-event-kind (first events))))
      (is (eq :up (tui-input:input-event-key (first events))))
      (is (string= "" (tui-input:input-decoder-buffer decoder))))))

(test tui-input-decoder-emits-application-cursor-arrows
  (let* ((protocol (make-tui-input-fixture))
         (up (first (tui-input:decode-input-sequence
                     protocol (format nil "~COA" #\Esc))))
         (down (first (tui-input:decode-input-sequence
                       protocol (format nil "~COB" #\Esc))))
         (right (first (tui-input:decode-input-sequence
                        protocol (format nil "~COC" #\Esc))))
         (left (first (tui-input:decode-input-sequence
                       protocol (format nil "~COD" #\Esc)))))
    (is (eq :up (tui-input:input-event-key up)))
    (is (string= "up" (tui-input:input-event-key-id up)))
    (is (eq :down (tui-input:input-event-key down)))
    (is (eq :right (tui-input:input-event-key right)))
    (is (eq :left (tui-input:input-event-key left)))))

(test tui-input-decoder-emits-bracketed-paste
  (let* ((protocol (make-tui-input-fixture))
         (decoder (tui-input:make-input-decoder :protocol protocol)))
    (is (null (tui-input:input-decoder-feed
               decoder
               (format nil "~C[200~~hello" #\Esc))))
    (is (tui-input:input-decoder-paste-mode decoder))
    (is (null (tui-input:input-decoder-feed
               decoder
               (format nil "~%world"))))
    (let ((events (tui-input:input-decoder-feed
                   decoder
                   (format nil "~C[201~~x" #\Esc))))
      (is (= 2 (length events)))
      (is (eq :paste (tui-input:input-event-kind (first events))))
      (is (string= (format nil "hello~%world")
                   (tui-input:input-event-text (first events))))
      (is (eq :text (tui-input:input-event-kind (second events))))
      (is (string= "x" (tui-input:input-event-text (second events))))
      (is (not (tui-input:input-decoder-paste-mode decoder))))))

(test tui-input-decoder-coalesces-unbracketed-printable-run
  "A terminal without bracketed paste delivers a paste as a bare printable run.
The decoder coalesces the whole run into ONE text event, so it drives a single
editor insert (and a single undo step) instead of one event -- and one O(value)
buffer rebuild -- per character."
  (let* ((protocol (make-tui-input-fixture))
         (decoder (tui-input:make-input-decoder :protocol protocol))
         (events (tui-input:input-decoder-feed decoder "hello world")))
    (is (= 1 (length events)))
    (is (eq :text (tui-input:input-event-kind (first events))))
    (is (string= "hello world" (tui-input:input-event-text (first events))))
    (is (string= "" (tui-input:input-decoder-buffer decoder)))))

(test tui-input-decoder-breaks-coalesced-run-at-control-and-escape
  "Coalescing stops at any non-printable char: a control byte and an escape
sequence each split the surrounding printable runs into their own text events,
preserving the middle char's key classification."
  (let* ((protocol (make-tui-input-fixture))
         (decoder (tui-input:make-input-decoder :protocol protocol))
         (events (tui-input:input-decoder-feed
                  decoder
                  (format nil "ab~Ccd" (code-char 1)))))
    (is (equal '(:text :key :text)
               (mapcar #'tui-input:input-event-kind events)))
    (is (string= "ab" (tui-input:input-event-text (first events))))
    (is (string= "cd" (tui-input:input-event-text (third events)))))
  (let* ((protocol (make-tui-input-fixture))
         (decoder (tui-input:make-input-decoder :protocol protocol))
         (events (tui-input:input-decoder-feed
                  decoder
                  (format nil "ab~C[Ccd" #\Esc))))
    (is (equal '(:text :key :text)
               (mapcar #'tui-input:input-event-kind events)))
    (is (eq :right (tui-input:input-event-key (second events))))
    (is (string= "ab" (tui-input:input-event-text (first events))))
    (is (string= "cd" (tui-input:input-event-text (third events))))))

(test tui-input-decoder-normalizes-csi-u-ctrl-c-to-interrupt
  "Under the kitty keyboard protocol ctrl+c arrives re-encoded as a CSI u sequence instead of the legacy 0x03 byte. Both encodings are the same key, so both emit the interrupt event rather than a keymap-routed key event."
  (let* ((protocol (make-tui-input-fixture))
         (csi-u (first (tui-input:decode-input-sequence
                        protocol (format nil "~C[99;5u" #\Esc))))
         (legacy (first (tui-input:decode-input-sequence
                         protocol (string (code-char 3))))))
    (is (eq :interrupt (tui-input:input-event-kind csi-u)))
    (is (eq :interrupt (tui-input:input-event-kind legacy)))))

(test tui-input-decoder-emits-interrupt-and-terminal-response
  (let* ((protocol (make-tui-input-fixture))
         (interrupt (first (tui-input:decode-input-sequence
                            protocol (string (code-char 3)))))
         (response (first (tui-input:decode-input-sequence
                           protocol (format nil "~C[?7u" #\Esc)))))
    (is (eq :interrupt (tui-input:input-event-kind interrupt)))
    (is (eq :terminal-response (tui-input:input-event-kind response)))
    (is (string= (format nil "~C[?7u" #\Esc)
                 (tui-input:input-event-raw response)))))

(test (tui-input-decoder-hotpatch-preserves-state :fixture interactive-authority)
  (let* ((protocol (make-tui-input-fixture))
         (decoder (tui-input:make-input-decoder :protocol protocol)))
    (is (null (tui-input:input-decoder-feed decoder (string #\Esc))))
    (is (string= (string #\Esc)
                 (tui-input:input-decoder-buffer decoder)))
    (is (eq decoder
            (tui-input:recode-input-decoder
             decoder
             :function (lambda (decoder data)
                         (declare (ignore data))
                         (tui-input:input-decoder-flush decoder)))))
    (is (= 1 (tui-core:behavior-version
              (tui-input:input-decoder-behavior decoder))))
    (let ((events (tui-input:input-decoder-feed decoder "")))
      (is (= 1 (length events)))
      (is (eq :escape (tui-input:input-event-key (first events))))
      (is (string= "" (tui-input:input-decoder-buffer decoder))))))

(test tui-input-routing-handles-clear-screen-without-touching-input
  (let* ((protocol (make-tui-input-fixture))
         (view (make-instance 'route-probe-view :protocol protocol))
         (cleared nil))
    (is (tui-input:route-input-event
         view
         (first (tui-input:decode-input-sequence protocol (string (code-char 12))))
         (tui-input:make-input-route-context
          :clear-screen-handler (lambda ()
                                  (setf cleared t)))))
    (is (not (null cleared)))
    (is (null (route-probe-inputs view)))))

(test tui-input-routing-sends-text-keys-and-paste-to-view
  (let* ((protocol (make-tui-input-fixture))
         (view (make-instance 'route-probe-view :accept-paste nil :protocol protocol)))
    (tui-input:route-input-event
     view
     (first (tui-input:decode-input-sequence protocol "a"))
     nil)
    (tui-input:route-input-event
     view
     (first (tui-input:decode-input-sequence protocol (string #\Return)))
     nil)
    (tui-input:route-input-event
     view
     (tui-input:make-paste-input-event "pasted")
     nil)
    (is (equal '("pasted" "enter" "a")
               (route-probe-inputs view)))
    (is (equal '("pasted") (route-probe-pastes view)))))

