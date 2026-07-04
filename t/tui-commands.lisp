(in-package #:kli/tests)

(defun load-tui-command-stack (context)
  (install-extensions context
                      commands:*commands-extension-manifest*
                      tui-views:*tui-views-extension-manifest*
                      tui-input:*tui-input-extension-manifest*
                      tui-editor:*tui-editor-extension-manifest*
                      tui-terminal:*tui-terminal-extension-manifest*
                      tui-transcript:*tui-transcript-extension-manifest*))

(defun register-test-tui-command (context name runner)
  (let* ((protocol (kli:active-protocol context))
         (provider (ext:require-capability-provider protocol
                                                    :commands
                                                    :contract :commands/v1))
         (command (commands:make-command :name name :runner runner)))
    (ext:provider-call provider
                       :register-command
                       context
                       name
                       command
                       :source :test)))

(test tui-commands-parses-slash-command-input
  (is (tui-commands:slash-command-input-p "/clear"))
  (is (not (tui-commands:slash-command-input-p "clear")))
  (let ((parsed (tui-commands:parse-slash-command "/echo hello world")))
    (is (eq :echo (getf parsed :name)))
    (is (string= "hello world" (getf parsed :tail)))
    (is (equal '("hello" "world") (getf parsed :words)))))

(test tui-commands-dispatches-to-command-provider
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-tui-command-stack context)
    (register-test-tui-command
     context
     :ping
     (lambda (command arguments context &key call-id on-update)
       (declare (ignore command context call-id on-update))
       (format nil "pong ~A" (getf arguments :tail))))
    (multiple-value-bind (handled-p events)
        (tui-commands:dispatch-slash-command context "/ping now")
      (is (not (null handled-p)))
      (is (= 1 (length events)))
      (is (string= "pong now"
                   (tui-transcript:event-text (first events))))
      (is (null (tui-transcript:event-status (first events)))
          "a successful result renders as a plain notice"))))

(test tui-commands-result-renders-body-verbatim
  "A command response shows only its body, no name prefix -- the command is
echoed above as the user line -- so multi-line output is preserved as-is and a
one-liner stays a single line."
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-tui-command-stack context)
    (register-test-tui-command
     context
     :rows
     (lambda (command arguments context &key call-id on-update)
       (declare (ignore command context call-id on-update))
       (if (string= (getf arguments :tail) "one")
           "single"
           (format nil "alpha~%beta~%gamma"))))
    (multiple-value-bind (handled-p events)
        (tui-commands:dispatch-slash-command context "/rows many")
      (declare (ignore handled-p))
      (is (string= (format nil "alpha~%beta~%gamma")
                   (tui-transcript:event-text (first events)))
          "multi-line output is preserved verbatim"))
    (multiple-value-bind (handled-p events)
        (tui-commands:dispatch-slash-command context "/rows one")
      (declare (ignore handled-p))
      (is (string= "single"
                   (tui-transcript:event-text (first events)))
          "single-line output stays a single line"))))

(test tui-commands-error-result-with-text-carries-error-status
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-tui-command-stack context)
    (register-test-tui-command
     context
     :boom
     (lambda (command arguments context &key call-id on-update)
       (declare (ignore command arguments context call-id on-update))
       (commands:make-command-result
        :content (list (list :type :text :text "boom"))
        :error-p t)))
    (multiple-value-bind (handled-p events)
        (tui-commands:dispatch-slash-command context "/boom")
      (is (not (null handled-p)))
      (is (string= "boom"
                   (tui-transcript:event-text (first events))))
      (is (eq :error (tui-transcript:event-status (first events)))))))

(test tui-commands-error-result-without-text-renders-failed-as-error
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-tui-command-stack context)
    (register-test-tui-command
     context
     :boom
     (lambda (command arguments context &key call-id on-update)
       (declare (ignore command arguments context call-id on-update))
       (commands:make-command-result :error-p t)))
    (multiple-value-bind (handled-p events)
        (tui-commands:dispatch-slash-command context "/boom")
      (is (not (null handled-p)))
      (is (string= "/boom failed."
                   (tui-transcript:event-text (first events))))
      (is (eq :error (tui-transcript:event-status (first events)))))))

(test tui-commands-renders-unknown-command
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-tui-command-stack context)
    (multiple-value-bind (handled-p events)
        (tui-commands:dispatch-slash-command context "/missing")
      (is (not (null handled-p)))
      (is (string= "Unknown command: /missing."
                   (tui-transcript:event-text (first events)))))))

(test (tui-commands-dispatches-with-mode-id :fixture interactive-authority)
  (let ((captured-default nil)
        (captured-alpha nil))
    (let* ((context (ensure-tui-app-stack))
           (app (tui-app:make-tui-app :context context :columns 32)))
      (register-test-tui-command
       context :probe
       (lambda (command arguments context &key call-id on-update)
         (declare (ignore command context call-id on-update))
         (setf captured-default arguments)
         ""))
      (tui-app:tui-app-feed app "/probe" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (is (eq :default-mode (getf captured-default :mode-id)))
      (is (eq app (getf captured-default :app))))
    (let* ((context (ensure-tui-app-stack))
           (app (tui-app:make-tui-app :context context
                                      :columns 32
                                      :mode-id :alpha)))
      (register-test-tui-command
       context :probe
       (lambda (command arguments context &key call-id on-update)
         (declare (ignore command context call-id on-update))
         (setf captured-alpha arguments)
         ""))
      (tui-app:tui-app-feed app "/probe" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (is (eq :alpha (getf captured-alpha :mode-id)))
      (is (eq app (getf captured-alpha :app))))))

(test (tui-app-routes-slash-input-through-tui-commands :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack))
         (calls 0)
         (app (tui-app:make-tui-app
               :context context
               :columns 48
               :on-submit
               (lambda (app input)
                 (declare (ignore app input))
                 (incf calls)))))
    (register-test-tui-command
     context
     :ping
     (lambda (command arguments context &key call-id on-update)
       (declare (ignore command context call-id on-update))
       (format nil "pong ~A" (getf arguments :tail))))
    (tui-app:tui-app-feed app "/ping now" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (let ((events (tui-app:tui-app-transcript-events app)))
      (is (= 0 calls))
      (is (= 2 (length events)))
      (is (string= "/ping now"
                   (tui-transcript:event-text (first events))))
      (is (string= "pong now"
                   (tui-transcript:event-text (second events)))))))
