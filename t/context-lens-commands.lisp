(in-package #:kli/tests)

(defun context-commands-test-context ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        commands:*commands-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*
                        auth:*auth-extension-manifest*
                        models:*model-registry-extension-manifest*
                        rt:*model-runtime-extension-manifest*
                        agents:*agent-loop-extension-manifest*
                        agent-session:*agent-session-extension-manifest*
                        ctx-commands:*context-commands-extension-manifest*)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (bind-agent-session-mode context :deltas '("ack"))
    (values context protocol)))

(defun context-command-agent-context (context)
  (agent-session:agent-session-context
   (agent-session-service context) :default-mode context))

(test (context-commands-registers-five-context-commands :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore context))
    (let ((commands (ext:require-capability-provider
                     protocol :commands :contract :commands/v1)))
      (dolist (name '(:context/inspect :context/stage
                      :context/commit :context/diff :context/revert))
        (is (not (null (ext:provider-call commands :find-command name)))
            "command ~S must be registered" name)))))

(test (context-commands-inspect-reports-projection-and-staged-counts :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let* ((agent-context (context-command-agent-context context))
           (projected-count
             (length (ctx:context-projected-messages agent-context)))
           (result (invoke-test-command context :context/inspect))
           (details (commands:command-result-details result)))
      (is (not (commands:command-result-error-p result)))
      (is (search "Context epoch 0"
                  (command-result-text context result)))
      (is (= 0 (getf details :staged-count)))
      (is (= projected-count (getf details :projected-count))))))

(test (context-commands-stage-and-commit-applies-patch-to-projection :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let* ((agent-context (context-command-agent-context context))
           (before (length (ctx:context-projected-messages agent-context))))
      (let ((stage (invoke-test-command context :context/stage
                                        '(:tail "append added context"))))
        (is (not (commands:command-result-error-p stage)))
        (is (= 1 (length (ctx:context-staged-patches agent-context)))))
      (let ((commit (invoke-test-command context :context/commit)))
        (is (not (commands:command-result-error-p commit)))
        (is (search "epoch 1" (command-result-text context commit))))
      (is (zerop (length (ctx:context-staged-patches agent-context))))
      (is (= 1 (ctx:context-epoch agent-context)))
      (let ((projected (ctx:context-projected-messages agent-context)))
        (is (= (1+ before) (length projected)))
        (is (string= "added context"
                     (sess:message-content (first (last projected)))))))))

(test (context-commands-commit-without-staged-patches-reports-noop :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let ((result (invoke-test-command context :context/commit)))
      (is (not (commands:command-result-error-p result)))
      (is (search "No staged patches"
                  (command-result-text context result))))))

(test (context-commands-revert-drops-staged-patches :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let ((agent-context (context-command-agent-context context)))
      (invoke-test-command context :context/stage '(:tail "append x"))
      (invoke-test-command context :context/stage '(:tail "append y"))
      (is (= 2 (length (ctx:context-staged-patches agent-context))))
      (let ((result (invoke-test-command context :context/revert)))
        (is (not (commands:command-result-error-p result)))
        (is (search "2" (command-result-text context result))))
      (is (zerop (length (ctx:context-staged-patches agent-context)))))))

(test (context-commands-diff-lists-staged-and-empty-state :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let ((empty (invoke-test-command context :context/diff)))
      (is (search "No pending"
                  (command-result-text context empty))))
    (invoke-test-command context :context/stage '(:tail "append diff-me"))
    (let ((diff (invoke-test-command context :context/diff)))
      (is (search "diff-me" (command-result-text context diff))))))

(test (context-commands-stage-without-capability-returns-error-result :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let* ((ext:*call-subject* (ext:make-subject :capabilities '()))
           (result (invoke-test-command context :context/stage
                                        '(:tail "append blocked"))))
      (is (commands:command-result-error-p result)))))

(test (context-commands-stage-with-blank-tail-returns-usage-error :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let ((result (invoke-test-command context :context/stage '(:tail ""))))
      (is (commands:command-result-error-p result))
      (is (search "Usage" (command-result-text context result))))))

(test (context-commands-tui-parser-dispatches-context-inspect :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack"))))
    (install-extension context
                       ctx-commands:*context-commands-extension-manifest*)
    (multiple-value-bind (handled-p events)
        (tui-commands:dispatch-slash-command context
                                             "/context inspect"
                                             :mode-id :default-mode)
      (is (not (null handled-p)))
      (is (= 1 (length events)))
      (is (search "Context epoch"
                  (tui-transcript:event-text (first events)))))))

(test (context-commands-tui-parser-routes-stage-tail-to-runner :fixture interactive-authority)
  (let* ((context (ensure-tui-app-stack-with-session :deltas '("ack")))
         (agent-context (context-command-agent-context context)))
    (install-extension context
                       ctx-commands:*context-commands-extension-manifest*)
    (tui-commands:dispatch-slash-command context
                                         "/context stage append hello world"
                                         :mode-id :default-mode)
    (let ((staged (ctx:context-staged-patches agent-context)))
      (is (= 1 (length staged)))
      (let ((message (getf (ctx:context-patch-payload (first staged))
                           :message)))
        (is (string= "hello world" (sess:message-content message)))))))

(test (context-commands-stage-remove-by-index-creates-remove-patch :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let ((agent-context (context-command-agent-context context)))
      (let ((result (invoke-test-command context :context/stage
                                         '(:tail "remove 2"))))
        (is (not (commands:command-result-error-p result))))
      (let* ((staged (ctx:context-staged-patches agent-context))
             (patch (first staged))
             (payload (ctx:context-patch-payload patch)))
        (is (= 1 (length staged)))
        (is (eq :remove-message (ctx:context-patch-kind patch)))
        (is (= 2 (getf payload :index)))
        (is (null (getf payload :message-id)))))))

(test (context-commands-stage-replace-rewrites-message-content :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let ((agent-context (context-command-agent-context context)))
      (let ((result (invoke-test-command context :context/stage
                                         '(:tail "replace 0 fresh text"))))
        (is (not (commands:command-result-error-p result))))
      (let* ((staged (ctx:context-staged-patches agent-context))
             (patch (first staged))
             (payload (ctx:context-patch-payload patch))
             (message (getf payload :message)))
        (is (= 1 (length staged)))
        (is (eq :replace-message (ctx:context-patch-kind patch)))
        (is (= 0 (getf payload :index)))
        (is (string= "fresh text" (sess:message-content message)))))))

(test (context-commands-stage-unknown-subcommand-returns-usage-error :fixture interactive-authority)
  (multiple-value-bind (context protocol) (context-commands-test-context)
    (declare (ignore protocol))
    (let ((result (invoke-test-command context :context/stage
                                       '(:tail "drop everything"))))
      (is (commands:command-result-error-p result))
      (is (search "Unknown sub-command"
                  (command-result-text context result))))))
