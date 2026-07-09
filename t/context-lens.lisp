(in-package #:kli/tests)

(defun context-lens-provider (protocol)
  (ext:require-capability-provider protocol
                                   :context/lens
                                   :contract :context/lens/v1))

(defun context-lens-test-context ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*
                        ctx:*context-lens-extension-manifest*)
    (values context protocol)))

(defun context-lens-test-session (context &key (id :context-lens-session))
  (let* ((store (session-log-store context))
         (session (sess:create-session store context :id id)))
    (sess:append-session-entry
     store
     session
     (sess:make-message-entry (sess:make-user-message "root" :id :root-message)
                              :id :root-entry)
     context)
    (sess:append-session-entry
     store
     session
     (sess:make-message-entry (sess:make-assistant-message "assistant"
                                                           :id :assistant-message)
                              :id :assistant-entry)
     context)
    session))

(test context-lens-registers-provider-and-requires-session-log
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (signals error
      (install-extension context ctx:*context-lens-extension-manifest*)))
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore context))
    (is (context-lens-provider protocol))))

(test context-lens-rebuilds-projection-from-session-branch
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session
                                                  store
                                                  context
                                                  :id :agent-context)))
      (is (eq agent-context
              (kli:find-live-object (kli:context-registry context)
                                    :agent-context)))
      (is (equal '("root" "assistant")
                 (mapcar #'sess:message-content
                         (ctx:context-projected-messages
                          agent-context))))
      (is (= 0 (ctx:context-epoch agent-context))))))

(test (context-lens-gates-pandoric-inspection :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (ctx:inspect-agent-context agent-context)))
      (let ((inspection (ctx:inspect-agent-context agent-context)))
        (is (= 0 (getf inspection :epoch)))
        (is (equal '("root" "assistant")
                   (mapcar #'sess:message-content
                           (getf (getf inspection :projection)
                                 :messages))))))))

(test (context-lens-stages-commits-and-replays-patches :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context))
           (initial-leaf (sess:session-leaf-id session))
           (patch (ctx:make-append-message-patch
                   (sess:make-user-message "patched"
                                           :id :patched-message))))
      (ctx:stage-context-patch agent-context patch)
      (is (eq initial-leaf (sess:session-leaf-id session)))
      (is (equal '("root" "assistant")
                 (mapcar #'sess:message-content
                         (ctx:context-projected-messages agent-context))))
      (is (= 1 (length (ctx:context-staged-patches agent-context))))
      (let ((sealed-before (ctx:seal-context-projection
                            agent-context
                            context)))
        (is (equal '("root" "assistant")
                   (mapcar #'sess:message-content
                           (ctx:transcript-display-items sealed-before))))
        (ctx:commit-context-patches agent-context context)
        (is (= 1 (ctx:context-epoch agent-context)))
        (is (not (eq initial-leaf (sess:session-leaf-id session))))
        (is (equal '("root" "assistant" "patched")
                   (mapcar #'sess:message-content
                           (ctx:context-projected-messages
                            agent-context))))
        (is (equal '("root" "assistant")
                   (mapcar #'sess:message-content
                           (ctx:transcript-display-items sealed-before))))
        (ctx:rebuild-context-projection agent-context)
        (is (equal '("root" "assistant" "patched")
                   (mapcar #'sess:message-content
                           (ctx:context-projected-messages
                            agent-context))))))))

(test (context-lens-fresh-rebuild-derives-epoch-from-committed-sets :fixture interactive-authority)
  "A resumed context derives its epoch from the committed patch-sets on the branch,
never a stale stored counter: rebuilding fresh over a session that already carries
N committed edits reports epoch N, and the next commit bases at N without colliding
with an on-disk set."
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context)))
      (dotimes (i 3)
        (ctx:stage-context-patch
         agent-context
         (ctx:make-append-message-patch
          (sess:make-user-message (format nil "edit-~D" i))))
        (ctx:commit-context-patches agent-context context))
      (is (= 3 (ctx:context-epoch agent-context)))
      (let ((resumed (ctx:make-agent-context
                      session store context
                      :leaf-id (sess:session-leaf-id session))))
        (is (= 3 (ctx:context-epoch resumed))
            "fresh rebuild derives epoch 3, not the 0 initform")
        (ctx:stage-context-patch
         resumed
         (ctx:make-append-message-patch (sess:make-user-message "after-resume")))
        (ctx:commit-context-patches resumed context)
        (is (= 4 (ctx:context-epoch resumed))
            "the post-resume commit based at 3, advancing to 4 with no collision")))))

(test (context-lens-recode-preserves-projection-and-staged-patches :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context))
           (patch (ctx:make-append-message-patch
                   (sess:make-user-message "staged"))))
      (ctx:stage-context-patch agent-context patch)
      (ctx:recode-context-capsule agent-context)
      (is (equal '("root" "assistant")
                 (mapcar #'sess:message-content
                         (ctx:context-projected-messages agent-context))))
      (is (= 1 (length (ctx:context-staged-patches agent-context)))))))

(test (context-lens-seal-splices-extra-messages-into-snapshot-only :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context))
           (extra (sess:make-user-message "EXTRA")))
      (is (equal '("root" "assistant")
                 (mapcar #'sess:message-content
                         (ctx:transcript-display-items
                          (ctx:seal-context-projection agent-context context))))
          "no key seals the base projection unchanged")
      (is (equal '("root" "assistant" "EXTRA")
                 (mapcar #'sess:message-content
                         (ctx:transcript-display-items
                          (ctx:seal-context-projection agent-context context
                                                       :extra-messages
                                                       (list extra)))))
          "extra messages ride the sealed snapshot")
      (is (equal '("root" "assistant")
                 (mapcar #'sess:message-content
                         (ctx:context-projected-messages agent-context)))
          "the durable projection is untouched"))))

(test (context-lens-seals-typed-view-items-with-purpose-materializers :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (sess:create-session store context :id :context-view-session))
           (assistant (sess:make-assistant-message
                       "assistant"
                       :id :assistant-tool-call-message
                       :metadata
                       (list :tool-calls
                             (list (list :id :call-1
                                         :name "read"
                                         :arguments-json "{}")))))
           (tool-result (sess:make-tool-result-message "tool output"
                                                       :id :tool-result-message
                                                       :tool-call-id :call-1
                                                       :tool-name "read")))
      (sess:append-session-entry
       store
       session
       (sess:make-message-entry (sess:make-user-message "root"
                                                        :id :root-message)
                                :id :root-entry)
       context)
      (sess:append-session-entry
       store
       session
       (sess:make-message-entry assistant :id :assistant-entry)
       context)
      (sess:append-session-entry
       store
       session
       (sess:make-message-entry tool-result :id :tool-result-entry)
       context)
      (let* ((agent-context (ctx:make-agent-context session store context))
             (sealed (ctx:seal-context-projection agent-context context))
             (view (ctx:sealed-context-view sealed))
             (transcript (ctx:transcript-context-view sealed))
             (summarizer (ctx:summarizer-context-view sealed))
             (provider-replay (ctx:provider-replay-context-view sealed))
             (items (ctx:context-view-items view)))
        (is (typep view 'ctx:sealed-context-view))
        (is (eq :editable (ctx:context-view-kind view)))
        (is (eq :transcript (ctx:context-view-kind transcript)))
        (is (eq :summarizer (ctx:context-view-kind summarizer)))
        (is (eq :provider-replay (ctx:context-view-kind provider-replay)))
        (is (equal '(:message :message :tool-result)
                   (mapcar #'ctx:context-view-item-payload-kind items)))
        (is (equal '(:message :message :tool-result)
                   (mapcar #'ctx:context-view-item-payload-kind
                           (ctx:context-view-items transcript))))
        (is (equal '(:message :message :tool-result)
                   (mapcar #'ctx:context-view-item-payload-kind
                           (ctx:context-view-items summarizer))))
        (is (equal '(:message :message :tool-result)
                   (mapcar #'ctx:context-view-item-payload-kind
                           (ctx:context-view-items provider-replay))))
        (is (equal '(:message :root-message)
                   (ctx:context-view-item-id (first items))))
        (is (equal '(:tool-result :tool-result-message)
                   (ctx:context-view-item-id (third items))))
        (is (equal '(:tool-call :call-1)
                   (ctx:context-view-item-group-id (third items))))
        (is (equal '(:entry-id :root-entry
                     :message-id :root-message
                     :source :message)
                   (ctx:context-view-provenance-for-item view (first items))))
        (is (equal '("root" "assistant" "tool output")
                   (mapcar #'sess:message-content
                           (ctx:transcript-display-items sealed))))
        (is (equal '(:message :message :tool-result)
                   (mapcar #'ctx:context-view-item-payload-kind
                           (ctx:summarizer-input-items sealed))))
        (is (equal '(:message :message :tool-result)
                   (mapcar #'ctx:context-view-item-payload-kind
                           (ctx:provider-replay-items sealed))))
        (is (equal '("root" "assistant" "tool output")
                   (mapcar #'sess:message-content
                           (ctx:provider-replay-messages sealed))))))))

(defun context-lens-tool-call-session (context &key tool-result-p)
  (let* ((store (session-log-store context))
         (session (sess:create-session store context :id (gensym "TOOL-CALL-SESSION-"))))
    (sess:append-session-entry
     store
     session
     (sess:make-message-entry
      (sess:make-user-message "run read" :id (gensym "TOOL-USER-")))
     context)
    (sess:append-session-entry
     store
     session
     (sess:make-message-entry
      (sess:make-assistant-message
       ""
       :id (gensym "TOOL-ASSISTANT-")
       :metadata
       (list :tool-calls
             (list (list :id :call-read
                         :name "read"
                         :arguments-json "{\"path\":\"a\"}")))))
     context)
    (when tool-result-p
      (sess:append-session-entry
       store
       session
       (sess:make-message-entry
        (sess:make-tool-result-message "file body"
                                       :id (gensym "TOOL-RESULT-")
                                       :tool-call-id :call-read
                                       :tool-name "read"))
       context))
    (values store session)))

(test (provider-replay-preserves-normal-completed-tool-pairs :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (store session)
        (context-lens-tool-call-session context :tool-result-p t)
      (let* ((agent-context (ctx:make-agent-context session store context))
             (sealed (ctx:seal-context-projection agent-context context))
             (messages (ctx:provider-replay-messages sealed)))
        (is (equal '(:user :assistant :tool-result)
                   (mapcar #'sess:message-role messages)))
        (is (equal :call-read
                   (getf (first (getf (sess:message-metadata
                                        (second messages))
                                       :tool-calls))
                         :id)))
        (is (eq :call-read (sess:tool-call-id (third messages))))))))

(test (provider-replay-fails-closed-on-missing-tool-result :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (multiple-value-bind (store session)
        (context-lens-tool-call-session context)
      (let* ((agent-context (ctx:make-agent-context session store context))
             (sealed (ctx:seal-context-projection agent-context context)))
        (handler-case
            (progn
              (ctx:provider-replay-messages sealed)
              (fail "missing tool result should fail closed"))
          (ctx:context-view-validation-error (condition)
            (let ((diagnostic (ctx:context-view-validation-diagnostic condition)))
              (is (eq :fail-closed (getf diagnostic :repair-policy)))
              (is (equal '(:call-read)
                         (mapcar (lambda (call) (getf call :id))
                                 (getf diagnostic :missing-tool-results))))
              (is (null (getf diagnostic :orphan-tool-results))))))
        (is (typep (sess:session-leaf-entry store session) 'sess:message-entry))
        (is-false (typep (sess:session-leaf-entry store session)
                         'sess:transcript-repair-entry))))))

(test (provider-replay-synthesizes-durable-aborted-repair :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let ((seen-repair-event nil))
      (with-extension-load-authority
        (ext:load-extension-source
         context
         `(ext:defextension transcript-repair-event-capture
            (:provides
             (event-handler :capture-transcript-repair
               :event-type :context/transcript-repair
               :handler ,(lambda (event ctx)
                           (declare (ignore ctx))
                           (setf seen-repair-event event)))))))
    (multiple-value-bind (store session)
        (context-lens-tool-call-session context)
      (let* ((agent-context (ctx:make-agent-context session store context))
             (sealed (ctx:seal-context-projection
                      agent-context
                      context
                      :repair-policy :synthesize-aborted))
             (leaf (sess:session-leaf-entry store session))
             (items (ctx:provider-replay-items sealed))
             (messages (ctx:provider-replay-messages sealed)))
        (is (typep leaf 'sess:transcript-repair-entry))
        (is (eq :synthesized-aborted (sess:entry-repair-kind leaf)))
        (is (equal '(:message :message :repair)
                   (mapcar #'ctx:context-view-item-payload-kind items)))
        (is (equal '((:tool-call :call-read) (:tool-call :call-read))
                   (mapcar #'ctx:context-view-item-group-id
                           (rest items))))
        (is (eq :tool-result (sess:message-role (third messages))))
        (is (eq :call-read (sess:tool-call-id (third messages))))
        (is (getf (sess:message-metadata (third messages))
                  :transcript-repair))
        (is (typep seen-repair-event 'event:event))
        (is (eq :call-read
                (getf (event:event-payload seen-repair-event)
                      :tool-call-id)))
        (is (eq :repair
                (getf (ctx:context-view-provenance-for-item
                       (ctx:sealed-context-view sealed)
                       (third items))
                      :source)))
        (let* ((record (sess:serialize-record leaf))
               (round (sess:deserialize-value record)))
          (is (typep round 'sess:transcript-repair-entry))
          (is (eq :call-read
                  (sess:tool-call-id (sess:entry-message round))))))))))
