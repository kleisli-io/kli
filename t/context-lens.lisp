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
                           (ctx:context-model-messages sealed-before))))
        (ctx:commit-context-patches agent-context context)
        (is (= 1 (ctx:context-epoch agent-context)))
        (is (not (eq initial-leaf (sess:session-leaf-id session))))
        (is (equal '("root" "assistant" "patched")
                   (mapcar #'sess:message-content
                           (ctx:context-projected-messages
                            agent-context))))
        (is (equal '("root" "assistant")
                   (mapcar #'sess:message-content
                           (ctx:context-model-messages sealed-before))))
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
                         (ctx:context-model-messages
                          (ctx:seal-context-projection agent-context context))))
          "no key seals the base projection unchanged")
      (is (equal '("root" "assistant" "EXTRA")
                 (mapcar #'sess:message-content
                         (ctx:context-model-messages
                          (ctx:seal-context-projection agent-context context
                                                       :extra-messages
                                                       (list extra)))))
          "extra messages ride the sealed snapshot")
      (is (equal '("root" "assistant")
                 (mapcar #'sess:message-content
                         (ctx:context-projected-messages agent-context)))
          "the durable projection is untouched"))))
