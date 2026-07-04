(in-package #:kli/tests)
(in-suite all)

(defun persist-session-to-file (root)
  "Persist a two-message session carrying one committed context patch and
return the session id."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((store (sess:make-file-session-store root))
           (session (sess:create-session store context :id :reload-target))
           (agent-context (ctx:make-agent-context session store context
                                                  :id :persist-context)))
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-user-message "hello" :id :m1) :id :e1)
       context)
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-assistant-message "hi" :id :m2) :id :e2)
       context)
      (ctx:stage-context-patch
       agent-context
       (ctx:make-append-message-patch
        (sess:make-user-message "patched" :id :pm)))
      (ctx:commit-context-patches agent-context context)
      (kli:object-id session))))

(test (load-on-open-switch-replays-committed-patch :fixture interactive-authority)
  "A session persisted by a file-session-store reloads into a fresh context's registered store and drives switch-agent-session unchanged. Committed context patches replay through rebuild-context-from-session, so the reloaded session needs only presence-in-store and a restored leaf."
  (let* ((root (temp-session-root))
         (session-id (persist-session-to-file root)))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (register-runtime-model context "reload-provider" "reload-model"
                              :auth-required-p nil)
      (let* ((path (sess:session-file-path
                    (sess:make-file-session-store root) session-id))
             (service (agent-session-service context))
             (loaded (sess:load-session-file (session-log-store context)
                                             path context)))
        (is (eq session-id (kli:object-id loaded)))
        (is (eq loaded (sess:find-session (session-log-store context)
                                          session-id)))
        (agent-session:switch-agent-session service :default-mode
                                            session-id context)
        (let ((messages (ctx:context-projected-messages
                         (agent-session:agent-session-context
                          service :default-mode context))))
          (is (equal '("hello" "hi" "patched")
                     (mapcar #'sess:message-content messages))))
        (ignore-errors (delete-file path))))))

(defun persist-branched-session-to-file (root)
  "Persist a three-entry session, branch it at the second entry, and return the
branched session id and branch-point entry id."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((store (sess:make-file-session-store root))
           (session (sess:create-session store context :id :branch-source)))
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-user-message "msg-1" :id :bm1)
                                :id :b1)
       context)
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-user-message "msg-2" :id :bm2)
                                :id :b2)
       context)
      (sess:append-session-entry
       store session
       (sess:make-message-entry (sess:make-user-message "msg-3" :id :bm3)
                                :id :b3)
       context)
      (let ((branched (sess:branch-session-at-entry store session :b2 context)))
        (values (kli:object-id branched) :b2)))))

(test (load-on-open-switch-reconstructs-persisted-branch :fixture interactive-authority)
  "A persisted branch reloads from file and reconstructs its chain, so switch-agent-session projects only the branched prefix."
  (let* ((root (temp-session-root)))
    (multiple-value-bind (branched-id branch-point)
        (persist-branched-session-to-file root)
      (multiple-value-bind (context protocol) (agent-session-test-context)
        (declare (ignore protocol))
        (register-runtime-model context "reload-provider" "reload-model"
                                :auth-required-p nil)
        (let* ((path (sess:session-file-path
                      (sess:make-file-session-store root) branched-id))
               (service (agent-session-service context))
               (loaded (sess:load-session-file (session-log-store context)
                                               path context)))
          (is (eq branch-point (sess:session-leaf-id loaded)))
          (is (equal '(:b1 :b2)
                     (mapcar #'kli:object-id
                             (sess:session-branch (session-log-store context)
                                                  loaded nil))))
          (agent-session:switch-agent-session service :default-mode
                                              branched-id context)
          (let ((messages (ctx:context-projected-messages
                           (agent-session:agent-session-context
                            service :default-mode context))))
            (is (equal '("msg-1" "msg-2")
                       (mapcar #'sess:message-content messages)))
            (is (null (find "msg-3" messages
                            :key #'sess:message-content :test #'string=))))
          (ignore-errors (delete-file path)))))))
