(in-package #:kli/tests)
(in-suite all)

(defun constant-summarizer (summary &rest details)
  "A summarizer LoL that always returns SUMMARY and DETAILS, ignoring the
prepared messages. Shared by the compaction executor and branch-summary
producer, which both call it with keyword arguments."
  (lambda (&key &allow-other-keys)
    (values summary details)))

(defun message-pairs (messages)
  (mapcar (lambda (m) (cons (sess:message-role m) (sess:message-content m)))
          messages))

(test (integration-fake-provider-session-persists-and-restores :fixture interactive-authority)
  "A fake-provider session accumulates every entry kind plus a committed context
patch and a real compaction, persists to a file, and reloads into a fresh
context. User, assistant, tool, and custom entries all round-trip. Compaction
shortens, leading with the summary and keeping the tail. The committed patch
replays on top of the rebuilt context."
  (let ((root (temp-session-root))
        (sid :integration-main)
        (path nil)
        (pre-count nil))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (bind-agent-session-mode context :session-id sid
                                       :metadata (list :fake-deltas '("assistant reply")))
      (let* ((service (agent-session-service context))
             (store (session-log-store context))
             (session (sess:find-session store sid)))
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "user question" context)
        (flet ((append! (entry)
                 (sess:append-session-entry store session entry context)))
          (append! (sized-message-entry :user 400 :im-bulk-u))
          (append! (sized-message-entry :assistant 400 :im-bulk-a))
          (append! (sess:make-message-entry
                    (sess:make-user-message "recent question") :id :im-rq))
          (append! (sess:make-message-entry
                    (sess:make-assistant-message "recent reply") :id :im-rr))
          (append! (sess:make-message-entry
                    (sess:make-tool-result-message "tool ran" :tool-name "bash")
                    :id :im-tool))
          (append! (sess:make-custom-entry :ui-state :id :im-custom
                                           :data '(:scroll 3))))
        (setf pre-count
              (length (sess:session-context-messages
                       (sess:build-session-context store session))))
        (let ((agent-context (agent-session:agent-session-context
                              service :default-mode context)))
          (ctx:stage-context-patch
           agent-context
           (ctx:make-append-message-patch (sess:make-user-message "patched note")))
          (ctx:commit-context-patches agent-context context))
        (agent-session:recode-compaction-policy
         service :keep-recent-tokens 20
         :summarizer (constant-summarizer "COMPACTED SUMMARY"
                                          :read-files '("ctx.lisp")))
        (agent-session:execute-session-compaction
         service
         (event:make-event :session-compaction-needed
                           :payload (list :mode :default-mode))
         context)
        (let ((file-store (sess:make-file-session-store root)))
          (sess:store-session file-store session context)
          (setf path (sess:session-file-path file-store sid)))))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (register-runtime-model context "reload-provider" "reload-model"
                              :auth-required-p nil)
      (let* ((store (session-log-store context))
             (service (agent-session-service context))
             (loaded (sess:load-session-file store path context)))
        (agent-session:switch-agent-session service :default-mode sid context)
        (let* ((chain-types (mapcar #'type-of (sess:session-branch store loaded nil)))
               (built (sess:session-context-messages
                       (sess:build-session-context store loaded)))
               (projected (ctx:context-projected-messages
                           (agent-session:agent-session-context
                            service :default-mode context))))
          (is (member 'sess:message-entry chain-types))
          (is (member 'sess:custom-entry chain-types))
          (is (member 'sess:compaction-entry chain-types))
          (is (string= "tool ran"
                       (sess:message-content
                        (find :tool-result built :key #'sess:message-role))))
          (is (typep (sess:session-leaf-entry store loaded)
                     'sess:compaction-entry))
          (is (< (length built) pre-count))
          (is (eq :user (sess:message-role (first built))))
          (is (string= "COMPACTED SUMMARY"
                       (sess:message-content (first built))))
          (is (string= "tool ran"
                       (sess:message-content (car (last built)))))
          (is (= (1+ (length built)) (length projected)))
          (is (string= "patched note"
                       (sess:message-content (car (last projected))))))))))

(test integration-branch-summary-persists-and-reconstructs
  "A prior summary on the old branch seeds cumulative file tracking. The
divergent chain reconstructs from the persisted leaf after a round-trip."
  (let ((root (temp-session-root))
        (sid :integration-branch)
        (path nil))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (let* ((store (session-log-store context))
             (session (sess:create-session store context :id sid)))
        (flet ((append! (entry)
                 (sess:append-session-entry store session entry context)))
          (append! (sess:make-message-entry (sess:make-user-message "r1") :id :ib-r1))
          (append! (sess:make-message-entry (sess:make-assistant-message "r2")
                                            :id :ib-r2))
          (append! (sess:make-branch-summary-entry
                    :root "prior" :id :ib-prior
                    :data (list :read-files (list "old.txt"))))
          (append! (sess:make-message-entry (sess:make-user-message "old") :id :ib-old))
          (setf (sess:session-leaf-id session) :ib-r2)
          (append! (sess:make-message-entry (sess:make-assistant-message "target")
                                            :id :ib-target)))
        (sess:produce-branch-summary
         store session :ib-old :ib-target
         :context context
         :summarizer (constant-summarizer "BRANCH SUMMARY"
                                          :read-files '("new.txt")
                                          :modified-files '("mod.txt")))
        (let ((file-store (sess:make-file-session-store root)))
          (sess:store-session file-store session context)
          (setf path (sess:session-file-path file-store sid)))))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (let* ((store (session-log-store context))
             (loaded (sess:load-session-file store path context))
             (leaf (sess:session-leaf-entry store loaded)))
        (is (equal '(:ib-r1 :ib-r2 :ib-target)
                   (subseq (mapcar #'kli:object-id
                                   (sess:session-branch store loaded nil))
                           0 3)))
        (is (typep leaf 'sess:branch-summary-entry))
        (is (eq :ib-r2 (sess:entry-from-id leaf)))
        (is (string= "BRANCH SUMMARY" (sess:entry-summary leaf)))
        (is (equal '("old.txt" "new.txt")
                   (getf (sess:entry-data leaf) :read-files)))
        (is (equal '("mod.txt")
                   (getf (sess:entry-data leaf) :modified-files)))))))

(test integration-corrupt-record-is-restartable-on-reload
  "The bad record is restartable on reload. The good entries still load."
  (let ((root (temp-session-root))
        (sid :integration-corrupt)
        (path nil))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (let* ((store (session-log-store context))
             (session (sess:create-session store context :id sid)))
        (sess:append-session-entry
         store session
         (sess:make-message-entry (sess:make-user-message "good1" :id :ic-m1)
                                  :id :ic-e1)
         context)
        (sess:append-session-entry
         store session
         (sess:make-message-entry (sess:make-assistant-message "good2" :id :ic-m2)
                                  :id :ic-e2)
         context)
        (let ((file-store (sess:make-file-session-store root)))
          (sess:store-session file-store session context)
          (setf path (sess:session-file-path file-store sid)))))
    (with-open-file (stream path :direction :output
                                 :if-exists :append :if-does-not-exist :error)
      (write-record-line stream '(:not-a-valid-record 1 2)))
    (multiple-value-bind (context protocol) (agent-session-test-context)
      (declare (ignore protocol))
      (let* ((store (session-log-store context))
             (loaded (handler-bind
                         ((sess:session-load-error
                            (lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'sess:skip-entry))))
                       (sess:load-session-file store path context))))
        (is (equal '(:ic-e1 :ic-e2)
                   (mapcar #'kli:object-id (sess:session-branch store loaded nil))))
        (is (equal '("good1" "good2")
                   (mapcar #'sess:message-content
                           (sess:session-context-messages
                            (sess:build-session-context store loaded)))))))))
