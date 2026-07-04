(in-package #:kli/tests)
(in-suite all)

(defun load-session-command-stack (context)
  (load-tui-app-stack context)
  (install-extension context snapshot:*snapshot-extension-manifest*)
  (install-extension context
                     session-commands:*session-commands-extension-manifest*))

(defun ensure-session-command-context ()
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-session-command-stack context)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (bind-agent-session-mode context)
    context))

(defun session-command-context-no-default-mode ()
  "Like ensure-session-command-context but binds no mode, so a test owns exactly
the modes it binds."
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-session-command-stack context)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    context))

;;; store listing and deletion

(test session-commands-list-stored-sessions-newest-first
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root)))
    (flet ((seed (id user)
             (let ((session (sess:create-session store ctx :id id)))
               (sess:append-session-entry
                store session
                (sess:make-message-entry (sess:make-user-message user)) ctx)
               (sess:append-session-entry
                store session
                (sess:make-message-entry (sess:make-assistant-message "reply"))
                ctx))))
      (seed :sc-1 "first one")
      (seed :sc-2 "second one")
      (seed :sc-3 "third one"))
    (let ((rows (sess:list-stored-sessions store)))
      (is (equal '(:sc-3 :sc-2 :sc-1)
                 (mapcar (lambda (row) (getf row :id)) rows)))
      (is (= 2 (getf (first rows) :entry-count)))
      (is (string= "third one" (getf (first rows) :preview)))
      (is (string= "first one"
                   (getf (find :sc-1 rows :key (lambda (row) (getf row :id)))
                         :preview))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test session-commands-delete-stored-session-removes-file
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root)))
    (sess:create-session store ctx :id :del-1 :metadata '(:name "doomed"))
    (sess:create-session store ctx :id :del-2 :metadata '(:name "spared"))
    (let ((path (sess:session-file-path store :del-1)))
      (is (probe-file path))
      (is (eq t (sess:delete-stored-session store :del-1)))
      (is (null (probe-file path)))
      (is (null (sess:find-session store :del-1)))
      (is (equal '(:del-2)
                 (mapcar (lambda (row) (getf row :id))
                         (sess:list-stored-sessions store))))
      (is (null (sess:delete-stored-session store :del-1))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

;;; agent/session lifecycle ops

(test (session-commands-rename-and-info :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (bind-agent-session-mode context)))
      (is (string= "my-chat"
                   (agent-session:rename-agent-session service :default-mode
                                                       "my-chat" context)))
      (let ((info (agent-session:session-mode-info service :default-mode context)))
        (is (string= "my-chat" (getf info :name)))
        (is (eq :agent-session-test-session (getf info :id)))
        (is (string= "agent-session-provider" (getf info :provider)))
        (is (string= "agent-session-model" (getf info :model)))
        (is (integerp (getf info :entry-count)))))))

(test session-commands-info-nil-when-mode-unbound
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (agent-session-service context)))
      (is (null (agent-session:session-mode-info service :missing-mode context))))))

(test (session-commands-resume-switches-bound-session :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((service (bind-agent-session-mode context))
           (store (session-log-store context))
           (second (sess:create-session store context :id :resume-second)))
      (is (eq :resume-second
              (agent-session:resume-agent-session
               service :default-mode (kli:object-id second) context)))
      (is (eq :resume-second (mode-session-id service :default-mode))))))

(test (session-commands-compact-appends-entry :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let* ((service (bind-agent-session-mode
                     context
                     :metadata (list :fake-deltas '("STRUCTURED SUMMARY"))))
           (store (session-log-store context))
           (session (sess:find-session store :agent-session-test-session)))
      (flet ((append! (entry)
               (sess:append-session-entry store session entry context)))
        (append! (sized-message-entry :user 400 :sc-u1))
        (append! (sized-message-entry :assistant 400 :sc-a1))
        (append! (sized-message-entry :user 40 :sc-u2)))
      (agent-session:recode-compaction-policy service :keep-recent-tokens 5)
      (let ((entry (agent-session:compact-agent-session
                    service :default-mode context
                    :custom-instructions "focus on the bug")))
        (is (typep entry 'sess:compaction-entry))
        (is (string= "STRUCTURED SUMMARY" (sess:entry-summary entry)))
        (is (eq entry (sess:session-leaf-entry store session)))))))

(test (session-commands-compact-noop-on-small-session :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (bind-agent-session-mode
                    context :metadata (list :fake-deltas '("S")))))
      (is (null (agent-session:compact-agent-session
                 service :default-mode context))))))

;;; command runners

(test (session-commands-name-sets-and-reports :fixture interactive-authority)
  (let ((context (ensure-session-command-context)))
    (is (search "Named: hi there"
                (command-result-text
                 context
                 (invoke-test-command context :name
                                      (list :mode-id :default-mode
                                            :tail "hi there")))))
    (is (search "Name: hi there"
                (command-result-text
                 context
                 (invoke-test-command context :name
                                      (list :mode-id :default-mode)))))))

(test (session-commands-session-reports-id-and-model :fixture interactive-authority)
  (let* ((context (ensure-session-command-context))
         (text (command-result-text
                context
                (invoke-test-command context :session
                                     (list :mode-id :default-mode)))))
    (is (search "id: agent-session-test-session" text))
    (is (search "model: agent-session-provider/agent-session-model" text))
    (is (search "file: (memory)" text))
    (is (search "tokens:" text))))

(test (session-commands-resume-lists-nothing-for-in-memory-store :fixture interactive-authority)
  (let ((context (ensure-session-command-context)))
    (is (search "No saved sessions."
                (command-result-text
                 context
                 (invoke-test-command context :resume
                                      (list :mode-id :default-mode)))))))

(defun install-file-session-store (context root)
  "Swap CONTEXT's in-memory :session-store for a file-backed one rooted at
ROOT, returning the new store."
  (let ((registry (kli:context-registry context)))
    (kli:remove-live-object registry :session-store)
    (kli:register-live-object registry (sess:make-file-session-store root))))

(test (session-commands-resume-reports-dropped-tail :fixture interactive-authority)
  "Resuming a session whose file lost a torn trailing record reports the drop
alongside the resume confirmation."
  (let* ((context (ensure-session-command-context))
         (root (temp-session-root))
         (store (install-file-session-store context root))
         (path (sess:session-file-path store :stored-torn)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-record-line s (sess:serialize-record
                            (sess:make-session-file-header :stored-torn
                                                           :leaf-id :e1)))
      (write-record-line s (sess:serialize-record
                            (sess:make-message-entry
                             (sess:make-user-message "prefix" :id :m1) :id :e1)))
      (write-string "(:record :type :message-entry :fields (:content \"torn" s))
    (let ((text (command-result-text
                 context
                 (invoke-test-command context :resume
                                      (list :mode-id :default-mode
                                            :words '("stored-torn"))))))
      (is (search "Resumed stored-torn." text))
      (is (search "Dropped a corrupt trailing record." text)))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-resume-refuses-corrupt-session :fixture interactive-authority)
  "A corrupt session file lists as a (corrupt) row and refuses to resume with
an error result instead of signaling."
  (let* ((context (ensure-session-command-context))
         (root (temp-session-root))
         (store (install-file-session-store context root))
         (path (sess:session-file-path store :broken)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-string "not a session" s))
    (is (search "(corrupt)"
                (command-result-text
                 context
                 (invoke-test-command context :resume
                                      (list :mode-id :default-mode)))))
    (let ((result (invoke-test-command context :resume
                                       (list :mode-id :default-mode
                                             :words '("broken")))))
      (is (commands:command-result-error-p result))
      (is (search "corrupt" (command-result-text context result))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-compact-reports-nothing-to-compact :fixture interactive-authority)
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (load-session-command-stack context)
    (install-extension context *test-fake-model-provider-extension-manifest*)
    (bind-agent-session-mode context :metadata (list :fake-deltas '("S")))
    (is (search "Nothing to compact."
                (command-result-text
                 context
                 (invoke-test-command context :compact
                                      (list :mode-id :default-mode)))))))

(test (session-commands-compact-refuses-while-busy :fixture interactive-authority)
  "Compacting under an in-flight turn would mutate the entries the turn is
built on, so the runner refuses while the session is busy."
  (let* ((context (ensure-session-command-context))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding))))
    (agents:set-agent-state agent :running)
    (let ((result (invoke-test-command context :compact
                                       (list :mode-id :default-mode))))
      (is (commands:command-result-error-p result))
      (is (search "busy" (command-result-text context result))))
    (agents:set-agent-state agent :idle)))

(test (session-commands-compact-defers-to-worker-spawner :fixture interactive-authority)
  "With a :worker-spawner in the arguments the runner hands the compaction
body to the spawner and returns an empty result -- the outcome reports
through the compaction events instead."
  (let ((context (ensure-session-command-context))
        (spawned '()))
    (let ((result (invoke-test-command
                   context :compact
                   (list :mode-id :default-mode
                         :worker-spawner (lambda (thunk)
                                           (push thunk spawned))))))
      (is (null (commands:command-result-content result)))
      (is (not (commands:command-result-error-p result)))
      (is (= 1 (length spawned)))
      (multiple-value-bind (entry status) (funcall (first spawned))
        (is (null entry))
        (is (eq :nothing-to-compact status))))))

(test (session-commands-compact-reports-failure-distinctly :fixture interactive-authority)
  "A failed summarization reports as an error result, no longer collapsing to
\"Nothing to compact.\". Fault policy binds NIL for the production barrier."
  (let* ((context (ensure-session-command-context))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (store (session-log-store context))
         (session (sess:find-session store :agent-session-test-session)))
    (flet ((append! (entry)
             (sess:append-session-entry store session entry context)))
      (append! (sized-message-entry :user 400 :scf-u1))
      (append! (sized-message-entry :assistant 400 :scf-a1))
      (append! (sized-message-entry :user 40 :scf-u2)))
    (agent-session:recode-compaction-policy
     service :keep-recent-tokens 5
     :summarizer (lambda (&rest args)
                   (declare (ignore args))
                   (error "summarizer exploded")))
    (let ((result (let ((ext:*extension-fault-policy* nil))
                    (invoke-test-command context :compact
                                         (list :mode-id :default-mode)))))
      (is (commands:command-result-error-p result))
      (is (search "Compaction failed: " (command-result-text context result)))
      (is (search "summarizer exploded"
                  (command-result-text context result))))))

(test (session-commands-compact-via-tui-runs-off-loop-thread :fixture interactive-authority)
  "/compact submitted through the TUI defers to the worker spawner; the
outcome lands in the transcript as a compaction event row once the worker
settles."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48)))
    (tui-app:tui-app-feed app "/compact" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (settle-tui-app app)
    (let ((texts (mapcar #'tui-transcript:event-text
                         (tui-app:tui-app-transcript-events app))))
      (is (find "Nothing to compact." texts :test #'string=)
          "the manual no-op outcome projects from the worker's finished event"))))

;;; continue-last boot helpers

(test session-commands-continue-last-requested-p-detects-flags
  (is (app:continue-last-requested-p :argv '("-c")))
  (is (app:continue-last-requested-p :argv '("--continue")))
  (is (app:continue-last-requested-p :argv '("--profile" "x" "-c")))
  (is (not (app:continue-last-requested-p :argv '())))
  (is (not (app:continue-last-requested-p :argv '("--profile" "x")))))

(test session-commands-latest-session-id-returns-newest
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root)))
    (sess:create-session store ctx :id :latest-a :metadata '(:name "older"))
    (sess:create-session store ctx :id :latest-b :metadata '(:name "newer"))
    (is (eq :latest-b (app::latest-session-id store)))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test session-commands-latest-session-id-skips-corrupt-rows
  "Continue-last boot must never try to resume a corrupt file: a corrupt
newest row is skipped in favor of the newest loadable session."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root)))
    (sess:create-session store ctx :id :latest-good :metadata '(:name "good"))
    (with-open-file (s (sess:session-file-path store :zzz-corrupt)
                       :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
      (write-string "garbage" s))
    (is (eq :latest-good (app::latest-session-id store)))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-stay-model-visible :fixture interactive-authority)
  (let ((context (ensure-session-command-context)))
    (let ((provider (command-provider context)))
      (dolist (name '(:compact :session :name :resume :branches))
        (is (getf (commands:command-metadata
                   (ext:provider-call provider :find-command name))
                  :model-visible t)
            "Command ~A must not opt out of model visibility" name)))))

(test (session-commands-snapshot-registration-drains-on-deactivation
       :fixture interactive-authority)
  "The /snapshot command registers while session-commands is active and drains
from the commands provider when the extension is retracted -- the registration
is one entry in the contribution list every sibling command rides."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (handle (load-session-command-stack context))
         (commands (command-provider context)))
    (is (ext:provider-call commands :find-command :snapshot)
        "the snapshot command is live while session-commands is active")
    (ext:retract-manifest handle protocol context)
    (is (null (ext:provider-call commands :find-command :snapshot))
        "retracting session-commands drains the snapshot registration")))

(test (session-commands-snapshot-completer-subcommands-then-paths
       :fixture interactive-authority)
  "The /snapshot completer offers save/restore first, then real cwd path
completion after the chosen subcommand. Path candidates carry the subcommand
prefix so each replaces the whole tail, and hidden entries stay hidden."
  (let* ((context (ensure-session-command-context))
         (command (ext:provider-call (command-provider context)
                                     :find-command :snapshot))
         (complete (commands:command-completer command)))
    (is (not (null complete)) "the snapshot command carries a completer")
    (let ((blank (funcall complete command "")))
      (is (equal '("save" "restore") (getf blank :candidates)))
      (is (string= "save|restore" (getf blank :hint))))
    (is (equal '("save") (getf (funcall complete command "sav") :candidates)))
    (let ((root (make-completion-file-tree)))
      (uiop:with-current-directory (root)
        (let* ((result (funcall complete command "save "))
               (candidates (getf result :candidates)))
          (is (string= "<path>" (getf result :hint)))
          (is (member "save src/" candidates :test #'string=))
          (is (member "save README.md" candidates :test #'string=))
          (is (every (lambda (c) (text:string-prefix-p "save " c)) candidates))
          (is (notany (lambda (c) (search ".hidden" c)) candidates)))))))

(test (session-commands-rewind-steps-back-one-turn :fixture interactive-authority)
  (let* ((context (ensure-session-command-context))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding)))
         (store (agents:agent-store agent))
         (session (agents:agent-session agent))
         (old-session-id (kli:object-id session)))
    (is (search "Nothing to rewind."
                (command-result-text
                 context
                 (invoke-test-command context :rewind
                                      (list :mode-id :default-mode)))))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "prompt")) context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "reply")) context)
    (is (search "Rewound one turn."
                (command-result-text
                 context
                 (invoke-test-command context :rewind
                                      (list :mode-id :default-mode)))))
    (is (not (eq old-session-id (mode-session-id service :default-mode))))))

(test (session-commands-rewind-refuses-while-busy :fixture interactive-authority)
  "Rewinding under an in-flight turn would switch the session out from under
the worker, so the runner refuses while the session is busy."
  (let* ((context (ensure-session-command-context))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding))))
    (agents:set-agent-state agent :running)
    (let ((result (invoke-test-command context :rewind
                                       (list :mode-id :default-mode))))
      (is (commands:command-result-error-p result))
      (is (search "busy" (command-result-text context result))))
    (agents:set-agent-state agent :idle)))

(test (session-commands-rewind-stays-model-visible :fixture interactive-authority)
  (let ((context (ensure-session-command-context)))
    (is (getf (commands:command-metadata
               (ext:provider-call (command-provider context)
                                  :find-command :rewind))
              :model-visible t))))

;;; /branches

(test session-commands-listing-rows-carry-branch-metadata
  "A branched session's listing row carries the branch metadata its header
stores and previews the first prompt after the branch point, while a plain
session's row leaves them empty."
  (let* ((root (temp-session-root))
         (ctx (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (parent (sess:create-session store ctx :id :tree-parent)))
    (sess:append-session-entry
     store parent
     (sess:make-message-entry (sess:make-user-message "shared prompt") :id :tp-e1)
     ctx)
    (sess:append-session-entry
     store parent
     (sess:make-message-entry (sess:make-assistant-message "shared reply")
                              :id :tp-e2)
     ctx)
    (let ((branch (sess:branch-session-at-entry store parent :tp-e2 ctx)))
      (sess:append-session-entry
       store branch
       (sess:make-message-entry (sess:make-user-message "divergent prompt"))
       ctx)
      (let* ((rows (sess:list-stored-sessions store))
             (branch-row (find (kli:object-id branch) rows
                               :key (lambda (row) (getf row :id))))
             (parent-row (find :tree-parent rows
                               :key (lambda (row) (getf row :id)))))
        (is (eq :tree-parent (getf branch-row :branched-from)))
        (is (eq :tp-e2 (getf branch-row :branched-at)))
        (is (string= "divergent prompt" (getf branch-row :branch-preview)))
        (is (string= "shared prompt" (getf branch-row :branch-point-preview))
            "the prompt the branch rewound past")
        (is (string= "shared prompt" (getf branch-row :preview))
            "the plain preview is the shared prefix's first prompt")
        (is (equal '(:tp-e1 :tp-e2) (getf parent-row :entry-ids))
            "rows carry entry ids in file order for branch-point ordering")
        (is (null (getf parent-row :branched-from)))
        (is (null (getf parent-row :branch-preview)))
        (is (null (getf parent-row :branch-point-preview)))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test session-commands-session-forest-orders-depth-first
  "SESSION-FOREST arranges listing rows depth-first by :branched-from with
:depth and :tree-prefix annotations. Siblings order by where each diverged
in the parent (:branched-at position in the parent's :entry-ids), not by
listing order, and unknown branch points keep listing order at the end. Rows
without a stored parent among the rows are roots, so files predating
branch metadata degrade to a flat list, and metadata cycles still list
every row instead of dropping them."
  (let ((forest (sess:session-forest
                 (list (list :id :f-c :branched-from :f-a :branched-at :e9)
                       (list :id :f-b :branched-from :f-a :branched-at :e1)
                       (list :id :f-a :entry-ids '(:e1 :e9))
                       (list :id :f-orphan :branched-from :f-missing)))))
    (is (equal '((:f-a 0 "") (:f-b 1 "├─ ") (:f-c 1 "└─ ") (:f-orphan 0 ""))
               (mapcar (lambda (row) (list (getf row :id)
                                           (getf row :depth)
                                           (getf row :tree-prefix)))
                       forest))
        "siblings order by branch point, glyphs mark fork and last fork"))
  (let ((nested (sess:session-forest
                 (list (list :id :n-a :entry-ids '(:e1 :e2))
                       (list :id :n-b :branched-from :n-a :branched-at :e1)
                       (list :id :n-c :branched-from :n-a :branched-at :e2)
                       (list :id :n-d :branched-from :n-b)))))
    (is (equal '((:n-a 0 "") (:n-b 1 "├─ ") (:n-d 2 "│  └─ ")
                 (:n-c 1 "└─ "))
               (mapcar (lambda (row) (list (getf row :id)
                                           (getf row :depth)
                                           (getf row :tree-prefix)))
                       nested))
        "a grandchild under a non-last child carries the continuation bar"))
  (let ((cyclic (sess:session-forest
                 (list (list :id :cy-a :branched-from :cy-b)
                       (list :id :cy-b :branched-from :cy-a)))))
    (is (equal '((:cy-a 0 "") (:cy-b 1 "└─ "))
               (mapcar (lambda (row) (list (getf row :id)
                                           (getf row :depth)
                                           (getf row :tree-prefix)))
                       cyclic)))))

(test (session-commands-branches-lists-forest-headless :fixture interactive-authority)
  "/branches renders the stored-session forest as indented text rows marking
the active session, and degrades to a notice when nothing is stored."
  (let ((context (ensure-session-command-context)))
    (is (search "No saved sessions."
                (command-result-text
                 context
                 (invoke-test-command context :branches
                                      (list :mode-id :default-mode)))))
    (let* ((root (temp-session-root))
           (store (install-file-session-store context root))
           (parent (sess:create-session store context :id :tree-list-parent)))
      (sess:append-session-entry
       store parent
       (sess:make-message-entry (sess:make-user-message "shared prompt")
                                :id :tl-e1)
       context)
      (let* ((branch (sess:branch-session-at-entry store parent :tl-e1 context))
             (branch-display (string-downcase
                              (symbol-name (kli:object-id branch)))))
        (sess:append-session-entry
         store branch
         (sess:make-message-entry
          (sess:make-user-message (format nil "divergent~%prompt")))
         context)
        (invoke-test-command context :resume
                             (list :mode-id :default-mode
                                   :words (list branch-display)))
        (let ((text (command-result-text
                     context
                     (invoke-test-command context :branches
                                          (list :mode-id :default-mode)))))
          (is (search "  tree-list-parent  shared prompt" text))
          (is (search (format nil "* └─ ~A  @\"shared prompt\" -> divergent prompt"
                              branch-display)
                      text)
              "the branch row carries trunk glyphs, the active marker, the
prompt it rewound past, and its divergent prompt flattened to one line")))
      (ignore-errors (uiop:delete-directory-tree root :validate (constantly t))))))

(test (session-commands-branches-opens-menu-with-app :fixture interactive-authority)
  "With a TUI app riding on the arguments, /branches opens a selection menu
over the session forest, depth-indented with branch-point prompts as labels,
and accepting a row switches onto that session. With nothing stored it
answers directly instead of opening an empty menu."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service)))
    (is (search "No saved sessions."
                (command-result-text
                 context
                 (invoke-test-command context :branches
                                      (list :mode-id :default-mode :app app)))))
    (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app))))
    (let* ((root (temp-session-root))
           (store (install-file-session-store context root))
           (parent (sess:create-session store context :id :tree-menu-parent)))
      (sess:append-session-entry
       store parent
       (sess:make-message-entry (sess:make-user-message "shared prompt")
                                :id :tm-e1)
       context)
      (let ((branch (sess:branch-session-at-entry store parent :tm-e1 context)))
        (sess:append-session-entry
         store branch
         (sess:make-message-entry (sess:make-user-message "divergent prompt"))
         context)
        (let ((result (invoke-test-command context :branches
                                           (list :mode-id :default-mode
                                                 :app app))))
          (is (null (commands:command-result-content result))
              "the menu is the feedback, the result stays empty"))
        (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
          (is (not (null popup)))
          (is (equal (list "tree-menu-parent"
                           (string-downcase
                            (symbol-name (kli:object-id branch))))
                     (mapcar #'tui-editor:completion-candidate-insert
                             (tui-editor:completion-popup-candidates popup))))
          (is (equal '("  shared prompt"
                       "  └─ @\"shared prompt\" -> divergent prompt")
                     (mapcar #'tui-editor:completion-candidate-description
                             (tui-editor:completion-popup-candidates popup)))
              "labels carry trunk glyphs and branch context"))
        (tui-editor::accept-completion (tui-app:tui-app-editor app))
        (is (eq :tree-menu-parent (mode-session-id service :default-mode))
            "accepting the selected row switches onto that session")
        (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
            "the menu closes on accept"))
      (ignore-errors (uiop:delete-directory-tree root :validate (constantly t))))))

(test (session-commands-menu-survives-typed-submit :fixture interactive-authority)
  "A selection menu opened by a typed command survives the input cycle that
submitted it: the post-input completion refresh derives token popups from
the buffer and must not clobber a menu the dispatched command just opened.
The double-Esc arc never sees this -- Esc routes around the editor -- so
typed /branches and typed /rewind pin it here."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (editor (tui-app:tui-app-editor app))
         (root (temp-session-root))
         (store (install-file-session-store context root))
         (typed-root (sess:create-session store context :id :typed-menu-root)))
    (sess:append-session-entry
     store typed-root
     (sess:make-message-entry (sess:make-user-message "typed root prompt"))
     context)
    (tui-app:tui-app-feed app "/branches" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (let ((popup (tui-editor:editor-completion editor)))
      (is (not (null popup)) "typed /branches leaves its menu open")
      (is (eq :selection (tui-editor:completion-popup-kind popup))))
    (tui-editor::dismiss-editor-completion editor)
    (tui-app:tui-app-feed app "/resume" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (let ((popup (tui-editor:editor-completion editor)))
      (is (not (null popup)) "typed bare /resume leaves its menu open")
      (is (eq :selection (tui-editor:completion-popup-kind popup))))
    (tui-editor::dismiss-editor-completion editor)
    (let* ((service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding)))
           (agent-store (agents:agent-store agent))
           (session (agents:agent-session agent)))
      (sess:append-session-entry
       agent-store session
       (sess:make-message-entry (sess:make-user-message "a prompt")) context)
      (sess:append-session-entry
       agent-store session
       (sess:make-message-entry (sess:make-assistant-message "a reply"))
       context))
    (tui-app:tui-app-feed app "/rewind" :render nil)
    (tui-app:tui-app-feed app (string #\Return) :render nil)
    (let ((popup (tui-editor:editor-completion editor)))
      (is (not (null popup)) "typed /rewind leaves its menu open")
      (is (eq :selection (tui-editor:completion-popup-kind popup))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-rewind-accepts-numeric-tail :fixture interactive-authority)
  "/rewind N steps back N user turns directly, bypassing the menu even with
a TUI app at hand -- an explicit count is a command, the menu is for
browsing. A count past the available turns reports nothing to rewind, and a
tail that is not a positive integer reports an error instead of silently
acting."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding)))
         (store (agents:agent-store agent))
         (session (agents:agent-session agent))
         (old-session-id (kli:object-id session)))
    (flet ((turn (prompt reply)
             (sess:append-session-entry
              store session
              (sess:make-message-entry (sess:make-user-message prompt))
              context)
             (sess:append-session-entry
              store session
              (sess:make-message-entry (sess:make-assistant-message reply))
              context)))
      (turn "first prompt" "first reply")
      (turn "second prompt" "second reply"))
    (let ((result (invoke-test-command context :rewind
                                       (list :mode-id :default-mode
                                             :tail "junk"))))
      (is (commands:command-result-error-p result))
      (is (search "Not a turn count: junk"
                  (command-result-text context result))))
    (is (search "Nothing to rewind."
                (command-result-text
                 context
                 (invoke-test-command context :rewind
                                      (list :mode-id :default-mode
                                            :tail "5")))))
    (is (search "Rewound 2 turns."
                (command-result-text
                 context
                 (invoke-test-command context :rewind
                                      (list :mode-id :default-mode
                                            :tail "2"
                                            :app app)))))
    (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
        "an explicit count acts directly, no menu opens")
    (is (not (eq old-session-id (mode-session-id service :default-mode))))))

(test (session-commands-branches-menu-refuses-corrupt-row :fixture interactive-authority)
  "A corrupt session file stays visible in the branches menu but accepting
it refuses to switch, leaving the bound session in place."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (root (temp-session-root))
         (store (install-file-session-store context root))
         (path (sess:session-file-path store :broken-branch)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-string "not a session" s))
    (let ((result (invoke-test-command context :branches
                                       (list :mode-id :default-mode :app app))))
      (is (null (commands:command-result-content result))))
    (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
      (is (not (null popup)))
      (is (search "(corrupt)"
                  (tui-editor:completion-candidate-description
                   (first (tui-editor:completion-popup-candidates popup))))))
    (tui-editor::accept-completion (tui-app:tui-app-editor app))
    (is (eq :agent-session-test-session (mode-session-id service :default-mode))
        "the corrupt row is refused, the bound session stays")
    (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app))))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-rewind-opens-menu-with-app :fixture interactive-authority)
  "With a TUI app riding on the arguments, /rewind opens the selection menu
on the app's editor and reports nothing -- the menu is the feedback. With no
targets it answers directly instead of opening an empty menu."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding)))
         (store (agents:agent-store agent))
         (session (agents:agent-session agent)))
    (is (search "Nothing to rewind."
                (command-result-text
                 context
                 (invoke-test-command context :rewind
                                      (list :mode-id :default-mode :app app)))))
    (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app))))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "a prompt")) context)
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-assistant-message "a reply")) context)
    (let ((result (invoke-test-command context :rewind
                                       (list :mode-id :default-mode :app app))))
      (is (null (commands:command-result-content result))
          "the menu is the feedback, the result stays empty"))
    (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
      (is (not (null popup)))
      (is (string= "a prompt"
                   (tui-editor:completion-candidate-description
                    (first (tui-editor:completion-popup-candidates popup))))))))

(test (session-commands-resume-opens-menu-with-app :fixture interactive-authority)
  "With a TUI app riding on the arguments, bare /resume opens a selection
menu over the stored sessions, newest first with the active session marked,
and accepting a row switches onto that session. A selector tail still acts
directly, and with nothing stored the command answers instead of opening an
empty menu."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service)))
    (is (search "No saved sessions."
                (command-result-text
                 context
                 (invoke-test-command context :resume
                                      (list :mode-id :default-mode :app app)))))
    (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app))))
    (let* ((root (temp-session-root))
           (store (install-file-session-store context root))
           (older (sess:create-session store context :id :resume-menu-a))
           (newer (sess:create-session store context :id :resume-menu-b)))
      (sess:append-session-entry
       store older
       (sess:make-message-entry (sess:make-user-message "alpha prompt"))
       context)
      (sess:append-session-entry
       store newer
       (sess:make-message-entry (sess:make-user-message "beta prompt"))
       context)
      (let ((result (invoke-test-command context :resume
                                         (list :mode-id :default-mode
                                               :app app))))
        (is (null (commands:command-result-content result))
            "the menu is the feedback, the result stays empty"))
      (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
        (is (not (null popup)))
        (is (eq :selection (tui-editor:completion-popup-kind popup)))
        (is (equal '("resume-menu-b" "resume-menu-a")
                   (mapcar #'tui-editor:completion-candidate-insert
                           (tui-editor:completion-popup-candidates popup)))
            "rows list newest first")
        (is (equal '("  1 msgs  beta prompt" "  1 msgs  alpha prompt")
                   (mapcar #'tui-editor:completion-candidate-description
                           (tui-editor:completion-popup-candidates popup)))
            "descriptions carry entry counts and opening prompts"))
      (tui-editor::accept-completion (tui-app:tui-app-editor app))
      (is (eq :resume-menu-b (mode-session-id service :default-mode))
          "accepting the selected row switches onto that session")
      (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
          "the menu closes on accept")
      (invoke-test-command context :resume
                           (list :mode-id :default-mode :app app))
      (let* ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app)))
             (description (tui-editor:completion-candidate-description
                           (first (tui-editor:completion-popup-candidates
                                   popup)))))
        (is (eql 0 (search "* " description)) "the active session is marked")
        (is (search "beta prompt" description)))
      (tui-editor::dismiss-editor-completion (tui-app:tui-app-editor app))
      (is (search "Resumed resume-menu-a."
                  (command-result-text
                   context
                   (invoke-test-command context :resume
                                        (list :mode-id :default-mode
                                              :app app
                                              :words (list "resume-menu-a")))))
          "a selector acts directly even with a TUI at hand")
      (is (null (tui-editor:editor-completion (tui-app:tui-app-editor app)))
          "an explicit selector opens no menu")
      (ignore-errors (uiop:delete-directory-tree root :validate (constantly t))))))

(test (session-commands-pickers-skip-blank-sessions :fixture interactive-authority)
  "Entry-less unnamed session files -- the empties old boots left behind --
carry nothing to resume, so the /resume and /branches pickers skip them
while corrupt rows stay visible."
  (let* ((context (ensure-session-command-context))
         (app (tui-app:make-tui-app :context context :columns 48))
         (root (temp-session-root))
         (store (install-file-session-store context root))
         (real (sess:create-session store context :id :picker-real)))
    (sess:append-session-entry
     store real
     (sess:make-message-entry (sess:make-user-message "real work"))
     context)
    (sess::write-session-file store (sess::make-session :id :picker-blank))
    (is (not (null (probe-file (sess:session-file-path store :picker-blank))))
        "the legacy header-only file exists on disk")
    (invoke-test-command context :resume
                         (list :mode-id :default-mode :app app))
    (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
      (is (not (null popup)))
      (is (equal '("picker-real")
                 (mapcar #'tui-editor:completion-candidate-insert
                         (tui-editor:completion-popup-candidates popup)))
          "the blank row is skipped by the resume menu"))
    (tui-editor::dismiss-editor-completion (tui-app:tui-app-editor app))
    (invoke-test-command context :branches
                         (list :mode-id :default-mode :app app))
    (let ((popup (tui-editor:editor-completion (tui-app:tui-app-editor app))))
      (is (equal '("picker-real")
                 (mapcar #'tui-editor:completion-candidate-insert
                         (tui-editor:completion-popup-candidates popup)))
          "the blank row is skipped by the branches menu"))
    (tui-editor::dismiss-editor-completion (tui-app:tui-app-editor app))
    (let ((text (command-result-text
                 context
                 (invoke-test-command context :resume
                                      (list :mode-id :default-mode)))))
      (is (search "picker-real" text))
      (is (null (search "picker-blank" text))
          "the headless listing skips the blank row too"))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-snapshot-restore-refuses-while-busy
       :fixture interactive-authority)
  "Restore replaces every mode's state, so it refuses while a turn is in flight
rather than tearing the running session out from under it."
  (let* ((context (ensure-session-command-context))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service)))
         (agent (kli:find-live-object
                 (kli:context-registry context)
                 (agent-session:mode-binding-agent-id binding)))
         (root (temp-session-root))
         (path (namestring (merge-pathnames "snap.lisp" root))))
    (ensure-directories-exist path)
    (invoke-test-command context :snapshot
                         (list :mode-id :default-mode
                               :tail (format nil "save ~A" path)))
    (agents:set-agent-state agent :running)
    (let ((result (invoke-test-command
                   context :snapshot
                   (list :mode-id :default-mode
                         :tail (format nil "restore ~A" path)))))
      (is (commands:command-result-error-p result))
      (is (search "busy" (command-result-text context result))))
    (agents:set-agent-state agent :idle)
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-snapshot-restore-replays-bound-session
       :fixture interactive-authority)
  "An in-place /snapshot restore re-binds the mode through the canonical switch,
whose :session-switch the TUI listener replays -- the transcript shows the saved
conversation again with no TUI-specific restore code, the mode is bound back onto
the saved session, and the footer follows the rebind back to that session's model.
A clean restore reports no unfocused loss."
  (let* ((context (ensure-session-command-context))
         (service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (app (tui-app:make-tui-app :context context :columns 48))
         (store (session-log-store context))
         (saved (sess:find-session store :agent-session-test-session))
         (root (temp-session-root))
         (path (namestring (merge-pathnames "snap.lisp" root))))
    (ensure-directories-exist path)
    (flet ((footer () (app::context-usage-footer-text context :default-mode)))
      (sess:append-session-entry
       store saved
       (sess:make-message-entry (sess:make-user-message "RESTORE-ME")) context)
      (sess:append-session-entry
       store saved
       (sess:make-model-change-entry "agent-session-provider" "agent-session-model")
       context)
      (invoke-test-command context :snapshot
                           (list :mode-id :default-mode :app app
                                 :tail (format nil "save ~A" path)))
      (register-runtime-model context "other-provider" "other-model"
                              :auth-required-p nil
                              :metadata (list :fake-deltas '("x")))
      (let ((other (sess:create-session store context :id :other-session)))
        (sess:append-session-entry
         store other
         (sess:make-model-change-entry "other-provider" "other-model") context)
        (agent-session:switch-agent-session
         service :default-mode (kli:object-id other) context))
      (is (notany (lambda (te)
                    (let ((text (tui-transcript:event-text te)))
                      (and text (search "RESTORE-ME" text))))
                  (tui-app:tui-app-transcript-events app))
          "switching away clears the saved conversation")
      (is (search "other-model" (footer))
          "the footer tracks the switched-to session's model")
      (let ((result (invoke-test-command
                     context :snapshot
                     (list :mode-id :default-mode :app app
                           :tail (format nil "restore ~A" path)))))
        (is (null (search "unavailable" (command-result-text context result)))
            "a clean restore reports no unfocused loss"))
      (is (some (lambda (te)
                  (let ((text (tui-transcript:event-text te)))
                    (and text (search "RESTORE-ME" text))))
                (tui-app:tui-app-transcript-events app))
          "restore replays the saved session into the transcript")
      (is (eq :agent-session-test-session (tui-app:tui-app-active-session-id app))
          "restore re-binds the mode onto the saved session")
      (is (search "agent-session-model" (footer))
          "the footer follows the restore back to the bound session's model")
      (ignore-errors (uiop:delete-directory-tree root :validate (constantly t))))))

(test (session-commands-snapshot-restore-reports-unavailable-focused-mode
       :fixture interactive-authority)
  "A cross-image /snapshot restore whose focused mode's session file is gone tells
the user which mode was lost: the focused mode cannot rebind, so nothing is focused
and the command result names it. Driven through the real command, file store and
all -- the prior coverage asserted only the service slot, never the result text."
  (let* ((root (temp-session-root))
         (path (namestring (merge-pathnames "snap.lisp" root)))
         (alpha-file nil))
    (ensure-directories-exist path)
    (let* ((context (ensure-session-command-context))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (store (install-file-session-store context root)))
      (flet ((bind-persisted (mode-id session-id provider model)
               (bind-agent-session-mode context :mode-id mode-id
                                        :session-id session-id
                                        :provider-id provider :model-id model)
               (sess:append-session-entry
                store (sess:find-session store session-id)
                (sess:make-message-entry (sess:make-user-message "hi")) context)
               (agent-session:switch-agent-session
                service mode-id session-id context)))
        (bind-persisted :alpha :sa "alpha-provider" "alpha-model")
        (bind-persisted :charlie :sc "charlie-provider" "charlie-model")
        (setf alpha-file (sess:session-file-path store :sa))
        (agent-session:focus-agent-session-mode service :alpha context)
        (invoke-test-command context :snapshot
                             (list :mode-id :alpha
                                   :tail (format nil "save ~A" path)))))
    (delete-file alpha-file)
    (let ((context (ensure-session-command-context)))
      (install-file-session-store context root)
      (let ((result (invoke-test-command
                     context :snapshot
                     (list :mode-id :default-mode
                           :tail (format nil "restore ~A" path)))))
        (is (search "Focused mode ALPHA was unavailable; no mode is focused."
                    (command-result-text context result))
            "the lost focused mode is named in the command result")))
    (ignore-errors (uiop:delete-directory-tree root :validate (constantly t)))))

(test (session-commands-snapshot-restore-multi-mode-seeds-focused-mode
       :fixture interactive-authority)
  "Two modes, each with its own app and model. Restore re-seeds the registry global
-- which seeds newly built agents -- to the FOCUSED mode's model, not the last mode
the sort-ordered restore loop rebinds. Each app's footer still reads its own mode's
model, both transcripts replay, and the focused mode round-trips through the snapshot."
  (let* ((context (session-command-context-no-default-mode))
         (service (agent-session-service context))
         (store (session-log-store context))
         (registry (model-registry context))
         (root (temp-session-root))
         (path (namestring (merge-pathnames "snap.lisp" root))))
    (ensure-directories-exist path)
    (flet ((bind-mode (mode-id session-id provider model body)
             (bind-agent-session-mode context :mode-id mode-id :session-id session-id
                                      :provider-id provider :model-id model)
             (sess:append-session-entry
              store (sess:find-session store session-id)
              (sess:make-message-entry (sess:make-user-message body)) context)
             (sess:append-session-entry
              store (sess:find-session store session-id)
              (sess:make-model-change-entry provider model) context)
             (agent-session:switch-agent-session service mode-id session-id context))
           (footer (mode-id) (app::context-usage-footer-text context mode-id))
           (replayed-p (app needle)
             (some (lambda (te)
                     (let ((text (tui-transcript:event-text te)))
                       (and text (search needle text))))
                   (tui-app:tui-app-transcript-events app))))
      (bind-mode :alpha :sa "alpha-provider" "alpha-model" "ALPHA-WORK")
      (bind-mode :charlie :sc "charlie-provider" "charlie-model" "CHARLIE-WORK")
      (let ((app-alpha (tui-app:make-tui-app :context context :columns 48
                                             :mode-id :alpha))
            (app-charlie (tui-app:make-tui-app :context context :columns 48
                                               :mode-id :charlie)))
        (agent-session:focus-agent-session-mode service :alpha context)
        (is (string= "alpha-model" (models:model-selection-model-id
                                    (models:current-model-selection registry)))
            "focus seeds the registry on the focused mode's model")
        (invoke-test-command context :snapshot
                             (list :mode-id :alpha :tail (format nil "save ~A" path)))
        (models:select-model
         registry
         (models:find-model-definition registry "charlie-provider" "charlie-model")
         context)
        (is (string= "charlie-model" (models:model-selection-model-id
                                      (models:current-model-selection registry)))
            "precondition: the live seed advanced off the focused mode")
        (invoke-test-command context :snapshot
                             (list :mode-id :alpha
                                   :tail (format nil "restore ~A" path)))
        (is (eq :alpha (agent-session:session-active-mode-id service))
            "restore round-trips the focused mode")
        (is (string= "alpha-model" (models:model-selection-model-id
                                    (models:current-model-selection registry)))
            "restore re-seeds the registry to the focused mode's model, not charlie's")
        (is (search "alpha-model" (footer :alpha))
            "the focused mode's footer reads its own model")
        (is (search "charlie-model" (footer :charlie))
            "the non-focused mode is present and its footer reads its own model")
        (is (replayed-p app-alpha "ALPHA-WORK")
            "the focused mode's transcript replays its session")
        (is (replayed-p app-charlie "CHARLIE-WORK")
            "the non-focused mode's transcript replays its session"))
      (ignore-errors (uiop:delete-directory-tree root :validate (constantly t))))))
