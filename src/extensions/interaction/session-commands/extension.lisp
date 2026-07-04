(in-package #:kli/interaction/session-commands)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun service (context)
  (or (find-live-object (context-registry context) :agent-session-service)
      (error "No agent-session service is loaded.")))

(defun session-store (context)
  (find-live-object (context-registry context) :session-store))

(defun command-result (text)
  (make-command-result
   :content (list (make-command-text-content text))))

(defun arg-mode-id (arguments)
  (getf arguments :mode-id))

(defun arg-tail (arguments)
  "The command tail trimmed of surrounding whitespace, or NIL when empty."
  (let ((tail (getf arguments :tail)))
    (and tail
         (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) tail)))
           (and (plusp (length trimmed)) trimmed)))))

(defun id->display (id)
  (string-downcase (symbol-name id)))

(defun token->id (token)
  (intern (string-upcase token) :keyword))

(defun active-session-id (context mode-id)
  (getf (session-mode-info (service context) mode-id context) :id))

(defun tui-app-menu-provider (context arguments)
  "The :tui/app provider when this invocation can host a selection menu: a
dispatching app rode in on the arguments and the capability is loaded. NIL
headless, where menu commands degrade to direct action or text output."
  (and (getf arguments :app)
       (find-capability-provider (active-protocol context)
                                 :tui/app
                                 :contract :tui/app/v1)))

;;; /name

(defun run-name-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let ((mode-id (arg-mode-id arguments))
        (text    (arg-tail arguments)))
    (if text
        (progn
          (rename-agent-session (service context) mode-id text context)
          (command-result (format nil "Named: ~A" text)))
        (let ((name (getf (session-mode-info (service context) mode-id context)
                          :name)))
          (command-result (if name
                              (format nil "Name: ~A" name)
                              "No name set."))))))

;;; /session

(defun session-model-line (info)
  (let ((provider (getf info :provider))
        (model    (getf info :model))
        (thinking (getf info :thinking)))
    (cond
      ((and provider model)
       (format nil "model: ~(~A~)/~(~A~)~@[ ~(~A~)~]" provider model thinking))
      (model (format nil "model: ~(~A~)" model))
      (t "model: (unset)"))))

(defun session-tokens-line (info)
  (let ((usage (getf info :usage)))
    (format nil "tokens: ~D" (if usage (usage-total-tokens usage) 0))))

(defun format-session-info (info)
  (let ((file (getf info :file)))
    (format nil "id: ~A~%file: ~A~%~A~%~A"
            (id->display (getf info :id))
            (if file (namestring file) "(memory)")
            (session-model-line info)
            (session-tokens-line info))))

(defun run-session-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let ((info (session-mode-info (service context)
                                 (arg-mode-id arguments)
                                 context)))
    (command-result (if info
                        (format-session-info info)
                        "No active session."))))

;;; /resume

(defun format-session-row (row active-id)
  (let ((id      (getf row :id))
        (name    (getf row :name))
        (count   (getf row :entry-count))
        (preview (getf row :preview)))
    (format nil "~A~A~@[  ~A~]  ~D msgs~@[  ~A~]"
            (if (eq id active-id) "* " "  ")
            (id->display id)
            name count preview)))

(defun render-session-rows (rows active-id)
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (row) (format-session-row row active-id)) rows)))

(defun row-matches-p (row needle)
  (flet ((hit (field)
           (and (stringp field) (search needle (string-downcase field)))))
    (or (hit (id->display (getf row :id)))
        (hit (getf row :name))
        (hit (getf row :preview)))))

(defun resume-and-report (context mode-id id)
  (multiple-value-bind (resumed-id skipped-tail-p)
      (resume-agent-session (service context) mode-id id context)
    (declare (ignore resumed-id))
    (command-result
     (format nil "Resumed ~A.~:[~; Dropped a corrupt trailing record.~]"
             (id->display id) skipped-tail-p))))

(defun resume-row (context mode-id row)
  "Resume the session ROW describes, refusing a :corrupt row up front since
its file has no loadable header."
  (if (getf row :corrupt)
      (make-command-result
       :content (list (make-command-text-content
                       (format nil "Cannot resume ~A -- the session file is corrupt."
                               (id->display (getf row :id)))))
       :error-p t)
      (resume-and-report context mode-id (getf row :id))))

(defun resume-list (context mode-id store)
  (let ((rows (remove-if #'blank-session-row-p
                         (and store (list-stored-sessions store)))))
    (if rows
        (command-result (render-session-rows rows (active-session-id context mode-id)))
        (command-result "No saved sessions."))))

(defun resume-delete (context mode-id store token)
  (let ((id (token->id token)))
    (cond
      ((eq id (active-session-id context mode-id))
       (command-result "Cannot delete the active session."))
      ((and store (delete-stored-session store id))
       (command-result (format nil "Deleted ~A." (id->display id))))
      (t (command-result (format nil "No such session: ~A." token))))))

(defun resume-select (context mode-id store token)
  (let* ((rows  (and store (list-stored-sessions store)))
         (id    (token->id token))
         (exact (find id rows :key (lambda (row) (getf row :id)))))
    (cond
      (exact (resume-row context mode-id exact))
      (t
       (let ((matches (remove-if-not
                       (lambda (row) (row-matches-p row (string-downcase token)))
                       rows)))
         (cond
           ((null matches) (command-result (format nil "No match: ~A." token)))
           ((null (rest matches))
            (resume-row context mode-id (first matches)))
           (t (command-result
               (render-session-rows matches (active-session-id context mode-id))))))))))

(defun run-resume-command (command arguments context &key call-id on-update)
  "List, search, resume, or delete stored sessions. Bare with a TUI app at
hand, open the selection menu over the stored sessions and report nothing --
the menu is the feedback, and accepting a row switches onto that session.
Bare headless, render the text listing. A selector or delete tail acts
directly -- an explicit argument is a command, even with a TUI at hand."
  (declare (ignore command call-id on-update))
  (let ((mode-id (arg-mode-id arguments))
        (words   (getf arguments :words))
        (store   (session-store context))
        (tui     (tui-app-menu-provider context arguments)))
    (cond
      ((and (null words) tui)
       (if (provider-call tui :open-tui-app-resume-menu (getf arguments :app))
           (make-command-result)
           (command-result "No saved sessions.")))
      ((null words) (resume-list context mode-id store))
      ((and (string-equal (first words) "delete") (second words))
       (resume-delete context mode-id store (second words)))
      (t (resume-select context mode-id store (first words))))))

;;; /compact

(defun run-compact-command (command arguments context &key call-id on-update)
  "Compaction runs a summarizer model call. A dispatcher that provides a
:worker-spawner gets the call off its thread and the outcome arrives via the
compaction events, so the immediate result stays empty. Without a spawner the
call is synchronous and the result text reports the outcome directly."
  (declare (ignore command call-id on-update))
  (let ((service (service context))
        (mode-id (arg-mode-id arguments))
        (instructions (arg-tail arguments))
        (spawner (getf arguments :worker-spawner)))
    (cond
      ((agent-session-busy-p service mode-id context)
       (make-command-result
        :content (list (make-command-text-content
                        "Session is busy -- try again when the turn completes."))
        :error-p t))
      (spawner
       (funcall spawner
                (lambda ()
                  (compact-agent-session service mode-id context
                                         :custom-instructions instructions)))
       (make-command-result))
      (t
       (multiple-value-bind (entry status failure)
           (compact-agent-session service mode-id context
                                  :custom-instructions instructions)
         (declare (ignore entry))
         (case status
           (:compacted (command-result "Compacted."))
           (:aborted (command-result "Compaction aborted."))
           (:failed
            (make-command-result
             :content (list (make-command-text-content
                             (format nil "Compaction failed: ~A"
                                     (or failure "summarizer produced no summary"))))
             :error-p t))
           (t (command-result "Nothing to compact."))))))))

;;; /rewind

(defun rewind-by-count (service mode-id context count)
  "Step the session back COUNT user turns directly, rewinding to before the
COUNTth-newest prompt. Reports nothing to rewind when the session has fewer
turns than COUNT."
  (let ((target (nth (1- count) (list-rewind-targets service mode-id context))))
    (if target
        (progn
          (rewind-agent-session service mode-id context
                                :entry-id (getf target :entry-id))
          (command-result (format nil "Rewound ~D turn~:P." count)))
        (command-result "Nothing to rewind."))))

(defun run-rewind-command (command arguments context &key call-id on-update)
  "Step the conversation back. A numeric tail rewinds that many user turns
directly -- an explicit count is a command, even with a TUI at hand. With a
TUI app and no count, open the selection menu over the session's user turns
and report nothing: the menu is the feedback, and the rewind itself happens
on accept. Headless without a count, rewind the latest turn. The session
side branches before the chosen prompt and switches the mode; the TUI
reacts to the resulting :session-switch and :session-rewind events
(transcript replay, editor restore)."
  (declare (ignore command call-id on-update))
  (let* ((service (service context))
         (mode-id (arg-mode-id arguments))
         (tail (arg-tail arguments))
         (count (and tail (let ((n (ignore-errors (parse-integer tail))))
                            (and n (plusp n) n))))
         (tui (tui-app-menu-provider context arguments)))
    (cond
      ((and tail (null count))
       (make-command-result
        :content (list (make-command-text-content
                        (format nil "Not a turn count: ~A" tail)))
        :error-p t))
      ((agent-session-busy-p service mode-id context)
       (make-command-result
        :content (list (make-command-text-content
                        "Session is busy -- try again when the turn completes."))
        :error-p t))
      (count (rewind-by-count service mode-id context count))
      (tui
       (if (provider-call tui :open-tui-app-rewind-menu
                          (getf arguments :app))
           (make-command-result)
           (command-result "Nothing to rewind.")))
      ((rewind-agent-session service mode-id context)
       (command-result "Rewound one turn."))
      (t (command-result "Nothing to rewind.")))))

;;; /branches

(defun branch-row-label (row)
  "One-line text identifying ROW within the session tree: its branch
context when it has any, its opening prompt otherwise. The plain :preview
cannot tell branches apart -- a branch shares its prefix with its parent,
so every session in a tree opens with the same prompt."
  (or (session-row-branch-label row)
      (let ((text (getf row :preview)))
        (and text (substitute #\Space #\Newline text)))))

(defun format-branch-row (row active-id)
  (format nil "~A~A~A~@[  ~A~]~@[  ~A~]"
          (if (eq (getf row :id) active-id) "* " "  ")
          (getf row :tree-prefix "")
          (id->display (getf row :id))
          (getf row :name)
          (branch-row-label row)))

(defun render-branch-rows (forest active-id)
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (row) (format-branch-row row active-id)) forest)))

(defun run-branches-command (command arguments context &key call-id on-update)
  "Show the tree of stored sessions that rewind-branching creates. With a
TUI app at hand, open a selection menu over the forest and report nothing --
the menu is the feedback, and accepting a row switches onto that session.
Headless, render the forest as indented text rows."
  (declare (ignore command call-id on-update))
  (let ((mode-id (arg-mode-id arguments))
        (store (session-store context))
        (tui (tui-app-menu-provider context arguments)))
    (cond
      (tui
       (if (provider-call tui :open-tui-app-branches-menu (getf arguments :app))
           (make-command-result)
           (command-result "No saved sessions.")))
      (t
       (let ((forest (session-forest
                      (remove-if #'blank-session-row-p
                                 (and store (list-stored-sessions store))))))
         (if forest
             (command-result
              (render-branch-rows forest (active-session-id context mode-id)))
             (command-result "No saved sessions.")))))))

;;; /snapshot

;; Persist the active protocol to a file and restore it. The snapshot datum is
;; already a print/read-safe s-expression, so it writes and reads whole under
;; standard io syntax with read-eval disabled -- the way the session log persists
;; its records. The provider entries gate :protocol/snapshot and :protocol/restore
;; themselves, so the runner only routes and does the file I/O.

(defun snapshot-provider (context)
  (require-capability-provider (active-protocol context)
                               :runtime/snapshot
                               :contract :runtime/snapshot/v1))

(defun write-snapshot-file (path datum)
  (with-open-file (stream path :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (with-standard-io-syntax
      (prin1 datum stream)
      (terpri stream))))

(defun read-snapshot-file (path)
  (with-open-file (stream path)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (read stream)))))

(defun snapshot-subcommand (tail)
  "Split TAIL into its leading word and the trimmed remaining path."
  (let* ((trimmed (string-trim '(#\Space #\Tab) (or tail "")))
         (split (position #\Space trimmed)))
    (if split
        (values (subseq trimmed 0 split)
                (string-trim '(#\Space #\Tab) (subseq trimmed split)))
        (values trimmed ""))))

(defparameter +snapshot-subcommands+ '("save" "restore"))

(defun snapshot-command-completer (command tail)
  "Offer save/restore first, then real cwd path completion after the chosen
subcommand. Candidates replace the whole tail, so each path candidate carries
its subcommand prefix."
  (declare (ignore command))
  (let* ((text (or tail ""))
         (space (position #\Space text)))
    (if (null space)
        (list :candidates
              (remove-if-not (lambda (sub)
                               (kli/text:string-prefix-p text sub))
                             +snapshot-subcommands+)
              :hint "save|restore")
        (let ((sub (subseq text 0 space))
              (query (string-left-trim '(#\Space) (subseq text (1+ space)))))
          (if (member sub +snapshot-subcommands+ :test #'string-equal)
              (list :candidates
                    (mapcar (lambda (path)
                              (concatenate 'string sub " " path))
                            (kli/tui/completion:directory-path-strings
                             query (uiop:getcwd)))
                    :hint "<path>")
              (list :hint "save|restore"))))))

(defun any-session-mode-busy-p (service context)
  "True when any bound mode has a turn in flight. Restore replaces every mode's
state, so it refuses unless all are quiescent: a busy mode would be torn out from
under its running turn, and an in-place rehydrate that errored partway would
leave a half-restored service."
  (loop for mode-id being the hash-keys of (session-mode-bindings service)
        thereis (agent-session-busy-p service mode-id context)))

(defun run-restore-snapshot (path arguments context)
  "Restore the snapshot at PATH onto CONTEXT's active protocol, refusing while any
mode is busy. When restore reconstructs the agent-session service from scratch
(cross-image or a discarded protocol), the dispatching TUI app's listener was
left on the old service and the replay-driving :session-switch already fired, so
re-wire the app against the new service."
  (let ((service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (if (and service (any-session-mode-busy-p service context))
        (make-command-result
         :content (list (make-command-text-content
                         "Session is busy -- try again when the turn completes."))
         :error-p t)
        (progn
          (provider-call (snapshot-provider context) :restore-active-protocol
                         context (read-snapshot-file path))
          (let* ((restored (find-live-object (context-registry context)
                                             :agent-session-service))
                 (reconstructed-p (not (eq service restored)))
                 (tui (tui-app-menu-provider context arguments)))
            (when (and tui reconstructed-p)
              (provider-call tui :reattach-tui-app (getf arguments :app)))
            (command-result
             (format nil "Restored snapshot from ~A.~@[ Focused mode ~A was ~
                          unavailable; no mode is focused.~]"
                     path
                     (and restored
                          (session-restore-unfocused-mode restored)))))))))

(defun run-snapshot-command (command arguments context &key call-id on-update)
  (declare (ignore command call-id on-update))
  (multiple-value-bind (sub path) (snapshot-subcommand (arg-tail arguments))
    (cond
      ((zerop (length path))
       (command-result "Usage: /snapshot save|restore <path>"))
      ((string-equal sub "save")
       (write-snapshot-file path
                            (provider-call (snapshot-provider context)
                                           :snapshot-context context))
       (command-result (format nil "Saved snapshot to ~A." path)))
      ((string-equal sub "restore")
       (run-restore-snapshot path arguments context))
      (t (command-result "Usage: /snapshot save|restore <path>")))))

;;; registration

(defun make-session-command (name label description runner
                             &optional arguments metadata completer)
  (make-command :name name
                :label label
                :description description
                :arguments arguments
                :runner runner
                :completer completer
                :metadata metadata))

(defun register-session-command (commands context source name label description
                                 runner &optional arguments metadata completer)
  (provider-call commands
                 :register-command
                 context
                 name
                 (make-session-command name label description runner
                                       arguments metadata completer)
                 :source source
                 :tier :core))

(defun register-session-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context))
        (source   (contribution-extension contribution)))
    (list
     (register-session-command commands context source
                               :name "Name"
                               "Set or show the session display name."
                               #'run-name-command '(:tail :name))
     (register-session-command commands context source
                               :session "Session"
                               "Show the active session info."
                               #'run-session-command)
     (register-session-command commands context source
                               :resume "Resume"
                               "List, search, resume, or delete stored sessions."
                               #'run-resume-command '(:tail :selector))
     (register-session-command commands context source
                               :compact "Compact"
                               "Compact the session history with optional focus."
                               #'run-compact-command '(:tail :instructions))
     (register-session-command commands context source
                               :rewind "Rewind"
                               "Step the conversation back one or N user turns."
                               #'run-rewind-command '(:tail :count))
     (register-session-command commands context source
                               :branches "Branches"
                               "Show the session tree and switch branches."
                               #'run-branches-command)
     (register-session-command commands context source
                               :snapshot "Snapshot"
                               "Save or restore the session image to a file."
                               #'run-snapshot-command '(:tail :path)
                               nil #'snapshot-command-completer))))

(defun unregister-session-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context)))
    (dolist (registration (contribution-state contribution))
      (provider-call commands
                     :unregister-command
                     context
                     registration))))

(defextension session-commands
  (:requires
   (capability commands :contract commands/v1)
   (capability agent/session :contract agent/session/v1)
   (capability session/log :contract session/log/v1)
   (capability runtime/snapshot :contract runtime/snapshot/v1))
  (:provides
   (effect session-commands
     #'register-session-commands
     #'unregister-session-commands)))
