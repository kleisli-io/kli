(in-package #:kli/app)

(defun install-tool-result (text &key error-p)
  (kli/ext:make-tool-result
   :content (list (kli/ext:make-tool-text-content text))
   :error-p error-p))

(defun install-result-text (id state detail)
  "User-facing line for a finished install. DETAIL is the refusal reason on
:rejected."
  (case state
    (:installed (format nil "Installed ~A." id))
    (:rejected (format nil "Install of ~A rejected (~A)." id detail))
    (:cancelled (format nil "Install of ~A cancelled." id))
    (t (format nil "Install of ~A ended in ~A." id state))))

(defun parse-install-args (tail)
  "Split a /install argument TAIL into (values URL GIT-TREE-SHA1). Both nil unless
the tail carries exactly two whitespace-separated tokens."
  (when tail
    (let ((tokens (remove "" (uiop:split-string tail :separator '(#\Space #\Tab))
                          :test #'string=)))
      (when (= 2 (length tokens))
        (values (first tokens) (second tokens))))))

(defun open-install-confirm (app on-confirm)
  "Confirm/cancel menu over the current trust card. ON-CONFIRM fires on the
confirm row only. Esc dismisses with no action."
  (kli/tui/app:open-tui-app-menu
   app
   (list (list :insert "install" :description "Proceed" :value :confirm)
         (list :insert "cancel" :description "Do not install" :value :cancel))
   (lambda (choice)
     (if (eq choice :confirm)
         (funcall on-confirm)
         (kli/tui/app:tui-app-add-system-event app "Install cancelled.")))))

(defun start-remote-install-flow (app context spawn url hash)
  "A0: render the consent-to-load card and open the confirm menu. Confirm chains
to verification."
  (let ((card (a0-card-url url hash)))
    (kli/tui/app:tui-app-add-system-event app (getf card :text))
    (open-install-confirm
     app
     (lambda ()
       (remote-install-flow-verify app context spawn url hash)))))

(defun remote-install-flow-verify (app context spawn url hash)
  "A1: verify the artifact without loading (git hash floor plus the opt-in
signature ceiling), render the result card, and open the confirm menu. Confirm
commits on a worker."
  (multiple-value-bind (artifact meta) (direct-url-resolve-and-verify url hash)
    (if (null artifact)
        (kli/tui/app:tui-app-add-system-event
         app (format nil "/install: verification failed for ~A (~A)." url meta))
        (let ((card (a1-card-url url meta)))
          (kli/tui/app:tui-app-add-system-event app (getf card :text))
          (open-install-confirm
           app
           (lambda ()
             (remote-install-flow-commit app context spawn meta artifact)))))))

(defun remote-install-flow-commit (app context spawn meta artifact)
  "Phase B: place and index the verified artifact off the loop thread, then run
the dlopen-bearing activation back on the loop thread and await it before
marshaling the outcome. On darwin the loop thread is the initial thread, so the
native load avoids the call-within-initial-thread timeout; linux runs the same
path and the activation is cheap there. When SPAWN is nil the whole flow is
already on the loop thread, so the activation runs inline -- marshaling it would
deadlock. commit-remote-install binds the bounded grant itself."
  (let ((protocol (active-protocol context)))
    (flet ((commit (marshal)
             (multiple-value-bind (state detail)
                 (commit-remote-install meta artifact protocol context nil
                                        :marshal marshal)
               (kli/tui/app:enqueue-main-thread-task
                app
                (lambda ()
                  (kli/tui/app:tui-app-add-system-event
                   app (install-result-text (getf meta :id) state detail)))))))
      (if spawn
          (funcall spawn
                   (lambda ()
                     (commit (lambda (thunk)
                               (kli/tui/app:call-on-main-thread-task app thunk)))))
          (commit nil)))))

(defun run-remote-install-command (command arguments context
                                   &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let ((app (getf arguments :app))
        (spawn (getf arguments :worker-spawner)))
    (multiple-value-bind (url hash) (parse-install-args (rest-arg arguments))
      (cond
        ((null url) (reply "Usage: /install <url> <git-tree-sha1>"))
        ((null app) (reply "/install needs the interactive terminal."))
        (t (start-remote-install-flow app context spawn url hash)
           (kli/interaction/commands:make-command-result))))))

(defun make-install-policy-contract ()
  (kli/ext:make-provider-contract
   :id :install-policy/v1
   :capability :install-policy
   :required-entries '(:check-install-policy)))

(defun default-install-policy (pin-spec context)
  "Conservative default: every agent-initiated install defers to a human."
  (declare (ignore pin-spec context))
  :ask-human)

(defun make-default-install-policy-provider ()
  (kli/ext:make-provider
   :id :default-install-policy
   :capability :install-policy
   :contracts '(:install-policy/v1)
   :entries (list :check-install-policy #'default-install-policy)))

(defun install-policy-decision (protocol pin-spec context)
  "Soft policy lookup. An absent or misbehaving provider defers to a human."
  (let ((provider (kli/ext:find-capability-provider
                   protocol :install-policy :contract :install-policy/v1)))
    (or (and provider
             (ignore-errors
              (kli/ext:provider-call provider
                                     :check-install-policy pin-spec context)))
        :ask-human)))

(defun run-remote-install-grant (url hash protocol context)
  (multiple-value-bind (state detail)
      (install-remote-extension (list :url url :git-tree-sha1 hash) protocol context
                                :confirm-fn (constantly t))
    (install-tool-result (install-result-text (url-derived-id url) state detail)
                         :error-p (not (eq state :installed)))))

(defun run-remote-install-tool (tool parameters context
                                &key call-id on-update)
  (declare (ignore tool call-id on-update))
  (let ((url (kli/ext:tool-parameter parameters :url))
        (hash (kli/ext:tool-parameter parameters :hash)))
    (if (or (null url) (null hash)
            (and (stringp url) (zerop (length url)))
            (and (stringp hash) (zerop (length hash))))
        (install-tool-result
         "The install tool requires a url and a git-tree-sha1." :error-p t)
        (let ((decision (install-policy-decision (active-protocol context)
                                                 (list :url url :git-tree-sha1 hash)
                                                 context)))
          (case decision
            (:grant (run-remote-install-grant url hash (active-protocol context)
                                              context))
            (:deny (install-tool-result
                    (format nil "Install from ~A denied by policy." url)
                    :error-p t))
            (t (install-tool-result
                (format nil "Install from ~A needs human approval. Run /install ~A ~A in the terminal."
                        url url hash))))))))

(defextension remote-install-tool
  (:provides
   (tool install
     :label "Install"
     :description "Install an extension from a url, pinned to its git-tree-sha1."
     :parameters '(:object (:url :string) (:hash :string))
     :runner #'run-remote-install-tool
     :metadata '(:capabilities (:manifest/install-remote)))
   (contract install-policy/v1 (make-install-policy-contract))
   (capability install-policy (make-default-install-policy-provider))))

(defextension remote-install-command
  (:provides
   (command "install"
     :description "Install an extension from a url, pinned to its git-tree-sha1."
     :arguments '(:tail :id)
     :handler #'run-remote-install-command)))
