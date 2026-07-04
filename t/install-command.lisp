(in-package #:kli/tests)
(in-suite all)

(defun install-policy-provider (protocol context decision)
  "Install a policy provider that always returns DECISION, so the agent path's
routing can be driven without a real policy."
  (with-extension-load-authority
    (ext:install-manifest
     (ext:load-extension-manifest
      `(ext:defextension fixed-install-policy
         (:provides
          (contract install-policy/v1 (app::make-install-policy-contract))
          (capability install-policy
            (ext:make-provider
             :id :fixed-install-policy :capability :install-policy
             :contracts '(:install-policy/v1)
             :entries (list :check-install-policy (constantly ,decision)))))))
     protocol context)))

(defun install-tool-text (result)
  (tui-commands::text-content-value (first (ext::tool-result-content result))))

(defun install-memory-app (context)
  (tui-app::make-tui-app :context context :terminal-kind :memory))

(defun install-app-event-texts (app)
  (mapcar #'tui-transcript::event-text
          (reverse (tui-app::tui-app-transcript-events app))))

(defun install-app-popup-values (app)
  "The :value of each row in the editor's current selection menu, or nil when
no menu is open."
  (let ((popup (tui-editor::editor-completion (tui-app::tui-app-editor app))))
    (and popup
         (mapcar #'tui-editor::completion-candidate-value
                 (tui-editor::completion-popup-candidates popup)))))

(defun install-app-fire-row (app index)
  "Select row INDEX of the current menu and accept it, as Enter would."
  (let ((popup (tui-editor::editor-completion (tui-app::tui-app-editor app))))
    (setf (tui-editor::completion-popup-selected popup) index))
  (tui-editor::accept-completion (tui-app::tui-app-editor app)))

(defun install-app-has-text-p (app needle)
  (some (lambda (text) (search needle text)) (install-app-event-texts app)))

(test (install-tool-policy-routes :fixture extension-load-authority)
  "The install tool defers to a human with no policy, denies under a deny policy,
and installs from a url at the hash floor under a grant policy."
  (let ((url "https://ext.example/okx.lisp")
        (hash "deadbeef"))
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (is (eq :ask-human (app::install-policy-decision
                          protocol (list :url url :git-tree-sha1 hash) context)))
      (let ((result (app::run-remote-install-tool
                     nil (list :url url :hash hash) context)))
        (is (search "human approval" (install-tool-text result)))
        (is (null (ext::tool-result-error-p result)))))
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-policy-provider protocol context :deny)
      (is (eq :deny (app::install-policy-decision
                     protocol (list :url url :git-tree-sha1 hash) context)))
      (let ((result (app::run-remote-install-tool
                     nil (list :url url :hash hash) context)))
        (is-true (ext::tool-result-error-p result))
        (is (search "denied" (install-tool-text result))))))
  (let* ((artifact (trust-artifact-bytes "okx"))
         (url "https://ext.example/okx.lisp")
         (good (app::git-blob-sha1 artifact)))
    (let* ((*trust-load-sentinel* nil)
           (app::*registry-trust-roots* '())
           (app::*remote-install-fetcher* (trust-fetcher url artifact))
           (app::*remote-install-staging-root* (trust-staging-root))
           (context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-policy-provider protocol context :grant)
      (let ((result (app::run-remote-install-tool
                     nil (list :url url :hash good) context)))
        (is (search "Installed okx" (install-tool-text result)))
        (is (null (ext::tool-result-error-p result)))
        (is-true *trust-load-sentinel* "a granted install loads the author code")
        (is (not (null (gethash "okx" (app:remote-install-pins protocol))))
            "and records the pin")))))

(test install-command-two-phase-menu
  "The /install command needs the terminal headless, then on a memory terminal
renders the A0 card, opens the A1 menu from inside the A0 confirmation, installs
on the second confirmation, and loads nothing on a cancel."
  (let* ((artifact (trust-artifact-bytes "okx"))
         (url "https://ext.example/okx.lisp")
         (good (app::git-blob-sha1 artifact)))
    (let ((result (app::run-remote-install-command
                   nil (list :tail (format nil "~A ~A" url good))
                   (kli:make-kernel-host))))
      (is (search "interactive terminal" (tui-commands::command-result-text result))))
    (let ((result (app::run-remote-install-command
                   nil '(:tail "onlyoneword") (kli:make-kernel-host))))
      (is (search "Usage" (tui-commands::command-result-text result))
          "a tail without a url and a hash shows usage"))
    (let ((app::*registry-trust-roots* '())
          (app::*remote-install-fetcher* (trust-fetcher url artifact))
          (app::*remote-install-staging-root* (trust-staging-root)))
      (let* ((*trust-load-sentinel* nil)
             (context (kli:make-kernel-host))
             (protocol (switch-to-extension-protocol context))
             (app (install-memory-app context)))
        (app::start-remote-install-flow app context nil url good)
        (is-true (install-app-has-text-p app "Install from")
                 "the A0 consent card is shown")
        (is (equal '(:confirm :cancel) (install-app-popup-values app))
            "with a confirm/cancel menu")
        (install-app-fire-row app 0)
        (is-true (install-app-has-text-p app "Verified bytes match git")
                 "the A1 card is shown after confirming A0")
        (is (equal '(:confirm :cancel) (install-app-popup-values app))
            "and the A1 menu opens from inside the A0 action")
        (install-app-fire-row app 0)
        (tui-app::run-pending-main-thread-tasks app)
        (is-true (install-app-has-text-p app "Installed okx")
                 "the install completes on the second confirmation")
        (is-true *trust-load-sentinel* "and loads the author code")
        (is (not (null (gethash "okx" (app:remote-install-pins protocol))))
            "and records the pin"))
      (let* ((*trust-load-sentinel* nil)
             (context (kli:make-kernel-host))
             (app (progn (switch-to-extension-protocol context)
                         (install-memory-app context))))
        (app::start-remote-install-flow app context nil url good)
        (install-app-fire-row app 1)
        (is-true (install-app-has-text-p app "Install cancelled")
                 "a cancel reports the cancellation")
        (is (null (install-app-popup-values app)) "dismisses the menu")
        (is (null *trust-load-sentinel*) "and loads nothing")))))
