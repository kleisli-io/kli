(in-package #:kli/tests)

(defvar *author-extension-file-counter* 0)

(defun author-temp-path ()
  (merge-pathnames (format nil "kli-author-ext-~D.lisp"
                           (incf *author-extension-file-counter*))
                   (uiop:temporary-directory)))

(defun write-author-extension (content &optional (path (author-temp-path)))
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (write-string content out))
  path)

(defun author-single-units (&rest files)
  "Each FILE as a (:single FILE) unit — the loose-file discovery shape."
  (mapcar (lambda (file) (list :single file)) files))

(defun author-temp-tree ()
  (merge-pathnames (format nil "kli-author-tree-~D/"
                           (incf *author-extension-file-counter*))
                   (uiop:temporary-directory)))

(defun write-tree-file (root relative content)
  "Write CONTENT to ROOT/RELATIVE, creating intermediate directories."
  (let ((path (merge-pathnames relative root)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string content out))
    path))

(defun author-base-names (files)
  (mapcar #'file-namestring files))

(defun author-dir-unit-name (unit)
  "The trailing directory name of a (:dir DIR FILES) unit."
  (car (last (pathname-directory (second unit)))))

(defun author-delete-package (name)
  (let ((package (find-package name)))
    (when package (delete-package package))))

(defun author-command-text (result)
  (getf (first (commands:command-result-content result)) :text))

(defun author-host ()
  "A context on an extension protocol carrying the :commands capability."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context commands:*commands-extension-manifest*)
    (values context protocol)))

(defun author-commands-provider (protocol)
  (ext:require-capability-provider protocol :commands :contract :commands/v1))

(test author-derive-requires-union
  (let ((specs (ext:derive-requirement-specs
                '((command "a" :handler nil)
                  (command "b" :handler nil)
                  (on :tool/call handler)
                  (tool foo))
                '())))
    (is (= 1 (length specs))
        "two command clauses collapse to one requirement -- on and tool add none")
    (is (eq :capability (ext:normalize-extension-id (first (first specs)))))
    (is (eq :commands (ext:normalize-extension-id (second (first specs)))))))

(test author-derive-explicit-union-dedup
  (let* ((specs (ext:derive-requirement-specs
                 '((command "a" :handler nil))
                 '((capability commands :contract commands/v1
                    :provider-id my-provider))))
         (spec (first specs)))
    (is (= 1 (length specs)))
    (is (eq :my-provider
            (ext:normalize-extension-id (getf (cddr spec) :provider-id)))
        "an explicit requirement overrides the derived one of the same identity")))

(test author-derive-empty-for-bare-clause
  (is (null (ext:derive-requirement-specs
             '((on :ping handler) (tool foo))
             '()))
      "clauses that imply no requirement derive nothing"))

(test author-command-clause-lowers
  (let* ((manifest (ext:load-extension-manifest
                    '(author:defextension command-lowers-probe
                       (:provides
                        (command "hello"
                          :description "Say hello"
                          :handler (lambda (command arguments context
                                            &key call-id on-update)
                                     (declare (ignore command arguments context
                                                      call-id on-update))
                                     (author:reply "hi")))))))
         (contribution (first (ext:extension-contribution-list (funcall manifest)))))
    (is (typep contribution 'commands:command-contribution))
    (is (eq :command (ext:contribution-kind contribution)))
    (is (eq :hello (ext:contribution-name contribution)))
    (is (string= "hello" (commands:contribution-command-name contribution)))
    (is (string= "Say hello" (commands:contribution-command-description contribution)))
    (is (functionp (commands:contribution-command-runner contribution)))))

(test author-on-clause-lowers
  (let* ((manifest (ext:load-extension-manifest
                    '(author:defextension on-lowers-probe
                       (:provides
                        (on :tool/call (lambda (event context)
                                         (declare (ignore event context))
                                         nil))))))
         (contribution (first (ext:extension-contribution-list (funcall manifest)))))
    (is (typep contribution 'event:event-handler-contribution))
    (is (eq :tool/call (event:contribution-event-type contribution)))
    (is (eq :on-tool/call (ext:contribution-name contribution)))))

(defun author-contribution-kinds (extension)
  (mapcar #'ext:contribution-kind (ext:extension-contribution-list extension)))

(defun author-contribution-names (extension)
  (mapcar #'ext:contribution-name (ext:extension-contribution-list extension)))

(defun author-requirement-keys (extension)
  (mapcar (lambda (requirement)
            (list (ext:normalize-extension-id (ext:requirement-kind requirement))
                  (ext:normalize-extension-id (ext:requirement-name requirement))))
          (ext:extension-requirement-list extension)))

(test author-builder-equals-declarative
  (let ((declared (funcall
                   (ext:load-extension-manifest
                    '(author:defextension parity-probe
                       (:provides
                        (command "hi"
                          :handler (lambda (command arguments context
                                            &key call-id on-update)
                                     (declare (ignore command arguments context
                                                      call-id on-update))
                                     (author:reply "hi"))))))))
        (built (funcall
                (author:kli-extension (builder parity-probe)
                  (author:command builder "hi"
                    :handler (lambda (command arguments context
                                      &key call-id on-update)
                               (declare (ignore command arguments context
                                                call-id on-update))
                               (author:reply "hi")))))))
    (is (equal (author-contribution-kinds declared)
               (author-contribution-kinds built)))
    (is (equal (author-contribution-names declared)
               (author-contribution-names built)))
    (is (equal (author-requirement-keys declared)
               (author-requirement-keys built))
        "both surfaces derive the same normalized requirement")
    (is (equal '((:capability :commands)) (author-requirement-keys built)))))

(test author-builder-loop-registration
  (let ((extension (funcall
                    (author:kli-extension (builder loop-probe)
                      (dolist (name '("a" "b" "c"))
                        (author:command builder name
                          :handler (lambda (command arguments context
                                            &key call-id on-update)
                                     (declare (ignore command arguments context
                                                      call-id on-update))
                                     (author:reply name))))))))
    (is (= 3 (length (ext:extension-contribution-list extension))))
    (is (equal '(:a :b :c) (author-contribution-names extension)))
    (is (every (lambda (contribution) (eq :command (ext:contribution-kind contribution)))
               (ext:extension-contribution-list extension)))))

(test (author-command-roundtrip :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let* ((manifest (ext:load-extension-manifest
                      '(author:defextension roundtrip-probe
                         (:provides
                          (command "greet"
                            :description "Greet"
                            :handler (lambda (command arguments context
                                              &key call-id on-update)
                                       (declare (ignore command call-id on-update))
                                       (author:reply
                                        (format nil "Hello ~A!"
                                                (or (author:rest-arg arguments)
                                                    "world")))))))))
           (handle (with-extension-load-authority
                     (ext:install-manifest manifest protocol context)))
           (provider (author-commands-provider protocol)))
      (is (not (null (ext:provider-call provider :find-command :greet)))
          "the declared command is registered on install")
      (is (string= "Hello world!"
                   (author-command-text
                    (ext:provider-call provider :invoke-command :greet '() context))))
      (is (string= "Hello Ada!"
                   (author-command-text
                    (ext:provider-call provider :invoke-command
                                       :greet '(:tail "Ada") context)))
          "rest-arg surfaces the free-text tail to the runner")
      (ext:retract-manifest handle protocol context)
      (is (null (ext:provider-call provider :find-command :greet))
          "retract removes the command cleanly"))))

(test (author-builder-command-installs :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let* ((manifest (author:kli-extension (builder builder-install-probe)
                       (author:command builder "bgreet"
                         :handler (lambda (command arguments context
                                           &key call-id on-update)
                                    (declare (ignore command call-id on-update))
                                    (author:reply
                                     (format nil "Hi ~A"
                                             (or (author:rest-arg arguments)
                                                 "there")))))))
           (handle (with-extension-load-authority
                     (ext:install-manifest manifest protocol context)))
           (provider (author-commands-provider protocol)))
      (is (not (null (ext:provider-call provider :find-command :bgreet)))
          "a builder-built command installs — its derived requirement is satisfiable")
      (is (string= "Hi Ada"
                   (author-command-text
                    (ext:provider-call provider :invoke-command
                                       :bgreet '(:tail "Ada") context))))
      (ext:retract-manifest handle protocol context)
      (is (null (ext:provider-call provider :find-command :bgreet))))))

(test author-missing-capability-fails-fast
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (manifest (ext:load-extension-manifest
                    '(author:defextension needs-commands-probe
                       (:provides
                        (command "nope"
                          :handler (lambda (command arguments context
                                            &key call-id on-update)
                                     (declare (ignore command arguments context
                                                      call-id on-update))
                                     (author:reply "nope"))))))))
    (signals error (with-extension-load-authority
                     (ext:install-manifest manifest protocol context)))
    (is (null (ext:protocol-installed-contributions protocol))
        "a failed requirement check installs nothing")
    (is (not (ext:extension-loaded-p protocol :needs-commands-probe)))))

(test author-partial-install-rolls-back
  (multiple-value-bind (context protocol) (author-host)
    (let* ((provider (author-commands-provider protocol))
           (good (commands:make-command-contribution
                  :name "rollback-cmd"
                  :runner (lambda (command arguments context
                                   &key call-id on-update)
                            (declare (ignore command arguments context
                                             call-id on-update))
                            (commands:reply "ok"))))
           (bad (make-instance 'ext:contribution :kind :unsupported :name :unsupported))
           (extension (ext:make-extension :id :rollback-probe
                                          :contributions (list good bad))))
      (signals error (with-extension-load-authority
                       (ext:install-manifest (lambda () extension) protocol context)))
      (is (null (ext:provider-call provider :find-command :rollback-cmd))
          "the command installed before the failure is retracted")
      (is (not (ext:extension-loaded-p protocol :rollback-probe))))))

(test (author-discovery-loads-and-isolates :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((good (write-author-extension
                 "(defextension disc-good
  (:provides
   (command \"discok\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"ok\")))))"))
          (bad (write-author-extension
                "(defextension disc-bad
  (:provides (totally-bogus-kind x)))"))
          (warned nil))
      (unwind-protect
           (handler-bind ((warning (lambda (condition)
                                     (setf warned t)
                                     (muffle-warning condition))))
             (app:load-user-extensions context :config '()
                                       :units (author-single-units good bad)))
        (delete-file good)
        (delete-file bad))
      (let ((provider (author-commands-provider protocol)))
        (is (not (null (ext:provider-call provider :find-command :discok)))
            "the good extension installs despite a sibling failing to load"))
      (is-true warned "the malformed file signals a warning")
      (is (assoc :disc-good (app:user-extension-status context))
          "the good extension is registered as available")
      (is (not (ext:extension-loaded-p protocol :disc-bad))
          "the malformed extension is skipped"))))

(test (author-load-captures-compile-diagnostics-off-the-tty :fixture extension-load-authority)
  "Compiler output from loading a unit (a style warning prints the offending
   form) is recorded as a per-unit diagnostic instead of reaching the boot
   tty, where the TUI takeover would flash then wipe it."
  (multiple-value-bind (context protocol) (author-host)
    (let ((noisy (write-author-extension
                  "(defun author-noisy-probe (unused) 42)
(defextension diag-noisy
  (:provides
   (command \"diagnoisy\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"ok\")))))"))
          (out (make-string-output-stream))
          (err (make-string-output-stream)))
      (unwind-protect
           (let ((*standard-output* out)
                 (*error-output* err))
             (app:load-user-extensions context :config '()
                                       :units (author-single-units noisy)))
        (delete-file noisy))
      (is (string= "" (get-output-stream-string out))
          "nothing reaches standard output during the load")
      (is (string= "" (get-output-stream-string err))
          "nothing reaches error output during the load")
      (let ((entries (app:extension-diagnostics protocol)))
        (is (= 1 (length entries)))
        (is (equal noisy (car (first entries)))
            "the diagnostic is attributed to the unit that produced it")
        (is (search "UNUSED" (cdr (first entries)))
            "the captured text carries the compiler diagnostic")))))

(test author-load-failure-diagnostic-is-captured-and-still-signals
  "The failed-to-load warning keeps signaling for handlers, but its printing
   diverts into the unit's diagnostic instead of the tty."
  (multiple-value-bind (context protocol) (author-host)
    (let ((bad (write-author-extension
                "(defextension diag-bad
  (:provides (totally-bogus-kind x)))"))
          (err (make-string-output-stream))
          (warned nil))
      (unwind-protect
           (handler-bind ((warning (lambda (condition)
                                     (declare (ignore condition))
                                     (setf warned t))))
             (let ((*error-output* err))
               (app:load-user-extensions context :config '()
                                         :units (author-single-units bad))))
        (delete-file bad))
      (is-true warned "the failure still signals a warning for handlers")
      (is (string= "" (get-output-stream-string err))
          "the warning prints into the capture, not the tty")
      (let ((entries (app:extension-diagnostics protocol)))
        (is (= 1 (length entries)))
        (is (search "failed to load" (cdr (first entries))))))))

(test author-boot-load-keeps-prior-diagnostics
  "load-user-extensions must not clear already-captured diagnostics: at boot
   main captures the profile install as :boot, and a clear here would wipe
   that capture before it surfaces. The clear belongs to /reload semantics."
  (multiple-value-bind (context protocol) (author-host)
    (app::record-extension-diagnostic protocol :boot "settings: bad value")
    (app:load-user-extensions context :config '() :units '())
    (let ((entries (app:extension-diagnostics protocol)))
      (is (= 1 (length entries)))
      (is (eq :boot (car (first entries))))
      (is (string= "settings: bad value" (cdr (first entries)))))))

(test (author-reload-clears-the-previous-diagnostics :fixture extension-load-authority)
  "Diagnostics describe the most recent load pass, so a reload that fixes the
   noisy unit leaves none behind."
  (multiple-value-bind (context protocol) (author-host)
    (let ((file (write-author-extension
                 "(defun author-reload-noise-probe (unused) 42)
(defextension diag-reload
  (:provides
   (command \"diagreload\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"ok\")))))")))
      (unwind-protect
           (progn
             (app:load-user-extensions context :config '()
                                       :units (author-single-units file))
             (is (= 1 (length (app:extension-diagnostics protocol))))
             (write-author-extension
              "(defextension diag-reload
  (:provides
   (command \"diagreload\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"ok\")))))"
              file)
             (app:reload-user-extensions context :config '()
                                         :units (author-single-units file))
             (is (null (app:extension-diagnostics protocol))
                 "a clean reload drops the stale diagnostic"))
        (when (probe-file file) (delete-file file))))))

(test (author-reload-picks-up-edits :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((file (write-author-extension
                 "(defextension reload-probe
  (:provides
   (command \"before\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"before\")))))"))
          (provider (author-commands-provider protocol)))
      (unwind-protect
           (progn
             (app:load-user-extensions context :config '()
                                       :units (author-single-units file))
             (is (not (null (ext:provider-call provider :find-command :before))))
             (write-author-extension
              "(defextension reload-probe
  (:provides
   (command \"after\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"after\")))))"
              file)
             (app:reload-user-extensions context :config '()
                                         :units (author-single-units file))
             (is (null (ext:provider-call provider :find-command :before))
                 "the pre-edit command is retracted on reload")
             (is (not (null (ext:provider-call provider :find-command :after)))
                 "the post-edit command is installed on reload"))
        (when (probe-file file) (delete-file file))))))

(test (author-reload-command-preserves-live-extensions-on-failed-prepare
       :fixture interactive-authority)
  (multiple-value-bind (context protocol) (author-host)
    (install-extension context app::*user-extension-commands-extension-manifest*)
    (let* ((root (author-temp-tree))
           (file (write-tree-file
                  root ".kli/extensions/reload-survives.lisp"
                  "(defextension reload-survives
  (:provides
   (command \"survive\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"alive\")))))"))
           (provider (author-commands-provider protocol)))
      (unwind-protect
           (uiop:with-current-directory (root)
             (with-extension-load-authority
               (app:load-user-extensions context))
             (is (ext:provider-call provider :find-command :survive)
                 "precondition: the original extension is live")
             (write-author-extension "(error \"reload prepare failed\")" file)
             (invoke-test-command context :reload)
             (is (ext:provider-call provider :find-command :survive)
                 "the previous command survives a failed reload prepare")
             (is (ext:extension-loaded-p protocol :reload-survives)
                 "the previous extension handle remains installed")
	            (is (app:extension-diagnostics protocol)
	                "the failed reload still records diagnostics"))
	        (when (probe-file file) (delete-file file))))))

(test (author-reload-extensions-command-preserves-global-xdg-extensions
       :fixture interactive-authority)
  (multiple-value-bind (context protocol) (author-host)
    (install-extension context app::*user-extension-commands-extension-manifest*)
    (let* ((root (author-temp-tree))
           (project (merge-pathnames "project/" root))
           (file (write-tree-file
                  root "kli/extensions/global-survives.lisp"
                  "(defextension global-survives
  (:provides
   (command \"global-survive\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"alive\")))))"))
           (provider (author-commands-provider protocol))
           (old-xdg (uiop:getenv "XDG_CONFIG_HOME")))
      (ensure-directories-exist project)
      (unwind-protect
           (progn
             (setf (uiop:getenv "XDG_CONFIG_HOME") (namestring root))
             (uiop:with-current-directory (project)
               (with-extension-load-authority
                 (app:load-user-extensions context))
               (is (ext:provider-call provider :find-command :global-survive)
                   "precondition: the global extension is live")
               (write-author-extension "(error \"reload prepare failed\")" file)
               (invoke-test-command context :reload
                                    '(:tail "extensions"
                                      :words ("extensions")))
               (is (ext:provider-call provider :find-command :global-survive)
                   "the global command survives `/reload extensions`")
               (is (ext:extension-loaded-p protocol :global-survives)
                   "the global extension handle remains installed")
               (is (app:extension-diagnostics protocol)
                   "the failed reload still records diagnostics")))
        (if old-xdg
            (setf (uiop:getenv "XDG_CONFIG_HOME") old-xdg)
            (sb-posix:unsetenv "XDG_CONFIG_HOME"))
        (when (probe-file file) (delete-file file))))))

(test (author-index-does-not-install :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (declare (ignore context))
    (let ((file (write-author-extension
                 "(defextension index-only-probe
  (:provides
   (command \"idx\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"idx\")))))"))
          (before (length (ext:protocol-installed-contributions protocol))))
      (unwind-protect
           (app::index-unit protocol (list :single file))
        (delete-file file))
      (is (= before (length (ext:protocol-installed-contributions protocol)))
          "indexing a unit installs nothing")
      (is (gethash :index-only-probe (app::available-extensions protocol))
          "indexing registers the extension as available"))))

(test (author-disabled-extension-not-installed :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (flet ((command-file (id name)
             (write-author-extension
              (format nil "(defextension ~A
  (:provides
   (command \"~A\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"~A\")))))"
                      id name name))))
      (let ((fa (command-file "probe-a" "cmda"))
            (fb (command-file "probe-b" "cmdb"))
            (fc (command-file "probe-c" "cmdc")))
        (unwind-protect
             (app:load-user-extensions context
                                       :config '(:disabled (:probe-b))
                                       :units (author-single-units fa fb fc))
          (mapc #'delete-file (list fa fb fc)))
        (let ((provider (author-commands-provider protocol)))
          (is (not (null (ext:provider-call provider :find-command :cmda))))
          (is (not (null (ext:provider-call provider :find-command :cmdc))))
          (is (null (ext:provider-call provider :find-command :cmdb))
              "the disabled extension is indexed but not installed"))
        (is (= 3 (length (app:user-extension-status context)))
            "all three extensions are available")
        (is (null (cdr (assoc :probe-b (app:user-extension-status context))))
            "probe-b is available but not enabled")))))

(defun lifecycle-mutation-waits-for-lock (thunk)
  (let ((started (sb-thread:make-semaphore
                  :name "kli-lifecycle-mutation-started"))
        (done (sb-thread:make-semaphore
               :name "kli-lifecycle-mutation-done"))
        (worker nil)
        (outcome nil))
    (unwind-protect
         (progn
           (app::with-extension-lifecycle-lock
             (setf worker
                   (sb-thread:make-thread
                    (lambda ()
                      (sb-thread:signal-semaphore started)
                      (setf outcome
                            (handler-case
                                (cons :values
                                      (multiple-value-list
                                       (with-extension-load-authority
                                         (funcall thunk))))
                              (error (condition)
                                (cons :error condition))))
                      (sb-thread:signal-semaphore done))
                    :name "kli-lifecycle-mutation-test"))
             (is (sb-thread:wait-on-semaphore started :timeout 1)
                 "worker reached the lifecycle mutation")
             (is (not (sb-thread:wait-on-semaphore done :timeout 0.05))
                 "lifecycle mutation waits for the transition lock"))
           (is (sb-thread:wait-on-semaphore done :timeout 1)
               "lifecycle mutation completes after the transition lock releases")
           (ignore-errors (sb-thread:join-thread worker :timeout 1))
           (when (eq (car outcome) :error)
             (error (cdr outcome)))
           (cdr outcome))
      (when worker
        (ignore-errors (sb-thread:join-thread worker :timeout 1))
        (when (sb-thread:thread-alive-p worker)
          (ignore-errors (sb-thread:terminate-thread worker)))))))

(test (author-extension-mutations-wait-for-lifecycle-lock
       :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((entry (app::make-user-extension-entry
                  :id :lifecycle-probe
                  :manifest (lambda ()
                              (ext:make-extension :id :lifecycle-probe))
                  :metadata '(:version "1.0.0"))))
      (setf (gethash :lifecycle-probe (app::available-extensions protocol))
            entry)
      (is (equal '(t :enabled)
                 (lifecycle-mutation-waits-for-lock
                  (lambda ()
                    (app:enable-user-extension context :lifecycle-probe)))))
      (is (gethash :lifecycle-probe (app::installed-user-handles protocol)))
      (is (equal '(t :disabled)
                 (lifecycle-mutation-waits-for-lock
                  (lambda ()
                    (app:disable-user-extension context :lifecycle-probe)))))
      (is (null (gethash :lifecycle-probe
                         (app::installed-user-handles protocol))))
      (app::install-user-extension protocol entry context)
      (app:record-remote-install-pin
       protocol (list :id "lifecycle-probe"
                      :source-kind :registry
                      :version "1.0.0"))
      (is (equal '(t :uninstalled)
                 (lifecycle-mutation-waits-for-lock
                  (lambda ()
                    (app:uninstall-remote-extension context "lifecycle-probe")))))
      (is (null (gethash :lifecycle-probe
                         (app::installed-user-handles protocol))))
      (is (zerop (hash-table-count (app:remote-install-pins protocol)))))))

(test (author-enable-disable-roundtrip :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((file (write-author-extension
                 "(defextension toggle-probe
  (:provides
   (command \"rt\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"rt\")))))"))
          (provider (author-commands-provider protocol)))
      (unwind-protect
           (progn
             (app:load-user-extensions context :config '()
                                       :units (author-single-units file))
             (is (not (null (ext:provider-call provider :find-command :rt))))
             (app:disable-user-extension context :toggle-probe)
             (is (null (ext:provider-call provider :find-command :rt))
                 "disable retracts the command")
             (app:enable-user-extension context :toggle-probe)
             (is (not (null (ext:provider-call provider :find-command :rt)))
                 "enable reinstalls from the available registry"))
        (when (probe-file file) (delete-file file))))))

(test (author-autoload-nil-stays-inert :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((file (write-author-extension
                 "(defextension autoload-off-probe
  (:metadata (:autoload nil))
  (:provides
   (command \"auto\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"auto\")))))")))
      (unwind-protect
           (app:load-user-extensions context :config '()
                                     :units (author-single-units file))
        (delete-file file))
      (is (gethash :autoload-off-probe (app::available-extensions protocol))
          "an :autoload nil extension is still indexed as available")
      (is (null (ext:provider-call (author-commands-provider protocol)
                                   :find-command :auto))
          "an :autoload nil extension is not installed by default"))))

(test author-order-files-package-extension
  (let ((ordered (app::order-files
                  (list #p"/u/zed.lisp" #p"/u/extension.lisp"
                        #p"/u/package.lisp" #p"/u/alpha.lisp"))))
    (is (equal '("package.lisp" "alpha.lisp" "zed.lisp" "extension.lisp")
               (author-base-names ordered))
        "package.lisp leads, extension.lisp trails, the rest alphabetical")))

(test author-discover-units-recursion
  (let ((root (author-temp-tree)))
    (unwind-protect
         (progn
           (write-tree-file root "hello.lisp" "")
           (write-tree-file root "clock/util.lisp" "")
           (write-tree-file root "clock/extension.lisp" "")
           (write-tree-file root "suite/alpha/extension.lisp" "")
           (write-tree-file root "suite/beta/package.lisp" "")
           (write-tree-file root "suite/beta/helper.lisp" "")
           (write-tree-file root "suite/beta/extension.lisp" "")
           (let* ((units (app:discover-units root))
                  (singles (remove-if-not (lambda (u) (eq :single (first u))) units))
                  (dirs (remove-if-not (lambda (u) (eq :dir (first u))) units)))
             (is (= 1 (length singles)))
             (is (string= "hello.lisp" (file-namestring (second (first singles))))
                 "the loose top-level file is a single unit")
             (is (equal '("alpha" "beta" "clock")
                        (sort (mapcar #'author-dir-unit-name dirs) #'string<))
                 "suite/ is a group: it recurses into alpha and beta, not itself")
             (let ((beta (find "beta" dirs :key #'author-dir-unit-name :test #'string=)))
               (is (equal '("package.lisp" "helper.lisp" "extension.lisp")
                          (author-base-names (third beta)))
                   "a unit's files are ordered package-first, extension-last"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(test author-marked-dir-not-recursed
  (let ((root (author-temp-tree)))
    (unwind-protect
         (progn
           (write-tree-file root "nested/extension.lisp" "")
           (write-tree-file root "nested/sub/extra.lisp" "")
           (let ((units (app:discover-units root)))
             (is (= 1 (length units)) "nested/ is one unit, sub/ is not descended")
             (is (eq :dir (first (first units))))
             (is (string= "nested" (author-dir-unit-name (first units))))
             (is (equal '("extension.lisp")
                        (author-base-names (third (first units))))
                 "only the marked dir's top-level files are constituents")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(test (author-synth-cross-file-symbol :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((root (author-temp-tree)))
      (unwind-protect
           (progn
             (write-tree-file root "clock/util.lisp"
                              "(defun pad2 (n) (format nil \"~2,'0D\" n))")
             (write-tree-file root "clock/extension.lisp"
                              "(defextension clock
  (:provides
   (command \"clock\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command call-id on-update))
                (reply (pad2 (length (or (rest-arg arguments) \"\"))))))))")
             (app:load-user-extensions context :config '()
                                       :units (app:discover-units root))
             (let ((provider (author-commands-provider protocol)))
               (is (not (null (ext:provider-call provider :find-command :clock))))
               (is (string= "04"
                            (author-command-text
                             (ext:provider-call provider :invoke-command
                                                :clock '(:tail "abcd") context)))
                   "pad2 from util.lisp resolves in the synthesized package")))
        (author-delete-package "KLI/USER-EXT/CLOCK")
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(test (author-honor-package-cross-file :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((root (author-temp-tree)))
      (unwind-protect
           (progn
             (write-tree-file root "beta/package.lisp"
                              "(defpackage #:author-beta (:use #:cl #:kli/author))")
             (write-tree-file root "beta/helper.lisp"
                              "(in-package #:author-beta)
(defun greet (name) (format nil \"beta greets ~A\" name))")
             (write-tree-file root "beta/extension.lisp"
                              "(in-package #:author-beta)
(defextension beta
  (:provides
   (command \"beta\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command call-id on-update))
                (reply (greet (or (rest-arg arguments) \"world\")))))))")
             (app:load-user-extensions context :config '()
                                       :units (app:discover-units root))
             (let ((provider (author-commands-provider protocol)))
               (is (string= "beta greets Mika"
                            (author-command-text
                             (ext:provider-call provider :invoke-command
                                                :beta '(:tail "Mika") context)))
                   "greet from helper.lisp resolves in the honored package"))
             (is (null (find-package "KLI/USER-EXT/BETA"))
                 "a dir with its own package.lisp gets no synthesized package"))
        (author-delete-package "AUTHOR-BETA")
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(test (author-group-leaf-single :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((root (author-temp-tree)))
      (unwind-protect
           (progn
             (write-tree-file root "suite/alpha/extension.lisp"
                              "(defextension alpha
  (:provides
   (command \"alpha\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply \"alpha\")))))")
             (app:load-user-extensions context :config '()
                                       :units (app:discover-units root))
             (let ((provider (author-commands-provider protocol)))
               (is (not (null (ext:provider-call provider :find-command :alpha)))
                   "a group recurses to its leaf, which installs as one extension")
               (is (string= "alpha"
                            (author-command-text
                             (ext:provider-call provider :invoke-command
                                                :alpha '() context))))))
        (author-delete-package "KLI/USER-EXT/ALPHA")
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(test author-unit-two-defextensions-ambiguous
  (let ((root (author-temp-tree)))
    (unwind-protect
         (progn
           (write-tree-file root "twoext/extension.lisp"
                            "(defextension twoext-a (:provides))
(defextension twoext-b (:provides))")
           (let ((unit (first (app:discover-units root))))
             (signals error
               (ext:call-with-manifest-capture
                (lambda () (app::load-files-for-unit unit))))))
      (author-delete-package "KLI/USER-EXT/TWOEXT")
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(test author-unit-no-defextension-errors
  (let ((root (author-temp-tree)))
    (unwind-protect
         (progn
           (write-tree-file root "noext/extension.lisp" "(defun helper () 1)")
           (let ((unit (first (app:discover-units root))))
             (signals error
               (ext:call-with-manifest-capture
                (lambda () (app::load-files-for-unit unit))))))
      (author-delete-package "KLI/USER-EXT/NOEXT")
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(test (author-synth-package-reload-resets :fixture extension-load-authority)
  (multiple-value-bind (context protocol) (author-host)
    (let ((root (author-temp-tree))
          (units nil))
      (unwind-protect
           (progn
             (write-tree-file root "clock/util.lisp"
                              "(defun pad2 (n) (format nil \"~2,'0D\" n))")
             (write-tree-file root "clock/extension.lisp"
                              "(defextension clock
  (:provides
   (command \"clock\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command call-id on-update))
                (reply (pad2 (length (or (rest-arg arguments) \"\"))))))))")
             (setf units (app:discover-units root))
             (app:load-user-extensions context :config '() :units units)
             (let ((package (find-package "KLI/USER-EXT/CLOCK")))
               (is (not (null package)) "the synthesized package exists after load")
               (intern "STALE-JUNK" package)
               (is (find-symbol "STALE-JUNK" package)
                   "a stale symbol is present before reload"))
             (app:reload-user-extensions context :config '() :units units)
             (let ((package (find-package "KLI/USER-EXT/CLOCK")))
               (is (null (find-symbol "STALE-JUNK" package))
                   "reload recreates the package, dropping stale symbols")
               (is (string= "04"
                            (author-command-text
                             (ext:provider-call (author-commands-provider protocol)
                                                :invoke-command :clock
                                                '(:tail "abcd") context)))
                   "the command still runs after reload")))
        (author-delete-package "KLI/USER-EXT/CLOCK")
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(test (author-asd-manifest-honors-dependency-order :fixture extension-load-authority)
  ;; A dir unit carrying an .asd loads through ASDF, honoring its declared
  ;; component order. The components are listed so file-ordering convention
  ;; would break: consumer reads *base* from provider, and every file lives in
  ;; the package the packages component defines, yet alphabetically consumer
  ;; sorts before packages and provider. ASDF's :depends-on must drive the load.
  ;; Fasls must compile into the XDG cache, never beside the read-only source.
  (multiple-value-bind (context protocol) (author-host)
    (let ((root (author-temp-tree))
          (cache (author-temp-tree)))
      (with-env-var ("XDG_CACHE_HOME" (namestring cache))
        (unwind-protect
             (progn
               (write-tree-file root "packages.lisp"
                                "(defpackage #:kli-asd-probe (:use #:cl #:kli/author))")
               (write-tree-file root "provider.lisp"
                                "(in-package #:kli-asd-probe)
(defparameter *base* 40)")
               (write-tree-file root "consumer.lisp"
                                "(in-package #:kli-asd-probe)
(defparameter *derived* (+ *base* 2))")
               (write-tree-file root "extension.lisp"
                                "(in-package #:kli-asd-probe)
(defextension asdprobe
  (:provides
   (command \"asdprobe\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply (princ-to-string *derived*))))))")
               (write-tree-file root "asdprobe.asd"
                                "(defsystem \"asdprobe\"
  :serial nil
  :components ((:file \"extension\" :depends-on (\"consumer\"))
               (:file \"consumer\" :depends-on (\"provider\"))
               (:file \"provider\" :depends-on (\"packages\"))
               (:file \"packages\")))")
               (app:load-user-extensions context :config '()
                                         :units (app:discover-units root))
               (let ((provider (author-commands-provider protocol)))
                 (is (not (null (ext:provider-call provider :find-command :asdprobe)))
                     "the .asd unit indexes and installs as one extension")
                 (is (string= "42"
                              (author-command-text
                               (ext:provider-call provider :invoke-command
                                                  :asdprobe '() context)))
                     "consumer sees provider's *base* -- ASDF honored :depends-on"))
               (is (not (null (directory
                               (merge-pathnames "**/*.fasl"
                                                (app::extension-fasl-cache-root)))))
                   "extension systems compile into the writable XDG cache"))
          (author-delete-package "KLI-ASD-PROBE")
          (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
          (uiop:delete-directory-tree cache :validate t :if-does-not-exist :ignore))))))

(test author-discover-units-asd-over-nested-marker
  ;; A root .asd claims the whole tree as one unit even when the extension.lisp
  ;; marker is nested, so the dir is not descended and the unit roots at the .asd.
  (let ((root (author-temp-tree)))
    (unwind-protect
         (progn
           (write-tree-file root "probe.asd" "(defsystem \"probe\")")
           (write-tree-file root "src/extension.lisp" "")
           (write-tree-file root "src/util.lisp" "")
           (let ((units (app:discover-units root)))
             (is (= 1 (length units)) "the root .asd makes the whole tree one unit")
             (is (eq :dir (first (first units))))
             (is (string= (namestring (truename root))
                          (namestring (truename (second (first units)))))
                 "the unit roots at the .asd dir, not the nested marker's src/")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(test (author-asd-root-loads-across-nested-marker :fixture extension-load-authority)
  ;; A root .asd whose extension.lisp marker is NESTED. Convention ordering loads
  ;; consumer before provider (alphabetical) and before the shared package, so
  ;; only ASDF's declared :depends-on makes the tree load at all.
  (multiple-value-bind (context protocol) (author-host)
    (let ((root (author-temp-tree))
          (cache (author-temp-tree)))
      (with-env-var ("XDG_CACHE_HOME" (namestring cache))
        (unwind-protect
             (progn
               (write-tree-file root "src/packages.lisp"
                                "(defpackage #:kli-asd-nested-probe (:use #:cl #:kli/author))")
               (write-tree-file root "src/provider.lisp"
                                "(in-package #:kli-asd-nested-probe)
(defparameter *base* 40)")
               (write-tree-file root "src/consumer.lisp"
                                "(in-package #:kli-asd-nested-probe)
(defparameter *derived* (+ *base* 2))")
               (write-tree-file root "src/extension.lisp"
                                "(in-package #:kli-asd-nested-probe)
(defextension asdnested
  (:provides
   (command \"asdnested\"
     :handler (lambda (command arguments context &key call-id on-update)
                (declare (ignore command arguments context call-id on-update))
                (reply (princ-to-string *derived*))))))")
               (write-tree-file root "nestedprobe.asd"
                                "(defsystem \"nestedprobe\"
  :serial nil
  :components ((:module \"src\"
                :serial nil
                :components ((:file \"extension\" :depends-on (\"consumer\"))
                             (:file \"consumer\" :depends-on (\"provider\"))
                             (:file \"provider\" :depends-on (\"packages\"))
                             (:file \"packages\")))))")
               (app:load-user-extensions context :config '()
                                         :units (app:discover-units root))
               (let ((provider (author-commands-provider protocol)))
                 (is (not (null (ext:provider-call provider :find-command :asdnested)))
                     "the root-.asd/nested-marker tree indexes and installs as one extension")
                 (is (string= "42"
                              (author-command-text
                               (ext:provider-call provider :invoke-command
                                                  :asdnested '() context)))
                     "consumer saw provider's *base* -- ASDF drove declared order past the nested marker")))
          (author-delete-package "KLI-ASD-NESTED-PROBE")
          (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
          (uiop:delete-directory-tree cache :validate t :if-does-not-exist :ignore))))))

(test author-place-verified-artifact-forms-unit-via-discover
  ;; place-verified-artifact forms its unit through discover-units, so a placed
  ;; tree yields exactly the unit a later boot's discover-units finds. With a
  ;; nested marker the unit roots below the placement dir; the old hand-rolled
  ;; (:dir <id>/ ...) rooted one level too high and would not rediscover.
  (let ((app::*remote-install-staging-root* (author-temp-tree)))
    (unwind-protect
         (flet ((octets (s) (sb-ext:string-to-octets s :external-format :utf-8)))
           (let* ((payload
                    (list (cons "inner/package.lisp"
                                (octets "(defpackage #:kli/tests/place-probe (:use #:cl #:kli/author))"))
                          (cons "inner/extension.lisp"
                                (octets "(in-package #:kli/tests/place-probe)"))))
                  (unit (app::place-verified-artifact "placeprobe" payload :directory))
                  (dir (second unit)))
             (is (eq :dir (first unit)))
             (is (string= "inner" (author-dir-unit-name unit))
                 "the placed unit roots at the nested marker dir, not the placement root")
             (is (equal (app:discover-units dir) (list unit))
                 "place returns exactly the unit discover-units finds for the placed tree")))
      (uiop:delete-directory-tree app::*remote-install-staging-root*
                                  :validate t :if-does-not-exist :ignore))))
