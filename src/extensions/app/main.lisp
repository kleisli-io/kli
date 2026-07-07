(in-package #:kli/app)

(defvar *current-context* nil
  "Debug-only REPL handle on the active context, never read by the harness. Authoritative state is the context main returns and the per-protocol live-object registry.")
(defvar *current-app* nil
  "Debug-only REPL handle on the active app, never read by the harness. Authoritative state is the context main returns and the per-protocol live-object registry.")

(defvar *boot-snapshot-context* nil
  "A context with the default profile pre-installed, baked into the image at dump
time so boot reuses it instead of re-running the deterministic manifest install on
every start. NIL in a plain image, which always does the full install. Holds only
the pure installed object graph -- no open fds, threads, sockets, or db handles,
which are all acquired at session bind or first use -- so it survives the dump.
Settings are captured from the build environment (empty); boot rebinds the config
dirs, reloads the user's settings, and re-applies the wiring before reuse.")

(define-capability-binding tui-app
  :capability :tui/app
  :contract :tui/app/v1)

(defparameter +profile-arg+ "--profile"
  "Command-line flag selecting the boot profile by name.")

(defparameter +profile-env-var+ "KLI_PROFILE"
  "Environment fallback for the boot profile name when --profile is absent.")

(defparameter *default-profile* :interactive-terminal
  "Profile booted when neither --profile nor KLI_PROFILE names one.")

(defun profile-arg-value (argv)
  "The token following --profile in ARGV, or NIL when the flag is absent."
  (loop for (flag value) on argv
        when (string= flag +profile-arg+)
          do (return value)))

(defun settings-profile-token (settings)
  "The non-empty string under the \"profile\" settings key, or NIL when the
key is absent or holds any other shape."
  (let ((value (settings-value settings "profile")))
    (and (stringp value) (plusp (length value)) value)))

(defun select-profile-name (&key (argv (uiop:command-line-arguments))
                                 (env (uiop:getenv +profile-env-var+))
                                 (settings (load-settings)))
  "Resolve the boot profile name: --profile, then KLI_PROFILE, then the
\"profile\" settings key (project over global via the merged SETTINGS), then
the default. Returns a normalized keyword."
  (let ((token (or (profile-arg-value argv)
                   (and env (plusp (length env)) env)
                   (settings-profile-token settings))))
    (if token
        (normalize-extension-id token)
        *default-profile*)))

(defun resolve-boot-profile (name settings)
  "Resolved profile for NAME against the data profiles declared in SETTINGS.
A name that is neither a builtin nor a resolvable data profile warns and
falls back to *default-profile* (fail-soft, the warning surfaces as a :boot
diagnostic)."
  (let ((specs (parse-profile-specs settings)))
    (or (resolve-profile-spec name specs)
        (progn
          (warn "kli: booting the default ~(~A~) profile instead"
                *default-profile*)
          (resolve-profile-spec *default-profile* specs)))))

(defun main (&key (profile *default-profile*) (settings (load-settings)))
  "Boot a fresh context and install the named profile. A data profile from
SETTINGS installs its base builtin's manifest and records the resolved
profile, whose deltas gate the user-extension phase. The profile's settings
are recorded as the overlay layer before the install, so the config service
carries them when settings-wiring applies. The install trust roots are resolved
from SETTINGS and bound here, the one boot every entry point shares, so the
headless install verb, the MCP install tool, and an interactive session enforce
one authenticity policy. Returns the context, constructs no TUI app, and enters
no run loop. Output printed during the install (settings warnings most commonly)
is captured as a :boot diagnostic -- the TUI takeover would wipe it from the
terminal, so it surfaces through the transcript instead."
  (let* ((context (make-kernel-host))
         (boot (active-protocol context))
         (protocol (install-protocol boot (make-extension-protocol) context)))
    (switch-protocol boot (object-id protocol) context)
    (call-with-diagnostics-capture
     protocol :boot
     (lambda ()
       (setf *registry-trust-roots*
             (parse-trust-roots (settings-value settings "trustRoots")))
       (let ((resolved (resolve-boot-profile profile settings)))
         (record-settings-overlay protocol
                                  (resolved-profile-settings resolved))
         ;; Boot installs the first-party profile under substrate authority.
         (with-system-authority
           (install-manifest (find-profile-manifest (resolved-profile-base
                                                     resolved))
                             protocol context))
         (record-active-profile protocol resolved))))
    (setf *current-context* context
          *current-app* nil)
    context))

(defun profile-interactive-p (context)
  "True when the booted profile installed the terminal-UI app, so the boot
path should construct the TUI and enter its loop."
  (extension-loaded-p (active-protocol context) :tui-app))

(defun write-fatal-log (condition)
  (handler-case
      (let ((path (merge-pathnames "kli/last-fatal.log"
                                   (uiop:xdg-cache-home))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
          (format out "~&~%==== ~A ====~%~A~%"
                  (get-universal-time) condition)
          #+sbcl (sb-debug:print-backtrace :stream out :count 60))
        path)
    (error () nil)))

(defun fatal-exit (condition)
  (let ((path (write-fatal-log condition)))
    (ignore-errors
     (format *error-output* "~&Fatal: ~A~%~@[(details: ~A)~%~]"
             condition path)))
  (uiop:quit 2))

(defmacro with-fatal-error-handler (() &body body)
  "Binary boundary. The outer handler-case is the primary fatal contract on both binaries (CL evaluates handler clauses before invoke-debugger). The thread-aware *debugger-hook* let-binding is the primary contract on the wrapper-less binary and is shadowed by the wrapper's sb-ext:*invoke-debugger-hook* on the swank-bearing one."
  (let ((main-thread (gensym "MAIN-THREAD")))
    `(let* ((,main-thread sb-thread:*current-thread*)
            (*debugger-hook*
              (lambda (c h)
                (declare (ignore h))
                (cond
                  ((eq sb-thread:*current-thread* ,main-thread)
                   (fatal-exit c))
                  (t
                   (let ((*debugger-hook* nil))
                     (invoke-debugger c)))))))
       (handler-case (progn ,@body)
         (error (c) (fatal-exit c))))))

(defun build-tui-app (context &key extra-manifests)
  "Construct the interactive TUI app over the active protocol, which the
selected profile has already populated with the terminal-UI extensions.
EXTRA-MANIFESTS (debug-only, requiring the TUI app) install on top first."
  (let ((protocol (active-protocol context)))
    (let ((*call-subject* *install-subject*))
      (dolist (manifest extra-manifests)
        (install-manifest manifest protocol context)))
    (let ((*call-subject* *ui-subject*))
      (let ((app (tui-app-call protocol :make-tui-app
                               :terminal-kind :process
                               :context context)))
        (setf *current-app* app)
        (with-boot-stage ("session-restore") (bind-default-session app context))
        (register-spinner-widget app context)
        (register-context-usage-footer app context)
        app))))

(defun context-usage-footer-text (context mode-id)
  "Live footer text for CONTEXT's current model and MODE-ID's latest reported
usage, or NIL when no model registry is present. Reads the current selection and
the binding's last usage, then formats the readout (trailing-token refinement is
deferred -- messages/entries are NIL for now)."
  (let* ((registry-store (context-registry context))
         (registry (find-live-object registry-store :model-registry-service))
         (service (find-live-object registry-store :agent-session-service))
         (binding (and service (gethash mode-id (session-mode-bindings service))))
         (cb (and binding (mode-binding-context-binding binding)))
         (usage (and cb (or (context-binding-live-usage cb)
                            (context-binding-usage cb))))
         (selection (and service (mode-current-selection service mode-id context))))
    (and registry
         (format-context-usage-readout
          (session-context-usage-readout registry selection usage nil nil)))))

(defun register-context-usage-footer (app context)
  "Register the context-usage footer widget on APP's protocol. The widget pulls
the live readout on every render, so the footer tracks the current model and
token usage with no event-driven cache to invalidate. A read fault degrades to
an empty footer rather than breaking the render loop."
  (let ((mode-id (tui-app-mode-id app)))
    (register-widget
     (active-protocol context)
     :context
     (lambda (protocol theme width)
       (declare (ignore protocol theme))
       (let ((text (ignore-errors (context-usage-footer-text context mode-id))))
         (when (and text (plusp (length text)))
           (list (if (> (length text) width)
                     (subseq text 0 width)
                     text))))))))

(defun register-spinner-widget (app context)
  "Register the working-indicator widget on APP's protocol, placed directly
   above the input box. The closure reads APP's live spinner state every frame,
   so the glyph tracks the poll-driven phase with no event cache, and it
   contributes nothing while idle. The latest tool-execution progress update
   rides along as the line's detail."
  (register-widget
   (active-protocol context)
   :spinner
   (lambda (protocol theme width)
     (declare (ignore protocol theme))
     (render-spinner-line (tui-app-spinner-active-p app)
                          (tui-app-spinner-phase app)
                          width
                          (tui-app-tool-update-text app)))
   :placement :above-input))

(defparameter +continue-args+ '("-c" "--continue")
  "Command-line flags that resume the most recently stored session on boot.")

(defun continue-last-requested-p (&key (argv (uiop:command-line-arguments)))
  "True when ARGV carries a continue-last flag."
  (loop for arg in argv
        thereis (member arg +continue-args+ :test #'string=)))

(defun latest-session-id (store)
  "Id of the newest loadable stored session in STORE, or NIL when there are
none. Skips :corrupt rows so continue-last boot never tries to resume a file
with no loadable header."
  (loop for row in (kli/session/log:list-stored-sessions store)
        unless (getf row :corrupt)
          return (getf row :id)))

(defun bind-default-session (app context
                             &key (continue-p (continue-last-requested-p)))
  "Bind an agent session to APP's mode so the user can chat immediately on boot.
With CONTINUE-P and a stored session present, resume the newest one, else start
fresh. No-op when the agent-session service is absent (e.g. minimal profiles)."
  (let ((service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (when service
      (let* ((mode-id (tui-app-mode-id app))
             (store   (find-live-object (context-registry context)
                                        :session-store))
             (last-id (and continue-p store (latest-session-id store))))
        (if last-id
            (resume-agent-session service mode-id last-id context)
            (reset-agent-session service mode-id context :reason :initial))
        (focus-agent-session-mode service mode-id context)))))

(defparameter +fault-injection-env-var+ "KLI_DEBUG_FAULT_INJECTION"
  "Binary-boundary opt-in for fault-injection extensions. When set in the process environment, run-tui-main installs the debug-only extensions listed by fault-injection-manifests on top of the normal TUI boot. Unset in production, so production builds never register the debug commands.")

(defun fault-injection-manifests ()
  "Extra extension manifests to install when the fault-injection env var
   is set. Returns NIL when unset, so build-tui-app's extra-manifests loop
   becomes a no-op in production."
  (when (uiop:getenv +fault-injection-env-var+)
    (list kli/debug:*fault-injection-extension-manifest*)))

(defun run-headless (context)
  "Hold the main thread so the booted substrate stays alive without a terminal UI loop. The agent-session remains reachable over swank on the debug binary."
  (declare (ignore context))
  (loop (sleep 3600)))

(defun write-boot-diagnostics-log (entries)
  "Write captured extension diagnostics to the cache log, superseding the
previous boot's. Returns the path, or NIL when the cache is unwritable."
  (handler-case
      (let ((path (merge-pathnames "kli/boot-diagnostics.log"
                                   (uiop:xdg-cache-home))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
          (dolist (entry entries)
            (format out "~&==== ~A ====~%~A~%"
                    (extension-diagnostic-label (car entry)) (cdr entry))))
        path)
    (error () nil)))

(defparameter +inline-diagnostic-limit+ 200
  "A captured diagnostic at most this long and on one line (a settings
warning, typically) renders its text directly in the transcript. Longer or
multi-line output gets a pointer row to the cache log instead.")

(defun surface-boot-diagnostics (app context)
  "Report diagnostics captured during boot as transcript system events - the
takeover wipes anything printed to the tty, so the transcript is where the
user learns an extension misbehaved or a setting was ignored. The full text
lands in the cache log. Short one-line diagnostics render inline, anything
bigger gets a row naming its source and pointing at the log."
  (let* ((protocol (active-protocol context))
         (entries (extension-diagnostics protocol)))
    (when entries
      (let ((path (write-boot-diagnostics-log entries)))
        (dolist (entry entries)
          (let ((text (cdr entry)))
            (tui-app-call protocol :tui-app-add-system-event app
                          (if (and (<= (length text) +inline-diagnostic-limit+)
                                   (not (find #\Newline text)))
                              (format nil "~A: ~A"
                                      (extension-diagnostic-label (car entry))
                                      text)
                              (format nil "Extension diagnostics from ~A~@[ (see ~A)~]."
                                      (extension-diagnostic-label (car entry))
                                      path)))))))))

(defun report-boot-diagnostics (context)
  "Headless boots have no transcript - print captured extension diagnostics to
standard error as they would have appeared without the capture."
  (dolist (entry (extension-diagnostics (active-protocol context)))
    (format *error-output* "~&~A~%" (cdr entry))))

(defun run-selected-profile (context &key extra-manifests)
  (if (profile-interactive-p context)
      (let ((app (with-boot-stage ("build-tui-app")
                   (build-tui-app context :extra-manifests extra-manifests))))
        (surface-boot-diagnostics app context)
        (let ((*call-subject* *ui-subject*))
          (boot-marker "enter-tui-loop")
          (tui-app-call (active-protocol context) :run-tui-app app)))
      (progn
        (report-boot-diagnostics context)
        (run-headless context))))

(defun build-boot-snapshot ()
  "Install the default profile into a fresh context and record it as the boot
snapshot. Invoked at image-dump time, after every extension has loaded, so the
dumped image carries the installed protocol ready to reuse. The config service
loads whatever settings the build environment carries (none), so boot must rebind
and reload before reuse. Clears the debug REPL handles main sets, so a booted
image starts clean. A condition escaping the install is reported on the stream
bound at entry -- the process stderr at dump time -- before exiting nonzero:
main's diagnostics capture rebinds the output streams, so the script debugger's
report of an unhandled error would land in the capture buffer and vanish with
the dying process."
  (let ((stderr *error-output*))
    (handler-bind ((serious-condition
                     (lambda (condition)
                       (format stderr "~&Fatal: boot snapshot install failed: ~A~%"
                               condition)
                       (sb-debug:print-backtrace :stream stderr :count 60)
                       (finish-output stderr)
                       (sb-posix:exit 1))))
      (let ((*image-dump-in-progress* t))
        (setf *boot-snapshot-context*
              (main :profile *default-profile* :settings (load-settings))
              *current-context* nil
              *current-app* nil))))
  *boot-snapshot-context*)

(defun boot-snapshot-usable-p (settings &key (argv (uiop:command-line-arguments))
                                             (env (uiop:getenv +profile-env-var+)))
  "True when the baked snapshot matches this boot: a snapshot exists, the default
profile is the interactive terminal, and nothing overrides the profile (--profile,
KLI_PROFILE, or the settings \"profile\" key). Any override falls back to a full
install, since the snapshot only carries the default profile."
  (and *boot-snapshot-context*
       (eq *default-profile* :interactive-terminal)
       (null (profile-arg-value argv))
       (not (and env (plusp (length env))))
       (null (settings-profile-token settings))))

(defun reuse-boot-snapshot (settings)
  "Rehydrate the baked snapshot for this boot.

Runtime config directories and profile settings are rebound as data first; only
after that do installed contributions refresh in full install order. Refresh
hooks own explicit runtime-sensitive install-time effects (for example provider
credentials/catalogues and settings consumers) while preserving the installed
topology. Returns the context, or NIL when no usable snapshot matches so the
caller does a full install."
  (when (boot-snapshot-usable-p settings)
    (let* ((context *boot-snapshot-context*)
           (protocol (active-protocol context))
           (overlay (resolved-profile-settings
                     (resolve-boot-profile *default-profile* settings))))
      (clear-extension-diagnostics protocol)
      (call-with-diagnostics-capture
       protocol :boot
       (lambda ()
         (rebind-config-dirs (find-config-service context))
         (record-settings-overlay protocol overlay)
         (set-settings-overlay (find-config-service context) overlay)
         (with-system-authority
           (refresh-runtime-contributions protocol context))))
      (setf *current-context* context *current-app* nil)
      context)))

(defun profile-install-context (settings &key snapshot-failure)
  (let ((context (with-boot-stage ("profile-install")
                   (main :profile (select-profile-name :settings settings)
                         :settings settings))))
    (when snapshot-failure
      (record-extension-diagnostic
       (active-protocol context)
       :snapshot-reuse
       (format nil "Snapshot reuse failed; fell back to full profile install: ~A"
               snapshot-failure)))
    context))

(defun boot-context (settings)
  (multiple-value-bind (snapshot-context snapshot-failure)
      (handler-case
          (values (with-boot-stage ("snapshot-reuse")
                    (reuse-boot-snapshot settings))
                  nil)
        (error (condition)
          (setf *boot-snapshot-context* nil)
          (values nil condition)))
    (or snapshot-context
        (profile-install-context settings :snapshot-failure snapshot-failure))))

(defun run-tui-main ()
  (with-fatal-error-handler ()
    (let* ((settings (with-boot-stage ("load-settings") (load-settings)))
           (context (boot-context settings)))
      (with-boot-stage ("user-extensions") (boot-user-extensions context))
      (run-selected-profile context
                            :extra-manifests (fault-injection-manifests)))))

(defun relocation-probe-main ()
  "Non-interactive entrypoint that reports the relocation state and exits. The init
hooks have already fired by the time any toplevel runs, so this only observes: it
prints the blessed SONAME set recorded at build time, where each one resolves via
locate-relocated-lib in the current environment, the surviving cl+ssl loaded-flag
for reference, and whether a libcrypto symbol is actually resolvable and callable in
the running image. The loaded-flag is a stale value carried across the dump and is
not load-bearing, so the exit status keys on the live calls instead: 0 only when both
openssl and sqlite resolve a live symbol that returns a version string, 1 otherwise. A
relocated boot must therefore prove each blessed lib is usable, not merely that a flag
survived the dump."
  (format t "~&blessed-sonames: ~{~A~^, ~}~%"
          kli/runtime/relocation:*blessed-sonames*)
  (dolist (soname kli/runtime/relocation:*blessed-sonames*)
    (format t "~&  ~A -> ~A~%"
            soname
            (kli/runtime/relocation:locate-relocated-lib soname)))
  (format t "~&libcrypto-loaded-flag: ~A~%"
          (cffi:foreign-library-loaded-p 'cl+ssl::libcrypto))
  (let* ((libssl-ptr (cffi:foreign-symbol-pointer "TLS_method"))
         (libcrypto-usable
           (handler-case
               (let ((version-fn (cffi:foreign-symbol-pointer "OpenSSL_version")))
                 (if (null version-fn)
                     (progn (format t "~&openssl-call: unresolved~%") nil)
                     (let ((version (cffi:foreign-funcall-pointer
                                     version-fn () :int 0 :string)))
                       (format t "~&openssl-call: ~A~%" version)
                       (and (stringp version) (plusp (length version))))))
             (error (condition)
               (format t "~&openssl-call: error ~A~%" condition)
               nil)))
         (usable (and libcrypto-usable (not (null libssl-ptr)))))
    (format t "~&libssl-symbol-resolved: ~A~%" (not (null libssl-ptr)))
    (format t "~&openssl-usable: ~A~%" usable)
    (let* ((sqlite-loaded-flag (cffi:foreign-library-loaded-p 'sqlite-ffi::sqlite3-lib))
           (sqlite-usable
             (handler-case
                 (let ((version-fn (cffi:foreign-symbol-pointer "sqlite3_libversion")))
                   (if (null version-fn)
                       (progn (format t "~&sqlite-call: unresolved~%") nil)
                       (let ((version (cffi:foreign-funcall-pointer
                                       version-fn () :string)))
                         (format t "~&sqlite-call: ~A~%" version)
                         (and (stringp version) (plusp (length version))))))
               (error (condition)
                 (format t "~&sqlite-call: error ~A~%" condition)
                 nil))))
      (format t "~&libsqlite3-loaded-flag: ~A~%" sqlite-loaded-flag)
      (format t "~&sqlite-usable: ~A~%" sqlite-usable)
      (finish-output)
      (sb-posix:exit (if (and usable sqlite-usable) 0 1)))))
