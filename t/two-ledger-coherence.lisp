(in-package #:kli/tests)
(in-suite all)

(defun coherence-entry (&key (id :demo) (version "1.0.0"))
  (app::make-user-extension-entry
   :id id
   :manifest (lambda () (ext:make-extension :id id))
   :metadata (list :version version)))

(defun shadow-reasons (protocol)
  (mapcar (lambda (record) (getf record :reason))
          (app::nix-shadowed-installs protocol)))

(test (boot-install-skips-nix-declared-baseline :fixture extension-load-authority)
  "A disk extension whose id is a nix-declared baseline is shadowed at boot:
not installed, recorded :shadowed-by-nix-baseline. With no baseline it installs."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (setf (gethash :demo (app::available-extensions protocol)) (coherence-entry))
    (let ((profiles:*nix-declared-baseline-ids* (list "demo")))
      (app::install-enabled-extensions protocol nil context))
    (is (null (gethash :demo (app::installed-user-handles protocol)))
        "the shadowed extension is not installed")
    (is (member :shadowed-by-nix-baseline (shadow-reasons protocol))))
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (setf (gethash :demo (app::available-extensions protocol)) (coherence-entry))
    (app::install-enabled-extensions protocol nil context)
    (is (gethash :demo (app::installed-user-handles protocol))
        "without a baseline the extension installs normally")))

(test restore-replay-skips-nix-declared-baseline
  "A registry pin whose id is now nix-declared is shadowed on restore: skipped
and recorded -- keyed on the image's baseline, not the pin's source-kind."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (setf (gethash :demo (app::available-extensions protocol)) (coherence-entry))
    (let ((profiles:*nix-declared-baseline-ids* (list "demo")))
      (is (eq :shadowed
              (app:reinstall-remote-pin
               (list :id "demo" :source-kind :registry :version "1.0.0")
               protocol context))))
    (is (null (gethash :demo (app::installed-user-handles protocol))))
    (is (member :shadowed-by-nix-baseline (shadow-reasons protocol)))))

(test uninstall-refuses-nix-declared
  "Uninstall of a nix-declared baseline id is refused with the nix-declared
message; the runtime cannot remove what the image owns."
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((profiles:*nix-declared-baseline-ids* (list "fixture-ext")))
      (multiple-value-bind (ok state) (app:uninstall-remote-extension context "fixture-ext")
        (is (null ok))
        (is (eq :nix-declared state)))
      (let ((message (app::uninstall-reply-text context "fixture-ext")))
        (is (search "Nix-declared" message))
        (is (search "kli.extensions" message))))))

(test (uninstall-drops-handle-and-pin :fixture interactive-authority)
  "Uninstall of a runtime-installed extension drops both its live handle and
its durable install-set pin."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (app::install-user-extension protocol (coherence-entry) context)
    (app:record-remote-install-pin
     protocol (list :id "demo" :source-kind :registry :version "1.0.0"))
    (is (gethash :demo (app::installed-user-handles protocol)))
    (is (gethash "demo" (app:remote-install-pins protocol)))
    (multiple-value-bind (ok state) (app:uninstall-remote-extension context "demo")
      (is-true ok)
      (is (eq :uninstalled state)))
    (is (null (gethash :demo (app::installed-user-handles protocol))))
    (is (zerop (hash-table-count (app:remote-install-pins protocol))))))

(test uninstall-of-unknown-is-not-installed
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (multiple-value-bind (ok state) (app:uninstall-remote-extension context "ghost")
      (is (null ok))
      (is (eq :not-installed state)))))

(test disable-of-baseline-child-is-not-enabled-no-op
  "A nix-declared baseline child is absent from available-extensions and
installed-user-handles, so /disable is a :not-enabled no-op; only a rebuild
removes a baseline child."
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((profiles:*nix-declared-baseline-ids* (list "fixture-ext")))
      (multiple-value-bind (ok state) (app:disable-user-extension context "fixture-ext")
        (is (null ok))
        (is (eq :not-enabled state))))))

(test nix-baseline-writes-no-pin-and-not-enumerated
  "Ledger A writes zero pins: a nix-declared baseline never enters the
install-set, so an uninstall-all enumeration never hands a projected nix pin to
retract."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (let ((profiles:*nix-declared-baseline-ids* (list "fixture-ext")))
      (app:record-remote-install-pin
       protocol (list :id "blessed" :source-kind :registry :version "1.0.0"))
      (let ((ids (loop for id being the hash-keys of (app:remote-install-pins protocol)
                       collect id)))
        (is (member "blessed" ids :test #'string=))
        (is (not (member "fixture-ext" ids :test #'string=))))
      (maphash (lambda (id pin)
                 (declare (ignore id))
                 (is (not (eq :nix (getf pin :source-kind)))))
               (app:remote-install-pins protocol)))))

(test nix-config-plist-to-table-converts-keys-and-nests
  "Baked settings convert to a string-keyed table: key names map verbatim, so
the serialiser's bar-escaped camelCase keys survive, and nested objects recurse."
  (let ((table (config::nix-config-plist->table
                '(:|defaultProvider| "synthetic" :|model| "sonnet"
                  :|nested| (:|depth| 2)))))
    (is (equal "synthetic" (gethash "defaultProvider" table)))
    (is (equal "sonnet" (gethash "model" table)))
    (is (hash-table-p (gethash "nested" table)))
    (is (eql 2 (gethash "depth" (gethash "nested" table))))))

(test settings-nix-baked-is-lowest-layer
  "nix-baked settings are the lowest layer: a shared key resolves to global,
a nix-only key survives."
  (let ((root (ensure-directories-exist
               (merge-pathnames "kli-coherence-settings/"
                                (uiop:temporary-directory)))))
    (let ((global (merge-pathnames "settings.json" root)))
      (with-open-file (out global :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
        (write-string "{\"shared\":\"from-global\"}" out))
      (let ((merged (config:load-settings
                     :global-path global :project-path nil
                     :nix-baked (config::nix-config-plist->table
                                 '(:|shared| "from-nix" :|only-nix| "kept")))))
        (is (equal "from-global" (config:settings-value merged "shared")))
        (is (equal "kept" (config:settings-value merged "only-nix")))))))
