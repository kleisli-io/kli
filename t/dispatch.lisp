(in-package #:kli/tests)
(in-suite all)

;;;; argv dispatch routing, the version surface, and self-update refusal.

(test dispatch-routes-leading-token
  "dispatch-command maps the leading token to a command keyword; on an
interactive terminal UI flags and unrecognized tokens fall through to the
terminal UI, and on piped stdin they route to one-shot print mode."
  (flet ((cmd (argv &optional (interactive t))
           (nth-value 0 (app::dispatch-command argv interactive))))
    (is (eq :tui (cmd '())))
    (is (eq :version (cmd '("version"))))
    (is (eq :version (cmd '("--version"))))
    (is (eq :version (cmd '("-V"))))
    (is (eq :update (cmd '("update"))))
    (is (eq :docs (cmd '("docs"))))
    (is (eq :install (cmd '("install"))))
    (is (eq :help (cmd '("help"))))
    (is (eq :help (cmd '("--help"))))
    (is (eq :help (cmd '("-h"))))
    (is (eq :tui (cmd '("--profile" "minimal"))))
    (is (eq :tui (cmd '("-c"))))
    (is (eq :tui (cmd '("frobnicate"))))
    ;; Piped (non-terminal) stdin: no command and unrecognized tokens route to
    ;; one-shot print mode; explicit commands are unaffected.
    (is (eq :print (cmd '() nil)))
    (is (eq :print (cmd '("frobnicate") nil)))
    (is (eq :print (cmd '("--profile" "minimal") nil)))
    (is (eq :version (cmd '("version") nil)))
    (is (eq :help (cmd '("--help") nil)))
    (is (eq :mcp-serve (cmd '("mcp-serve") nil)))))

(test dispatch-passes-command-args-through
  "The matched command carries the tokens that follow it; a fall-through to the
UI keeps the whole argument vector."
  (multiple-value-bind (command args) (app::dispatch-command '("update" "--yes"))
    (is (eq :update command))
    (is (equal '("--yes") args)))
  (multiple-value-bind (command args)
      (app::dispatch-command '("docs" "extend/lisp-extensions/anatomy"))
    (is (eq :docs command))
    (is (equal '("extend/lisp-extensions/anatomy") args)))
  (multiple-value-bind (command args) (app::dispatch-command '("--profile" "x") t)
    (is (eq :tui command))
    (is (equal '("--profile" "x") args)))
  (multiple-value-bind (command args) (app::dispatch-command '("--profile" "x") nil)
    (is (eq :print command))
    (is (equal '("--profile" "x") args))))

(test print-version-carries-current-version
  "print-version writes a line containing the SemVer and the build id."
  (let ((line (with-output-to-string (s) (app::print-version s))))
    (is (search app:+kli-version+ line))
    (is (search (app:build-id) line))))

(test nix-store-binary-detected
  "nix-store-binary-p recognizes a Nix-store image and rejects a user-local one."
  (is-true (app::nix-store-binary-p "/nix/store/abcd-kli/bin/kli"))
  (is-false (app::nix-store-binary-p "/home/u/.local/lib/kli/bin/kli"))
  (is-false (app::nix-store-binary-p nil)))

(test detect-install-dir-walks-to-root
  "detect-install-dir signals on a store image and walks up from
<root>/lib/kli/bin/kli to the install root for a tarball image."
  (signals app::nix-managed-binary
    (app::detect-install-dir "/nix/store/abcd-kli/bin/kli"))
  (is (equal "/home/u/.local/"
             (namestring (app::detect-install-dir
                          "/home/u/.local/lib/kli/bin/kli")))))

(test self-update-refuses-on-nix-store
  "run-self-update refuses with a non-zero code on a Nix-store image and never
reaches the network."
  (let ((app::*self-update-binary-path* "/nix/store/abcd-kli/bin/kli")
        (app::*self-update-http*
          (lambda (url) (declare (ignore url)) (error "network must not be reached"))))
    (let* ((err (make-string-output-stream))
           (code (let ((*error-output* err)) (app::run-self-update '()))))
      (is (eql 1 code))
      (is (search "Nix" (get-output-stream-string err))))))

(test parse-checksums-extracts-matching-line
  "parse-checksums returns the hash on the artifact's tarball line and nil when
the artifact is absent."
  (let ((text (format nil "aaaa  kli-linux-x86_64.tar.gz~%bbbb  kli-darwin-aarch64.tar.gz~%")))
    (is (equal "aaaa" (app::parse-checksums text "kli-linux-x86_64")))
    (is (equal "bbbb" (app::parse-checksums text "kli-darwin-aarch64")))
    (is (null (app::parse-checksums text "kli-linux-aarch64")))))

(test release-artifact-name-is-os-arch
  "release-artifact-name names a kli-<os>-<arch> artifact for the build host."
  (let ((name (app::release-artifact-name)))
    (is (eql 0 (search "kli-" name)))
    (is (search (app::host-os) name))
    (is (search (app::host-arch) name))))

(test parse-update-args-reads-flags
  "parse-update-args reads --yes and --version and rejects an unknown option."
  (multiple-value-bind (auto target) (app::parse-update-args '("--yes"))
    (is-true auto)
    (is (null target)))
  (multiple-value-bind (auto target) (app::parse-update-args '("--version" "v0.2.0"))
    (is-false auto)
    (is (equal "v0.2.0" target)))
  (signals error (app::parse-update-args '("--bogus"))))

;;;; The headless `install` verb: argv parsing, then the end-to-end driver over a
;;;; real loopback origin (the web-install fixture serves the artifact). run-install
;;;; boots the headless profile, so these witness the whole CLI path -- boot, verify,
;;;; durable non-activating placement, and the state-derived exit code.

(defun install-cli-staging-root ()
  "A fresh staging root per test, so a placement check never reads another test's
file."
  (ensure-directories-exist
   (merge-pathnames (format nil "kli-install-cli-~A/" (gensym))
                    (uiop:temporary-directory))))

(test parse-install-argv-reads-url-sha-and-yes
  "parse-install-argv reads the two positionals and --yes/-y, and reports a usage
failure on a missing, extra, or unknown-flag invocation."
  (multiple-value-bind (url sha auto ok)
      (app::parse-install-argv '("http://h/x.lisp" "abc"))
    (is (equal "http://h/x.lisp" url))
    (is (equal "abc" sha))
    (is-false auto)
    (is-true ok))
  (multiple-value-bind (url sha auto ok)
      (app::parse-install-argv '("--yes" "http://h/x" "abc"))
    (declare (ignore url sha))
    (is-true auto)
    (is-true ok))
  (is-true (nth-value 2 (app::parse-install-argv '("http://h/x" "-y" "abc"))))
  (is-false (nth-value 3 (app::parse-install-argv '())))
  (is-false (nth-value 3 (app::parse-install-argv '("only-url"))))
  (is-false (nth-value 3 (app::parse-install-argv '("u" "s" "extra"))))
  (is-false (nth-value 3 (app::parse-install-argv '("--bogus" "u" "s")))))

(test install-cli-bad-args-exit-usage
  "A missing, extra, or unknown-flag invocation exits 2 without booting, and writes
the usage line to stderr."
  (flet ((code (args)
           (let ((*error-output* (make-string-output-stream))
                 (*standard-output* (make-string-output-stream)))
             (app::run-install args))))
    (is (eql 2 (code '())))
    (is (eql 2 (code '("only-url"))))
    (is (eql 2 (code '("u" "s" "extra"))))
    (is (eql 2 (code '("--bogus" "u" "s")))))
  (let ((err (make-string-output-stream)))
    (let ((*error-output* err) (*standard-output* (make-string-output-stream)))
      (app::run-install '()))
    (is (search "Usage:" (get-output-stream-string err))
        "the usage line goes to stderr")))

(test install-cli-places-durably-under-declared-id
  "kli install <loopback> <sha> --yes verifies over a real HTTP fetch and places the
extension durably under its manifest-declared id: it exits 0, prints the declared id
(NOT the url leaf) to stdout and the outcome line to stderr, trial-loads the code,
and writes <declared-id>.lisp -- the file a later process rediscovers."
  (let* ((*web-install-load-sentinel* nil)
         (bytes (web-install-artifact-bytes))
         (good (app::git-blob-sha1 bytes))
         (staging (install-cli-staging-root))
         (app::*registry-trust-roots* '())
         (app::*remote-install-staging-root* staging))
    ;; The url leaf differs from the declared id, so stdout naming the declared id
    ;; proves the CLI surfaces the code's own identity, not the url it came from.
    (multiple-value-bind (url shutdown) (web-install-serve-once bytes "provisional.lisp")
      (unwind-protect
           (let* ((out (make-string-output-stream))
                  (err (make-string-output-stream))
                  (code (let ((*standard-output* out) (*error-output* err))
                          (app::run-install (list url good "--yes")))))
             (is (eql 0 code) "a granted install over loopback exits 0")
             (is (equal "web-probe"
                        (string-trim '(#\Newline #\Space)
                                     (get-output-stream-string out)))
                 "stdout carries the manifest-declared id, not the url leaf")
             (is (search "Installed web-probe." (get-output-stream-string err))
                 "stderr carries the outcome line")
             (is-true *web-install-load-sentinel* "the artifact trial-loaded")
             (is (not (null (probe-file (merge-pathnames "web-probe.lisp" staging))))
                 "placed durably under <declared-id>.lisp")
             (is (null (probe-file (merge-pathnames "provisional.lisp" staging)))
                 "and never under the url-derived leaf"))
        (funcall shutdown)))))

(test install-cli-wrong-sha-installs-nothing
  "An artifact whose fetched bytes do not match the declared git-tree-sha1 exits 3,
trial-loads nothing, and places no file -- the integrity floor holds on the CLI
path."
  (let* ((*web-install-load-sentinel* nil)
         (bytes (web-install-artifact-bytes))
         (wrong (app::git-blob-sha1
                 (sb-ext:string-to-octets "not the served bytes"
                                          :external-format :utf-8)))
         (staging (install-cli-staging-root))
         (app::*registry-trust-roots* '())
         (app::*remote-install-staging-root* staging))
    (multiple-value-bind (url shutdown) (web-install-serve-once bytes "provisional.lisp")
      (unwind-protect
           (let* ((err (make-string-output-stream))
                  (code (let ((*standard-output* (make-string-output-stream))
                              (*error-output* err))
                          (app::run-install (list url wrong "--yes")))))
             (is (eql 3 code) "a git-tree-sha1 mismatch exits 3")
             (is (null *web-install-load-sentinel*) "and nothing trial-loads")
             (is (search "rejected" (get-output-stream-string err))
                 "the outcome names the refusal")
             (is (null (probe-file (merge-pathnames "web-probe.lisp" staging)))
                 "and no file is placed"))
        (funcall shutdown)))))

(test install-cli-non-tty-without-yes-refuses
  "On a non-terminal without --yes the install refuses non-zero WITHOUT hanging and
before any fetch: consent is denied at the first card, so the fetcher is never
called and there is no prompt to block on."
  (let* ((fetched nil)
         (app::*registry-trust-roots* '())
         (app::*remote-install-fetcher*
           (lambda (url) (declare (ignore url)) (setf fetched t) nil))
         (app::*remote-install-staging-root* (install-cli-staging-root))
         ;; An empty stdin, so even if the harness runs under a tty the per-stage
         ;; prompt reads end-of-input and defaults to no rather than blocking.
         (*standard-input* (make-string-input-stream ""))
         (err (make-string-output-stream))
         (code (let ((*standard-output* (make-string-output-stream))
                     (*error-output* err))
                 (app::run-install
                  (list "http://127.0.0.1:1/provisional.lisp"
                        (app::git-blob-sha1 (web-install-artifact-bytes)))))))
    (is (eql 3 code) "a non-tty install without --yes refuses non-zero")
    (is-false fetched "refusing at consent before any fetch, so it never blocks")))
