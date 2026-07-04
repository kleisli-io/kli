(in-package #:kli/app)

(defvar *self-update-repo* "kleisli-io/kli"
  "GitHub owner/name the self-updater pulls releases from.")

(defvar *self-update-http* nil
  "Test seam: a function of (url) returning (values body-string status) that
replaces the live GitHub API GET. Nil performs the real request.")

(defvar *self-update-fetcher* nil
  "Test seam: a function of (url) returning octets or nil that replaces the live
tarball and checksums GET. Nil performs the real request.")

(defvar *self-update-binary-path* nil
  "Override for the running image path used to locate the install dir. Nil reads
the live runtime pathname.")

(define-condition nix-managed-binary (error) ()
  (:report "kli is managed by Nix; update through your Nix configuration instead."))

(defun running-binary-path ()
  "Absolute path of the running image, or nil when it cannot be resolved."
  (or *self-update-binary-path*
      (ignore-errors (namestring (truename sb-ext:*runtime-pathname*)))))

(defun nix-store-binary-p (path)
  "True when PATH names a binary under the read-only Nix store, where self-update
must refuse because Nix owns the binary."
  (let ((prefix "/nix/store/"))
    (and path
         (>= (length path) (length prefix))
         (string= prefix path :end2 (length prefix)))))

(defun detect-install-dir (path)
  "Install root for a tarball image at <root>/lib/kli/bin/kli, found by dropping
bin/kli then the lib/kli directories. Signals nix-managed-binary for a store
image."
  (when (nix-store-binary-p path)
    (error 'nix-managed-binary))
  (let ((dir (uiop:pathname-directory-pathname path)))
    (dotimes (i 3 dir)
      (setf dir (uiop:pathname-parent-directory-pathname dir)))))

(defun release-artifact-name ()
  "Release artifact base name for this host, e.g. kli-linux-x86_64."
  (let ((os (host-os))
        (arch (host-arch)))
    (when (or (string= os "unknown") (string= arch "unknown"))
      (error "Unsupported platform: ~A-~A" os arch))
    (format nil "kli-~A-~A" os arch)))

(defparameter *ca-bundle-file-candidates*
  '("/etc/ssl/certs/ca-certificates.crt"   ; debian, ubuntu, alpine, arch, nixos
    "/etc/pki/tls/certs/ca-bundle.crt"     ; rhel, fedora, centos
    "/etc/ssl/cert.pem")                   ; alpine, macos, libressl, *bsd
  "Host CA bundle files probed, in order, when no CA environment override is set.")

(defparameter *ca-bundle-dir-candidates*
  '("/etc/ssl/certs")
  "Host CA directories probed after the bundle files.")

(defvar *ca-getenv* #'uiop:getenv
  "Test seam: the environment reader for CA-store discovery. Rebindable to control
the probed environment; nil-safe.")

(defun ca-probe-file (path)
  "PATH when it names an existing regular file, else nil."
  (and (probe-file path) (not (uiop:directory-exists-p path)) path))

(defun ca-probe-dir (path)
  "PATH when it names an existing directory, else nil."
  (and (uiop:directory-exists-p path) path))

(defun discover-ca-bundle (&key (getenv *ca-getenv*)
                                (probe-file #'ca-probe-file)
                                (probe-dir #'ca-probe-dir))
  "Locate a host CA trust store for TLS verification, as (values path kind) with
KIND :file or :directory, or nil when none is found. Honors the OpenSSL and curl
overrides first (SSL_CERT_FILE, SSL_CERT_DIR, CURL_CA_BUNDLE), then probes the
well-known distro locations. The relocatable image bundles its own OpenSSL whose
build-time OPENSSLDIR is absent on a foreign host, so :verify :required must be
handed an explicit anchor rather than OpenSSL's default search path. An override is
taken only when its path exists, so a stale variable falls through to the candidates."
  (flet ((as-file (path)
           (let ((r (and path (funcall probe-file path)))) (and r (list r :file))))
         (as-dir (path)
           (let ((r (and path (funcall probe-dir path)))) (and r (list r :directory)))))
    (let ((hit (or (as-file (funcall getenv "SSL_CERT_FILE"))
                   (as-dir  (funcall getenv "SSL_CERT_DIR"))
                   (as-file (funcall getenv "CURL_CA_BUNDLE"))
                   (some #'as-file *ca-bundle-file-candidates*)
                   (some #'as-dir  *ca-bundle-dir-candidates*))))
      (when hit
        (values (first hit) (second hit))))))

(defun ca-verify-plist (path kind)
  "drakma keyword args forcing TLS verification against the anchor at PATH,
dispatching on KIND to :ca-file or :ca-directory."
  (list* :verify :required
         (ecase kind
           (:file (list :ca-file path))
           (:directory (list :ca-directory path)))))

(defun update-verify-args ()
  "drakma verification args for a self-update request, computed from the discovered
host CA store. Signals when no store is found: the updater fails closed rather than
fetch tarball and checksums over an unverified channel."
  (multiple-value-bind (path kind) (discover-ca-bundle)
    (unless path
      (error "No system CA trust store found to verify the kli update download; ~
set SSL_CERT_FILE to your CA bundle."))
    (ca-verify-plist path kind)))

(defvar *self-update-transport* #'drakma:http-request
  "Test seam: the HTTP transport both verified self-update requests go through.
Nil is not valid; defaults to drakma and is rebindable to capture request args.")

(defun verified-http-request (url &rest args)
  "drakma:http-request for URL with TLS verification forced against the discovered
host CA store, threading ARGS through. Both self-update fetchers route here, so the
verification is applied in exactly one place."
  (apply *self-update-transport* url
         :user-agent "kli-updater"
         (append args (update-verify-args))))

(defun update-http-get (url)
  "GitHub API GET returning (values body-string status)."
  (if *self-update-http*
      (funcall *self-update-http* url)
      (multiple-value-bind (body status)
          (verified-http-request
           url :additional-headers '(("Accept" . "application/vnd.github+json")))
        (values (if (stringp body)
                    body
                    (flexi-streams:octets-to-string body :external-format :utf-8))
                status))))

(defun update-fetch-octets (url)
  "Raw octets at URL, or nil on a non-success status or transport failure."
  (if *self-update-fetcher*
      (funcall *self-update-fetcher* url)
      (handler-case
          (multiple-value-bind (body status)
              (verified-http-request url :force-binary t)
            (and (eql status 200) body))
        (error () nil))))

(defun github-release-tag (subpath)
  "The tag_name from the releases endpoint at SUBPATH, e.g. \"/latest\" or
\"/tags/v0.1.0\"."
  (let ((url (format nil "https://api.github.com/repos/~A/releases~A"
                     *self-update-repo* subpath)))
    (multiple-value-bind (body status) (update-http-get url)
      (unless (eql status 200)
        (error "GitHub API returned HTTP ~A for ~A" status url))
      (let ((tag (gethash "tag_name" (com.inuoe.jzon:parse body))))
        (unless tag
          (error "No release tag in the GitHub response for ~A" url))
        tag))))

(defun resolve-target-version (target)
  "The release tag to install: TARGET when given (validated to exist), else the
latest release."
  (if target
      (github-release-tag (format nil "/tags/~A" target))
      (github-release-tag "/latest")))

(defun parse-checksums (text artifact-name)
  "Expected sha-256 hex for ARTIFACT-NAME's tarball from a checksums.txt body
(\"<hash>  <file>\"), or nil when the artifact is absent."
  (let ((tarball (concatenate 'string artifact-name ".tar.gz")))
    (loop for line in (uiop:split-string text :separator '(#\Newline))
          when (search tarball line)
            return (first (uiop:split-string (string-trim '(#\Space #\Tab) line)
                                             :separator '(#\Space))))))

(defun download-verified-tarball (version artifact-name dest)
  "Download <artifact-name>.tar.gz for VERSION to DEST and check it against the
release checksums.txt over the same bytes written. Returns DEST; signals on a
download failure or checksum mismatch."
  (let* ((base (format nil "https://github.com/~A/releases/download/~A"
                       *self-update-repo* version))
         (tarball (update-fetch-octets
                   (format nil "~A/~A.tar.gz" base artifact-name))))
    (unless tarball
      (error "Could not download ~A.tar.gz for ~A" artifact-name version))
    (let ((sums (update-fetch-octets (format nil "~A/checksums.txt" base))))
      (when sums
        (let ((expected (parse-checksums
                         (flexi-streams:octets-to-string sums :external-format :utf-8)
                         artifact-name))
              (actual (sha256-hex tarball)))
          (when (and expected (not (string-equal expected actual)))
            (error "Checksum mismatch for ~A.tar.gz~%  expected ~A~%  actual   ~A"
                   artifact-name expected actual)))))
    (with-open-file (out dest :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (write-sequence tarball out))
    dest))

(defun extract-tarball (tarball dest)
  "Extract TARBALL into DEST with tar. Signals on a non-zero exit."
  (ensure-directories-exist (uiop:ensure-directory-pathname dest))
  (multiple-value-bind (out err code)
      (uiop:run-program (list "tar" "-xzf" (namestring tarball)
                              "-C" (namestring dest))
                        :output :string :error-output :string
                        :ignore-error-status t)
    (declare (ignore out))
    (unless (zerop code)
      (error "Failed to extract ~A: ~A" tarball err))))

(defun extracted-payload-dir (staging artifact-name)
  "The payload directory inside an extracted release tarball, whose top level is
the artifact directory."
  (uiop:ensure-directory-pathname
   (merge-pathnames (concatenate 'string artifact-name "/")
                    (uiop:ensure-directory-pathname staging))))

(defun swap-install-payload (lib-dir payload)
  "Move PAYLOAD into LIB-DIR, keeping a sibling .bak of the prior install for
rollback. Returns the backup dir, or nil when there was nothing to back up."
  (let ((backup (uiop:ensure-directory-pathname
                 (format nil "~A.bak"
                         (string-right-trim "/" (namestring lib-dir))))))
    (when (uiop:directory-exists-p backup)
      (uiop:delete-directory-tree backup :validate t))
    (let ((had (uiop:directory-exists-p lib-dir)))
      (when had (rename-file lib-dir backup))
      (rename-file payload lib-dir)
      (and had backup))))

(defun verify-installed-binary (install-dir)
  "Run the installed wrapper's `version` and return its trimmed output on
success, nil otherwise."
  (let ((wrapper (namestring
                  (merge-pathnames "bin/kli"
                                   (uiop:ensure-directory-pathname install-dir)))))
    (multiple-value-bind (out err code)
        (uiop:run-program (list wrapper "version")
                          :output :string :error-output :string
                          :ignore-error-status t)
      (declare (ignore err))
      (and (zerop code) (string-trim '(#\Newline #\Space) out)))))

(defun perform-update (install-dir version)
  "Download, verify, and atomically install VERSION's payload under INSTALL-DIR
at lib/kli, rolling back to the prior install if the new binary fails to run.
The payload location and the install wrapper are the install layout the
relocation tooling owns, so this mirrors it rather than rewriting the wrapper."
  (let* ((artifact (release-artifact-name))
         (lib-dir (uiop:ensure-directory-pathname
                   (merge-pathnames "lib/kli/"
                                    (uiop:ensure-directory-pathname install-dir))))
         (tmp (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "kli-update-~A/" (get-universal-time))
                                (uiop:temporary-directory)))))
    (ensure-directories-exist tmp)
    (unwind-protect
         (let* ((tarball (download-verified-tarball
                          version artifact
                          (merge-pathnames (format nil "~A.tar.gz" artifact) tmp)))
                (staging (merge-pathnames "staging/" tmp)))
           (extract-tarball tarball staging)
           (let ((backup (swap-install-payload
                          lib-dir (extracted-payload-dir staging artifact))))
             (if (verify-installed-binary install-dir)
                 (when backup
                   (uiop:delete-directory-tree backup :validate t))
                 (progn
                   (when (uiop:directory-exists-p lib-dir)
                     (uiop:delete-directory-tree lib-dir :validate t))
                   (when backup (rename-file backup lib-dir))
                   (error "the updated binary failed to run; rolled back")))))
      (when (uiop:directory-exists-p tmp)
        (ignore-errors (uiop:delete-directory-tree tmp :validate t))))
    (format t "~&kli updated to ~A.~%" version)))

(defun parse-update-args (args)
  "Parse the `update` options. Returns (values auto-confirm target-version);
signals on a missing --version value or an unknown option."
  (let ((auto nil) (target nil) (skip nil))
    (loop for (arg . rest) on args
          do (cond
               (skip (setf skip nil))
               ((member arg '("--yes" "-y") :test #'string=) (setf auto t))
               ((string= arg "--version")
                (unless rest (error "update: --version needs a value"))
                (setf target (first rest) skip t))
               (t (error "update: unknown option ~A" arg))))
    (values auto target)))

(defun confirm-update (current remote auto)
  "Prompt for confirmation of an update from CURRENT to REMOTE, defaulting to
yes on an empty line. AUTO skips the prompt."
  (or auto
      (progn
        (format t "~&  current: ~A~%  new:     ~A~2%Proceed? [Y/n] " current remote)
        (force-output)
        (let ((line (read-line *standard-input* nil "")))
          (or (zerop (length line)) (char-equal (char line 0) #\Y))))))

(defun run-self-update (args)
  "Self-update kli from a GitHub release. Returns a process exit code: 0 for a
successful update, an up-to-date image, or a declined prompt; non-zero for a
refusal or failure. Refuses outright on a Nix-store image."
  (handler-case
      (let ((install-dir (detect-install-dir (running-binary-path))))
        (multiple-value-bind (auto target) (parse-update-args args)
          (let ((remote (resolve-target-version target))
                (current +kli-version+))
            (cond
              ((string= (string-left-trim "v" remote)
                        (string-left-trim "v" current))
               (format t "~&Already up to date (~A).~%" current)
               0)
              ((not (confirm-update current remote auto))
               (format t "~&Update cancelled.~%")
               0)
              (t (perform-update install-dir remote)
                 0)))))
    (nix-managed-binary (condition)
      (format *error-output* "~&~A~%" condition)
      1)
    (error (condition)
      (format *error-output* "~&kli update failed: ~A~%" condition)
      1)))
