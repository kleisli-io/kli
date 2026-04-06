;;; kli update — self-update command
;;;
;;; Downloads the latest (or pinned) kli release from GitHub,
;;; verifies the checksum, and atomically replaces the current
;;; installation with rollback on failure.

(in-package #:kli)

(defvar *github-repo* "kleisli-io/kli")
(defvar *github-api-url*
  (format nil "https://api.github.com/repos/~A/releases" *github-repo*))
(defvar *github-dl-url*
  (format nil "https://github.com/~A/releases/download" *github-repo*))

;;; --- Version comparison ---

(defun parse-version (str)
  "Parse version string like 'v0.1.0' or '0.1.0' into list of integers."
  (let ((s (string-left-trim "v" str)))
    (mapcar #'parse-integer (uiop:split-string s :separator "."))))

(defun version> (a b)
  "Return T if version string A is newer than B."
  (let ((va (parse-version a))
        (vb (parse-version b)))
    (loop for x in va for y in vb
          when (> x y) return t
          when (< x y) return nil
          finally (return nil))))

;;; --- Platform detection ---

(defun detect-platform ()
  "Detect OS and architecture from compile-time features.
   Returns (VALUES os arch artifact-name)."
  (let ((os #+linux "linux"
            #+darwin "darwin"
            #-(or linux darwin) nil)
        (arch #+x86-64 "x86_64"
              #+arm64 "aarch64"
              #-(or x86-64 arm64) nil))
    (unless (and os arch)
      (error "Unsupported platform: ~A ~A" (software-type) (machine-type)))
    (values os arch (format nil "kli-~A-~A" os arch))))

;;; --- Install location detection ---

(defun detect-install-dir ()
  "Detect install directory from the running binary's location.
   Returns the install dir (e.g., /home/user/.local).
   Signals error if running from Nix store (use Nix to update instead)."
  (let* ((exe (namestring (truename sb-ext:*runtime-pathname*)))
         (nix-prefix "/nix/store/"))
    (when (and (>= (length exe) (length nix-prefix))
               (string= nix-prefix exe :end2 (length nix-prefix)))
      (error "kli is managed by Nix. Update via your Nix configuration instead."))
    ;; Binary lives at <install-dir>/lib/kli/bin/kli
    ;; Walk up: kli -> bin/ -> kli/ -> lib/ -> <install-dir>/
    (let ((path (pathname exe)))
      (loop repeat 4
            do (setf path (uiop:pathname-parent-directory-pathname path)))
      (namestring path))))

;;; --- Argument parsing ---

(defun parse-update-args (args)
  "Parse update command arguments.
   Returns (VALUES auto-confirm target-version).
   Accepts: --yes/-y, --version VERSION"
  (let ((auto-confirm nil)
        (target-version nil)
        (skip-next nil))
    (loop for (arg . rest) on args
          do (cond
               (skip-next
                (setf skip-next nil))
               ((or (string= arg "--yes") (string= arg "-y"))
                (setf auto-confirm t))
               ((string= arg "--version")
                (unless rest
                  (format *error-output* "Error: --version requires a value (e.g., --version v0.3.0)~%")
                  (uiop:quit 1))
                (setf target-version (first rest)
                      skip-next t))
               (t
                (format *error-output* "Error: unknown option '~A'~%" arg)
                (format *error-output* "Usage: kli update [--yes] [--version VERSION]~%")
                (uiop:quit 1))))
    (values auto-confirm target-version)))

;;; --- GitHub API ---

(defun fetch-latest-version ()
  "Fetch the latest release tag from GitHub API.
   Returns tag string like 'v0.2.0'."
  (multiple-value-bind (body status)
      (dexador:get (format nil "~A/latest" *github-api-url*)
                   :headers '(("Accept" . "application/vnd.github.v3+json")
                              ("User-Agent" . "kli-updater"))
                   :force-string t)
    (unless (= status 200)
      (error "Failed to check for updates: GitHub API returned HTTP ~A" status))
    (let ((tag (gethash "tag_name" (yason:parse body))))
      (unless tag
        (error "Could not parse release info from GitHub API response"))
      tag)))

(defun fetch-version-info (version)
  "Fetch release info for a specific version tag. Validates it exists."
  (multiple-value-bind (body status)
      (dexador:get (format nil "~A/tags/~A" *github-api-url* version)
                   :headers '(("Accept" . "application/vnd.github.v3+json")
                              ("User-Agent" . "kli-updater"))
                   :force-string t
                   :max-redirects 10)
    (unless (= status 200)
      (error "Version ~A not found on GitHub (HTTP ~A).~%Check available versions at https://github.com/~A/releases"
             version status *github-repo*))
    (gethash "tag_name" (yason:parse body))))

;;; --- User confirmation ---

(defun confirm-update (current-version new-version &key auto-confirm)
  "Prompt user to confirm the update. Returns T if confirmed."
  (when auto-confirm (return-from confirm-update t))
  (format t "~%  Current: v~A~%" current-version)
  (format t "  New:     ~A~%~%" new-version)
  (format t "Proceed? [Y/n] ")
  (force-output)
  (let ((response (read-line *standard-input* nil "")))
    (or (zerop (length response))
        (char-equal (char response 0) #\Y))))

;;; --- Download and verification ---

(defun download-to-file (url path)
  "Download URL content to a file. Returns path on success."
  (multiple-value-bind (body status)
      (dexador:get url
                   :headers '(("User-Agent" . "kli-updater"))
                   :force-binary t
                   :max-redirects 10)
    (unless (= status 200)
      (error "Download failed: HTTP ~A from ~A" status url))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence body out))
    path))

(defun sha256-file (path)
  "Compute SHA-256 hex digest of a file."
  (let ((digest (ironclad:make-digest :sha256))
        (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (loop for bytes-read = (read-sequence buffer in)
            while (plusp bytes-read)
            do (ironclad:update-digest digest buffer :end bytes-read)))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defun parse-checksums (text artifact-name)
  "Extract expected SHA-256 for artifact-name from checksums.txt content.
   Format: '<sha256>  <filename>' (two spaces between hash and name)."
  (let ((tarball (format nil "~A.tar.gz" artifact-name)))
    (loop for line in (uiop:split-string text :separator '(#\Newline))
          when (search tarball line)
            return (first (uiop:split-string (string-trim '(#\Space #\Tab) line)
                                             :separator '(#\Space))))))

(defun download-and-verify (version artifact-name tmp-dir)
  "Download release tarball and verify its SHA-256 checksum.
   Returns path to verified tarball."
  (let* ((tarball-url (format nil "~A/~A/~A.tar.gz" *github-dl-url* version artifact-name))
         (checksums-url (format nil "~A/~A/checksums.txt" *github-dl-url* version))
         (tarball-path (merge-pathnames
                        (format nil "~A.tar.gz" artifact-name)
                        (uiop:ensure-directory-pathname tmp-dir))))
    ;; Download tarball
    (format t "  Downloading ~A.tar.gz... " artifact-name)
    (force-output)
    (download-to-file tarball-url tarball-path)
    (format t "ok~%")

    ;; Verify checksum
    (format t "  Verifying checksum... ")
    (force-output)
    (handler-case
        (let ((checksums-text
                (multiple-value-bind (body status)
                    (dexador:get checksums-url
                                :headers '(("User-Agent" . "kli-updater"))
                                :force-string t
                                :max-redirects 10)
                  (if (= status 200) body nil))))
          (if checksums-text
              (let ((expected (parse-checksums checksums-text artifact-name))
                    (actual (sha256-file tarball-path)))
                (cond
                  ((null expected)
                   (format t "skipped (not in checksums.txt)~%"))
                  ((string= expected actual)
                   (format t "ok~%"))
                  (t
                   (error "Checksum mismatch!~%  Expected: ~A~%  Actual:   ~A" expected actual))))
              (format t "skipped (checksums.txt not available)~%")))
      (dexador:http-request-failed ()
        (format t "skipped (could not fetch checksums.txt)~%")))

    tarball-path))

;;; --- Atomic replacement ---

(defun extract-tarball (tarball-path dest-dir)
  "Extract tarball to destination directory."
  (ensure-directories-exist (uiop:ensure-directory-pathname dest-dir))
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program
       (list "tar" "-xzf" (namestring tarball-path) "-C" (namestring dest-dir))
       :output :string
       :error-output :string
       :ignore-error-status t)
    (declare (ignore output))
    (unless (zerop exit-code)
      (error "Failed to extract tarball: ~A" error-output))))

(defun write-wrapper-script (wrapper-path lib-dir)
  "Write the kli wrapper script that sets KLI_DATA_DIR and execs the binary."
  (with-open-file (out wrapper-path :direction :output :if-exists :supersede)
    (format out "#!/bin/sh~%")
    (format out "export KLI_DATA_DIR=\"~Ashare/kli\"~%" (namestring lib-dir))
    (format out "exec \"~Abin/kli\" \"$@\"~%" (namestring lib-dir)))
  (sb-posix:chmod (namestring wrapper-path) #o755))

(defun verify-new-binary (lib-dir)
  "Run the new kli binary to verify it works. Returns T on success."
  (let ((binary (namestring (merge-pathnames "bin/kli"
                                             (uiop:ensure-directory-pathname lib-dir)))))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list binary "version")
                          :output :string
                          :error-output :string
                          :ignore-error-status t
                          :environment
                          (list (format nil "KLI_DATA_DIR=~Ashare/kli"
                                        (namestring (uiop:ensure-directory-pathname lib-dir)))))
      (declare (ignore error-output))
      (when (zerop exit-code)
        (format t "  Verified: ~A" (string-trim '(#\Newline #\Space) output))
        t))))

;;; --- Main update command ---

(defun update (&optional args)
  "Self-update kli to the latest or a specific version.
   Usage: kli update [--yes] [--version VERSION]"
  (multiple-value-bind (auto-confirm target-version)
      (parse-update-args (or args nil))
    (format t "kli update~%~%")

    ;; Detect where we're installed
    (let* ((install-dir (detect-install-dir))
           (lib-dir (format nil "~Alib/kli/" install-dir))
           (bin-dir (format nil "~Abin/" install-dir))
           (wrapper-path (format nil "~Akli" bin-dir)))

      ;; Resolve target version
      (format t "  Checking for updates... ")
      (force-output)
      (let* ((remote-version
               (if target-version
                   (progn
                     (fetch-version-info target-version) ; validate it exists
                     target-version)
                   (fetch-latest-version)))
             (current-version *version*))
        (format t "~A~%" remote-version)

        ;; Compare versions
        (let ((remote-clean (string-left-trim "v" remote-version))
              (current-clean (string-left-trim "v" current-version)))
          (when (string= remote-clean current-clean)
            (format t "~%Already up to date (v~A).~%" current-version)
            (return-from update))

          ;; Confirm with user
          (unless (confirm-update current-version remote-version
                                  :auto-confirm auto-confirm)
            (format t "Update cancelled.~%")
            (return-from update))

          ;; Perform update
          (format t "~%Updating to ~A...~%~%" remote-version)
          (multiple-value-bind (os arch artifact-name)
              (detect-platform)
            (declare (ignore os arch))
            (let ((tmp-dir (namestring
                            (uiop:ensure-directory-pathname
                             (format nil "/tmp/kli-update-~A" (get-universal-time))))))
              (ensure-directories-exist (pathname tmp-dir))
              (unwind-protect
                   (progn
                     ;; Download and verify
                     (let ((tarball-path (download-and-verify remote-version artifact-name tmp-dir)))
                       ;; Extract to temp staging area
                       (let ((staging-dir (format nil "~Astaging/" tmp-dir)))
                         (format t "  Extracting... ")
                         (force-output)
                         (extract-tarball tarball-path staging-dir)
                         (format t "ok~%")

                         ;; Backup current installation
                         (let ((backup-dir (format nil "~A.bak" (string-right-trim "/" lib-dir))))
                           (format t "  Replacing installation... ")
                           (force-output)
                           ;; Remove stale backup if present
                           (when (probe-file backup-dir)
                             (uiop:delete-directory-tree (pathname (uiop:ensure-directory-pathname backup-dir))
                                                         :validate t))
                           ;; Backup → Replace
                           (when (probe-file lib-dir)
                             (rename-file lib-dir backup-dir))
                           (rename-file staging-dir lib-dir)
                           (format t "ok~%")

                           ;; Regenerate wrapper script
                           (format t "  Updating wrapper... ")
                           (force-output)
                           (write-wrapper-script wrapper-path lib-dir)
                           (format t "ok~%")

                           ;; Verify new binary
                           (format t "  Verifying... ")
                           (force-output)
                           (if (verify-new-binary lib-dir)
                               (progn
                                 (format t "~%")
                                 ;; Success — remove backup
                                 (when (probe-file backup-dir)
                                   (uiop:delete-directory-tree
                                    (pathname (uiop:ensure-directory-pathname backup-dir))
                                    :validate t)))
                               ;; Failure — rollback
                               (progn
                                 (format *error-output* "FAILED~%")
                                 (format *error-output* "~%Rolling back to previous version...~%")
                                 (when (probe-file lib-dir)
                                   (uiop:delete-directory-tree
                                    (pathname (uiop:ensure-directory-pathname lib-dir))
                                    :validate t))
                                 (rename-file backup-dir lib-dir)
                                 (write-wrapper-script wrapper-path lib-dir)
                                 (error "Update verification failed. Rolled back to v~A." current-version)))))))

                ;; Cleanup temp dir
                (when (probe-file tmp-dir)
                  (ignore-errors
                    (uiop:delete-directory-tree (pathname (uiop:ensure-directory-pathname tmp-dir))
                                                :validate t))))))

          ;; Success message
          (format t "~%kli updated to ~A.~%" remote-version)
          (format t "~%Run 'kli init' in your projects to update project configuration.~%"))))))
