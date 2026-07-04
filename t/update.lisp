(in-package #:kli/tests)
(in-suite all)

;;;; Atomic swap + rollback branch matrix for kli self-update, driven through
;;;; the *self-update-fetcher*/*self-update-http*/*self-update-binary-path* seams
;;;; over a real on-disk install fixture (no network).

(defun %write-exec (path content)
  "Write CONTENT to PATH and mark it executable."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
    (write-string content out))
  (sb-posix:chmod (namestring path) #o755)
  path)

(defun %slurp-octets (path)
  "PATH's bytes as an (unsigned-byte 8) vector."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun %fresh-dir (tag)
  "A freshly created temp directory tagged with TAG."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "~A/" (symbol-name (gensym tag)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun %install-root (&optional (marker "0.1.0"))
  "A temp install root laid out as the relocatable tree: <root>/bin/kli delegates
to <root>/lib/kli/bin/kli, whose `version` echoes MARKER. Returns the root."
  (let ((root (%fresh-dir "KLI-U1-ROOT")))
    (%write-exec (merge-pathnames "bin/kli" root)
                 (format nil "#!/bin/sh~%exec \"$(dirname \"$0\")/../lib/kli/bin/kli\" \"$@\"~%"))
    (%write-exec (merge-pathnames "lib/kli/bin/kli" root)
                 (format nil "#!/bin/sh~%echo ~A~%" marker))
    root))

(defun %payload (artifact inner-body)
  "Build a release tarball topped by ARTIFACT/ whose bin/kli runs INNER-BODY.
Returns (values tarball-octets checksums-text) with a matching checksums line."
  (let ((build (%fresh-dir "KLI-U1-PL"))
        (tgz (merge-pathnames "p.tar.gz" (%fresh-dir "KLI-U1-TGZ"))))
    (%write-exec (merge-pathnames (format nil "~A/bin/kli" artifact) build) inner-body)
    (uiop:run-program (list "tar" "-czf" (namestring tgz)
                            "-C" (namestring build) artifact)
                      :output :string :error-output :string)
    (let ((octets (%slurp-octets tgz)))
      (values octets
              (format nil "~A  ~A.tar.gz~%" (app::sha256-hex octets) artifact)))))

(defun %fetcher (octets checksums-text)
  "A *self-update-fetcher*: OCTETS for the tarball URL, CHECKSUMS-TEXT for the
checksums URL, nil otherwise."
  (lambda (url)
    (cond ((search "checksums.txt" url)
           (flexi-streams:string-to-octets checksums-text :external-format :utf-8))
          ((search ".tar.gz" url) octets)
          (t nil))))

(defmacro %with-fixture ((root marker) &body body)
  "Bind ROOT to a fresh install fixture (old version MARKER) with the binary-path
seam pointed at its inner image, cleaning the tree afterwards."
  `(let ((,root (%install-root ,marker)))
     (unwind-protect
          (let ((app::*self-update-binary-path*
                  (namestring (merge-pathnames "lib/kli/bin/kli" ,root))))
            ,@body)
       (uiop:delete-directory-tree ,root :validate t))))

(test update-happy-swap-installs-payload-and-drops-backup
  "A good payload whose binary runs is installed and the rollback backup deleted."
  (%with-fixture (root "0.1.0")
    (multiple-value-bind (octets sums)
        (%payload (app::release-artifact-name) (format nil "#!/bin/sh~%echo 0.1.1~%"))
      (let ((app::*self-update-fetcher* (%fetcher octets sums)))
        (app::perform-update (app::detect-install-dir (app::running-binary-path)) "v0.1.1"))
      (is (uiop:directory-exists-p (merge-pathnames "lib/kli/" root)))
      (is (not (uiop:directory-exists-p (merge-pathnames "lib/kli.bak/" root))))
      (is (equal "0.1.1" (app::verify-installed-binary root))))))

(test update-rollback-restores-backup-and-signals
  "A payload whose binary fails to run is rolled back to the prior install and
the failure signalled."
  (%with-fixture (root "0.1.0")
    (multiple-value-bind (octets sums)
        (%payload (app::release-artifact-name) (format nil "#!/bin/sh~%exit 1~%"))
      (let ((app::*self-update-fetcher* (%fetcher octets sums)))
        (signals error
          (app::perform-update (app::detect-install-dir (app::running-binary-path)) "v0.1.1")))
      (is (uiop:directory-exists-p (merge-pathnames "lib/kli/" root)))
      (is (not (uiop:directory-exists-p (merge-pathnames "lib/kli.bak/" root))))
      (is (equal "0.1.0" (app::verify-installed-binary root))))))

(test update-checksum-mismatch-signals-before-swap
  "A checksums line that does not match the tarball bytes aborts before any swap."
  (%with-fixture (root "0.1.0")
    (multiple-value-bind (octets sums)
        (%payload (app::release-artifact-name) (format nil "#!/bin/sh~%echo 0.1.1~%"))
      (declare (ignore sums))
      (let ((app::*self-update-fetcher*
              (%fetcher octets
                        (format nil "~A  ~A.tar.gz~%"
                                (make-string 64 :initial-element #\0)
                                (app::release-artifact-name)))))
        (signals error
          (app::perform-update (app::detect-install-dir (app::running-binary-path)) "v0.1.1")))
      (is (equal "0.1.0" (app::verify-installed-binary root)))
      (is (not (uiop:directory-exists-p (merge-pathnames "lib/kli.bak/" root)))))))

(test update-refuses-nix-store-binary
  "A store-path image refuses self-update: detect-install-dir signals and the
command exits non-zero without touching the network."
  (signals app::nix-managed-binary
    (app::detect-install-dir
     "/nix/store/0000000000000000000000000000000000000000-kli/bin/kli"))
  (let ((app::*self-update-binary-path*
          "/nix/store/0000000000000000000000000000000000000000-kli/bin/kli"))
    (is (= 1 (app::run-self-update '())))))

(test update-resolves-latest-and-explicit-target
  "resolve-target-version reads the tag from /latest or /tags/<v> and signals on
a non-success status."
  (let ((app::*self-update-http*
          (lambda (url)
            (cond ((search "/latest" url) (values "{\"tag_name\":\"v0.9.0\"}" 200))
                  ((search "/tags/v0.4.2" url) (values "{\"tag_name\":\"v0.4.2\"}" 200))
                  (t (values "{}" 404))))))
    (is (equal "v0.9.0" (app::resolve-target-version nil)))
    (is (equal "v0.4.2" (app::resolve-target-version "v0.4.2")))
    (signals error (app::resolve-target-version "v9.9.9"))))
