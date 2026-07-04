(in-package #:kli/tests)
(in-suite all)

(defvar *trust-load-sentinel* nil
  "Set t by a test artifact when its source is loaded, so a test can tell a real
load apart from pure verification.")

(defun trust-fetcher (&rest url-bytes-pairs)
  "A *remote-install-fetcher* serving fixed octets per url, nil for an unknown url."
  (lambda (url)
    (loop for (candidate bytes) on url-bytes-pairs by #'cddr
          when (string= candidate url) return bytes)))

(defun trust-staging-root ()
  (ensure-directories-exist
   (merge-pathnames "kli-remote-install-trust/" (uiop:temporary-directory))))

(defun trust-artifact-bytes (id)
  "A tiny extension source that marks the load sentinel and defines one extension,
loaded in kli/author like any single-file user extension."
  (sb-ext:string-to-octets
   (format nil "(setf kli/tests::*trust-load-sentinel* t)~%(defextension ~A (:provides))~%"
           id)
   :external-format :utf-8))

(defun trust-sign (bytes)
  "An ed25519 key pair over BYTES: returns (values TRUST-HEX SIGNATURE-OCTETS),
the public key as a hex trust-root and the detached signature over the raw bytes."
  (multiple-value-bind (private public) (ironclad:generate-key-pair :ed25519)
    (values (ironclad:byte-array-to-hex-string (ironclad:ed25519-key-y public))
            (ironclad:sign-message private bytes))))

(test direct-url-trust-floor-and-ceiling
  "The integrity floor verifies the git-tree-sha1 over the fetched bytes and
rejects a tampered or unpinned artifact; the opt-in signature ceiling, active only
when trust roots are configured, requires a valid detached signature from a trusted
key; and the two-phase install refuses under a subject lacking
:manifest/install-remote."
  (let* ((artifact (trust-artifact-bytes "flx"))
         (url "https://ext.example/flx.lisp")
         (good (app::git-blob-sha1 artifact)))
    (let ((app::*registry-trust-roots* '())
          (app::*remote-install-fetcher* (trust-fetcher url artifact)))
      (multiple-value-bind (bytes meta) (app::direct-url-resolve-and-verify url good)
        (is (equalp artifact bytes) "the floor returns the verified bytes")
        (is (eq :hash (getf meta :trust)) "and marks them integrity-only"))
      (is (null (app::direct-url-resolve-and-verify url "00"))
          "a git-tree-sha1 mismatch refuses")
      (is (eq :git-tree-sha1
              (nth-value 1 (app::direct-url-resolve-and-verify url "00")))
          "with the hash reason")
      (is (eq :git-tree-sha1
              (nth-value 1 (app::direct-url-resolve-and-verify url nil)))
          "an unpinned artifact (nil hash) is never trusted")
      (is (eq :fetch-failed
              (nth-value 1 (app::direct-url-resolve-and-verify "https://no/x.lisp" good)))
          "a fetch failure defers rather than crashes"))
    (multiple-value-bind (trust-hex signature) (trust-sign artifact)
      (let ((sig-url (concatenate 'string url app::*signature-url-suffix*)))
        (let ((app::*registry-trust-roots* (list trust-hex))
              (app::*remote-install-fetcher*
                (trust-fetcher url artifact sig-url signature)))
          (multiple-value-bind (bytes meta) (app::direct-url-resolve-and-verify url good)
            (is (equalp artifact bytes) "a trusted signature passes the ceiling")
            (is (eq :signed (getf meta :trust)) "and marks the install signed")
            (is (stringp (getf meta :signed-by))
                "recording the signing key fingerprint")))
        (let ((app::*registry-trust-roots* (list trust-hex))
              (app::*remote-install-fetcher* (trust-fetcher url artifact)))
          (is (eq :signature-missing
                  (nth-value 1 (app::direct-url-resolve-and-verify url good)))
              "a configured trust root with no signature refuses"))
        (let ((other-hex (trust-sign artifact)))
          (let ((app::*registry-trust-roots* (list other-hex))
                (app::*remote-install-fetcher*
                  (trust-fetcher url artifact sig-url signature)))
            (is (eq :signature-untrusted
                    (nth-value 1 (app::direct-url-resolve-and-verify url good)))
                "a signature from an untrusted key refuses"))))))
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (let ((ext:*call-subject* (ext:make-subject :capabilities '(:file/read))))
      (signals ext:capability-denied
        (app::install-remote-extension
         (list :url "https://ext.example/x.lisp" :git-tree-sha1 "00")
         protocol context)))))

(test (direct-url-install-and-restore :fixture extension-load-authority)
  "A granted two-phase install verifies, loads, and pins at the hash floor;
verification alone loads nothing; and the restore re-fetch refuses and defers
before any load on a hash mismatch or a signature downgrade."
  (let* ((artifact (trust-artifact-bytes "okx"))
         (url "https://ext.example/okx.lisp")
         (good (app::git-blob-sha1 artifact)))
    (let ((*trust-load-sentinel* nil)
          (app::*registry-trust-roots* '())
          (app::*remote-install-fetcher* (trust-fetcher url artifact))
          (app::*remote-install-staging-root* (trust-staging-root)))
      (let* ((context (kli:make-kernel-host))
             (protocol (switch-to-extension-protocol context)))
        (app::direct-url-resolve-and-verify url good)
        (is (null *trust-load-sentinel*) "verification alone loads nothing")
        (multiple-value-bind (state detail)
            (app::install-remote-extension
             (list :url url :git-tree-sha1 good) protocol context
             :confirm-fn (lambda (stage card) (declare (ignore stage card)) t))
          (declare (ignore detail))
          (is (eq state :installed) "a granted install completes")
          (is-true *trust-load-sentinel* "and loads the author code")
          (let ((pin (gethash "okx" (app:remote-install-pins protocol))))
            (is (not (null pin)) "and records the pin")
            (is (eq :url (getf pin :source-kind)) "as a url pin")
            (is (eq :hash (getf pin :trust)) "at the integrity floor")
            (is (equal url (getf pin :url)) "carrying the url for restore"))))))
  (let* ((good-bytes (trust-artifact-bytes "vbe"))
         (evil-bytes (trust-artifact-bytes "vbe-evil"))
         (url "https://ext.example/vbe.lisp")
         (good (app::git-blob-sha1 good-bytes)))
    (let ((*trust-load-sentinel* nil)
          (app::*registry-trust-roots* '())
          (app::*remote-install-fetcher* (trust-fetcher url evil-bytes))
          (app::*remote-install-staging-root* (trust-staging-root)))
      (let* ((context (kli:make-kernel-host))
             (protocol (switch-to-extension-protocol context))
             (pin (list :id "vbe" :url url :git-tree-sha1 good :trust :hash)))
        (is (null (app::url-resolver pin protocol context))
            "a re-fetch whose bytes fail the hash refuses to resolve")
        (is (null *trust-load-sentinel*) "and loads nothing")
        (let ((deferred (app::deferred-installs protocol)))
          (is (= 1 (length deferred)))
          (is (equal "vbe" (getf (first deferred) :id)))
          (is (eq :git-tree-sha1 (getf (first deferred) :reason))
              "recording the refusal on the silent restore path"))))
    (let ((*trust-load-sentinel* nil)
          (app::*registry-trust-roots* '())
          (app::*remote-install-fetcher* (trust-fetcher url good-bytes))
          (app::*remote-install-staging-root* (trust-staging-root)))
      (let* ((context (kli:make-kernel-host))
             (protocol (switch-to-extension-protocol context))
             (pin (list :id "vbe" :url url :git-tree-sha1 good :trust :signed)))
        (is (null (app::url-resolver pin protocol context))
            "a signed pin whose re-fetch verifies only at the hash floor refuses")
        (is (null *trust-load-sentinel*) "and loads nothing")
        (is (eq :signature-downgrade
                (getf (first (app::deferred-installs protocol)) :reason))
            "recording the downgrade refusal")))))

;;; Transactional, non-activating placement keyed by declared identity. The commit
;;; path stages the verified artifact to a temp sibling, trial-loads it there to
;;; learn the manifest-declared id, and safe-replaces it into <declared-id>/ without
;;; installing it into the running protocol. So placement is named by the code's own
;;; identity, not the url; a trial-load failure never touches the live installed set;
;;; a reinstall replaces the tree wholesale; and nothing is activated.

(defvar *sp2-load-sentinel* nil
  "Set t by a directory fixture's marker, so a test can tell an ordered cross-file
trial-load apart from a placement that loaded nothing.")

(defun sp2-staging-root ()
  (ensure-directories-exist
   (merge-pathnames (format nil "kli-sp2-~A/" (gensym)) (uiop:temporary-directory))))

(defun sp2-orphan-files (with-extra)
  "Directory files declaring extension `orphanprobe`: package.lisp, an optional inert
extra.lisp, and the extension.lisp marker. Dropping extra.lisp on a reinstall must
leave no orphan on disk."
  (flet ((octets (s) (sb-ext:string-to-octets s :external-format :utf-8)))
    (append
     (list (cons "package.lisp"
                 (octets "(defpackage #:kli/tests/sp2-orphan (:use #:cl #:kli/author))")))
     (when with-extra
       (list (cons "extra.lisp"
                   (octets (format nil "(in-package #:kli/tests/sp2-orphan)~%(defun sp2-extra () 1)~%")))))
     (list (cons "extension.lisp"
                 (octets (format nil "(in-package #:kli/tests/sp2-orphan)~%(defextension orphanprobe (:provides))~%")))))))

(defun sp2-dir-fixture-files ()
  "A 3-file directory extension declaring `dirprobe2`: package.lisp declares the
package, a-core.lisp defines a base value, and extension.lisp (loaded last) reads
that value at LOAD time -- so a successful trial-load proves the earlier file loaded
first -- sets the sentinel, and declares a tool. Its url leaf differs from the
declared id, so a test can tell declared-id placement from url-derived placement."
  (flet ((octets (s) (sb-ext:string-to-octets s :external-format :utf-8)))
    (list
     (cons "package.lisp"
           (octets "(defpackage #:kli/tests/sp2-dir (:use #:cl #:kli/author))"))
     (cons "a-core.lisp"
           (octets (format nil "(in-package #:kli/tests/sp2-dir)~%(defparameter *sp2-base* 40)~%")))
     (cons "extension.lisp"
           (octets (format nil "(in-package #:kli/tests/sp2-dir)~@
                                (setf kli/tests::*sp2-load-sentinel* t)~@
                                (defparameter *sp2-derived* (+ *sp2-base* 2))~@
                                (defextension dirprobe2~@
                                  (:provides~@
                                   (tool sp2ping~@
                                     :description \"pong\"~@
                                     :parameters '(:object)~@
                                     :runner (lambda (tool parameters context &key call-id on-update)~@
                                               (declare (ignore tool parameters context call-id on-update))~@
                                               *sp2-derived*))))~%"))))))

(defun sp2-dir-bundle-bytes (files)
  "Serialize FILES (an (RELPATH . OCTETS) alist) into a kli-dir-bundle-v1 envelope,
the same bundle shape a directory artifact ships as."
  (sb-ext:string-to-octets
   (with-output-to-string (s)
     (format s "{\"format\":\"~A\",\"files\":{" app::+dir-bundle-format+)
     (loop for (path . octets) in files
           for first = t then nil
           do (unless first (write-char #\, s))
              (format s "~S:~S" path (cl-base64:usb8-array-to-base64-string octets)))
     (format s "}}"))
   :external-format :utf-8))

(test direct-url-directory-trust-floor-and-ceiling
  "The opt-in signature ceiling applies to a directory bundle exactly as to a single
file: the detached signature is over the raw bundle envelope, while the pin is the git
tree id over the unpacked files. With no trust roots the bundle installs at the
integrity floor; with a root set, a valid signature from a trusted key passes and
marks the install signed, while a missing signature or one from an untrusted key
refuses -- the same trust matrix the single-file ceiling holds, over the dir shape."
  (let* ((files (sp2-dir-fixture-files))
         (bytes (sp2-dir-bundle-bytes files))
         (url "https://ext.example/dirprobe2.bundle")
         (good (app::git-tree-sha1 files)))
    (let ((app::*registry-trust-roots* '())
          (app::*remote-install-fetcher* (trust-fetcher url bytes)))
      (multiple-value-bind (payload meta) (app::direct-url-resolve-and-verify url good)
        (is (not (null payload)) "the floor returns the unpacked tree")
        (is (eq :directory (getf meta :artifact-kind)) "detected as a directory bundle")
        (is (eq :hash (getf meta :trust)) "and marks it integrity-only")))
    (multiple-value-bind (trust-hex signature) (trust-sign bytes)
      (let ((sig-url (concatenate 'string url app::*signature-url-suffix*)))
        (let ((app::*registry-trust-roots* (list trust-hex))
              (app::*remote-install-fetcher*
                (trust-fetcher url bytes sig-url signature)))
          (multiple-value-bind (payload meta) (app::direct-url-resolve-and-verify url good)
            (is (not (null payload)) "a trusted signature passes the ceiling")
            (is (eq :directory (getf meta :artifact-kind)) "still a directory install")
            (is (eq :signed (getf meta :trust)) "and marks the install signed")
            (is (stringp (getf meta :signed-by)) "recording the signing key fingerprint")))
        (let ((app::*registry-trust-roots* (list trust-hex))
              (app::*remote-install-fetcher* (trust-fetcher url bytes)))
          (is (eq :signature-missing
                  (nth-value 1 (app::direct-url-resolve-and-verify url good)))
              "a configured trust root with no signature refuses the directory"))
        (let ((other-hex (trust-sign bytes)))
          (let ((app::*registry-trust-roots* (list other-hex))
                (app::*remote-install-fetcher*
                  (trust-fetcher url bytes sig-url signature)))
            (is (eq :signature-untrusted
                    (nth-value 1 (app::direct-url-resolve-and-verify url good)))
                "a signature from an untrusted key refuses the directory")))))))

(test (no-activate-places-single-file-under-declared-id :fixture extension-load-authority)
  "The non-activating install path places a single-file extension under its
manifest-declared id, NOT the url-derived leaf: it trial-loads the code, records the
pin under the declared id, writes <declared-id>.lisp (and no <url-id>.lisp), and
installs nothing into the running protocol."
  (let* ((*trust-load-sentinel* nil)
         (artifact (trust-artifact-bytes "realid"))
         (url "https://ext.example/urlname.lisp")
         (good (app::git-blob-sha1 artifact))
         (staging (sp2-staging-root))
         (app::*registry-trust-roots* '())
         (app::*remote-install-fetcher* (trust-fetcher url artifact))
         (app::*remote-install-staging-root* staging))
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (multiple-value-bind (payload meta) (app::direct-url-resolve-and-verify url good)
        (is (equal "urlname" (getf meta :id)) "the url derives a different provisional id")
        (multiple-value-bind (state id)
            (app::commit-remote-install-no-activate meta payload protocol nil)
          (is (eq :installed state) "the no-activate install completes")
          (is (equal "realid" id) "returning the manifest-declared id, not the url leaf")
          (is-true *trust-load-sentinel* "having trial-loaded the author code")
          (let ((pin (gethash "realid" (app:remote-install-pins protocol))))
            (is (not (null pin)) "the pin is keyed by the declared id")
            (is (equal "realid" (getf pin :id)) "pin :id is the declared id")
            (is (equal "realid" (getf pin :dir-leaf)) "pin :dir-leaf is the declared id"))
          (is (null (gethash "urlname" (app:remote-install-pins protocol)))
              "no pin under the url-derived id")
          (is (not (null (probe-file (merge-pathnames "realid.lisp" staging))))
              "the source is placed under <declared-id>.lisp")
          (is (null (probe-file (merge-pathnames "urlname.lisp" staging)))
              "and never under the url-derived name")
          (is (zerop (hash-table-count (app::installed-user-handles protocol)))
              "and nothing is activated into the running protocol"))))))

(test (no-activate-places-directory-under-declared-id :fixture extension-load-authority)
  "The non-activating install path places a multi-file directory under its
manifest-declared id, trial-loads it in declared order (the marker reads at load time
a value an earlier-ordered file defines, so a clean load proves the order), records
the declared-id pin, and activates nothing."
  (let* ((*sp2-load-sentinel* nil)
         (files (sp2-dir-fixture-files))
         (bytes (sp2-dir-bundle-bytes files))
         (good (app::git-tree-sha1 files))
         (staging (sp2-staging-root))
         (app::*registry-trust-roots* '())
         (app::*remote-install-fetcher*
           (trust-fetcher "https://ext.example/urlname.bundle" bytes))
         (app::*remote-install-staging-root* staging))
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (multiple-value-bind (payload meta)
          (app::direct-url-resolve-and-verify "https://ext.example/urlname.bundle" good)
        (multiple-value-bind (state id)
            (app::commit-remote-install-no-activate meta payload protocol nil)
          (is (eq :installed state) "the no-activate directory install completes")
          (is (equal "dirprobe2" id) "under the declared id, not the url leaf")
          (is-true *sp2-load-sentinel*
                   "having trial-loaded the author code across files in order")
          (let ((pin (gethash "dirprobe2" (app:remote-install-pins protocol))))
            (is (not (null pin)) "the pin is keyed by the declared id")
            (is (eq :directory (getf pin :artifact-kind)) "flagged a directory install")
            (is (equal "dirprobe2" (getf pin :dir-leaf)) "pin :dir-leaf is the declared id"))
          (is (null (gethash "urlname" (app:remote-install-pins protocol)))
              "no pin under the url-derived id")
          (is (uiop:directory-exists-p (merge-pathnames "dirprobe2/" staging))
              "the tree is placed under <declared-id>/")
          (is (not (uiop:directory-exists-p (merge-pathnames "urlname/" staging)))
              "and never under the url-derived name")
          (dolist (leaf '("package.lisp" "a-core.lisp" "extension.lisp"))
            (is (not (null (probe-file (merge-pathnames leaf
                                        (merge-pathnames "dirprobe2/" staging)))))
                "every source file lands in the declared-id dir"))
          (is (null (ext:find-tool protocol :sp2ping))
              "the trial-loaded tool is NOT registered into the running protocol")
          (is (zerop (hash-table-count (app::installed-user-handles protocol)))
              "and nothing is activated"))))))

(test (no-activate-trial-load-failure-leaves-live-set-intact :fixture extension-load-authority)
  "A trial-load failure on the non-activating path is a no-op on the live installed
set: a prior good install's pin and placed file survive, no file is written for the
broken artifact, and the temp stage leaves no residue."
  (let* ((keeper (trust-artifact-bytes "keeper"))
         (keeper-url "https://ext.example/keeper.lisp")
         (broken (sb-ext:string-to-octets "(error \"trial-load must fail\")"
                                           :external-format :utf-8))
         (broken-url "https://ext.example/broken.lisp")
         (staging (sp2-staging-root))
         (app::*registry-trust-roots* '())
         (app::*remote-install-fetcher*
           (trust-fetcher keeper-url keeper broken-url broken))
         (app::*remote-install-staging-root* staging))
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (multiple-value-bind (payload meta)
          (app::direct-url-resolve-and-verify keeper-url (app::git-blob-sha1 keeper))
        (app::commit-remote-install-no-activate meta payload protocol nil))
      (is (not (null (gethash "keeper" (app:remote-install-pins protocol))))
          "the prior install is recorded")
      (multiple-value-bind (payload meta)
          (app::direct-url-resolve-and-verify broken-url (app::git-blob-sha1 broken))
        (multiple-value-bind (state reason)
            (app::commit-remote-install-no-activate meta payload protocol nil)
          (is (eq :rejected state) "a trial-load failure is rejected")
          (is (eq :index-failed reason) "with the trial-load reason")))
      (is (not (null (gethash "keeper" (app:remote-install-pins protocol))))
          "the prior pin survives the failed install")
      (is (not (null (probe-file (merge-pathnames "keeper.lisp" staging))))
          "and its placed source survives")
      (is (= 1 (hash-table-count (app:remote-install-pins protocol)))
          "no pin is recorded for the broken artifact")
      (is (null (probe-file (merge-pathnames "broken.lisp" staging)))
          "no source is placed for the broken artifact")
      (is (null (uiop:subdirectories staging))
          "and the temp stage leaves no residue"))))

(test (no-activate-reinstall-drops-orphan-files :fixture extension-load-authority)
  "Reinstalling a directory that drops a file replaces the tree wholesale: the dropped
file is gone from <declared-id>/, so no orphan of the prior version survives."
  (let* ((v1 (sp2-orphan-files t))
         (v2 (sp2-orphan-files nil))
         (v1-url "https://ext.example/orphan-v1.bundle")
         (v2-url "https://ext.example/orphan-v2.bundle")
         (staging (sp2-staging-root))
         (app::*registry-trust-roots* '())
         (app::*remote-install-fetcher*
           (trust-fetcher v1-url (sp2-dir-bundle-bytes v1)
                          v2-url (sp2-dir-bundle-bytes v2)))
         (app::*remote-install-staging-root* staging))
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context))
           (final (merge-pathnames "orphanprobe/" staging)))
      (multiple-value-bind (payload meta)
          (app::direct-url-resolve-and-verify v1-url (app::git-tree-sha1 v1))
        (app::commit-remote-install-no-activate meta payload protocol nil))
      (is (not (null (probe-file (merge-pathnames "extra.lisp" final))))
          "the first version places the extra file")
      (multiple-value-bind (payload meta)
          (app::direct-url-resolve-and-verify v2-url (app::git-tree-sha1 v2))
        (is (eq :installed (app::commit-remote-install-no-activate meta payload protocol nil))
            "the reinstall completes"))
      (is (null (probe-file (merge-pathnames "extra.lisp" final)))
          "the dropped file is gone -- the reinstall replaced the tree wholesale")
      (is (not (null (probe-file (merge-pathnames "package.lisp" final))))
          "the retained files remain")
      (is (not (null (probe-file (merge-pathnames "extension.lisp" final))))
          "the retained files remain"))))
