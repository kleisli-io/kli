(in-package #:kli/tests)
(in-suite all)

;;; The one layer the other remote-install tests do not exercise: the real HTTP
;;; transport. They all bind *remote-install-fetcher* to serve fixed bytes with
;;; no network. Here the fetcher is left UNBOUND, so direct-url-resolve-and-verify
;;; runs drakma against a live loopback origin -- a faithful "installs over the
;;; web" witness. Loopback works inside the Nix build sandbox (private netns with
;;; the loopback interface up), so this stays a pure check: no VM, no real network.

(defvar *web-install-load-sentinel* nil
  "Set t by the fixture artifact when its source is loaded, so a test can tell a
real load apart from a refusal that loads nothing.")

(defun web-install-staging-root ()
  (ensure-directories-exist
   (merge-pathnames "kli-web-install/" (uiop:temporary-directory))))

(defun web-install-artifact-bytes ()
  "A single-file user extension: it marks the load sentinel and provides one tool
that returns a known result. No in-package form, so it loads in kli/author like
any author single-file extension."
  (sb-ext:string-to-octets
   (concatenate 'string
     "(setf kli/tests::*web-install-load-sentinel* t)" (string #\Newline)
     "(defextension web-probe" (string #\Newline)
     "  (:provides" (string #\Newline)
     "   (tool ping" (string #\Newline)
     "     :description \"returns pong\"" (string #\Newline)
     "     :parameters '(:object)" (string #\Newline)
     "     :runner (lambda (tool parameters context &key call-id on-update)" (string #\Newline)
     "               (declare (ignore tool parameters context call-id on-update))" (string #\Newline)
     "               \"pong\"))))" (string #\Newline))
   :external-format :utf-8))

(defun web-install-drain-request (stream)
  "Read the HTTP request head from STREAM up to the terminating blank line, so the
client's write completes before we reply -- a byte stack can RST a socket that
still has unread request bytes, truncating the response the client is reading."
  (let ((a 0) (b 0) (c 0) (d 0))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (shiftf a b c d byte)
          until (and (= a 13) (= b 10) (= c 13) (= d 10)))))

(defun web-install-serve-once (bytes &optional (leaf "web-probe.lisp"))
  "Serve BYTES as one HTTP/1.0 200 response to the first GET on a fresh loopback
socket bound to an ephemeral port (Nix-sandbox-safe: no hardcoded port). LEAF is
the URL's last path segment, which the install path derives the extension id from.
Returns (values URL SHUTDOWN): URL points a drakma client at the origin; SHUTDOWN
closes the listener and joins the accept thread. The accept loop runs on a
background thread so the in-process drakma client can connect; listen queues the
connect, so there is no accept/connect race."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (thread
           (sb-thread:make-thread
            (lambda ()
              (let ((conn (usocket:socket-accept server :element-type '(unsigned-byte 8))))
                (unwind-protect
                     (let ((stream (usocket:socket-stream conn)))
                       (web-install-drain-request stream)
                       (let ((header (sb-ext:string-to-octets
                                      (format nil "HTTP/1.0 200 OK~C~CContent-Length: ~D~C~C~C~C"
                                              #\Return #\Linefeed
                                              (length bytes)
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed))))
                         (write-sequence header stream)
                         (write-sequence bytes stream)
                         (finish-output stream)))
                  (ignore-errors (usocket:socket-close conn)))))
            :name "web-install-origin")))
    (values (format nil "http://127.0.0.1:~D/~A" port leaf)
            (lambda ()
              (ignore-errors (usocket:socket-close server))
              (ignore-errors (sb-thread:join-thread thread :timeout 2))))))

(test (web-install-over-real-transport :fixture extension-load-authority)
  "A user-authored single-file extension installs over a REAL drakma HTTP fetch
(no *remote-install-fetcher* stub) from a loopback origin, pins at the integrity
floor, and its tool is then reachable and returns its result -- proving the
installed extension works, not merely that it loaded."
  (let* ((*web-install-load-sentinel* nil)
         (bytes (web-install-artifact-bytes))
         (good (app::git-blob-sha1 bytes))
         (app::*remote-install-staging-root* (web-install-staging-root)))
    (multiple-value-bind (url shutdown) (web-install-serve-once bytes)
      (unwind-protect
           (let* ((context (kli:make-kernel-host))
                  (protocol (switch-to-extension-protocol context)))
             (multiple-value-bind (state detail)
                 (app::install-remote-extension
                  (list :url url :git-tree-sha1 good) protocol context
                  :confirm-fn (constantly t))
               (declare (ignore detail))
               (is (eq :installed state) "the install completes over real HTTP")
               (is-true *web-install-load-sentinel* "and loads the author code")
               (let ((pin (gethash "web-probe" (app:remote-install-pins protocol))))
                 (is (not (null pin)) "recording the url pin")
                 (is (eq :url (getf pin :source-kind)) "as a url pin")
                 (is (eq :hash (getf pin :trust)) "at the integrity floor")
                 (is (equal url (getf pin :url)) "carrying the url for restore"))
               (let ((tool (ext:find-tool protocol :ping)))
                 (is (not (null tool)) "the installed extension's tool is registered")
                 (let ((result (ext:invoke-tool protocol :ping '() context)))
                   (is (null (ext:tool-result-error-p result))
                       "invoking the tool succeeds")
                   (is (equal "pong"
                              (getf (first (ext:tool-result-content result)) :text))
                       "and the tool works, returning its result")))))
        (funcall shutdown)))))

(test (web-install-rejects-hash-mismatch :fixture extension-load-authority)
  "Over the same real transport, an artifact whose fetched bytes do not match the
declared git-tree-sha1 is rejected and nothing loads -- the integrity floor holds
over the wire, not only against a stubbed fetcher."
  (let* ((*web-install-load-sentinel* nil)
         (bytes (web-install-artifact-bytes))
         (wrong-hash (app::git-blob-sha1
                      (sb-ext:string-to-octets "not the served bytes")))
         (app::*remote-install-staging-root* (web-install-staging-root)))
    (multiple-value-bind (url shutdown) (web-install-serve-once bytes)
      (unwind-protect
           (let* ((context (kli:make-kernel-host))
                  (protocol (switch-to-extension-protocol context)))
             (multiple-value-bind (state detail)
                 (app::install-remote-extension
                  (list :url url :git-tree-sha1 wrong-hash) protocol context
                  :confirm-fn (constantly t))
               (is (eq :rejected state) "a hash mismatch over the wire is rejected")
               (is (eq :git-tree-sha1 detail) "with the integrity-floor reason")
               (is (null *web-install-load-sentinel*) "and nothing loads")))
        (funcall shutdown)))))

;;; Directory installs. A multi-file extension ships as one bundle blob pinned by a
;;; REAL git tree object id. The same loopback origin serves it, so the drakma
;;; transport, the tree-sha1 floor, unpack-and-place, and the ordered dir-unit load
;;; are all witnessed end to end.

(defvar *dir-install-load-sentinel* nil
  "Set t by the directory fixture's marker file when the unit is loaded, so a test
can tell a real cross-file load apart from a refusal that loads nothing.")

(defun dir-install-fixture-files ()
  "A three-file directory extension as an alist of (RELPATH . OCTETS): package.lisp
declares the package, a-core.lisp provides a helper, and extension.lisp (the marker,
loaded last) marks the sentinel and provides a tool that calls the helper. The tool
returning the helper's value proves a real cross-file load, not merely that the
files landed."
  (flet ((octets (s) (sb-ext:string-to-octets s :external-format :utf-8)))
    (list
     (cons "package.lisp"
           (octets "(defpackage #:kli/tests/dir-probe (:use #:cl #:kli/author))"))
     (cons "a-core.lisp"
           (octets (format nil "(in-package #:kli/tests/dir-probe)~%(defun probe-answer () \"pong-dir\")~%")))
     (cons "extension.lisp"
           (octets (format nil "(in-package #:kli/tests/dir-probe)~@
                                (setf kli/tests::*dir-install-load-sentinel* t)~@
                                (defextension dir-probe~@
                                  (:provides~@
                                   (tool ping~@
                                     :description \"returns pong-dir\"~@
                                     :parameters '(:object)~@
                                     :runner (lambda (tool parameters context &key call-id on-update)~@
                                               (declare (ignore tool parameters context call-id on-update))~@
                                               (probe-answer)))))~%"))))))

(defun dir-install-bundle-bytes (files)
  "Serialize FILES (an alist of (RELPATH . OCTETS)) into a kli-dir-bundle-v1
envelope: a JSON object mapping each relpath to its base64 content. Paths and
base64 carry no JSON metacharacters, so ~S string syntax is valid JSON."
  (sb-ext:string-to-octets
   (with-output-to-string (s)
     (format s "{\"format\":\"~A\",\"files\":{" app::+dir-bundle-format+)
     (loop for (path . octets) in files
           for first = t then nil
           do (unless first (write-char #\, s))
              (format s "~S:~S" path (cl-base64:usb8-array-to-base64-string octets)))
     (format s "}}"))
   :external-format :utf-8))

(test git-tree-sha1-matches-git-write-tree
  "The directory pin is a REAL git tree object id: git-tree-sha1 over a fixed file
set equals the tree sha `git write-tree` produces for the same tree (a.lisp = \"A\\n\",
b/c.lisp = \"C\", the nested path exercising subtree hashing and git's name order),
so an author pins with plain git and the client verifies the identical id."
  (is (string= "aa63c2438e773384070fc84561dc53b5f7a9917e"
               (app::git-tree-sha1
                (list (cons "a.lisp" (sb-ext:string-to-octets (format nil "A~%")
                                                              :external-format :utf-8))
                      (cons "b/c.lisp" (sb-ext:string-to-octets "C"
                                                                :external-format :utf-8)))))))

(test (web-install-directory-over-real-transport :fixture extension-load-authority)
  "A multi-file directory extension installs over a REAL drakma HTTP fetch (no
*remote-install-fetcher* stub) from a loopback origin, pinned by a real
git-tree-sha1, and its tool -- which calls a helper defined in a SEPARATE file --
is then reachable and returns its result, proving an ordered cross-file load, not
merely that the bundle unpacked."
  (let* ((*dir-install-load-sentinel* nil)
         (files (dir-install-fixture-files))
         (bytes (dir-install-bundle-bytes files))
         (good (app::git-tree-sha1 files))
         (app::*remote-install-staging-root* (web-install-staging-root)))
    (multiple-value-bind (url shutdown) (web-install-serve-once bytes "dir-probe.bundle")
      (unwind-protect
           (let* ((context (kli:make-kernel-host))
                  (protocol (switch-to-extension-protocol context)))
             (multiple-value-bind (state detail)
                 (app::install-remote-extension
                  (list :url url :git-tree-sha1 good) protocol context
                  :confirm-fn (constantly t))
               (declare (ignore detail))
               (is (eq :installed state) "the directory install completes over real HTTP")
               (is-true *dir-install-load-sentinel*
                        "and loads the author code across files")
               (let ((pin (gethash "dir-probe" (app:remote-install-pins protocol))))
                 (is (not (null pin)) "recording the url pin")
                 (is (eq :url (getf pin :source-kind)) "as a url pin")
                 (is (eq :directory (getf pin :artifact-kind)) "flagged a directory install")
                 (is (eq :hash (getf pin :trust)) "at the integrity floor")
                 (is (equal url (getf pin :url)) "carrying the url for restore"))
               (let ((tool (ext:find-tool protocol :ping)))
                 (is (not (null tool)) "the installed extension's tool is registered")
                 (let ((result (ext:invoke-tool protocol :ping '() context)))
                   (is (null (ext:tool-result-error-p result))
                       "invoking the tool succeeds")
                   (is (equal "pong-dir"
                              (getf (first (ext:tool-result-content result)) :text))
                       "and the cross-file helper resolves, returning its result")))))
        (funcall shutdown)))))

(test (web-install-directory-rejects-hash-mismatch :fixture extension-load-authority)
  "Over the same real transport, a directory bundle whose unpacked tree does not
match the declared git-tree-sha1 is rejected and nothing loads -- the tree floor
holds over the wire, the same refusal the single-file blob floor gives."
  (let* ((*dir-install-load-sentinel* nil)
         (files (dir-install-fixture-files))
         (bytes (dir-install-bundle-bytes files))
         (wrong (app::git-tree-sha1
                 (list (cons "package.lisp"
                             (sb-ext:string-to-octets "(defpackage #:nope)"
                                                      :external-format :utf-8)))))
         (app::*remote-install-staging-root* (web-install-staging-root)))
    (multiple-value-bind (url shutdown) (web-install-serve-once bytes "dir-probe.bundle")
      (unwind-protect
           (let* ((context (kli:make-kernel-host))
                  (protocol (switch-to-extension-protocol context)))
             (multiple-value-bind (state detail)
                 (app::install-remote-extension
                  (list :url url :git-tree-sha1 wrong) protocol context
                  :confirm-fn (constantly t))
               (is (eq :rejected state) "a tree-sha mismatch over the wire is rejected")
               (is (eq :git-tree-sha1 detail) "with the integrity-floor reason")
               (is (null *dir-install-load-sentinel*) "and nothing loads")))
        (funcall shutdown)))))
