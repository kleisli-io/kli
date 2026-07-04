(in-package #:kli/app)

(defun sha256-hex (bytes)
  "Lowercase hex sha-256 of BYTES, the transport hash over the exact bytes
fetched and about to be loaded."
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 bytes)))

(defun git-blob-digest (bytes)
  "Raw 20-byte git blob object id of BYTES: sha-1 over the git blob header
(\"blob <len>\" then a nul byte) followed by the content. The binary form
embedded in a git tree entry; git-blob-sha1 is its hex projection."
  (let* ((header (ironclad:ascii-string-to-byte-array
                  (format nil "blob ~D~C" (length bytes) #\Nul)))
         (object (concatenate '(simple-array (unsigned-byte 8) (*)) header bytes)))
    (ironclad:digest-sequence :sha1 object)))

(defun git-blob-sha1 (bytes)
  "Git blob object id of BYTES as a lowercase hex string. Equals git hash-object,
so it is a durable content identity that survives a transport re-compression the
raw transport sha256 would not."
  (ironclad:byte-array-to-hex-string (git-blob-digest bytes)))

;;; Directory artifacts. A multi-file extension ships as one bundle blob whose
;;; identity is the REAL git tree object id over its unpacked tree -- the same id
;;; git write-tree produces -- so an author pins with git and the client verifies
;;; the exact bytes it will load. Regular files are mode 100644, nested dirs are
;;; trees; executable and symlink modes are out of scope, as extension source is
;;; plain readable Lisp.

(defun git-name-lessp (a b)
  "Byte-wise order of two tree-entry names, git's tree ordering. A directory name
sorts as if it carried a trailing slash (appended by the caller), so a file and a
dir sharing a prefix interleave the way git write-tree lays them out."
  (let ((x (sb-ext:string-to-octets a :external-format :utf-8))
        (y (sb-ext:string-to-octets b :external-format :utf-8)))
    (loop for i below (min (length x) (length y))
          for cx = (aref x i) for cy = (aref y i)
          when (/= cx cy) return (< cx cy)
          finally (return (< (length x) (length y))))))

(defun git-tree-entry-sort-key (entry)
  "ENTRY is (NAME . NODE). A subtree sorts under NAME with a trailing slash."
  (if (eq (car (cdr entry)) :tree)
      (concatenate 'string (car entry) "/")
      (car entry)))

(defun tree-insert (tree path octets)
  "Insert OCTETS at PATH (a list of name segments) into TREE, an alist of (NAME .
NODE) with NODE (:blob . OCTETS) or (:tree . SUBTREE), returning the new alist."
  (destructuring-bind (head . rest) path
    (if (null rest)
        (acons head (cons :blob octets) tree)
        (let* ((existing (assoc head tree :test #'string=))
               (subtree (and existing (eq (car (cdr existing)) :tree)
                             (cdr (cdr existing)))))
          (acons head (cons :tree (tree-insert subtree rest octets))
                 (remove head tree :key #'car :test #'string=))))))

(defun files->git-tree (files)
  "Nest a flat (RELPATH . OCTETS) alist into a git tree: an alist of (NAME .
NODE), NODE (:blob . OCTETS) for a file or (:tree . SUBTREE) for a directory."
  (let ((tree '()))
    (dolist (pair files tree)
      (setf tree (tree-insert tree
                              (remove "" (uiop:split-string (car pair) :separator "/")
                                      :test #'string=)
                              (cdr pair))))))

(defun git-tree-digest (tree)
  "Raw 20-byte git tree object id of TREE, an alist of (NAME . NODE). Serializes
each entry as git does -- mode, space, name, nul, then the raw child object id --
in git's name order, and hashes the framed tree object."
  (let* ((sorted (sort (copy-list tree) #'git-name-lessp
                       :key #'git-tree-entry-sort-key))
         (body
           (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                  (loop for (name . node) in sorted
                        for tree-p = (eq (car node) :tree)
                        collect (concatenate '(simple-array (unsigned-byte 8) (*))
                                             (sb-ext:string-to-octets
                                              (format nil "~A ~A"
                                                      (if tree-p "40000" "100644") name)
                                              :external-format :utf-8)
                                             (make-array 1 :element-type '(unsigned-byte 8)
                                                           :initial-element 0)
                                             (if tree-p
                                                 (git-tree-digest (cdr node))
                                                 (git-blob-digest (cdr node)))))))
         (object (concatenate '(simple-array (unsigned-byte 8) (*))
                              (ironclad:ascii-string-to-byte-array
                               (format nil "tree ~D~C" (length body) #\Nul))
                              body)))
    (ironclad:digest-sequence :sha1 object)))

(defun git-tree-sha1 (files)
  "Lowercase hex git tree object id over FILES, an alist of (RELPATH . OCTETS).
RELPATHs nest into subtrees before hashing, so the result equals git write-tree
over the same file set."
  (ironclad:byte-array-to-hex-string
   (git-tree-digest (files->git-tree files))))

(define-condition artifact-verification-failed (error)
  ((reason :initarg :reason :reader artifact-verification-reason)
   (expected :initarg :expected :reader artifact-verification-expected)
   (actual :initarg :actual :reader artifact-verification-actual))
  (:report (lambda (condition stream)
             (format stream "Artifact verification failed (~A): expected ~A, got ~A"
                     (artifact-verification-reason condition)
                     (artifact-verification-expected condition)
                     (artifact-verification-actual condition)))))

(defparameter +dir-bundle-format+ "kli-dir-bundle-v1"
  "Envelope tag of a directory artifact: a JSON object {\"format\": <this>,
\"files\": {RELPATH: base64-octets}}. One self-describing blob, so a directory
extension fetches, verifies, and signs exactly like a single file does.")

(defun parse-dir-bundle (bytes)
  "Unpack a directory bundle into an alist of (RELPATH . OCTETS), or signal
artifact-verification-failed when BYTES are not a well-formed bundle -- a tampered
or truncated bundle refuses rather than loading a partial tree."
  (flet ((refuse ()
           (error 'artifact-verification-failed :reason :git-tree-sha1
                  :expected :directory-bundle :actual :malformed)))
    (let ((json (handler-case
                    (com.inuoe.jzon:parse
                     (sb-ext:octets-to-string bytes :external-format :utf-8))
                  (error () (refuse)))))
      (unless (and (hash-table-p json)
                   (equal (gethash "format" json) +dir-bundle-format+)
                   (hash-table-p (gethash "files" json)))
        (refuse))
      (let ((files '()))
        (maphash (lambda (path b64)
                   (unless (stringp b64) (refuse))
                   (push (cons path (cl-base64:base64-string-to-usb8-array b64)) files))
                 (gethash "files" json))
        files))))

(defun dir-bundle-p (bytes)
  "True when BYTES are a directory bundle envelope. The discriminator that routes a
fetched artifact to directory verification; single-file Lisp source, not being a
JSON object carrying the bundle tag, routes to the blob floor."
  (handler-case
      (let ((json (com.inuoe.jzon:parse
                   (sb-ext:octets-to-string bytes :external-format :utf-8))))
        (and (hash-table-p json)
             (equal (gethash "format" json) +dir-bundle-format+)))
    (error () nil)))

(defun verify-blob (bytes expected)
  "Return t when the git blob id of BYTES equals EXPECTED, else signal
artifact-verification-failed. A nil pin is a failure, not a skip: an unpinned
artifact is never trusted. The integrity floor for a single-file install."
  (let ((actual (git-blob-sha1 bytes)))
    (unless (and expected (string-equal expected actual))
      (error 'artifact-verification-failed :reason :git-tree-sha1
                                           :expected expected :actual actual))
    t))

(defun verify-tree (files expected)
  "Return t when the real git tree id over FILES equals EXPECTED, else signal
artifact-verification-failed. The integrity floor for a directory install: the pin
is a git tree object id, so it verifies the whole unpacked tree at once."
  (let ((actual (git-tree-sha1 files)))
    (unless (and expected (string-equal expected actual))
      (error 'artifact-verification-failed :reason :git-tree-sha1
                                           :expected expected :actual actual))
    t))

(defun resolve-artifact-payload (kind raw expected)
  "Verify RAW at KIND's integrity floor and return the load payload: (values
PAYLOAD nil) on a match, or (values nil REASON) on failure. A :file payload is RAW
itself; a :directory payload is the unpacked (RELPATH . OCTETS) tree. Nothing is
placed, so verification always precedes any load."
  (handler-case
      (ecase kind
        (:file (verify-blob raw expected) (values raw nil))
        (:directory (let ((files (parse-dir-bundle raw)))
                      (verify-tree files expected)
                      (values files nil))))
    (artifact-verification-failed (condition)
      (values nil (artifact-verification-reason condition)))))

(defun decode-ed25519-public-key (encoded)
  "Public key from its ENCODED form, a hex string of the 32 raw ed25519
public-key bytes. Nil on a malformed key so one bad trust-root entry denies
rather than crashing the verify loop."
  (handler-case
      (let ((raw (ironclad:hex-string-to-byte-array encoded)))
        (and (= (length raw) 32)
             (ironclad:make-public-key :ed25519 :y raw)))
    (error () nil)))

(defun ed25519-verify-raw (public-key message signature)
  "True when SIGNATURE is a valid ed25519 signature of MESSAGE under PUBLIC-KEY.
MESSAGE is the raw byte vector: ed25519 hashes it with sha-512 internally, so it
is never pre-hashed."
  (handler-case (and (ironclad:verify-signature public-key message signature) t)
    (error () nil)))

(defun decode-ed25519-private-key (encoded)
  "Private key from a hex string of the 32 raw ed25519 seed bytes, or nil if
malformed. The signing-side inverse of decode-ed25519-public-key."
  (handler-case
      (let ((raw (ironclad:hex-string-to-byte-array encoded)))
        (and (= (length raw) 32)
             (ironclad:make-private-key :ed25519 :x raw)))
    (error () nil)))

(defun ed25519-sign-raw (private-key message)
  "Detached ed25519 signature of the raw MESSAGE bytes, the inverse of
ed25519-verify-raw. Unhashed: ed25519 hashes with sha-512 internally."
  (ironclad:sign-message private-key message))

(defvar *registry-trust-roots* '()
  "Encoded hex ed25519 public keys of the publishers the user trusts. EMPTY by
default: an unconfigured image verifies integrity (the git hash) only, the
decentralized floor where anyone may publish. When NON-EMPTY the user has opted in
to authenticity -- every install must also carry a detached signature from one of
these keys over the raw artifact bytes, or it refuses.")

(defun parse-trust-roots (value)
  "Trust roots from a settings VALUE: the non-empty hex strings of a configured
trustRoots array, as a list. An array parses to a vector; empty and non-string
entries are dropped with a warning, so one stray entry never disables the roots
that remain -- the fail-secure direction for an authenticity ceiling. NIL (the key
absent) yields no roots, and a present non-array value is a misconfiguration that
carries nothing to trust, warned and treated as none -- either way the integrity
floor."
  (cond
    ((null value) '())
    ((and (vectorp value) (not (stringp value)))
     (loop for entry across value
           if (and (stringp entry) (plusp (length entry)))
             collect entry
           else
             do (warn "kli: ignoring a malformed trustRoots entry (~S)" entry)))
    (t
     (warn "kli: ignoring trustRoots (expected an array of hex public keys)")
     '())))

(defvar *remote-install-fetcher* nil
  "Injected fetch seam, a function of (url) returning raw octets or nil. Nil uses
drakma. Bound in tests to serve fixed bytes with no network.")

(defvar *signature-url-suffix* ".sig"
  "Detached-signature URL convention: the signature for an artifact at <url> is
fetched from <url><suffix>. Only consulted when *registry-trust-roots* is
non-empty, i.e. when the user has opted in to signature verification.")

(defvar *remote-install-staging-root* nil
  "Directory under which a verified artifact is placed before indexing. Nil uses
the user extension dir. Bound in tests to a temporary directory.")

(defun fetch-octets (url)
  "Raw octets at URL via *remote-install-fetcher* or drakma. Nil on any transport
failure or non-success status so a fetch failure defers rather than crashes."
  (when url
    (handler-case
        (if *remote-install-fetcher*
            (funcall *remote-install-fetcher* url)
            (multiple-value-bind (body status)
                (drakma:http-request url :force-binary t)
              (and (eql status 200) body)))
      (error () nil))))

(defparameter +deferred-installs-key+ :kli/app.deferred-installs
  "Storage key for installs refused on the restore re-fetch path. A pin whose
re-fetched bytes fail the hash or signature is recorded here and never loaded.
This is the verify-before-execute boundary on the silent restore path, which has
no trust card and runs under the all-passing system subject.")

(defun deferred-installs (protocol)
  "Refusal records from the restore re-fetch path, oldest first."
  (protocol-storage protocol +deferred-installs-key+))

(defun record-deferred-install (protocol record)
  "Append RECORD, a plist naming a refused pin and its reason, to PROTOCOL's
deferred-installs ledger."
  (setf (protocol-storage protocol +deferred-installs-key+)
        (append (deferred-installs protocol) (list record))))

(defun trusted-signature-fingerprint (bytes signature)
  "When SIGNATURE over the raw artifact BYTES verifies under a configured trust
root, the short fingerprint (leading hex) of the verifying key, else nil."
  (loop for encoded in *registry-trust-roots*
        for key = (decode-ed25519-public-key encoded)
        when (and key (ed25519-verify-raw key bytes signature))
          return (subseq encoded 0 (min 16 (length encoded)))))

(defun fetch-detached-signature (url)
  "Octets of the detached signature for the artifact at URL, by the
*signature-url-suffix* convention, or nil if absent."
  (fetch-octets (concatenate 'string url *signature-url-suffix*)))

(defun signature-status (url bytes)
  "The opt-in authenticity check over artifact BYTES fetched from URL. Returns
(values :hash nil) when no trust roots are configured -- integrity only, the
decentralized floor. When trust roots ARE configured, fetches the detached
signature and requires a valid ed25519 signature over the raw bytes from a trusted
key, returning (values :signed FINGERPRINT) on success or (values nil
:signature-missing) / (values nil :signature-untrusted) on failure."
  (if (null *registry-trust-roots*)
      (values :hash nil)
      (let ((sig (fetch-detached-signature url)))
        (if (null sig)
            (values nil :signature-missing)
            (let ((fp (trusted-signature-fingerprint bytes sig)))
              (if fp
                  (values :signed fp)
                  (values nil :signature-untrusted)))))))

(defun url-derived-id (url)
  "A human-readable id from URL's last path segment: the file name minus a
trailing .lisp/.json/.bundle and any query string. Cosmetic -- a pin's real
identity is its url plus git hash -- but it names the placed unit and keys the
install-set."
  (let* ((path (let ((q (position #\? url))) (if q (subseq url 0 q) url)))
         (slash (position #\/ path :from-end t))
         (leaf (if slash (subseq path (1+ slash)) path)))
    (or (loop for suffix in '(".lisp" ".json" ".bundle")
              when (and (> (length leaf) (length suffix))
                        (string= suffix leaf :start2 (- (length leaf) (length suffix))))
                return (subseq leaf 0 (- (length leaf) (length suffix))))
        leaf)))

(defun direct-url-resolve-and-verify (url git-tree-sha1 &optional artifact-kind)
  "Fetch URL, verify the git-tree-sha1 integrity floor over the fetched bytes,
then apply the opt-in signature ceiling. ARTIFACT-KIND (:file or :directory) routes
the verification; nil detects it -- a directory bundle envelope is a directory,
anything else a single file -- so a caller need not know the shape in advance while
restore can pin the kind it recorded. Returns (values PAYLOAD META) on full
success (PAYLOAD is the octets to place for a file, the unpacked tree for a
directory), or (values nil REASON) on any failure. Nothing is placed or loaded, so
the caller executes only after verification has passed."
  (let ((raw (fetch-octets url)))
    (when (null raw)
      (return-from direct-url-resolve-and-verify (values nil :fetch-failed)))
    (let ((kind (or artifact-kind (if (dir-bundle-p raw) :directory :file))))
      (multiple-value-bind (payload reason)
          (resolve-artifact-payload kind raw git-tree-sha1)
        (when reason
          (return-from direct-url-resolve-and-verify (values nil reason)))
        (multiple-value-bind (trust fingerprint-or-reason) (signature-status url raw)
          (if (null trust)
              (values nil fingerprint-or-reason)
              (values payload
                      (list :id (url-derived-id url)
                            :url url
                            :artifact-kind kind
                            :git-tree-sha1 git-tree-sha1
                            :transport-sha256 (sha256-hex raw)
                            :version nil
                            :native-libs nil
                            :trust trust
                            :signed-by (and (eq trust :signed) fingerprint-or-reason)))))))))

(defun staging-root ()
  (or *remote-install-staging-root*
      (uiop:xdg-config-home +user-config-dir+ "extensions/")))

(defun write-octets-file (path octets)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (write-sequence octets out))
  path)

(defun place-verified-artifact (id payload kind)
  "Place a verified artifact for extension ID under the staging root and return
its load unit. A :file PAYLOAD is octets written as <id>.lisp, a (:single ...)
unit. A :directory PAYLOAD is the unpacked (RELPATH . OCTETS) tree written under
<id>/; its unit is formed by discover-units on the placed tree, the same rule a
later boot uses, so install and rediscovery never disagree. Called only after
verification passed, so the bytes reaching the loader are the verified bytes."
  (ecase kind
    (:file
     (list :single
           (write-octets-file
            (merge-pathnames (format nil "~A.lisp" id) (staging-root)) payload)))
    (:directory
     (let ((dir (ensure-directories-exist
                 (merge-pathnames (format nil "~A/" id) (staging-root)))))
       (dolist (pair payload)
         (write-octets-file
          (merge-pathnames (uiop:parse-unix-namestring (car pair)) dir) (cdr pair)))
       (first (discover-units dir))))))

(defun declared-id-leaf (id)
  "Lowercase filesystem-and-pin leaf for a manifest-declared extension ID (a
keyword from normalize-extension-id, or a string). Names the placed <leaf>/ dir
or <leaf>.lisp file and the pin, so placement is keyed by the code's own identity
rather than the url it was fetched from."
  (string-downcase (if (symbolp id) (symbol-name id) id)))

(defun read-file-octets (path)
  "Raw octets of the file at PATH."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buffer in)
      buffer)))

(defun staged-tree-octets (dir payload)
  "Read PAYLOAD's relpaths back from DIR as an (RELPATH . OCTETS) alist, so the
staged tree is re-hashed from what actually landed on disk, not from the octets
still in memory."
  (loop for (relpath . nil) in payload
        collect (cons relpath
                      (read-file-octets
                       (merge-pathnames (uiop:parse-unix-namestring relpath) dir)))))

(defun safe-replace-dir (staged final)
  "Move the STAGED directory onto FINAL wholesale: rename any existing FINAL aside,
rename STAGED in, then drop the aside. A rename failure restores the aside and
resignals, so a failure leaves the prior install intact. The new tree fully
replaces the old, so a file the old install carried and the new one drops leaves
no orphan."
  (let* ((final (uiop:ensure-directory-pathname final))
         (had (uiop:directory-exists-p final))
         (aside (uiop:ensure-directory-pathname
                 (format nil "~A.superseded-~A"
                         (string-right-trim "/" (namestring final)) (gensym)))))
    (when had (rename-file final aside))
    (handler-case (rename-file (uiop:ensure-directory-pathname staged) final)
      (error (condition)
        (when had (rename-file aside final))
        (error condition)))
    (when had
      (uiop:delete-directory-tree aside :validate t :if-does-not-exist :ignore))))

(defun safe-replace-file (staged final)
  "Move the STAGED single-file artifact onto FINAL, replacing any prior file. Rename
is atomic on the shared staging filesystem, so a concurrent reader never sees a
half-written file."
  (when (probe-file final) (delete-file final))
  (rename-file staged final))

(defun stage-verified-artifact (payload kind expected-sha protocol)
  "Transactionally place PAYLOAD and trial-load it WITHOUT activating. Writes PAYLOAD
into a fresh temp sibling of the staging root, re-verifies its on-disk git identity
against EXPECTED-SHA, trial-loads it through index-unit (the unified unit rule -- no
manifest install), learns the manifest-declared id, and safe-replaces the stage into
<declared-leaf>/ (or <declared-leaf>.lisp). Returns (values ENTRY DECLARED-LEAF) on
success, or (values NIL REASON) on an integrity or trial-load failure, having removed
the temp stage so the live installed set is untouched."
  (let* ((root (staging-root))
         (stage (merge-pathnames (format nil ".staging-~A/" (gensym)) root)))
    (ensure-directories-exist stage)
    (unwind-protect
         (ecase kind
           (:file
            (let ((file (write-octets-file (merge-pathnames "artifact.lisp" stage)
                                           payload)))
              (if (ignore-errors (verify-blob (read-file-octets file) expected-sha))
                  (let ((entry (index-unit protocol (list :single file))))
                    (if entry
                        (let ((leaf (declared-id-leaf
                                     (user-extension-entry-id entry))))
                          (safe-replace-file
                           file (merge-pathnames (format nil "~A.lisp" leaf) root))
                          (values entry leaf))
                        (values nil :index-failed)))
                  (values nil :integrity-failed))))
           (:directory
            (dolist (pair payload)
              (write-octets-file
               (merge-pathnames (uiop:parse-unix-namestring (car pair)) stage)
               (cdr pair)))
            (if (ignore-errors (verify-tree (staged-tree-octets stage payload)
                                            expected-sha))
                (let ((entry (index-unit protocol (first (discover-units stage)))))
                  (if entry
                      (let ((leaf (declared-id-leaf
                                   (user-extension-entry-id entry))))
                        (safe-replace-dir
                         stage (merge-pathnames (format nil "~A/" leaf) root))
                        (setf stage nil)
                        (values entry leaf))
                      (values nil :index-failed)))
                (values nil :integrity-failed))))
      (when stage
        (ignore-errors
         (uiop:delete-directory-tree (uiop:ensure-directory-pathname stage)
                                     :validate t :if-does-not-exist :ignore))))))

(defun url-resolver (pin protocol context)
  "Restore-time *remote-install-resolver*: re-fetch PIN's code from its url under
the verify-before-execute invariant. Re-verifies the git hash floor and, when the
pin recorded :trust :signed, the signature ceiling, BEFORE any load. Returns an
indexed entry only after re-verification passes, else records the refusal in
deferred-installs and returns nil so the caller defers rather than loads. Loads
nothing on a verification failure or a signature downgrade."
  (declare (ignore context))
  (let ((id (getf pin :id))
        (url (getf pin :url))
        (git-tree-sha1 (getf pin :git-tree-sha1)))
    (multiple-value-bind (artifact meta)
        (direct-url-resolve-and-verify url git-tree-sha1 (getf pin :artifact-kind))
      (cond
        ((null artifact)
         (record-deferred-install protocol (list :id id :reason meta))
         nil)
        ((and (eq (getf pin :trust) :signed)
              (not (eq (getf meta :trust) :signed)))
         (record-deferred-install protocol (list :id id :reason :signature-downgrade))
         nil)
        (t
         (handler-case
             (index-unit protocol
                         (place-verified-artifact id artifact (getf meta :artifact-kind)))
           (error ()
             (record-deferred-install protocol (list :id id :reason :index-failed))
             nil)))))))

(setf *remote-install-resolver* #'url-resolver)

(defun a0-card-url (url git-tree-sha1)
  "Consent-to-load card for a direct-url install. No manifest exists, so the card
states the plain fact: author Lisp is about to load into the running image with
eval authority, pinned to the supplied git hash."
  (list :stage :a0
        :url url
        :git-tree-sha1 git-tree-sha1
        :text (format nil "Install from ~A. This loads author-provided Lisp code into the running image with eval authority, pinned to git ~A."
                      url git-tree-sha1)))

(defun a1-card-url (url meta)
  "Post-verification card for a direct-url install. Reports the verified content
identity and the trust level: integrity-only (unsigned) or signed by a trusted
key."
  (let ((trust (getf meta :trust))
        (fingerprint (getf meta :signed-by)))
    (list :stage :a1
          :url url
          :git-tree-sha1 (getf meta :git-tree-sha1)
          :transport-sha256 (getf meta :transport-sha256)
          :trust trust
          :signed-by fingerprint
          :text (format nil "Verified bytes match git ~A. ~A"
                        (getf meta :git-tree-sha1)
                        (if (eq trust :signed)
                            (format nil "Signed by trusted key ~A." fingerprint)
                            "Unsigned: integrity-pinned only.")))))

(defun build-url-pin (meta declared-id)
  "The snapshot-serializable install pin for a verified direct-url install. :id and
:dir-leaf are DECLARED-ID, the manifest-declared identity the artifact was placed
under, so the pin, the placed <declared-id>/ dir, and a later rediscovery all agree
and restore reconstructs the same placement. The url is provenance the restore
re-fetch re-verifies at the same trust level before any load; no pathname is stored."
  (list :id declared-id
        :source-kind :url
        :artifact-kind (getf meta :artifact-kind)
        :version (getf meta :version)
        :url (getf meta :url)
        :git-tree-sha1 (getf meta :git-tree-sha1)
        :transport-sha256 (getf meta :transport-sha256)
        :trust (getf meta :trust)
        :signed-by (getf meta :signed-by)
        :scope :user
        :dir-leaf declared-id
        :install-triple (host-triple)
        :native-libs '()
        :native-artifacts '()))

(defun commit-verified-install (meta artifact protocol on-phase
                                &key activate marshal)
  "Phase B core: under a bounded capability grant, transactionally stage and
trial-load the verified ARTIFACT WITHOUT activating it (stage-verified-artifact --
temp-stage, integrity re-verify, index, safe-replace into <declared-id>/), record
the declared-id pin, and -- only when ACTIVATE is supplied, a one-arg thunk of the
indexed entry that installs the manifest into the running protocol -- run it.
Returns (values :installed DECLARED-ID) or (values :rejected REASON). An integrity,
trial-load, or placement failure is a no-op on the live installed set: the filesystem
and the pin are untouched and nothing is activated.

Staging runs on the calling thread. The pin record (plus any ACTIVATE, whose native
load may dlopen) runs through MARSHAL when supplied -- a one-arg funcallable that runs
its thunk on the loop thread and blocks until it returns. On darwin that thread is the
initial thread, so the native load avoids the call-within-initial-thread timeout.
Without MARSHAL it runs inline, the only safe choice when the caller is already on the
loop thread. The grant is re-bound inside the thunk because dynamic bindings don't
cross threads."
  (let ((kli/ext:*call-subject*
          (kli/ext:make-subject
           :capabilities '(:manifest/install-remote :image/load-native))))
    (multiple-value-bind (entry declared)
        (handler-case
            (stage-verified-artifact artifact (getf meta :artifact-kind)
                                     (getf meta :git-tree-sha1) protocol)
          (error () (values nil :index-failed)))
      (cond
        ((null entry)
         (when on-phase
           (funcall on-phase (list :event :rejected :id (getf meta :id)
                                   :reason declared)))
         (values :rejected declared))
        (t
         (let ((subject kli/ext:*call-subject*)
               (pin (build-url-pin meta declared)))
           (flet ((commit ()
                    (let ((kli/ext:*call-subject* subject))
                      (when activate (funcall activate entry))
                      (record-remote-install-pin protocol pin))))
             (if marshal (funcall marshal #'commit) (commit))))
         (when on-phase (funcall on-phase (list :event :installed :id declared)))
         (values :installed declared))))))

(defun commit-remote-install (meta artifact protocol context on-phase
                              &key marshal)
  "Phase B, activating: transactionally stage and trial-load the verified ARTIFACT,
install the single manifest into the running protocol, and record the declared-id
pin. Returns (values :installed DECLARED-ID) or (values :rejected REASON). The
interactive/tool install path; the activation (plus the pin record) runs through
MARSHAL on the loop thread. See commit-verified-install."
  (commit-verified-install
   meta artifact protocol on-phase
   :marshal marshal
   :activate (lambda (entry) (install-user-extension protocol entry context))))

(defun commit-remote-install-no-activate (meta artifact protocol on-phase)
  "Phase B, non-activating: transactionally stage, trial-load, and pin the verified
ARTIFACT WITHOUT installing it into the running protocol -- the durable-install path
a headless install drives. The on-disk installed-set and its pin are updated;
nothing is activated into the running image. Returns (values :installed DECLARED-ID)
or (values :rejected REASON). See commit-verified-install."
  (commit-verified-install meta artifact protocol on-phase))

(defun install-remote-extension (pin-spec protocol context
                                 &key on-phase confirm-fn commit)
  "Two-phase direct-url install of the extension at PIN-SPEC's :url, pinned to its
:git-tree-sha1. Requires :manifest/install-remote. Phase A0 reports the
consent-to-load card; on grant it verifies the artifact (the git hash floor plus
the opt-in signature ceiling) WITHOUT loading and reports the A1 card; Phase B runs
only on a second grant: it places the verified artifact, activates the single
manifest under a bounded grant, records the pin, and journals. CONFIRM-FN is a
predicate of (stage card) consulted at A0 and A1, nil-safe so a headless caller
defaults to deny. ON-PHASE is a nil-safe reporter of (event plist). COMMIT, when
supplied, is a (meta artifact) function run for phase B in place of the default
activating commit -- the headless installer passes the non-activating commit so the
CLI path places durably without activating into the running image. Returns
(values STATE DETAIL) where STATE is :rejected, :cancelled, or :installed."
  (kli/ext:require-capability :manifest/install-remote)
  (let ((url (getf pin-spec :url))
        (git-tree-sha1 (getf pin-spec :git-tree-sha1)))
    (when (or (null url) (null git-tree-sha1))
      (when on-phase
        (funcall on-phase (list :event :rejected :reason :malformed-pin-spec)))
      (return-from install-remote-extension (values :rejected :malformed-pin-spec)))
    (let ((card-a0 (a0-card-url url git-tree-sha1)))
      (when on-phase (funcall on-phase (list :event :trust-card :card card-a0)))
      (unless (and confirm-fn (funcall confirm-fn :a0 card-a0))
        (when on-phase
          (funcall on-phase (list :event :cancelled :stage :a0)))
        (return-from install-remote-extension (values :cancelled :a0)))
      (multiple-value-bind (artifact meta)
          (direct-url-resolve-and-verify url git-tree-sha1)
        (when (null artifact)
          (when on-phase
            (funcall on-phase (list :event :rejected :reason meta)))
          (return-from install-remote-extension (values :rejected meta)))
        (let ((card-a1 (a1-card-url url meta)))
          (when on-phase (funcall on-phase (list :event :trust-card :card card-a1)))
          (unless (and confirm-fn (funcall confirm-fn :a1 card-a1))
            (when on-phase
              (funcall on-phase (list :event :cancelled :stage :a1)))
            (return-from install-remote-extension (values :cancelled :a1)))
          (if commit
              (funcall commit meta artifact)
              (commit-remote-install meta artifact protocol context on-phase)))))))

(defun a0-card-spawn (command arguments id)
  "Consent-to-spawn card for an isolated MCP server. States the plain fact: a
local subprocess is about to run, and kli will mediate every tool call it
serves."
  (list :stage :a0
        :command command
        :arguments arguments
        :id id
        :text (format nil "Spawn local subprocess ~A~{ ~A~} as isolated extension ~A. This runs an external MCP server process; kli mediates every tool call it serves."
                      command arguments id)))

(defun a1-card-spawn (id capability)
  "Authority card for an isolated MCP server. Reports the capability its tools
install under and that uninstall reverses the lift, before any tool is exposed."
  (list :stage :a1
        :id id
        :capability capability
        :text (format nil "Expose ~A's tools into the running image, each gated by capability ~A. kli mediates every call; uninstall retracts the extension and reaps the process."
                      id capability)))

(defun isolated-spawn-args (spec)
  "The optional lift keywords present in SPEC, as a plist to apply onto
lift-mcp-server. The id and capability are passed separately."
  (loop for key in '(:arguments :directory :environment :timeout
                     :client-name :client-version :tool-coordinates)
        for value = (getf spec key)
        when value append (list key value)))

(defun build-isolated-pin (id capability spec grant)
  "The snapshot-serializable install pin for a lifted isolated server: the
normalized ID as a string, the per-server CAPABILITY, SPEC's reconnection data,
and GRANT -- the bounded authority the install ran under -- as a serializable
datum, so restore re-spawns the same subprocess under no more authority than the
original install held. Every value rides the pin whitelist; directory is coerced
to a namestring, the grant to its datum."
  (let ((directory (getf spec :directory)))
    (list :id (string-downcase (symbol-name id))
          :source-kind :isolated-server
          :command (getf spec :command)
          :arguments (getf spec :arguments)
          :directory (and directory (namestring directory))
          :environment (getf spec :environment)
          :timeout (getf spec :timeout)
          :client-name (getf spec :client-name)
          :client-version (getf spec :client-version)
          :tool-coordinates (getf spec :tool-coordinates)
          :capability capability
          :install-grant (kli/ext:grant->datum grant)
          :scope :user)))

(defun install-isolated-extension (spec protocol context &key on-phase confirm-fn)
  "Two-phase spawn-and-lift of the MCP server SPEC describes (:command, plus
optional :arguments/:directory/:environment/:id/:capability/:timeout). Requires
:extension/spawn-process. A0 reports the consent-to-spawn card; on grant, A1
reports the authority the lifted tools install under. Phase B runs only on a
second grant: under a bounded grant scoped to this activation it spawns the
server and installs the manifest. CONFIRM-FN is a predicate of (stage card)
consulted at A0 and A1, nil-safe so a headless caller defaults to deny. ON-PHASE
is a nil-safe reporter of (event plist). Returns (values STATE DETAIL) where
STATE is :rejected, :cancelled, or :installed and DETAIL is the stage or, on
:installed, the install handle."
  (kli/ext:require-capability :extension/spawn-process)
  (let ((command (getf spec :command)))
    (when (null command)
      (when on-phase
        (funcall on-phase (list :event :rejected :reason :malformed-spec)))
      (return-from install-isolated-extension (values :rejected :malformed-spec)))
    (let* ((id (kli/runtime/isolated:lifted-server-id (getf spec :id)))
           (capability (kli/runtime/isolated:lifted-server-capability
                        id (getf spec :capability)))
           (card-a0 (a0-card-spawn command (getf spec :arguments) id)))
      (when on-phase (funcall on-phase (list :event :trust-card :card card-a0)))
      (unless (and confirm-fn (funcall confirm-fn :a0 card-a0))
        (when on-phase (funcall on-phase (list :event :cancelled :stage :a0)))
        (return-from install-isolated-extension (values :cancelled :a0)))
      (let ((card-a1 (a1-card-spawn id capability)))
        (when on-phase (funcall on-phase (list :event :trust-card :card card-a1)))
        (unless (and confirm-fn (funcall confirm-fn :a1 card-a1))
          (when on-phase (funcall on-phase (list :event :cancelled :stage :a1)))
          (return-from install-isolated-extension (values :cancelled :a1)))
        ;; The bounded grant is bound on the thread that runs install-manifest,
        ;; which spawns then installs synchronously; no marshal, so no
        ;; cross-thread re-capture is needed.
        (let ((kli/ext:*call-subject*
                (kli/ext:make-subject
                 :capabilities '(:manifest/install :extension/spawn-process))))
          (handler-case
              (let ((handle (kli/ext:install-manifest
                             (apply #'kli/runtime/isolated:lift-mcp-server command
                                    :id id :capability capability
                                    (isolated-spawn-args spec))
                             protocol context)))
                (record-remote-install-pin
                 protocol (build-isolated-pin
                           id capability spec
                           (kli/ext:subject-grant kli/ext:*call-subject*)))
                (when on-phase (funcall on-phase (list :event :installed :id id)))
                (values :installed handle))
            (error ()
              (when on-phase
                (funcall on-phase (list :event :rejected :reason :spawn-failed)))
              (values :rejected :spawn-failed))))))))
