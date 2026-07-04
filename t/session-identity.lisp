(in-package #:kli/tests)
(in-suite all)

(test run-identity-eager-mint-distinct-and-stable
  "Every protocol eager-mints a distinct 32-hex run-identity, stable across reads."
  (let ((ids (loop repeat 200 collect (ext:protocol-run-identity
                                       (ext:make-extension-protocol))))
        (p (ext:make-extension-protocol)))
    (is (= 200 (length (remove-duplicates ids :test #'string=)))
        "200 protocols mint 200 distinct identities")
    (is (every (lambda (s) (= 32 (length s))) ids)
        "each identity is 32 chars")
    (is (every (lambda (s) (every (lambda (c) (digit-char-p c 16)) s)) ids)
        "each identity is hexadecimal")
    (is (string= (ext:protocol-run-identity p) (ext:protocol-run-identity p))
        "a protocol's run-identity is stable across reads")))

(test run-identity-eager-mint-is-race-free
  "Concurrent construction mints disjoint run-identities: minting at construction
makes the lazy-initialization race structurally impossible."
  (let ((ids (make-array 64))
        (threads '()))
    (dotimes (i 64)
      (let ((i i))
        (push (sb-thread:make-thread
               (lambda ()
                 (setf (aref ids i)
                       (ext:protocol-run-identity (ext:make-extension-protocol)))))
              threads)))
    (mapc #'sb-thread:join-thread threads)
    (is (= 64 (length (remove-duplicates (coerce ids 'list) :test #'string=)))
        "64 concurrently-built protocols mint 64 distinct identities")))

(test run-identity-is-exempt-from-snapshot-capture
  "snapshot-protocol-storage never captures the run-identity, so a restore cannot
resurrect it; ordinary keys still capture."
  (let ((p (ext:make-extension-protocol)))
    (setf (ext:protocol-storage p :ordinary) 42)
    (let* ((captured (snapshot::snapshot-protocol-storage p))
           (keys (mapcar #'first captured)))
      (is (ext:protocol-run-identity p) "the protocol carries a run-identity")
      (is (notany #'ext:snapshot-exempt-storage-key-p keys)
          "no captured key is snapshot-exempt")
      (is (and (member :ordinary keys) t) "an ordinary key is captured"))))

(defvar *session-uuid-test-counter* 0)

(defun session-uuid-temp-root ()
  (merge-pathnames (format nil "kli-session-uuid-test-~D-~D/"
                           (get-universal-time)
                           (incf *session-uuid-test-counter*))
                   #p"/tmp/"))

(test session-uuid-minted-distinct-and-stable
  "Every session mints a distinct 32-hex uuid, stable across reads."
  (let ((uuids (loop repeat 200 collect (sess:session-uuid (sess:make-session))))
        (s (sess:make-session)))
    (is (= 200 (length (remove-duplicates uuids :test #'string=)))
        "200 sessions mint 200 distinct uuids")
    (is (every (lambda (u) (and (= 32 (length u))
                                (every (lambda (c) (digit-char-p c 16)) u)))
               uuids)
        "each uuid is 32 hex chars")
    (is (string= (sess:session-uuid s) (sess:session-uuid s))
        "a session's uuid is stable across reads")))

(test session-uuid-is-resume-stable-across-the-session-file
  "The uuid persists in the session-file header and is reinstated verbatim on
load -- not re-minted -- so a resumed session keeps its provider cache key."
  (let* ((root (session-uuid-temp-root))
         (context (kli:make-kernel-host))
         (store (sess:make-file-session-store root))
         (session (sess:create-session store context :id :uuid-target))
         (uuid (sess:session-uuid session)))
    (sess:append-session-entry
     store session
     (sess:make-message-entry (sess:make-user-message "hi" :id :m1) :id :e1)
     context)
    (let* ((path (sess:session-file-path store :uuid-target))
           (fresh (sess:make-file-session-store root))
           (loaded (sess:load-session-file fresh path (kli:make-kernel-host))))
      (is (stringp uuid) "a created session has a uuid")
      (is (string= uuid (sess:session-uuid loaded))
          "the loaded uuid matches verbatim, not a fresh mint")
      (ignore-errors (delete-file path)))))

(test (run-identity-reconstruct-re-mints-not-resurrects :fixture restore-authority)
  "A reconstructed protocol mints a fresh run-identity rather than resurrecting the
captured one; ordinary storage still rehydrates."
  (let* ((source-context (kli:make-kernel-host))
         (source (switch-to-extension-protocol source-context))
         (source-identity (ext:protocol-run-identity source)))
    (setf (ext:protocol-storage source :sentinel) 7)
    (let ((snapshot (snapshot:snapshot-context source-context))
          (target-context (kli:make-kernel-host)))
      (let ((restored (snapshot:restore-active-protocol target-context snapshot)))
        (is (not (string= (ext:protocol-run-identity restored) source-identity))
            "reconstruct re-mints rather than resurrecting the captured identity")
        (is (= 32 (length (ext:protocol-run-identity restored)))
            "the re-minted identity is well-formed")
        (is (eql 7 (ext:protocol-storage restored :sentinel))
            "ordinary storage rehydrates across the restore")))))
