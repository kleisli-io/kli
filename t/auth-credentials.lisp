(in-package #:kli/tests)

(in-suite all)

(defvar *auth-test-seq* 0)

(defmacro with-temp-credentials ((path-var) &body body)
  `(let* ((,path-var (merge-pathnames
                      (format nil "kli-auth-test-~D/credentials.json"
                              (incf *auth-test-seq*))
                      (uiop:temporary-directory)))
          (auth:*credentials-path* ,path-var))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree
        (uiop:pathname-directory-pathname ,path-var)
        :validate (constantly t)
        :if-does-not-exist :ignore))))

(defun auth-only-context ()
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        auth:*auth-extension-manifest*
                        oauth:*oauth-extension-manifest*)
    (values context protocol)))

(test (static-credential-resolves-and-redacts-inspection :fixture interactive-authority)
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (let ((store (credential-store context)))
      (auth:register-credential-reference
       store
       (auth:make-static-credential-reference "static-provider" "sk-secret-123")
       context)
      (is (auth:credential-available-p store "static-provider"))
      (let ((credential (auth:resolve-credential store "static-provider" context)))
        (is (string= "sk-secret-123"
                     (auth:resolved-credential-value credential)))
        (is (not (search "sk-secret-123"
                         (prin1-to-string (auth:inspect-auth-store store))
                         :test #'char=)))))))

(test (oauth-credential-resolves-when-fresh-and-refreshes-on-expiry :fixture interactive-authority)
  "Expired access still reports available because a refresh token is present.
Resolving then drives a refresh against the stubbed token endpoint."
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (let ((store (credential-store context)))
      (auth:register-credential-reference
       store
       (auth:make-oauth-credential-reference "oauth-fresh"
                                             :access "access-token"
                                             :refresh "refresh-token"
                                             :expires (+ (get-universal-time) 3600))
       context)
      (is (auth:credential-available-p store "oauth-fresh"))
      (is (string= "access-token"
                   (auth:resolved-credential-value
                    (auth:resolve-credential store "oauth-fresh" context))))
      (with-temp-credentials (path)
        (auth:register-credential-reference
         store
         (auth:make-oauth-credential-reference "oauth-expired"
                                               :access "stale-token"
                                               :refresh "refresh-token"
                                               :expires (- (get-universal-time) 1)
                                               :store-path path)
         context)
        (is (auth:credential-available-p store "oauth-expired"))
        (let ((oauth:*token-http*
                (lambda (grant-type params)
                  (declare (ignore grant-type params))
                  (values "{\"access_token\":\"fresh-access\",\"refresh_token\":\"fresh-refresh\",\"expires_in\":3600}"
                          200))))
          (is (string= "fresh-access"
                       (auth:resolved-credential-value
                        (auth:resolve-credential store "oauth-expired" context)))))))))

(test credentials-file-round-trips-and-is-0600
  (with-temp-credentials (path)
    (auth:write-credential-record "openai"
                                  (auth:static-credential-record "sk-file-key")
                                  path)
    (let ((record (auth:read-credential-record "openai" path)))
      (is (eq :static (auth:credential-record-kind record)))
      (is (string= "sk-file-key" (gethash "key" record))))
    (auth:write-credential-record "codex"
                                  (auth:oauth-credential-record
                                   :access "a" :refresh "r"
                                   :expires 123 :account-id "acct")
                                  path)
    (let ((record (auth:read-credential-record "codex" path)))
      (is (eq :oauth (auth:credential-record-kind record)))
      (is (string= "r" (gethash "refresh" record)))
      (is (eql 123 (gethash "expires" record))))
    (is (= #o600
           (logand (sb-posix:stat-mode (sb-posix:stat (namestring path)))
                   #o777)))))

(test save-credentials-failure-leaves-existing-file-intact
  "A failure mid-write must not truncate stored credentials: the write goes to
a temp file that only renames over the original on success. Infinity is
unrepresentable in JSON, so stringifying it fails the write partway through."
  (with-temp-credentials (path)
    (auth:write-credential-record "openai"
                                  (auth:static-credential-record "sk-original")
                                  path)
    (let ((bad (make-hash-table :test #'equal)))
      (setf (gethash "x" bad) sb-ext:double-float-positive-infinity)
      (signals error (auth:save-credentials bad path)))
    (let ((record (auth:read-credential-record "openai" path)))
      (is (string= "sk-original" (gethash "key" record))))
    (is (= #o600
           (logand (sb-posix:stat-mode (sb-posix:stat (namestring path)))
                   #o777)))))

(test (credential-reference-from-record-reconstructs-references :fixture interactive-authority)
  (let ((static-ref (auth:credential-reference-from-record
                     "p" (auth:static-credential-record "sk-x")))
        (oauth-ref (auth:credential-reference-from-record
                    "q" (auth:oauth-credential-record
                         :access "ax" :refresh "rx"
                         :expires (+ (get-universal-time) 60)
                         :account-id "acct"))))
    (is (typep static-ref 'auth:static-credential-reference))
    (is (string= "sk-x" (auth:credential-reference-value static-ref)))
    (is (typep oauth-ref 'auth:oauth-credential-reference))
    (is (string= "acct" (auth:oauth-credential-reference-account-id oauth-ref)))
    (is (string= "ax" (auth:credential-reference-value oauth-ref)))))

(test write-credential-record-serializes-concurrent-writers
  "Threads racing the load-modify-save must each survive: the write mutex
serializes the read-modify-write so no record is clobbered by a stale reload."
  (with-temp-credentials (path)
    (auth:write-credential-record "seed" (auth:static-credential-record "s") path)
    (let* ((n 24)
           (threads
             (loop for i from 0 below n
                   collect (let ((id (format nil "p~D" i)))
                             (sb-thread:make-thread
                              (lambda ()
                                (auth:write-credential-record
                                 id (auth:static-credential-record id) path)))))))
      (mapc #'sb-thread:join-thread threads)
      (let ((table (auth:load-credentials path)))
        (is (= (1+ n) (hash-table-count table))))
      (loop for i from 0 below n
            do (is (auth:read-credential-record (format nil "p~D" i) path))))))

(test (logout-drains-reference-and-blocks-resolve :fixture interactive-authority)
  "Logout removes the live in-memory reference, not just the on-disk record, so
the provider stops resolving without a restart."
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (let ((store (credential-store context)))
        (auth:store-oauth-credential store "codex" context
                                     :access "AT" :refresh "RT"
                                     :expires (+ (get-universal-time) 3600)
                                     :account-id "acct" :path path)
        (is (auth:credential-available-p store "codex"))
        (auth:forget-credential store "codex" context path)
        (is (null (auth:find-credential-references store "codex")))
        (is (null (auth:read-credential-record "codex" path)))
        (signals error (auth:resolve-credential store "codex" context))))))

(test (auth-extension-retract-drains-registrations :fixture interactive-authority)
  "Deactivating the auth extension drops the in-memory references and providers
its /auth commands accumulated, so a rolled-back auth subsystem leaves no orphan
live-objects in the registry."
  (with-temp-credentials (path)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context))
           (extension (install-extension context auth:*auth-extension-manifest*))
           (store (credential-store context)))
      (auth:set-static-credential store "static-provider" "sk-secret" context path)
      (is (auth:credential-available-p store "static-provider"))
      (is (plusp (hash-table-count (auth:auth-store-references store))))
      (is (plusp (hash-table-count (auth:auth-store-providers store))))
      (let ((reference (first (auth:find-credential-references store
                                                               "static-provider"))))
        (is (kli:find-live-object (kli:context-registry context)
                                  (kli:object-id reference)))
        (ext:deactivate-extension protocol extension context)
        (is (null (kli:find-live-object (kli:context-registry context)
                                        (kli:object-id reference)))
            "the reference live-object is gone after retract")
        (is (zerop (hash-table-count (auth:auth-store-references store)))
            "the references bucket drains")
        (is (zerop (hash-table-count (auth:auth-store-providers store)))
            "the providers bucket drains")))))

(test (auth-key-command-registers-resolvable-static-reference :fixture interactive-authority)
  (multiple-value-bind (context protocol) (model-command-test-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (let ((store (credential-store context))
            (result (invoke-test-command context :auth
                                         '(:words ("key" "openai" "sk-cmd-key")))))
        (is (not (commands:command-result-error-p result)))
        (is (null (search "sk-cmd-key" (command-result-text context result))))
        (is (auth:credential-available-p store "openai"))
        (is (string= "sk-cmd-key"
                     (auth:resolved-credential-value
                      (auth:resolve-credential store "openai" context))))))))
