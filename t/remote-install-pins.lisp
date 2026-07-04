(in-package #:kli/tests)
(in-suite all)

(defclass pin-clos-probe () ())

(defun demo-entry (&key (id :demo) (version "1.0.0") (boom nil))
  (app::make-user-extension-entry
   :id id
   :manifest (if boom
                 (lambda () (error "boom"))
                 (lambda () (ext:make-extension :id id)))
   :metadata (list :version version)))

(test pin-roundtrip
  "A multi-pin install-set, including a non-empty triple-keyed
:native-artifacts pin, serializes and deserializes through the snapshot
serializer with no pin-specific serializer code."
  (let ((pins (make-hash-table :test #'equal)))
    (setf (gethash "blessed" pins)
          (list :id "blessed" :source-kind :registry :version "1.2.0"
                :git-tree-sha1 "abc" :transport-sha256 "def" :scope :user
                :dir-leaf "blessed" :install-triple "linux-x86_64-glibc"
                :native-libs '(:sqlite3) :native-artifacts '()))
    (setf (gethash "novel" pins)
          (list :id "novel" :source-kind :registry :version "0.1.0" :scope :user
                :dir-leaf "novel" :install-triple "linux-x86_64-glibc"
                :native-libs '()
                :native-artifacts
                (list "linux-x86_64-glibc"
                      (list :url "https://e/x.tar" :sha256 "h1" :git-tree-sha1 "g1")
                      "darwin-aarch64"
                      (list :url "https://e/y.tar" :sha256 "h2"))))
    (let ((back (snapshot::deserialize-snapshot-value
                 (snapshot::serialize-snapshot-value pins))))
      (is (hash-table-p back))
      (is (eq 'equal (hash-table-test back)))
      (is (= 2 (hash-table-count back)))
      (is (equal (gethash "blessed" pins) (gethash "blessed" back)))
      (is (equal (gethash "novel" pins) (gethash "novel" back)))
      (is (equal (getf (gethash "novel" pins) :native-artifacts)
                 (getf (gethash "novel" back) :native-artifacts))
          "the non-empty native-artifacts map survives byte-faithfully"))))

(test pin-store-capture-rides-protocol-storage
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (app:record-remote-install-pin
     protocol (list :id "demo" :source-kind :registry :version "1.0.0"))
    (multiple-value-bind (captured skipped)
        (snapshot::snapshot-protocol-storage protocol)
      (is (find app:+remote-install-pins-key+ captured
                :key (lambda (pair)
                       (snapshot::deserialize-snapshot-value (first pair))))
          "the install-set is captured")
      (is (not (member (princ-to-string app:+remote-install-pins-key+) skipped
                       :test #'string=))
          "and not skipped"))))

(test pin-store-skips-whole-entry-on-unserializable-field
  "One unserializable field drops the entire install-set to skipped storage,
which is why validate-pin gates at construction rather than at capture."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (setf (gethash "bad" (app:remote-install-pins protocol))
          (list :id "bad" :version (make-instance 'pin-clos-probe)))
    (multiple-value-bind (captured skipped)
        (snapshot::snapshot-protocol-storage protocol)
      (declare (ignore captured))
      (is (member (princ-to-string app:+remote-install-pins-key+) skipped
                  :test #'string=)))))

(test validate-pin-accepts-good-rejects-non-atom-fields
  (is (equal (list :id "ok" :source-kind :registry)
             (app:validate-pin (list :id "ok" :source-kind :registry))))
  (is (app:validate-pin (list :id "n" :native-artifacts
                              (list "linux-x86_64-glibc"
                                    (list :url "u" :sha256 "h")))))
  (signals app:invalid-pin (app:validate-pin (list :id "x" :bad #p"/tmp/x")))
  (signals app:invalid-pin
    (app:validate-pin (list :id "x" :bad (make-instance 'pin-clos-probe))))
  (signals app:invalid-pin (app:validate-pin (list :id "x" :bad #'identity)))
  (signals app:invalid-pin (app:validate-pin (list :id "x" :bad 3.14)))
  (signals app:invalid-pin (app:validate-pin (list :id "x" :bad 'plain-symbol)))
  (signals app:invalid-pin (app:validate-pin (list :id :not-a-string)))
  (signals app:invalid-pin (app:validate-pin (list "id" "x")))
  (signals app:invalid-pin (app:validate-pin (cons :id (cons "x" :tail)))))

(test record-remote-install-pin-rejects-non-atom-field-at-install
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (signals app:invalid-pin
      (app:record-remote-install-pin
       protocol (list :id "x" :native-libs #p"/tmp/x")))
    (is (zerop (hash-table-count (app:remote-install-pins protocol)))
        "a rejected pin never reaches the store")))

(test host-triple-is-a-nonempty-os-arch-tag
  (let ((triple (app:host-triple)))
    (is (stringp triple))
    (is (plusp (length triple)))
    (is (find #\- triple))))

(test (restore-no-double-root :fixture restore-authority)
  "A snapshot with one runtime pin restores with exactly one root-activation:
replay re-installs the pin (a transient root) and the wholesale storage
overwrite is authoritative, so the root is not double-counted."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (entry (demo-entry)))
    (app::install-user-extension protocol entry context)
    (app:record-remote-install-pin
     protocol (list :id "demo" :source-kind :registry :version "1.0.0"))
    (is (equal '(:demo) (ext:protocol-root-activations protocol)))
    (let ((snapshot (snapshot:snapshot-context context))
          (target-context (kli:make-kernel-host)))
      (let ((target (switch-to-extension-protocol target-context)))
        (setf (gethash :demo (app::available-extensions target)) (demo-entry))
        (snapshot::apply-snapshot target target-context snapshot)
        (is (equal '(:demo) (ext:protocol-root-activations target)))
        (is (= 1 (length (ext:protocol-root-activations target))))))))

(test restore-rolls-back-already-activated-on-mid-replay-error
  "A pin that errors mid-replay rolls back the pins activated before it, via
the transient root-activations list -- proving the step-0 appends are needed."
  (let* ((context (kli:make-kernel-host))
         (registry (kli:context-registry context))
         (pins (make-hash-table :test #'equal)))
    (setf (gethash "a" pins) (list :id "a" :source-kind :registry))
    (setf (gethash "b" pins) (list :id "b" :source-kind :registry))
    (let ((snapshot
            (list :format-version 1
                  :kind :extension-protocol
                  :active-protocol :rollback-protocol
                  :extensions nil
                  :unrestorable-extensions nil
                  :storage (list (list (snapshot::serialize-snapshot-value
                                        app:+remote-install-pins-key+)
                                       (snapshot::serialize-snapshot-value pins)))
                  :objects nil))
          (app:*remote-install-resolver*
            (lambda (pin proto ctx)
              (declare (ignore proto ctx))
              (cond ((string= (getf pin :id) "a") (demo-entry :id :a))
                    ((string= (getf pin :id) "b") (demo-entry :id :b :boom t))))))
      (handler-case (snapshot:restore-active-protocol context snapshot)
        (error () nil))
      (is (null (kli:find-live-object registry :a))
          "the extension activated before the failing pin is retracted")
      (is (null (kli:find-live-object registry :rollback-protocol))
          "and the half-built protocol is removed"))))

(test (reinstall-remote-pin-records-version-gap-without-crashing :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (setf (gethash :demo (app::available-extensions protocol))
          (demo-entry :version "2.0.0"))
    (is (eq :reinstalled
            (app:reinstall-remote-pin
             (list :id "demo" :source-kind :registry :version "1.0.0")
             protocol context)))
    (let ((gap (first (app:restore-version-gaps protocol))))
      (is (not (null gap)))
      (is (eq :version-mismatch (getf gap :reason)))
      (is (equal "1.0.0" (getf gap :pinned)))
      (is (equal "2.0.0" (getf gap :available))))))
