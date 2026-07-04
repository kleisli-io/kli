(in-package #:kli/tests)
(in-suite all)

(defun trust-roots-settings (roots)
  "A settings table whose trustRoots key holds ROOTS in the vector shape a JSON
array parses to, so it drives main exactly as a real settings.json layer would."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "trustRoots" table) (coerce roots 'vector))
    table))

(test parse-trust-roots-reads-a-hex-array
  "A trustRoots array resolves to its non-empty hex strings; empty and non-string
entries are dropped so one stray entry never disables the roots that remain."
  (is (equal '("aa" "bb") (app::parse-trust-roots (vector "aa" "bb")))
      "a clean array yields its strings in order")
  (is (equal '("aa") (app::parse-trust-roots (vector "aa" "" 7)))
      "empty and non-string entries are filtered out")
  (is (null (app::parse-trust-roots (vector)))
      "an empty array is the integrity floor"))

(test parse-trust-roots-floors-on-absent-or-malformed
  "An absent key and a present non-array value both yield no roots -- the floor."
  (is (null (app::parse-trust-roots nil))
      "the key absent is the floor")
  (is (null (app::parse-trust-roots "aa"))
      "a bare string is not an array and floors")
  (is (null (app::parse-trust-roots (make-hash-table :test #'equal)))
      "an object is malformed and floors"))

(test main-binds-trust-roots-from-settings-across-entry-points
  "Booting through main -- the one boot the headless install verb, the MCP tool, and
an interactive session share -- resolves trustRoots from the merged settings and binds
*registry-trust-roots*. A configured root is then enforced at the single verify
chokepoint every entry point funnels through; an unconfigured image stays on the
integrity floor; and a stale binding is cleared to the floor when settings carry no
key, so the binding tracks settings unconditionally."
  (let* ((artifact (trust-artifact-bytes "cfg"))
         (url "https://ext.example/cfg.lisp")
         (good (app::git-blob-sha1 artifact)))
    (multiple-value-bind (trust-hex signature) (trust-sign artifact)
      (let ((sig-url (concatenate 'string url app::*signature-url-suffix*)))
        (let ((app::*registry-trust-roots* '()))
          (app:main :profile :headless
                    :settings (trust-roots-settings (list trust-hex)))
          (is (equal (list trust-hex) app::*registry-trust-roots*)
              "main resolves trustRoots from settings and binds the global")
          (let ((app::*remote-install-fetcher*
                  (trust-fetcher url artifact sig-url signature)))
            (multiple-value-bind (bytes meta)
                (app::direct-url-resolve-and-verify url good)
              (is (equalp artifact bytes)
                  "a trusted signature installs through the wired root")
              (is (eq :signed (getf meta :trust))
                  "and the chokepoint marks it signed")))
          (let ((app::*remote-install-fetcher* (trust-fetcher url artifact)))
            (is (eq :signature-missing
                    (nth-value 1 (app::direct-url-resolve-and-verify url good)))
                "an unsigned artifact refuses once a root is configured")))
        (let ((app::*registry-trust-roots* (list "deadbeef")))
          (app:main :profile :headless :settings (make-hash-table :test #'equal))
          (is (null app::*registry-trust-roots*)
              "settings with no trustRoots key clears the binding to the floor")
          (let ((app::*remote-install-fetcher* (trust-fetcher url artifact)))
            (multiple-value-bind (bytes meta)
                (app::direct-url-resolve-and-verify url good)
              (is (equalp artifact bytes) "the floor installs on integrity alone")
              (is (eq :hash (getf meta :trust))
                  "and marks the install integrity-only"))))))))
