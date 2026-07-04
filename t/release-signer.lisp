(in-package #:kli/tests)
(in-suite all)

(defun signer-random-bytes (n)
  (let ((a (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n a) (setf (aref a i) (random 256)))))

(defun signer-bundle-bytes (files)
  "FILES (an (RELPATH . OCTETS) alist) as a kli-dir-bundle-v1 envelope."
  (sb-ext:string-to-octets
   (with-output-to-string (s)
     (format s "{\"format\":\"~A\",\"files\":{" app::+dir-bundle-format+)
     (loop for (path . octets) in files for first = t then nil
           do (unless first (write-char #\, s))
              (format s "~S:~S" path (cl-base64:usb8-array-to-base64-string octets)))
     (format s "}}"))
   :external-format :utf-8))

(defun signer-cairn-files ()
  (flet ((octets (s) (sb-ext:string-to-octets s :external-format :utf-8)))
    (list (cons "cairn.asd" (octets "(defsystem \"cairn\" :serial t)"))
          (cons "version.sexp" (octets "\"0.1.0\""))
          (cons "src/package.lisp" (octets "(defpackage #:cairn (:use #:cl))")))))

(test release-signer-round-trip
  "verify(pub, m, sign(seed, m)) holds for a keypair the tool mints, over random
messages; a tampered message fails."
  (dotimes (trial 100)
    (multiple-value-bind (seed-hex pub-hex) (app::generate-ed25519-keypair)
      (let* ((msg (signer-random-bytes (random 512)))
             (priv (app::decode-ed25519-private-key seed-hex))
             (pub (app::decode-ed25519-public-key pub-hex))
             (sig (app::ed25519-sign-raw priv msg)))
        (is (app::ed25519-verify-raw pub msg sig)
            "the tool's own signature verifies under its published pubkey")
        (when (plusp (length msg))
          (let ((tampered (copy-seq msg)))
            (setf (aref tampered 0) (logxor 1 (aref tampered 0)))
            (is (null (app::ed25519-verify-raw pub tampered sig))
                "a flipped message does not verify")))))))

(test release-signer-bundle-through-live-verify
  "A signature the tool produces over a dir-bundle is accepted by the live install
ceiling; the pin gates signing, and an untrusted key refuses."
  (let* ((files (signer-cairn-files))
         (bundle (signer-bundle-bytes files))
         (pin (app::git-tree-sha1 files)))
    (multiple-value-bind (seed pub) (app::generate-ed25519-keypair)
      (let* ((sig (app::sign-bundle-bytes seed bundle pin))
             (url "https://kleisli-io.example/cairn.bundle")
             (sig-url (concatenate 'string url app::*signature-url-suffix*)))
        (signals app::artifact-verification-failed
          (app::sign-bundle-bytes seed bundle "00"))
        (let ((app::*registry-trust-roots* (list pub))
              (app::*remote-install-fetcher*
                (trust-fetcher url bundle sig-url sig)))
          (multiple-value-bind (payload meta)
              (app::direct-url-resolve-and-verify url pin)
            (is (not (null payload)) "the signed bundle resolves")
            (is (eq :signed (getf meta :trust)) "at the signature ceiling")
            (is (stringp (getf meta :signed-by)) "recording the key fingerprint")))
        (let ((app::*registry-trust-roots* (list (nth-value 1 (app::generate-ed25519-keypair))))
              (app::*remote-install-fetcher*
                (trust-fetcher url bundle sig-url sig)))
          (is (eq :signature-untrusted
                  (nth-value 1 (app::direct-url-resolve-and-verify url pin)))
              "a signature from an untrusted key refuses"))))))
