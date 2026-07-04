(in-package #:kli/app)

;;; Mint an ed25519 keypair and sign a kli-dir-bundle, over the same primitives
;;; the install-time verifier uses so verify accepts a signature by construction.
;;; A separate binary from the shipped kli: users verify, they never sign.

(defun ed25519-public-hex (private-key)
  (ironclad:byte-array-to-hex-string (ironclad:ed25519-key-y private-key)))

(defun generate-ed25519-keypair ()
  "Fresh keypair as (values SEED-HEX PUB-HEX): 32-byte seed, 32-byte y trust-root."
  (multiple-value-bind (private public) (ironclad:generate-key-pair :ed25519)
    (declare (ignore public))
    (values (ironclad:byte-array-to-hex-string (ironclad:ed25519-key-x private))
            (ed25519-public-hex private))))

(defun sign-bundle-bytes (seed-hex bundle-bytes &optional expected-pin)
  "Detached signature over the raw BUNDLE-BYTES. With EXPECTED-PIN, the bundle's
git-tree-sha1 must match first (else artifact-verification-failed), so a drifted
artifact is never signed. Signs the served bytes, not a re-serialization."
  (let ((private (decode-ed25519-private-key seed-hex)))
    (unless private
      (error "release-signer: malformed ed25519 seed (expected 64 hex chars)."))
    (when expected-pin
      (verify-tree (parse-dir-bundle bundle-bytes) expected-pin))
    (ed25519-sign-raw private bundle-bytes)))

(defun signer-arg (args flag)
  (second (member flag args :test #'string=)))

(defun write-hex-line (path hex mode)
  (if (string= path "-")
      (format t "~A~%" hex)
      (progn
        (with-open-file (out path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
          (format out "~A~%" hex))
        (sb-posix:chmod path mode))))

(defun read-seed-hex (spec)
  "SPEC is literal hex, or @FILE to read it from a file (LoadCredential's shape)."
  (if (and (plusp (length spec)) (char= (char spec 0) #\@))
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (uiop:read-file-string (subseq spec 1)))
      spec))

(defun release-signer-genkey (args)
  (multiple-value-bind (seed-hex pub-hex) (generate-ed25519-keypair)
    (let ((seed-out (signer-arg args "--seed-out"))
          (pub-out (signer-arg args "--pub-out")))
      (if seed-out
          (write-hex-line seed-out seed-hex #o600)
          (format t "seed: ~A~%" seed-hex))
      (if pub-out
          (write-hex-line pub-out pub-hex #o644)
          (format t "pub:  ~A~%" pub-hex))
      (finish-output)
      (sb-posix:exit 0))))

(defun release-signer-sign-bundle (args)
  (let ((in (signer-arg args "--in"))
        (seed-spec (signer-arg args "--seed"))
        (pin (signer-arg args "--pin"))
        (out (signer-arg args "--out")))
    (unless (and in seed-spec)
      (format *error-output* "release-signer sign-bundle: --in and --seed are required.~%")
      (sb-posix:exit 2))
    (let* ((bundle (read-file-octets in))
           (sig (sign-bundle-bytes (read-seed-hex seed-spec) bundle pin))
           (sig-path (or out (concatenate 'string in *signature-url-suffix*))))
      (write-octets-file sig-path sig)
      (format t "~&signed ~A -> ~A (~D bytes)~%" in sig-path (length sig))
      (finish-output)
      (sb-posix:exit 0))))

(defun release-signer-usage ()
  (format *error-output*
          "usage: release-signer genkey [--seed-out PATH] [--pub-out PATH]~%       release-signer sign-bundle --in BUNDLE --seed HEX|@FILE [--pin SHA] [--out SIG]~%"))

(defun release-signer-main ()
  (handler-case
      (let ((args (uiop:command-line-arguments)))
        (cond
          ((and args (string= (first args) "genkey"))
           (release-signer-genkey (rest args)))
          ((and args (string= (first args) "sign-bundle"))
           (release-signer-sign-bundle (rest args)))
          (t (release-signer-usage) (sb-posix:exit 2))))
    (error (condition)
      (format *error-output* "release-signer: ~A~%" condition)
      (finish-output *error-output*)
      (sb-posix:exit 1))))
