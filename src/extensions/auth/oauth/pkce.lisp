(in-package #:kli/auth/oauth)

(defun base64url (bytes)
  "RFC 4648 §5 base64url of BYTES (no padding)."
  (let ((s (cl-base64:usb8-array-to-base64-string
            (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
    (with-output-to-string (out)
      (loop for ch across s do
        (case ch
          (#\+ (write-char #\- out))
          (#\/ (write-char #\_ out))
          (#\= nil)
          (t (write-char ch out)))))))

(defun sha256 (bytes)
  (let ((d (ironclad:make-digest :sha256)))
    (ironclad:update-digest d bytes)
    (ironclad:produce-digest d)))

(defun secure-random-bytes (n)
  (ironclad:random-data n (ironclad:make-prng :os)))

(defun pkce-verifier ()
  (base64url (secure-random-bytes 32)))

(defun pkce-challenge (verifier)
  (base64url (sha256 (ironclad:ascii-string-to-byte-array verifier))))

(defun oauth-state ()
  (ironclad:byte-array-to-hex-string (secure-random-bytes 16)))
