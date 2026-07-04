(in-package #:kli/tests)
(in-suite all)

;;;; Compiled-in version + build-id, relocation-robust (no resource lookup).

(test version-semver-baked
  "+kli-version+ is the SemVer string baked from version.sexp."
  (is (stringp app:+kli-version+))
  (is (every (lambda (c) (or (digit-char-p c) (char= c #\.))) app:+kli-version+)))

(test version-build-id-nonempty-and-memoized
  "build-id is a non-empty string and returns the same value on re-read."
  (let ((id (app:build-id)))
    (is (stringp id))
    (is (plusp (length id)))
    (is (equal id (app:build-id)))))

(test version-current-version-carries-semver-and-build-id
  "current-version reads <SemVer> (<build-id>)."
  (let ((v (app:current-version)))
    (is (search app:+kli-version+ v))
    (is (search (app:build-id) v))))
