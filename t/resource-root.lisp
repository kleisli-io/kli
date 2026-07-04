(in-package #:kli/tests)
(in-suite all)

;;;; The KLI_DATA_DIR per-key override on buildlisp/resources:resource-root.
;;;; A relocated binary whose baked /nix/store paths are gone resolves
;;;; resources under $KLI_DATA_DIR/<key>/ instead.

(defun call-with-temp-data-dir (key thunk)
  "Create a temp <root> holding <root>/<key>/, call THUNK with both, clean up."
  (let* ((suffix (format nil "kli-resource-root-test-~A/" (random (expt 2 32))))
         (root (uiop:ensure-directory-pathname
                (merge-pathnames suffix (uiop:default-temporary-directory))))
         (key-dir (uiop:ensure-directory-pathname
                   (merge-pathnames (concatenate 'string key "/") root))))
    (ensure-directories-exist key-dir)
    (unwind-protect (funcall thunk root key-dir)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(test resource-root-env-unset-uses-baked
  "KLI_DATA_DIR unset resolves the baked store root, which exists."
  (with-env-var ("KLI_DATA_DIR" nil)
    (is (probe-file (buildlisp/resources:resource-root "kli/skills")))))

(test resource-root-env-override-wins
  "KLI_DATA_DIR/<key> takes precedence when the per-key dir exists."
  (call-with-temp-data-dir "kli/skills"
    (lambda (root key-dir)
      (with-env-var ("KLI_DATA_DIR" (namestring root))
        (is (equal (truename key-dir)
                   (truename (buildlisp/resources:resource-root "kli/skills"))))))))

(test resource-root-env-set-missing-key-falls-back
  "KLI_DATA_DIR set but the per-key dir absent falls back to the baked root."
  (call-with-temp-data-dir "kli/absent"
    (lambda (root key-dir)
      (declare (ignore key-dir))
      (with-env-var ("KLI_DATA_DIR" (namestring root))
        (let ((resolved (buildlisp/resources:resource-root "kli/skills")))
          (is (probe-file resolved))
          (is (not (search (namestring root) (namestring resolved)))))))))
