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

;;;; The unit-root resource manifest. A source-distributed directory unit has
;;;; no baked registration forms, so its resources.sexp maps each declared key
;;;; to a unit-relative directory and the loader registers key -> placed tree.

(defun call-with-temp-unit-dir (thunk)
  "Create a temp unit root, call THUNK with it, clean up."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames
               (format nil "kli-unit-manifest-test-~A/" (random (expt 2 32)))
               (uiop:default-temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect (funcall thunk dir)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(test unit-resource-manifest-registers-roots
  "A manifest entry (KEY . RELATIVE-DIR) makes resource-root resolve KEY
   against the unit's placed tree."
  (call-with-temp-unit-dir
   (lambda (dir)
     (let ((prompts (merge-pathnames "src/prompts/" dir)))
       (ensure-directories-exist prompts)
       (with-open-file (out (merge-pathnames "resources.sexp" dir)
                            :direction :output)
         (write-string "((\"kli/manifest-probe/prompts\" . \"src/prompts/\"))"
                       out))
       (with-env-var ("KLI_DATA_DIR" nil)
         (app::register-unit-resource-roots dir)
         (is (equal (truename prompts)
                    (truename (buildlisp/resources:resource-root
                               "kli/manifest-probe/prompts")))))))))

(test unit-resource-manifest-degrades-on-malformed-entries
  "Entries that are not string pairs naming a relative directory are skipped;
   nothing signals and the keys stay unresolved."
  (call-with-temp-unit-dir
   (lambda (dir)
     (with-open-file (out (merge-pathnames "resources.sexp" dir)
                          :direction :output)
       (write-string
        "((:not-a-string . 42) (\"kli/manifest-probe/skipped\" . :kw) \"loose\"
          (\"kli/manifest-probe/absolute\" . \"/etc/\"))"
        out))
     (with-env-var ("KLI_DATA_DIR" nil)
       (finishes (app::register-unit-resource-roots dir))
       (signals error
         (buildlisp/resources:resource-root "kli/manifest-probe/skipped"))
       (signals error
         (buildlisp/resources:resource-root "kli/manifest-probe/absolute"))))))

(test unit-resource-manifest-absent-is-a-no-op
  (call-with-temp-unit-dir
   (lambda (dir)
     (finishes (app::register-unit-resource-roots dir)))))
