(in-package #:kli/tests)
(in-suite all)

;;;; Pure unit tests for the relocation seam path and filter logic. These run in the
;;;; library-loaded suite (no dump), so they cover only the dump-independent
;;;; functions. The end-to-end relocated-boot check runs the program derivation.

(test relocation-directoryless-filter
  "directoryless-namestring-p keeps bare SONAMEs and rejects pathful entries."
  (is (reloc:directoryless-namestring-p "libcrypto.so.3"))
  (is (reloc:directoryless-namestring-p "libssl.so.3"))
  (is (reloc:directoryless-namestring-p "libsqlite3.so.0"))
  (is (not (reloc:directoryless-namestring-p "/nix/store/abc-openssl/lib/libcrypto.so.3")))
  (is (not (reloc:directoryless-namestring-p "lib/libssl.so.3")))
  (is (not (reloc:directoryless-namestring-p "./libcrypto.so.3")))
  (is (not (reloc:directoryless-namestring-p ""))))

(defun call-with-temp-lib-bundle (soname thunk)
  "Create a temp root holding lib/<soname> and a sibling share/kli, so that
   KLI_DATA_DIR=<root>/share/kli resolves the lib two levels up at <root>/lib.
   Cleans up afterwards."
  (let* ((suffix (format nil "kli-reloc-test-~A/" (random (expt 2 32))))
         (root (merge-pathnames suffix (uiop:default-temporary-directory)))
         (lib-dir (uiop:ensure-directory-pathname (merge-pathnames "lib/" root)))
         (share-dir (uiop:ensure-directory-pathname
                     (merge-pathnames "share/kli/" root)))
         (lib-file (merge-pathnames soname lib-dir)))
    (ensure-directories-exist lib-dir)
    (ensure-directories-exist share-dir)
    (with-open-file (s lib-file :direction :output :if-does-not-exist :create)
      (write-string "stub" s))
    (unwind-protect (funcall thunk share-dir lib-file)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(test relocation-locate-step1-kli-data-dir
  "locate-relocated-lib resolves <root>/lib/<soname> from KLI_DATA_DIR=<root>/share/kli."
  (call-with-temp-lib-bundle
   "libcrypto.so.3"
   (lambda (share-dir lib-file)
     (with-env-var ("KLI_DATA_DIR" (namestring share-dir))
       (is (equal (truename lib-file)
                  (truename (reloc:locate-relocated-lib "libcrypto.so.3"))))))))

(test relocation-locate-fallthrough-returns-soname
  "With no bundle on disk and KLI_DATA_DIR unset, locate-relocated-lib returns
   the bare SONAME so the OS dlopen resolves it via LD_LIBRARY_PATH (step 3)."
  (with-env-var ("KLI_DATA_DIR" nil)
    (is (equal "libcrypto.so.3"
               (reloc:locate-relocated-lib "libcrypto.so.3")))))
