(in-package #:kli/tests)
(in-suite all)

;;;; The blessed sqlite native lib, observed from the library-loaded image.
;;;; cl-sqlite's top-level (use-foreign-library sqlite3-lib) fires dlopen of
;;;; libsqlite3.so.0 at FASL load, so foreign-library-loaded-p proves the .so.0
;;;; SONAME resolved against LD_LIBRARY_PATH (the C lib carries no DT_SONAME; the
;;;; nixpkgs compat symlink is what dlopen finds). The compile-options query proves
;;;; the C lib was built with FTS5, which the flagship sqlite-backed extension needs.

(test sqlite-blessed-soname-loaded
  "cl-sqlite's foreign library is loaded -- libsqlite3.so.0 resolved at FASL load."
  (is (cffi:foreign-library-loaded-p 'sqlite-ffi::sqlite3-lib)))

(test sqlite-fts5-compiled-in
  "An in-memory sqlite reports ENABLE_FTS5 in its compile options."
  (let ((db (sqlite:connect ":memory:")))
    (unwind-protect
         (let ((options (mapcar #'first
                                (sqlite:execute-to-list db "PRAGMA compile_options"))))
           (is (member "ENABLE_FTS5" options :test #'string=)))
      (sqlite:disconnect db))))
