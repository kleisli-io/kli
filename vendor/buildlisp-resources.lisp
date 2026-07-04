;;;; Runtime resource location for buildLisp libraries.
;;;;
;;;; A library that declares `resources` gets this file plus a generated
;;;; registration form compiled into its FASL. Loading that FASL into any
;;;; image (test runner, dumped program) runs the registration, so
;;;; resource-path resolves no matter which program loaded the library —
;;;; a library has no stable self-location at runtime otherwise.

(defpackage #:buildlisp/resources
  (:use #:cl)
  (:export #:register-resource-root #:resource-root
           #:resource-path #:resource-string))

(in-package #:buildlisp/resources)

;; defvar, not defparameter: a second resource library loading into the
;; same image must not reset roots already registered by the first.
(defvar *resource-roots* (make-hash-table :test 'equal)
  "Author-chosen key string -> resource directory pathname.")

(defun register-resource-root (key root)
  (setf (gethash key *resource-roots*) (pathname root)))

(defparameter +data-dir-env+ "KLI_DATA_DIR"
  "When set, resource roots resolve under $KLI_DATA_DIR/<key>/ first, so a
binary copied off its build store still finds its resources. Unset = the
baked store-path literal, i.e. exact pre-override behavior.")

(defun %data-dir-env ()
  "Non-empty value of +data-dir-env+, or NIL. No uiop dependency: the
support package compiles into minimal libraries that may lack it."
  (let ((v #+sbcl (sb-ext:posix-getenv +data-dir-env+)
           #+ccl (ccl:getenv +data-dir-env+)
           #+ecl (ext:getenv +data-dir-env+)
           #-(or sbcl ccl ecl) nil))
    (and v (plusp (length v)) v)))

(defun %env-resource-root (key)
  "Per-key root under $KLI_DATA_DIR, or NIL when the env is unset or the
directory is absent (caller then falls back to the baked literal)."
  (let ((env (%data-dir-env)))
    (when env
      (let ((root (pathname (concatenate 'string
                                         (string-right-trim "/" env)
                                         "/" key "/"))))
        (and (ignore-errors (probe-file root)) root)))))

(defun resource-root (key)
  (or (%env-resource-root key)
      (gethash key *resource-roots*)
      (error "buildlisp/resources: no resource root registered for ~S" key)))

(defun resource-path (key relative)
  (merge-pathnames relative (resource-root key)))

(defun resource-string (key relative)
  (with-open-file (in (resource-path key relative)
                      :direction :input :external-format :utf-8)
    (let ((buf (make-string (file-length in))))
      (subseq buf 0 (read-sequence buf in)))))
