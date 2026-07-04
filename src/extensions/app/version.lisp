(in-package #:kli/app)

;; +kli-version+ (the SemVer) is baked in from version.sexp by the build and
;; loaded just before this file. Compiled-in, never a resource lookup, so it
;; resolves on a relocated binary whose store paths are gone.

(defvar *build-id* nil
  "Build identity for the running image, computed once on first read.")

(defun compute-build-id ()
  "Identify the build from the running executable path: the 32-char store
hash under /nix/store, else dev-<mtime>, else unknown-<universal-time>."
  (or (ignore-errors
        (let* ((exe #+sbcl sb-ext:*runtime-pathname*
                    #-sbcl (first (uiop:raw-command-line-arguments)))
               (truepath (and exe (namestring (truename exe))))
               (prefix "/nix/store/"))
          (when truepath
            (if (and (>= (length truepath) (+ (length prefix) 32))
                     (string= prefix truepath :end2 (length prefix)))
                (subseq truepath (length prefix)
                        (position #\- truepath :start (length prefix)))
                (format nil "dev-~A" (file-write-date truepath))))))
      (format nil "unknown-~A" (get-universal-time))))

(defun build-id ()
  "Memoized build id for the running image."
  (or *build-id* (setf *build-id* (compute-build-id))))

(defun current-version ()
  "Display version: <SemVer> (<build-id>)."
  (format nil "~A (~A)" +kli-version+ (build-id)))
