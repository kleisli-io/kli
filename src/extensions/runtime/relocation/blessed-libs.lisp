(in-package #:kli/runtime/relocation)

;;;; Relocation seam for build-time-blessed native libraries.
;;;;
;;;; A dumped image records the foreign libraries it loaded at build time. On a
;;;; host without those store paths, SBCL re-runs dlopen against the bare SONAME
;;;; before the toplevel runs. The build-time :dont-save flip (staged at preDump,
;;;; not here) prevents the pre-toplevel reopen crash; the init hook below is the
;;;; availability half, re-resolving each blessed lib from the relocated bundle.
;;;;
;;;; The sb-alien:: accessors are internal and coupled to SBCL's shared-object
;;;; struct layout, so the double-colon references are deliberate and version-locked.

(defparameter *blessed-sonames* nil
  "SONAMEs of build-time-blessed native libs, populated at preDump from the
   directory-less namestrings of sb-alien::*shared-objects*. Read from the live
   image rather than hand-listed or derived from DT_SONAME, because SBCL records
   exactly the string passed to load-shared-object and this self-maintains across
   soname major bumps.")

(defun directoryless-namestring-p (namestring)
  "True iff NAMESTRING is a bare SONAME with no directory component, the form SBCL
   records when it resolved a lib by SONAME against the load path. Used to filter
   sb-alien::*shared-objects* down to the blessed set."
  (and (stringp namestring)
       (let ((p (pathname namestring)))
         (and (null (pathname-directory p))
              (not (null (pathname-name p)))))))

(defun locate-relocated-lib (soname)
  "Resolve SONAME to a loadable pathname on a relocated host, or return SONAME so
   the OS dlopen resolves it against LD_LIBRARY_PATH. Order:

   1. KLI_DATA_DIR/../../lib/<soname>. The install layout is <root>/share/kli for
      KLI_DATA_DIR and <root>/lib for the bundled libs, so libs are two levels up.
   2. exe-relative lib/<soname> from (uiop:argv0).
   3. SONAME itself, letting dlopen resolve it via LD_LIBRARY_PATH.

   On a host that still has the store paths the file probes miss and step 3 resolves
   via the baked LD_LIBRARY_PATH, so the lib loads identically on both."
  (flet ((existing (path)
           (and path (probe-file path) (namestring (truename path)))))
    (let* ((data-dir (uiop:getenv "KLI_DATA_DIR"))
           (from-data-file (when data-dir
                             (existing (merge-pathnames
                                        soname
                                        (merge-pathnames
                                         (make-pathname :directory '(:relative :up :up "lib"))
                                         (uiop:ensure-directory-pathname data-dir)))))))
      (or from-data-file
          (let ((argv0 (ignore-errors (uiop:argv0))))
            (when argv0
              (existing (merge-pathnames
                         soname
                         (merge-pathnames
                          (make-pathname :directory '(:relative "lib"))
                          (uiop:pathname-directory-pathname
                           (uiop:ensure-pathname argv0 :truenamize t)))))))
          soname))))

(defun reopen-blessed-libs ()
  "Init hook that re-resolves every blessed lib from the relocated bundle so it is
   present for use on an external host. Fires after reopen-shared-objects, before
   the toplevel, on the initial thread, so it calls sb-alien:load-shared-object
   directly without CFFI's cross-thread dispatch. It must not be called from a
   worker thread, and never signals on a missing lib because a serious-condition
   in an init hook is fatal pre-toplevel."
  (dolist (soname *blessed-sonames*)
    (let ((path (locate-relocated-lib soname)))
      (when path
        (ignore-errors
         (sb-alien:load-shared-object path :dont-save t))))))
