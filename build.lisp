;;;; build.lisp -- produce a standalone `kli` executable. Run from this
;;;; directory once the external systems are installed:
;;;;
;;;;     qlot install
;;;;     qlot exec sbcl --script build.lisp
;;;;
;;;; The component list comes from `kli.asd`; the external systems come from the
;;;; checked-in `qlfile`. The result is `bin/kli`, the unwrapped
;;;; save-lisp-and-die image the relocation wrapper (install.sh) execs directly.

(require :asdf)
(require :sb-posix)

;; Discover ./kli.asd while keeping qlot's dependency source-registry intact.
(asdf:initialize-source-registry
 `(:source-registry (:directory ,(uiop:getcwd)) :inherit-configuration))

(asdf:load-system "kli")

(let ((image (merge-pathnames "bin/kli" (uiop:getcwd))))
  (ensure-directories-exist image)
  (save-lisp-and-die
   image
   :executable t
   :purify t
   ;; Freeze runtime options so every argument reaches the toplevel: the
   ;; relocation wrapper execs the image directly, with no `--` separator.
   :save-runtime-options t
   :toplevel
   (lambda ()
     ;; Reinitialize UIOP's temporary directory inside the dumped image.
     (uiop:setup-temporary-directory)
     (kli/app:dispatch-main))))
