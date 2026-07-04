(in-package #:kli/app)
;; Baked in at compile time; the nix build substitutes a generated literal.
(defparameter +kli-version+
  #.(let ((here (or *compile-file-truename* *load-truename*)))
      (with-open-file (s (merge-pathnames #p"../../../version.sexp" here))
        (read s))))
