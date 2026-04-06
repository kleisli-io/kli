(defpackage #:depot
  (:use #:cl)
  (:export
   ;; Git utilities
   #:find-git-root
   #:find-git-root-from
   ;; Coordination root
   #:coordination-root
   #:coordination-root-from
   ;; Utilities
   #:strip-trailing-slash))
