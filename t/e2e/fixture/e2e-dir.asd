;;; Component order here is deliberately not the load order: only :depends-on
;;; yields packages -> provider -> consumer -> extension. Alphabetical loading breaks.
(defsystem "e2e-dir"
  :serial nil
  :components ((:file "extension" :depends-on ("consumer"))
               (:file "consumer" :depends-on ("provider"))
               (:file "provider" :depends-on ("packages"))
               (:file "packages")))
