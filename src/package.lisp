;;; kli — top-level package and CLI entry point

(defpackage #:kli
  (:use #:cl)
  (:export #:main #:init #:*version* #:*build-id* #:ensure-build-id))
