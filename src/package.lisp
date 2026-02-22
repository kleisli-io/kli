;;; kli — top-level package and CLI entry point

(defpackage #:kli
  (:use #:cl)
  (:export #:main #:init #:update #:*version* #:*build-id* #:ensure-build-id
           #:*verbose*))

(in-package #:kli)

(defvar *version* "0.2.0")

(defvar *verbose* nil
  "When true, emit informational log messages to *error-output*.
   Set from KLI_LOG environment variable at startup.")
