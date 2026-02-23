;;; kli — top-level package and CLI entry point

(defpackage #:kli
  (:use #:cl)
  (:export #:main #:init #:update #:*version* #:*build-id* #:ensure-build-id
           #:*verbose*))

(in-package #:kli)

(defvar *version*
  (or (ignore-errors (asdf:component-version (asdf:find-system "kli")))
      "0.2.2"))

(defvar *verbose* nil
  "When true, emit informational log messages to *error-output*.
   Set from KLI_LOG environment variable at startup.")
