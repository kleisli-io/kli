;;;; Minimal extension fixture: exports a manifest symbol and contributes a
;;;; single no-op effect. Used to exercise the configured-image producer.

(defpackage #:stub/ext
  (:use #:cl)
  (:export #:*stub-ext-extension-manifest*))

(in-package #:stub/ext)

(kli/ext:defextension stub-ext
  (:provides
   (:effect :stub-marker
            (lambda (protocol contribution context)
              (declare (ignore protocol contribution context)))
            :no-op)))
