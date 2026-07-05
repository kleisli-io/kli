(in-package #:kli/tests)
(in-suite all)

;;;; call-reporting-load-warnings: unit-load warnings reach *error-output*
;;;; exactly once, except redefinition warnings, which are muffled silently —
;;;; a source unit installs its cross-file macros twice (compile-time
;;;; evaluation, then the fasl load), so reporting them is boot noise.

(defun call-capturing-load-report (thunk)
  "Run THUNK under call-reporting-load-warnings; return what it wrote to
*error-output*."
  (let ((*error-output* (make-string-output-stream)))
    (app::call-reporting-load-warnings thunk)
    (get-output-stream-string *error-output*)))

(test load-warning-is-reported-and-muffled
  "An ordinary warning is reported to *error-output* and muffled."
  (let ((report (call-capturing-load-report
                 (lambda () (warn "sample unit-load warning")))))
    (is (search "sample unit-load warning" report))))

(test load-redefinition-warning-is-muffled-silently
  "A redefinition warning is muffled without a report line."
  (let ((report (call-capturing-load-report
                 (lambda ()
                   (warn 'sb-kernel:redefinition-with-defmacro
                         :name 'sample-macro
                         :new-function (lambda (form env)
                                         (declare (ignore form env)))
                         :new-location nil)))))
    (is (string= "" report))))
