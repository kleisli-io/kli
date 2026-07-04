(in-package #:kli/tests)

(defun call-with-env-var (name value thunk)
  (let ((previous (uiop:getenv name)))
    (if value
        (setf (uiop:getenv name) value)
        (sb-posix:unsetenv name))
    (unwind-protect (funcall thunk)
      (if previous
          (setf (uiop:getenv name) previous)
          (sb-posix:unsetenv name)))))

(defmacro with-env-var ((name value) &body body)
  `(call-with-env-var ,name ,value (lambda () ,@body)))

(test fault-injection-env-var-gates-debug-extension-installation
  (with-env-var (app:+fault-injection-env-var+ nil)
    (is (null (app:fault-injection-manifests))
        "without env var, no debug extension manifests are returned"))
  (with-env-var (app:+fault-injection-env-var+ "1")
    (let ((manifests (app:fault-injection-manifests)))
      (is (= 1 (length manifests))
          "with env var set, exactly one debug manifest is returned")
      (is (eq (first manifests)
              kli/debug:*fault-injection-extension-manifest*)
          "the manifest is the fault-injection extension manifest"))))

(test fault-injection-extension-manifest-is-a-factory-thunk
  (is (functionp kli/debug:*fault-injection-extension-manifest*)
      "the manifest is a factory thunk that produces an extension instance"))
