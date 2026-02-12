;;;; claude-hooks - Testing Utilities
;;;;
;;;; Simulate hook lifecycle without stdin/stdout/exit for testing.

(in-package :claude-hooks)

(defun test-run-hook (handler input)
  "Simulate hook lifecycle for testing. Calls HANDLER with INPUT (hash-table or string).
Returns (VALUES stdout-string exit-code stderr-string).
Does not call sb-ext:exit."
  (let ((parsed-input (etypecase input
                        (hash-table input)
                        (string (handler-case (parse-json input)
                                  (error ()
                                    (return-from test-run-hook
                                      (values nil 1 "Failed to parse hook input JSON")))))))
        (stdout "")
        (stderr ""))
    (handler-case
        (let ((result (handler-case
                          (funcall handler parsed-input)
                        (hook-block-condition (c)
                          (setf stderr (hook-block-message c))
                          (return-from test-run-hook
                            (values nil 2 stderr))))))
          (when result
            (setf stdout (encode-json result)))
          (values stdout 0 stderr))
      (error (e)
        (values nil 1 (format nil "Hook error: ~a" e))))))
