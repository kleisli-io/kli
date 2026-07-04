(in-package #:kli/tests)

(defun fault-log-lines (seam)
  (let ((path (ext:fault-log-path seam)))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line collect line)))))

(test fault-barrier-contains-error-logs-and-returns-fallback
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (is (eq :fallback
              (ext:with-extension-fault-barrier (:seam :probe :id :boom :on-fault :fallback)
                (error "probe boom"))))
      (let ((lines (fault-log-lines :probe)))
        (is (= 1 (length lines)))
        (is (not (null (search "id=:BOOM" (first lines)))))
        (is (not (null (search "probe boom" (first lines)))))))))

(test fault-barrier-normal-path-returns-values-and-logs-nothing
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (is (equal '(1 2 3)
               (multiple-value-list
                (ext:with-extension-fault-barrier (:seam :probe :id :ok)
                  (values 1 2 3)))))
    (is (null (fault-log-lines :probe)))))

(test fault-barrier-escalate-logs-then-resignals
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (is (eq :reached-outer
            (handler-case
                (ext:with-extension-fault-barrier (:seam :kernel :id :invariant :policy :escalate)
                  (error "kernel invariant"))
              (error () :reached-outer))))
    (is (= 1 (length (fault-log-lines :kernel))))))

(test fault-barrier-policy-special-overrides-install-policy
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (is (eq :loud
            (let ((ext:*extension-fault-policy* :escalate))
              (handler-case
                  (ext:with-extension-fault-barrier (:seam :probe :id :x :on-fault :swallowed)
                    (error "must not be swallowed"))
                (error () :loud)))))))

(test fault-barrier-lets-serious-conditions-through
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (is (eq :passed-through
            (handler-case
                (ext:with-extension-fault-barrier (:seam :probe :id :oom :on-fault :wrong)
                  (error (make-condition 'storage-condition)))
              (storage-condition () :passed-through))))
    (is (null (fault-log-lines :probe)))))

(test fault-barrier-reentrant-entry-declines
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (is (eq :declined
            (let ((ext:*in-fault-barrier* t))
              (handler-case
                  (ext:with-extension-fault-barrier (:seam :probe :id :nested :on-fault :wrong)
                    (error "fault during handling"))
                (error () :declined)))))
    (is (null (fault-log-lines :probe)))))

(test fault-barrier-outer-supervisor-can-use-value-under-escalate
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (is (= 99
           (handler-bind ((error (lambda (c) (use-value 99 c))))
             (ext:with-extension-fault-barrier (:seam :probe :id :sup :policy :escalate)
               (error "supervisor decides")))))))

(test fault-barrier-reify-calls-hook-then-degrades
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil)
          (reified '()))
      (let ((ext:*fault-reify-hook*
              (lambda (condition seam id)
                (push (list seam id (princ-to-string condition)) reified))))
        (is (eq :degraded
                (ext:with-extension-fault-barrier (:seam :command :id :slash :policy :reify :on-fault :degraded)
                  (error "command boom")))))
      (is (equal '((:command :slash "command boom")) reified)))))

(test fault-barrier-faulting-reify-hook-keeps-containment
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil)
          (ext:*fault-reify-hook*
            (lambda (condition seam id)
              (declare (ignore condition seam id))
              (error "hook broken"))))
      (is (eq :still-degraded
              (ext:with-extension-fault-barrier (:seam :command :id :slash :policy :reify :on-fault :still-degraded)
                (error "original")))))
    (let ((lines (fault-log-lines :command)))
      (is (= 2 (length lines)))
      (is (not (null (search "ctx=reify-hook" (second lines))))))))

(test safely-invoke-wraps-funcall-seams
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (is (= 42 (ext:safely-invoke #'1+ :callback :inc 41)))
      (is (null (ext:safely-invoke (lambda () (error "callback boom")) :callback :boom)))
      (is (= 1 (length (fault-log-lines :callback)))))))

(test fault-barrier-nested-inner-contains-outer-continues
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil))
      (is (equal '(:inner-fb :outer-continued)
                 (ext:with-extension-fault-barrier (:seam :outer :id :o :on-fault :outer-fb)
                   (list (ext:with-extension-fault-barrier (:seam :inner :id :i :on-fault :inner-fb)
                           (error "inner boom"))
                         :outer-continued))))
      (is (= 1 (length (fault-log-lines :inner))))
      (is (null (fault-log-lines :outer))))))

(test fault-barrier-on-fault-form-is-lazy-and-lexical
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil)
          (evals 0))
      (flet ((fallback () (incf evals) :lex))
        (ext:with-extension-fault-barrier (:seam :probe :id :a :on-fault (fallback))
          :ok)
        (is (= 0 evals))
        (is (eq :lex
                (ext:with-extension-fault-barrier (:seam :probe :id :b :on-fault (fallback))
                  (error "x"))))
        (is (= 1 evals))))))

(test fault-barrier-inner-escalation-unwinds-outer-barrier-wholesale
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil)
          (reified '()))
      (let ((ext:*fault-reify-hook*
              (lambda (condition seam id)
                (declare (ignore seam id))
                (push (princ-to-string condition) reified))))
        (is (eq :outer-fallback
                (ext:with-extension-fault-barrier (:seam :outer :policy :reify :on-fault :outer-fallback)
                  (ext:with-extension-fault-barrier (:seam :inner :policy :escalate)
                    (error "inner escalates"))
                  :must-not-resume))))
      (is (equal '("inner escalates") reified))
      (is (= 1 (length (fault-log-lines :inner))))
      (is (= 1 (length (fault-log-lines :outer)))))))

(test fault-barrier-supervisor-skip-unit-returns-fallback
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (is (eq :skipped
            (handler-bind ((error (lambda (c)
                                    (declare (ignore c))
                                    (invoke-restart 'ext:skip-unit))))
              (ext:with-extension-fault-barrier (:seam :probe :id :sup :policy :escalate :on-fault :skipped)
                (error "supervisor skips")))))))

(define-condition provider-probe-signal (error) ())

(test provider-call-is-transparent-to-conditions
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((provider (list :deny (lambda () (error 'provider-probe-signal))
                          :api (lambda () (error "api fault")))))
      (is (eq :caught-typed
              (handler-case (ext:provider-call provider :deny)
                (provider-probe-signal () :caught-typed))))
      (is (typep (handler-case (progn (ext:provider-call provider :api) nil)
                   (error (c) c))
                 'simple-error)
          "a caller capturing conditions as values still sees them")
      (is (null (fault-log-lines :provider))
          "the dispatch point writes no fault log"))))

(test provider-call-breadcrumbs-reach-owner-fault-logs
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let ((ext:*extension-fault-policy* nil)
          (outer (list :outer
                       (lambda ()
                         (ext:provider-call
                          (list :inner (lambda () (error "inner boom")))
                          :inner)))))
      (is (null (ext:with-extension-fault-barrier (:seam :owner :id :probe)
                  (ext:provider-call outer :outer))))
      (let ((lines (fault-log-lines :owner)))
        (is (= 1 (length lines)))
        (is (not (null (search "ctx=((PLIST . INNER) (PLIST . OUTER))"
                               (first lines))))
            "the owner's fault line carries the dispatch chain")))))

(test provider-call-use-value-repairs-the-faulted-dispatch
  (let ((boom (list :boom (lambda () (error "repair me")))))
    (is (= 42 (handler-bind ((error (lambda (c)
                                      (declare (ignore c))
                                      (invoke-restart 'use-value 42))))
                (ext:provider-call boom :boom))))
    (is (= 8 (handler-bind ((error (lambda (c)
                                     (declare (ignore c))
                                     (invoke-restart 'use-value 7))))
               (ext:provider-call
                (list :outer (lambda ()
                               (1+ (ext:provider-call boom :boom))))
                :outer)))
        "the innermost dispatch is repaired -- the outer computation continues")))

(test provider-call-machinery-signals-outside-the-dispatch
  (let ((chain :unset) (restart :unset))
    (is (eq :machinery-signaled
            (handler-case
                (handler-bind ((error (lambda (c)
                                        (setf chain ext:*dispatch-chain*
                                              restart (find-restart 'use-value c)))))
                  (ext:provider-call (list :only (lambda () 1)) :missing))
              (error () :machinery-signaled))))
    (is (null chain) "no breadcrumb entry for the failed dispatch")
    (is (null restart) "no repair restart for machinery errors")))

(defmacro with-boot-timing-env ((value) &body body)
  "Run BODY with KLI_BOOT_TIMING bound to VALUE (a string, or NIL to unset),
restoring the prior environment afterward."
  (let ((prior (gensym "PRIOR")))
    `(let ((,prior (uiop:getenv "KLI_BOOT_TIMING")))
       (unwind-protect
            (progn
              (if ,value
                  (sb-posix:setenv "KLI_BOOT_TIMING" ,value 1)
                  (sb-posix:unsetenv "KLI_BOOT_TIMING"))
              ,@body)
         (if ,prior
             (sb-posix:setenv "KLI_BOOT_TIMING" ,prior 1)
             (sb-posix:unsetenv "KLI_BOOT_TIMING"))))))

(test boot-stage-is-a-silent-pass-through-when-disabled
  (with-boot-timing-env (nil)
    (is (null (ext:boot-timing-enabled-p)))
    (let ((*error-output* (make-string-output-stream)))
      (is (equal '(1 2 3)
                 (multiple-value-list (ext:with-boot-stage ("x") (values 1 2 3)))))
      (is (zerop (length (get-output-stream-string *error-output*)))))))

(test boot-stage-times-and-reports-when-enabled
  (with-boot-timing-env ("1")
    (is (ext:boot-timing-enabled-p))
    (let ((*error-output* (make-string-output-stream)))
      (is (eq :ok (ext:with-boot-stage ("demo-stage") :ok)))
      (let ((printed (get-output-stream-string *error-output*)))
        (is (search "[boot]" printed))
        (is (search "demo-stage" printed))
        (is (search "ms" printed))))))
