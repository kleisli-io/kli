(in-package #:kli/tests)

(defun call-with-temp-xdg-cache (thunk)
  (let* ((suffix (format nil "kli-test-xdg-~A/" (random (expt 2 32))))
         (dir (merge-pathnames suffix (uiop:default-temporary-directory)))
         (previous (uiop:getenv "XDG_CACHE_HOME")))
    (ensure-directories-exist dir)
    (setf (uiop:getenv "XDG_CACHE_HOME") (namestring dir))
    (unwind-protect (funcall thunk dir)
      (if previous
          (setf (uiop:getenv "XDG_CACHE_HOME") previous)
          (sb-posix:unsetenv "XDG_CACHE_HOME"))
      (uiop:delete-directory-tree dir :validate t
                                      :if-does-not-exist :ignore))))

(defmacro with-temp-xdg-cache ((cache-dir-var) &body body)
  `(call-with-temp-xdg-cache (lambda (,cache-dir-var) ,@body)))

(defun event-callback-log-lines ()
  (let ((path (ext:fault-log-path :event-callback)))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line collect line)))))

(test (fan-out-isolates-failing-event-handler-and-records-id :fixture extension-load-authority)
  (with-temp-xdg-cache (_)
    (declare (ignore _))
    (let* ((ext:*extension-fault-policy* nil)
           (context (kli:make-kernel-host))
           (boot (kli:active-protocol context))
           (proto (kli:install-protocol boot
                                        (ext:make-extension-protocol)
                                        context))
           (second-saw nil))
      (kli:switch-protocol boot (kli:object-id proto) context)
      (install-extensions context event:*events-extension-manifest*)
      (ext:load-extension-source
       context
       `(ext:defextension fan-out-failing-handlers
          (:provides
           (event-type :fan-out/probe)
           (event-handler :first-throws
             :event-type :fan-out/probe
             :handler ,(lambda (event ctx)
                         (declare (ignore event ctx))
                         (error "boom-first")))
           (event-handler :second-records
             :event-type :fan-out/probe
             :handler ,(lambda (event ctx)
                         (declare (ignore ctx))
                         (setf second-saw event))))))
      (let ((emitted (event:emit-event
                      context
                      (event:make-event :fan-out/probe :source :test))))
        (is (typep emitted 'event:event))
        (is (eq second-saw emitted)
            "second handler must still fire after the first throws"))
      (let ((lines (event-callback-log-lines)))
        (is (= 1 (length lines)))
        (is (search "id=:FIRST-THROWS" (first lines))
            "diagnostic line must carry the failing handler's id")))))

(test (event-handlers-fire-in-install-order :fixture extension-load-authority)
  "Handlers on one event type fire in contribution install order. The handler
list is stored install-ordered, so the per-event dispatch iterates it
directly."
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (proto (kli:install-protocol boot
                                      (ext:make-extension-protocol)
                                      context))
         (order '()))
    (kli:switch-protocol boot (kli:object-id proto) context)
    (install-extensions context event:*events-extension-manifest*)
    (ext:load-extension-source
     context
     `(ext:defextension dispatch-order-probe
        (:provides
         (event-type :dispatch-order/probe)
         (event-handler :installed-first
           :event-type :dispatch-order/probe
           :handler ,(lambda (event ctx)
                       (declare (ignore event ctx))
                       (push :first order)))
         (event-handler :installed-second
           :event-type :dispatch-order/probe
           :handler ,(lambda (event ctx)
                       (declare (ignore event ctx))
                       (push :second order))))))
    (event:emit-event context
                      (event:make-event :dispatch-order/probe :source :test))
    (is (equal '(:first :second) (nreverse order)))))
