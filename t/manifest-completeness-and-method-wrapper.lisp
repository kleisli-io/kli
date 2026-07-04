(in-package #:kli/tests)

(in-suite all)

(defclass sk1-base () ())
(defclass sk1-derived (sk1-base) ())

(defgeneric sk1-describe (x))

(ext:defextension sk1-cnm
  (:provides
   (method sk1-describe () (sk1-base) (x)
     (declare (ignore x))
     (list :base))
   (method sk1-describe () (sk1-derived) (x)
     (cons :derived (call-next-method)))
   (method sk1-describe (:around) (sk1-derived) (x)
     (declare (ignore x))
     (list :around (next-method-p) (call-next-method)))))

(test (method-contribution-honors-call-next-method-and-around :fixture interactive-authority)
  "An :around plus chained primary :method contribution installs and runs."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (extension (install-extension context *sk1-cnm-extension-manifest*)))
    (is (equal '(:around t (:derived :base))
               (sk1-describe (make-instance 'sk1-derived))))
    (is (equal '(:base) (sk1-describe (make-instance 'sk1-base))))
    (ext:deactivate-extension protocol extension context)
    (is (null (compute-applicable-methods
               #'sk1-describe (list (make-instance 'sk1-derived))))
        "retract drains the contributed methods")))

(defun method-installation-refcount (gf-name qualifiers specializer-names)
  "Current refcount for the method installation key, 0 when absent.
The suite shares one image, so drain tests on shared manifests assert
refcount deltas rather than image-global method absence."
  (let ((entry (gethash (kli/ext::method-installation-key
                         gf-name qualifiers specializer-names)
                        kli/ext::*method-installations*)))
    (if entry (second (first entry)) 0)))

(test (context-lens-deactivation-releases-serialize-methods :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (key-args (list 'sess:serialize-record '() '(ctx:context-patch))))
    (install-extensions context
                        event:*events-extension-manifest*
                        sess:*session-log-extension-manifest*)
    (let* ((before (apply #'method-installation-refcount key-args))
           (extension (install-extension
                       context ctx:*context-lens-extension-manifest*)))
      (is (= (1+ before) (apply #'method-installation-refcount key-args)))
      (is (eq :context-patch
              (sess:record-type
               (sess:serialize-record
                (ctx:make-context-patch :append-message
                                        :id :context-patch-mc-drain)))))
      (ext:deactivate-extension protocol extension context)
      (is (= before (apply #'method-installation-refcount key-args))
          "deactivating context-lens releases its serialize-record installation"))))

(test (oauth-deactivation-releases-refresh-method :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (key-args (list 'auth:refresh-oauth-credential '()
                         '(auth:oauth-credential-reference))))
    (install-extensions context auth:*auth-extension-manifest*)
    (let* ((before (apply #'method-installation-refcount key-args))
           (extension (install-extension
                       context oauth:*oauth-extension-manifest*)))
      (is (= (1+ before) (apply #'method-installation-refcount key-args)))
      (ext:deactivate-extension protocol extension context)
      (is (= before (apply #'method-installation-refcount key-args))
          "deactivating oauth releases the refresh-oauth-credential installation"))))

(defgeneric sk-collide-describe (x))

(ext:defextension sk-collide-a
  (:provides
   (method sk-collide-describe () (sk1-base) (x)
     (declare (ignore x))
     :body-a)))

(ext:defextension sk-collide-b
  (:provides
   (method sk-collide-describe () (sk1-base) (x)
     (declare (ignore x))
     :body-b)))

(test (method-body-collision-warns-and-restores :fixture interactive-authority)
  "Two extensions contributing one gf+qualifiers+specializers key with different
bodies: the later body shadows the earlier with a warning, and retracting it
reinstalls the surviving extension's body, so the gf is never left missing."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (instance (make-instance 'sk1-base))
         (warned nil)
         (ext-a (install-extension context *sk-collide-a-extension-manifest*)))
    (is (eq :body-a (sk-collide-describe instance)))
    (let ((ext-b (handler-bind
                     ((warning (lambda (c)
                                 (when (search "different body"
                                               (princ-to-string c))
                                   (setf warned t))
                                 (let ((restart (find-restart 'muffle-warning c)))
                                   (when restart (invoke-restart restart))))))
                   (install-extension context *sk-collide-b-extension-manifest*))))
      (is-true warned "a same-key different-body contribution warns")
      (is (eq :body-b (sk-collide-describe instance))
          "the later body shadows the earlier")
      (ext:deactivate-extension protocol ext-b context)
      (is (eq :body-a (sk-collide-describe instance))
          "retracting the shadowing body restores the surviving extension's body")
      (ext:deactivate-extension protocol ext-a context)
      (is (null (compute-applicable-methods #'sk-collide-describe
                                            (list instance)))
          "retracting the last contribution removes the method"))))

(test (tui-transcript-deactivation-releases-default-renderer :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (key-args (list 'tui-core:render-transcript-event '() '(t t t t t))))
    (install-extensions context
                        tui-views:*tui-views-extension-manifest*
                        tui-input:*tui-input-extension-manifest*
                        tui-editor:*tui-editor-extension-manifest*
                        tui-terminal:*tui-terminal-extension-manifest*)
    (let* ((before (apply #'method-installation-refcount key-args))
           (extension (install-extension
                       context tui-transcript:*tui-transcript-extension-manifest*)))
      (is (= (1+ before) (apply #'method-installation-refcount key-args)))
      (ext:deactivate-extension protocol extension context)
      (is (= before (apply #'method-installation-refcount key-args))
          "deactivating tui-transcript releases the default renderer"))))
