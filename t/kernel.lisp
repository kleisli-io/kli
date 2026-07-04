(in-package #:kli/tests)

(def-suite all)
(in-suite all)

(setf ext:*extension-fault-policy* :escalate)

(defclass command-extension-protocol (kli:protocol) ())

(defmethod kli:smoke-test-protocol ((protocol command-extension-protocol) context)
  (declare (ignore protocol context))
  t)

(defmethod ext:protocol-load-extension ((protocol command-extension-protocol)
                                        (definition cons)
                                        context)
  (declare (ignore protocol))
  (destructuring-bind (operator id &key (kind :command-object) state)
      definition
    (unless (eq operator :define-object)
      (error "Unknown command protocol operation: ~S" operator))
    (kli:register-live-object
     (kli:context-registry context)
     (obj:make-standard-live-object
                    :id id
                    :kind kind
                    :source :command-extension-protocol
                    :state state))))

(defclass broken-dispatch-protocol (kli:protocol) ())

(defmethod event:dispatch-event ((protocol broken-dispatch-protocol) event context)
  (declare (ignore protocol event context))
  (error "Broken dispatch protocol cannot handle events."))

(defvar *source-extension-file-counter* 0)

(defun write-source-extension-file ()
  (let ((path (merge-pathnames
               (format nil "kli-source-extension-~D.lisp"
                       (incf *source-extension-file-counter*))
               #p"/tmp/")))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string
       "(in-package #:kli/tests)

(ext:defextension file-source-extension
  (:provides
   (live-object file-source-object
     (obj:make-standard-live-object
                    :id :file-source-object
                    :kind :source-loaded
                    :state 42))))
"
       stream))
    path))

(defun surface-counter-handler (event context)
  (let ((counter (kli:find-live-object
                  (kli:context-registry context)
                  :surface-counter)))
    (incf (obj:object-state counter)
          (or (event:event-payload event) 1))))

(ext:defextension surface-counter-extension
  (:requires
   (:capability :test)
   (:capability :events :contract :events/v1))
  (:provides
   (live-object surface-counter
     (obj:make-standard-live-object
                    :id :surface-counter
                    :kind :counter
                    :state 0))
   (event-type :surface/increment)
   (event-handler :surface-increment
     :event-type :surface/increment
     :handler #'surface-counter-handler)))

(ext:defextension test-capability-provider
  (:provides
   (capability test)))

(ext:defextension command-protocol-extension
  (:provides
   (live-object command-protocol
     (make-instance 'command-extension-protocol
                    :id :command-protocol))))

(defun install-extension-protocol (context)
  (let ((boot (kli:active-protocol context)))
    (kli:install-protocol boot
                          (ext:make-extension-protocol)
                          context)))

(defun switch-to-extension-protocol (context)
  (let* ((boot (kli:active-protocol context))
         (extension-protocol (install-extension-protocol context)))
    (kli:switch-protocol boot
                         (kli:object-id extension-protocol)
                         context)
    extension-protocol))

(defun install-extension (context manifest)
  ;; Fixture provisioning drives the extension-load substrate.
  (with-extension-load-authority
    (ext:install-manifest manifest (kli:active-protocol context) context)))

(defun install-extensions (context &rest manifests)
  (with-extension-load-authority
    (let ((protocol (kli:active-protocol context)))
      (dolist (manifest manifests context)
        (ext:install-manifest manifest protocol context)))))

(defun load-control-plane (context)
  (install-extension context control:*control-extension-manifest*)
  (kli:find-live-object (kli:context-registry context)
                        :control-plane))

(test live-registry-table-is-synchronized
  "Agent worker threads register live objects while the TUI loop thread reads
the registry on every trampoline tick -- unsynchronized concurrent hash-table
access on SBCL is undefined."
  (is (sb-ext:hash-table-synchronized-p
       (kli::registry-objects (kli:make-registry)))))

(test kernel-boots-with-boot-protocol
  (let ((context (kli:make-kernel-host)))
    (is (typep (kli:active-protocol context) 'kli:boot-protocol))
    (is (eq :boot-protocol
            (kli:object-id (kli:active-protocol context))))
    (is (null (kli:find-live-object (kli:context-registry context)
                                    :control-plane)))
    (is (eq (kli:active-protocol context)
            (kli:find-live-object (kli:context-registry context)
                                  :boot-protocol)))))

(test boot-protocol-rejects-ordinary-extensions
  (let ((context (kli:make-kernel-host)))
    (signals error
      (ext:install-manifest (lambda () (ext:make-extension :id :demo))
                            (kli:active-protocol context)
                            context))))

(test boot-protocol-installs-only-protocol-objects
  (let ((context (kli:make-kernel-host)))
    (signals error
      (kli:install-protocol (kli:active-protocol context)
                            :extension-protocol
                            context))
    (signals error
      (kli:install-protocol (kli:active-protocol context)
                            '(:entry make-extension-protocol)
                            context))))

(test boot-protocol-does-not-load-extension-source
  (let ((context (kli:make-kernel-host)))
    (signals error
      (ext:load-extension-source
       context
       '(ext:defextension boot-source-extension
          (:provides
           (live-object boot-source-object
             (obj:make-standard-live-object
              :id :boot-source-object
              :kind :source-loaded))))))))

(test boot-protocol-installs-and-switches-extension-protocol
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (is (typep extension-protocol 'ext:extension-protocol))
    (is (eq boot (kli:active-protocol context)))
    (is (eq extension-protocol
            (kli:switch-protocol boot
                                 (kli:object-id extension-protocol)
                                 context)))
    (is (eq extension-protocol (kli:active-protocol context)))))

(test (extension-protocol-loads-extension-contributions :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context))
         (counter (obj:make-standard-live-object
                                 :id :counter
                                 :kind :counter
                                 :state 0))
         (seen nil)
         (extension
           (ext:make-extension
            :id :counter-extension
            :contributions
            (list
             (ext:make-live-object-contribution
              :name :counter
              :object counter)
             (event:make-event-type-contribution
              :name :counter/increment)
             (event:make-event-handler-contribution
              :name :increment-counter
              :event-type :counter/increment
              :handler (lambda (event _context)
                         (declare (ignore _context))
                         (incf (obj:object-state counter)
                               (or (event:event-payload event) 1))
                         (setf seen event)))))))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (is (eq extension (ext:install-manifest (lambda () extension)
                                            extension-protocol
                                            context)))
    (is (eq counter
            (kli:find-live-object (kli:context-registry context)
                                  :counter)))
    (event:emit-event context
                    (event:make-event :counter/increment
                                    :payload 3
                                    :source :test))
    (is (= 3 (obj:object-state counter)))
    (is (typep seen 'event:event))))

(test defextension-surface-loads-through-extension-protocol
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extensions context
                        *test-capability-provider-extension-manifest*
                        event:*events-extension-manifest*)
    (is (eq :surface-counter-extension
            (kli:object-id
             (install-extension
              context
              *surface-counter-extension-extension-manifest*))))
    (is (eq :counter
            (obj:object-kind
             (kli:find-live-object (kli:context-registry context)
                                   :surface-counter))))
    (event:emit-event context
                    (event:make-event :surface/increment
                                    :payload 4
                                    :source :test))
    (is (= 4
           (obj:object-state
            (kli:find-live-object (kli:context-registry context)
                                  :surface-counter))))))

(test (events-protocol-storage-is-per-protocol :fixture extension-load-authority)
  (let* ((ctx-a (kli:make-kernel-host))
         (boot-a (kli:active-protocol ctx-a))
         (proto-a (kli:install-protocol boot-a
                                        (ext:make-extension-protocol :id :proto-a)
                                        ctx-a))
         (ctx-b (kli:make-kernel-host))
         (boot-b (kli:active-protocol ctx-b))
         (proto-b (kli:install-protocol boot-b
                                        (ext:make-extension-protocol :id :proto-b)
                                        ctx-b)))
    (kli:switch-protocol boot-a (kli:object-id proto-a) ctx-a)
    (kli:switch-protocol boot-b (kli:object-id proto-b) ctx-b)
    (install-extensions ctx-a event:*events-extension-manifest*)
    (install-extensions ctx-b event:*events-extension-manifest*)
    (let ((types-a (ext:protocol-storage proto-a :kli/event/types))
          (types-b (ext:protocol-storage proto-b :kli/event/types))
          (handlers-a (ext:protocol-storage proto-a :kli/event/handlers))
          (handlers-b (ext:protocol-storage proto-b :kli/event/handlers)))
      (is (hash-table-p types-a))
      (is (hash-table-p types-b))
      (is (not (eq types-a types-b))
          "event-type tables must be per-protocol")
      (is (not (eq handlers-a handlers-b))
          "event-handler tables must be per-protocol"))
    (let ((seen-on-a 0))
      (ext:load-extension-source
       ctx-a
       `(ext:defextension proto-a-only-events
          (:provides
           (event-type :proto-a/ping)
           (event-handler :proto-a/sink
             :event-type :proto-a/ping
             :handler ,(lambda (event ctx)
                         (declare (ignore event ctx))
                         (incf seen-on-a))))))
      (event:emit-event ctx-a
                        (event:make-event :proto-a/ping :source :test))
      (event:emit-event ctx-b
                        (event:make-event :proto-a/ping :source :test))
      (is (= 1 seen-on-a)
          "handler registered on proto-a must not fire on proto-b"))))

(test (events-protocol-storage-clears-on-retract :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extensions context event:*events-extension-manifest*)
    (is (hash-table-p (ext:protocol-storage extension-protocol
                                            :kli/event/types)))
    (let ((events-extension
            (kli:find-live-object (kli:context-registry context)
                                  :events)))
      (ext:deactivate-extension extension-protocol events-extension context))
    (is (null (ext:protocol-storage extension-protocol :kli/event/types))
        "retracting :events must clear the per-protocol types storage")
    (is (null (ext:protocol-storage extension-protocol :kli/event/handlers))
        "retracting :events must clear the per-protocol handlers storage")))

(test extension-protocol-definitions-slot-is-per-instance
  (let ((proto-a (ext:make-extension-protocol :id :proto-a))
        (proto-b (ext:make-extension-protocol :id :proto-b))
        (factory-a (lambda ()
                     (ext:make-extension
                      :id :shared-id
                      :contributions
                      (list (ext:make-live-object-contribution
                             :name :shared
                             :object (obj:make-standard-live-object
                                      :id :shared :kind :a))))))
        (factory-b (lambda ()
                     (ext:make-extension
                      :id :shared-id
                      :contributions
                      (list (ext:make-live-object-contribution
                             :name :shared
                             :object (obj:make-standard-live-object
                                      :id :shared :kind :b)))))))
    (ext:define-extension-in proto-a :shared-id factory-a)
    (ext:define-extension-in proto-b :shared-id factory-b)
    (let ((ext-a (ext:make-defined-extension proto-a :shared-id))
          (ext-b (ext:make-defined-extension proto-b :shared-id)))
      (is (eq :a (obj:object-kind
                  (ext:contribution-object
                   (first (ext:extension-contribution-list ext-a))))))
      (is (eq :b (obj:object-kind
                  (ext:contribution-object
                   (first (ext:extension-contribution-list ext-b))))))
      (is (eq factory-a (ext:find-extension-definition proto-a :shared-id)))
      (is (eq factory-b (ext:find-extension-definition proto-b :shared-id))))
    (signals error
      (ext:make-defined-extension proto-a :no-such-extension-anywhere))))

(test per-protocol-storage-is-keyed-and-isolated
  (let ((proto-a (ext:make-extension-protocol :id :proto-a))
        (proto-b (ext:make-extension-protocol :id :proto-b)))
    (is (null (ext:protocol-storage proto-a :extension/state)))
    (is (eq :fallback
            (ext:protocol-storage proto-a :extension/state :fallback)))
    (setf (ext:protocol-storage proto-a :extension/state)
          (list :hello))
    (is (equal '(:hello) (ext:protocol-storage proto-a :extension/state)))
    (is (null (ext:protocol-storage proto-b :extension/state)))
    (setf (ext:protocol-storage proto-b :extension/state) :other)
    (is (equal '(:hello) (ext:protocol-storage proto-a :extension/state)))
    (is (eq :other (ext:protocol-storage proto-b :extension/state)))))

(test ensure-protocol-storage-materializes-only-once
  (let ((proto (ext:make-extension-protocol :id :proto))
        (call-count 0))
    (let ((table (ext:ensure-protocol-storage
                  proto :extension/cache
                  (lambda ()
                    (incf call-count)
                    (make-hash-table :test #'eq)))))
      (is (hash-table-p table))
      (is (= 1 call-count))
      (is (eq table (ext:ensure-protocol-storage
                     proto :extension/cache
                     (lambda ()
                       (incf call-count)
                       (make-hash-table :test #'eq)))))
      (is (= 1 call-count)))))

(test extension-protocol-rejects-unsatisfied-requirements
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (signals error
      (install-extension context
                         *surface-counter-extension-extension-manifest*))
    (is (null (kli:find-live-object (kli:context-registry context)
                                    :surface-counter)))))

(test (extension-protocol-registers-and-retracts-capabilities :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context))
         (extension nil))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (setf extension (install-extension context
                                       *test-capability-provider-extension-manifest*))
    (is (ext:capability-provided-p extension-protocol :test))
    (is (ext:extension-loaded-p extension-protocol :test-capability-provider))
    (is (= 1 (length (ext:find-capabilities extension-protocol :test))))
    (ext:deactivate-extension extension-protocol extension context)
    (is (not (ext:capability-provided-p extension-protocol :test)))
    (is (not (ext:extension-loaded-p extension-protocol
                                     :test-capability-provider)))))

(test extension-can-define-new-extension-protocol
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extension context *command-protocol-extension-extension-manifest*)
    (let ((command-protocol
            (kli:find-live-object (kli:context-registry context)
                                  :command-protocol)))
      (is (typep command-protocol 'command-extension-protocol))
      (is (eq command-protocol
              (kli:switch-protocol boot :command-protocol context)))
      (is (eq command-protocol (kli:active-protocol context)))
      (ext:protocol-load-extension command-protocol
                                   '(:define-object :command-object :state 99)
                                   context)
      (let ((object (kli:find-live-object (kli:context-registry context)
                                          :command-object)))
        (is (eq :command-object (obj:object-kind object)))
        (is (= 99 (obj:object-state object)))))))

(test (extension-protocol-loads-extension-source-form :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (let ((extensions
            (ext:load-extension-source
             context
             '(ext:defextension form-source-extension
                (:provides
                 (live-object form-source-object
                   (obj:make-standard-live-object
                                  :id :form-source-object
                                  :kind :source-loaded
                                  :state 7)))))))
      (is (= 1 (length extensions)))
      (is (eq :form-source-extension
              (kli:object-id (first extensions))))
      (let ((object (kli:find-live-object (kli:context-registry context)
                                          :form-source-object)))
        (is (eq :source-loaded (obj:object-kind object)))
        (is (= 7 (obj:object-state object)))))))

(test (extension-protocol-loads-extension-source-file :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context))
         (source-path (write-source-extension-file)))
    (unwind-protect
         (progn
           (kli:switch-protocol boot (kli:object-id extension-protocol) context)
           (let ((extensions
                   (ext:load-extension-source context source-path)))
             (is (= 1 (length extensions)))
             (is (eq :file-source-extension
                     (kli:object-id (first extensions))))
             (let ((object (kli:find-live-object (kli:context-registry context)
                                                 :file-source-object)))
               (is (eq :source-loaded (obj:object-kind object)))
               (is (= 42 (obj:object-state object))))))
      (when (probe-file source-path)
        (delete-file source-path)))))

(test (defextension-inside-source-load-registers-in-protocol :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (ext:load-extension-source
     context
     '(ext:defextension inverted-scope-extension
        (:provides
         (live-object inverted-scope-object
           (obj:make-standard-live-object
            :id :inverted-scope-object
            :kind :inverted-scope
            :state 11)))))
    (is (functionp
         (gethash :inverted-scope-extension
                  (ext:extension-factories extension-protocol)))
        "defextension inside load-extension-source must register into the active protocol")))

(test defextension-outside-source-load-binds-manifest-variable
  "defextension at top level, with no defining protocol, binds the manifest var
in the current package as a factory thunk. Calling the thunk produces a fresh
extension instance independent of any protocol."
  (is (null ext:*defining-protocol*))
  (eval '(ext:defextension manifest-variable-binding-extension
          (:provides
           (live-object manifest-variable-binding-object
             (obj:make-standard-live-object
              :id :manifest-variable-binding-object
              :kind :manifest-variable-binding
              :state 0)))))
  (let ((manifest (symbol-value
                   (find-symbol "*MANIFEST-VARIABLE-BINDING-EXTENSION-EXTENSION-MANIFEST*"
                                '#:kli/tests))))
    (is (functionp manifest)
        "defextension binds the manifest variable to a factory thunk")
    (let ((extension (funcall manifest)))
      (is (typep extension 'ext:extension))
      (is (eq :manifest-variable-binding-extension
              (kli:object-id extension))))))

(test (defextension-in-different-source-loads-stays-isolated :fixture extension-load-authority)
  (let* ((ctx-a (kli:make-kernel-host))
         (boot-a (kli:active-protocol ctx-a))
         (proto-a (kli:install-protocol boot-a
                                        (ext:make-extension-protocol :id :proto-a)
                                        ctx-a))
         (ctx-b (kli:make-kernel-host))
         (boot-b (kli:active-protocol ctx-b))
         (proto-b (kli:install-protocol boot-b
                                        (ext:make-extension-protocol :id :proto-b)
                                        ctx-b)))
    (kli:switch-protocol boot-a (kli:object-id proto-a) ctx-a)
    (kli:switch-protocol boot-b (kli:object-id proto-b) ctx-b)
    (ext:load-extension-source
     ctx-a
     '(ext:defextension isolated-source-extension
        (:provides
         (live-object isolated-source-object
           (obj:make-standard-live-object
            :id :isolated-source-object
            :kind :variant-a)))))
    (ext:load-extension-source
     ctx-b
     '(ext:defextension isolated-source-extension
        (:provides
         (live-object isolated-source-object
           (obj:make-standard-live-object
            :id :isolated-source-object
            :kind :variant-b)))))
    (is (eq :variant-a
            (obj:object-kind
             (kli:find-live-object (kli:context-registry ctx-a)
                                   :isolated-source-object))))
    (is (eq :variant-b
            (obj:object-kind
             (kli:find-live-object (kli:context-registry ctx-b)
                                   :isolated-source-object))))))

(test (extension-deactivation-retracts-contributions :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context))
         (counter (obj:make-standard-live-object
                                 :id :lifecycle-counter
                                 :kind :counter
                                 :state 0))
         (extension
           (ext:make-extension
            :id :lifecycle-extension
            :contributions
            (list
             (ext:make-live-object-contribution
              :name :lifecycle-counter
              :object counter)
             (event:make-event-type-contribution
              :name :lifecycle/increment)
             (event:make-event-handler-contribution
              :name :lifecycle-increment
              :event-type :lifecycle/increment
              :handler (lambda (event _context)
                         (declare (ignore _context))
                         (incf (obj:object-state counter)
                               (or (event:event-payload event) 1))))))))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (ext:install-manifest (lambda () extension) extension-protocol context)
    (event:emit-event context
                    (event:make-event :lifecycle/increment
                                    :payload 5
                                    :source :test))
    (is (= 5 (obj:object-state counter)))
    (is (eq extension
            (ext:deactivate-extension extension-protocol extension context)))
    (is (null (kli:find-live-object (kli:context-registry context)
                                    :lifecycle-extension)))
    (is (null (kli:find-live-object (kli:context-registry context)
                                    :lifecycle-counter)))
    (event:emit-event context
                    (event:make-event :lifecycle/increment
                                    :payload 5
                                    :source :test))
    (is (= 5 (obj:object-state counter)))))

(test (extension-recode-replaces-contributions :fixture recode-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context))
         (old-object (obj:make-standard-live-object
                                    :id :recode-object
                                    :kind :recode
                                    :state :old))
         (new-object (obj:make-standard-live-object
                                    :id :recode-object
                                    :kind :recode
                                    :state :new))
         (old-extension
           (ext:make-extension
            :id :recode-extension
            :contributions
            (list
             (ext:make-live-object-contribution
              :name :recode-object
              :object old-object))))
         (new-extension
           (ext:make-extension
            :id :recode-extension
            :version 2
            :contributions
            (list
             (ext:make-live-object-contribution
              :name :recode-object
              :object new-object)))))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (ext:install-manifest (lambda () old-extension) extension-protocol context)
    (is (eq old-object
            (kli:find-live-object (kli:context-registry context)
                                  :recode-object)))
    (is (eq new-extension
            (ext:recode-extension extension-protocol
                                  old-extension
                                  new-extension
                                  context)))
    (is (eq new-extension
            (kli:find-live-object (kli:context-registry context)
                                  :recode-extension)))
    (is (eq new-object
            (kli:find-live-object (kli:context-registry context)
                                  :recode-object)))
    (is (eq :new (obj:object-state
                  (kli:find-live-object (kli:context-registry context)
                                        :recode-object))))))

(test (extension-recode-rolls-back-on-failure :fixture recode-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context))
         (old-object (obj:make-standard-live-object
                                    :id :rollback-old-object
                                    :kind :rollback
                                    :state :old))
         (new-object (obj:make-standard-live-object
                                    :id :rollback-new-object
                                    :kind :rollback
                                    :state :new))
         (old-extension
           (ext:make-extension
            :id :rollback-extension
            :contributions
            (list
             (ext:make-live-object-contribution
              :name :rollback-old-object
              :object old-object))))
         (bad-extension
           (ext:make-extension
            :id :rollback-extension
            :version 2
            :contributions
            (list
             (ext:make-live-object-contribution
              :name :rollback-new-object
              :object new-object)
             (make-instance 'ext:contribution
                            :kind :unsupported
                            :name :unsupported)))))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (ext:install-manifest (lambda () old-extension) extension-protocol context)
    (signals error
      (ext:recode-extension extension-protocol
                            old-extension
                            bad-extension
                            context))
    (is (eq old-extension
            (kli:find-live-object (kli:context-registry context)
                                  :rollback-extension)))
    (is (eq old-object
            (kli:find-live-object (kli:context-registry context)
                                  :rollback-old-object)))
    (is (eq :old (obj:object-state
                  (kli:find-live-object (kli:context-registry context)
                                        :rollback-old-object))))
    (is (null (kli:find-live-object (kli:context-registry context)
                                    :rollback-new-object)))))

(test standard-object-extension-registers-service
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extension context obj:*standard-object-extension-manifest*)
    (is (ext:extension-loaded-p extension-protocol :standard-object))))

(test events-extension-registers-service
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extension context event:*events-extension-manifest*)
    (is (ext:extension-loaded-p extension-protocol :events))))

(test introspection-extension-describes-context
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extensions context
                        obj:*standard-object-extension-manifest*
                        intro:*introspection-extension-manifest*
                        journal:*journal-extension-manifest*)
    (let ((summary (intro:context-summary context)))
      (is (eq :extension-protocol (getf summary :active-protocol)))
      (is (member :journal-service (getf summary :objects)))
      (is (eq :journal-service
              (getf (intro:describe-live-object
                     (kli:find-live-object (kli:context-registry context)
                                           :journal-service))
                    :id))))))

(test introspection-describe-by-id-round-trips
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extensions context
                        obj:*standard-object-extension-manifest*
                        intro:*introspection-extension-manifest*
                        journal:*journal-extension-manifest*)
    (let ((registry (kli:context-registry context)))
      (is (string= "JOURNAL-SERVICE" (intro:object-id-string :journal-service)))
      (is (eq :journal-service
              (getf (intro:describe-by-id registry "JOURNAL-SERVICE") :id)))
      (is (null (intro:describe-by-id registry "NO-SUCH-OBJECT"))))))

(test journal-extension-records-entries
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extension context journal:*journal-extension-manifest*)
    (let* ((journal (kli:find-live-object (kli:context-registry context)
                                          :journal-service))
           (entry (journal:record-journal-entry journal
                                            :observed
                                            :payload 13
                                            :source :test)))
      (is (typep journal 'journal:journal-service))
      (is (equal '(:type :observed :payload 13 :source :test) entry))
      (is (equal (list entry) (journal:journal-entries journal))))))

(test (snapshot-extension-restores-active-protocol :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extension context snapshot:*snapshot-extension-manifest*)
    (is (ext:extension-loaded-p extension-protocol :snapshot))
    (let ((snapshot (snapshot:snapshot-context context)))
      (is (eq :extension-protocol (getf snapshot :active-protocol)))
      (kli:switch-protocol boot :boot-protocol context)
      (is (eq boot (kli:active-protocol context)))
      (is (eq extension-protocol
              (snapshot:restore-active-protocol context snapshot)))
      (is (eq extension-protocol (kli:active-protocol context))))))

(test (activate-extension-tracks-root-activation-order :fixture extension-load-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (is (null (ext:protocol-root-activations protocol)))
    (let ((journal (install-extension context
                                      journal:*journal-extension-manifest*)))
      (install-extension context snapshot:*snapshot-extension-manifest*)
      (is (equal '(:journal :snapshot)
                 (ext:protocol-root-activations protocol)))
      (ext:deactivate-extension protocol journal context)
      (is (equal '(:snapshot)
                 (ext:protocol-root-activations protocol))))))

(test snapshot-value-serialization-round-trips
  (flet ((round-trip (value)
           (snapshot::deserialize-snapshot-value
            (snapshot::serialize-snapshot-value value))))
    (dolist (value (list 42 "s" :k nil t
                         '(1 (2 "x" (:k)) nil t)
                         '(:snapshot/list :a)
                         '(:snapshot/hash-table 1 2)))
      (is (equal value (round-trip value))))
    (is (eq 'kli:live-object (round-trip 'kli:live-object)))
    (let ((table (make-hash-table :test #'equal)))
      (setf (gethash "k" table) '(:v 1))
      (setf (gethash :nested table) (make-hash-table :test #'eq))
      (let ((restored (round-trip table)))
        (is (hash-table-p restored))
        (is (eq 'equal (hash-table-test restored)))
        (is (equal '(:v 1) (gethash "k" restored)))
        (is (eq 'eq (hash-table-test (gethash :nested restored))))))
    (signals snapshot:snapshot-unserializable
      (snapshot::serialize-snapshot-value #'identity))))

(test (snapshot-captures-manifests-storage-and-live-object-state :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        journal:*journal-extension-manifest*
                        snapshot:*snapshot-extension-manifest*)
    (let ((journal (kli:find-live-object (kli:context-registry context)
                                         :journal-service)))
      (journal:record-journal-entry journal :observed :payload 7 :source :test))
    (setf (ext:protocol-storage protocol :kli/tests.snapshot-sample)
          '(:alpha 1 "beta"))
    (let ((snapshot (snapshot:snapshot-context context)))
      (is (eq :extension-protocol (getf snapshot :active-protocol)))
      (is (equal '(:journal :snapshot)
                 (mapcar (lambda (record) (getf record :id))
                         (getf snapshot :extensions)))
          "the snapshot names every root extension in activation order")
      (is (every (lambda (record) (getf record :manifest))
                 (getf snapshot :extensions))
          "every defextension-made root carries its manifest variable")
      (is (null (getf snapshot :unrestorable-extensions)))
      (is (equal '(:alpha 1 "beta")
                 (second (find :kli/tests.snapshot-sample
                               (getf snapshot :storage)
                               :key #'first)))
          "protocol storage is captured as serialized data")
      (let ((record (find :journal-service (getf snapshot :objects)
                          :key (lambda (record) (getf record :id)))))
        (is (not (null record))
            "contributed live objects appear in the snapshot")
        (is (not (null (getf record :slots)))
            "live-object slot state is captured, not just the id")))))

(test (snapshot-restore-reconstructs-discarded-protocol :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (protocol (switch-to-extension-protocol context))
         (handles (list (install-extension
                         context journal:*journal-extension-manifest*)
                        (install-extension
                         context snapshot:*snapshot-extension-manifest*))))
    (let ((journal (kli:find-live-object (kli:context-registry context)
                                         :journal-service)))
      (journal:record-journal-entry journal :observed :payload 13 :source :test))
    (setf (ext:protocol-storage protocol :kli/tests.snapshot-sample)
          '(:alpha 1 "beta"))
    (let ((snapshot (snapshot:snapshot-context context)))
      (kli:switch-protocol boot :boot-protocol context)
      (ext:retract-installed-extensions handles protocol context)
      (kli:remove-live-object (kli:context-registry context)
                              (kli:object-id protocol))
      (is (null (kli:find-live-object (kli:context-registry context)
                                      :extension-protocol)))
      (is (null (kli:find-live-object (kli:context-registry context)
                                      :journal-service)))
      (let ((restored (snapshot:restore-active-protocol context snapshot)))
        (is (not (eq protocol restored))
            "restore reconstructs a discarded protocol instead of erroring")
        (is (eq restored (kli:active-protocol context)))
        (is (ext:extension-loaded-p restored :journal))
        (is (ext:extension-loaded-p restored :snapshot))
        (is (equal '(:alpha 1 "beta")
                   (ext:protocol-storage restored :kli/tests.snapshot-sample)))
        (is (equal '((:type :observed :payload 13 :source :test))
                   (journal:journal-entries
                    (kli:find-live-object (kli:context-registry context)
                                          :journal-service)))
            "live-object slot state is rehydrated byte-faithfully")))))

(test (snapshot-restore-survives-image-restart :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        journal:*journal-extension-manifest*
                        snapshot:*snapshot-extension-manifest*)
    (let ((journal (kli:find-live-object (kli:context-registry context)
                                         :journal-service)))
      (journal:record-journal-entry journal :observed :payload 17 :source :test))
    (let ((table (make-hash-table :test #'equal)))
      (setf (gethash "k" table) '(:v 1))
      (setf (ext:protocol-storage protocol :kli/tests.snapshot-table) table))
    (let* ((snapshot (snapshot:snapshot-context context))
           (fresh (kli:make-kernel-host))
           (restored (snapshot:restore-active-protocol fresh snapshot)))
      (is (eq restored (kli:active-protocol fresh)))
      (is (ext:extension-loaded-p restored :journal))
      (is (ext:extension-loaded-p restored :snapshot))
      (let ((table (ext:protocol-storage restored :kli/tests.snapshot-table)))
        (is (hash-table-p table))
        (is (eq 'equal (hash-table-test table)))
        (is (equal '(:v 1) (gethash "k" table))))
      (is (equal '((:type :observed :payload 17 :source :test))
                 (journal:journal-entries
                  (kli:find-live-object (kli:context-registry fresh)
                                        :journal-service))))
      (is (eq protocol (kli:active-protocol context))
          "restoring into a fresh context leaves the original untouched"))))

(test (snapshot-restore-errors-on-unrestorable-extension :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (protocol (switch-to-extension-protocol context))
         (handle (install-extension
                  context
                  (lambda () (ext:make-extension :id :anonymous-extension)))))
    (let ((snapshot (snapshot:snapshot-context context)))
      (is (equal '(:anonymous-extension)
                 (getf snapshot :unrestorable-extensions))
          "extensions without a manifest variable are named, not dropped")
      (kli:switch-protocol boot :boot-protocol context)
      (ext:deactivate-extension protocol handle context)
      (kli:remove-live-object (kli:context-registry context)
                              (kli:object-id protocol))
      (signals error
        (snapshot:restore-active-protocol context snapshot)))))

(test (snapshot-save-round-trips-durable-datum-with-grant-set
       :fixture interactive-authority)
  "The snapshot datum the provider returns is print/read-safe whole: it writes
and reads back equal under standard io syntax, and the grant-set authority map
rides along as captured storage."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (merge-pathnames "kli-snapshot-save.sexp"
                                (uiop:temporary-directory))))
    (install-extension context snapshot:*snapshot-extension-manifest*)
    (ext::grant-set-put protocol :triager
                        (ext:make-grant :capabilities '(:image/inspect)))
    (let ((datum (snapshot:snapshot-context context)))
      (session-commands::write-snapshot-file path datum)
      (is (equal datum (session-commands::read-snapshot-file path))
          "the durable datum round-trips through prin1/read unchanged")
      (is (member ext::+grant-set-storage-key+
                  (mapcar (lambda (entry)
                            (snapshot::deserialize-snapshot-value (first entry)))
                          (getf datum :storage)))
          "the saved storage carries the grant-set authority map"))))

(test (snapshot-restore-reestablishes-protocol-and-grant-set
       :fixture interactive-authority)
  "Restoring a saved snapshot file into a fresh image re-establishes the active
protocol and the grant-set authority recorded in it, byte-faithfully."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (merge-pathnames "kli-snapshot-restore.sexp"
                                (uiop:temporary-directory))))
    (install-extension context snapshot:*snapshot-extension-manifest*)
    (ext::grant-set-put protocol :triager
                        (ext:make-grant :capabilities '(:image/inspect)))
    (session-commands::write-snapshot-file
     path (snapshot:snapshot-context context))
    (let* ((fresh (kli:make-kernel-host))
           (restored (snapshot:restore-active-protocol
                      fresh (session-commands::read-snapshot-file path))))
      (is (eq restored (kli:active-protocol fresh))
          "the restored protocol becomes active in the fresh image")
      (is (ext:grant-equiv-p (ext:grant-set-lookup protocol :triager)
                             (ext:grant-set-lookup restored :triager))
          "the recorded authority survives save and restore"))))

(test (snapshot-restore-denied-without-restore-capability
       :fixture interactive-authority)
  "The provider entries gate themselves: restore is denied to a subject lacking
:protocol/restore, and save is denied to one lacking :protocol/snapshot."
  (let ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((datum (snapshot:snapshot-context context)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:protocol/snapshot))))
        (signals ext:capability-denied
          (snapshot:restore-active-protocol context datum)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:protocol/restore))))
        (signals ext:capability-denied
          (snapshot:snapshot-context context))))))

(test (history-extension-records-protocol-switches :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (let ((control (load-control-plane context)))
    (install-extension context history:*history-extension-manifest*)
    (let ((history (kli:find-live-object (kli:context-registry context)
                                         :history-service)))
      (is (typep history 'history:history-service))
      (is (eq boot
              (history:history-switch-protocol history
                                           control
                                           :boot-protocol
                                           context)))
      (is (equal '(:type :switch-protocol
                   :from :extension-protocol
                   :to :boot-protocol)
                 (first (history:history-entries history))))))))

(test (control-plane-recovers-when-active-protocol-is-broken :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (extension-protocol (switch-to-extension-protocol context))
         (control (load-control-plane context))
         (boot (kli:active-protocol context))
         (broken (make-instance 'broken-dispatch-protocol
                                :id :broken-protocol)))
    (declare (ignore extension-protocol))
    (setf boot (kli:find-live-object (kli:context-registry context)
                                     :boot-protocol))
    (kli:register-live-object (kli:context-registry context) broken)
    (setf (kli:active-protocol context) broken)
    (signals error
      (event:emit-event context
                      (event:make-event :cannot-dispatch
                                      :source :test)))
    (is (eq broken (kli:active-protocol context)))
    (is (eq boot (control:control-recover-protocol control context)))
    (is (eq boot (kli:active-protocol context)))))

(test (control-plane-rolls-back-protocol-transaction :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (let ((control (load-control-plane context)))
      (is (eq boot (control:control-switch-protocol control
                                                :boot-protocol
                                                context)))
      (is (eq boot (kli:active-protocol context)))
      (is (eq extension-protocol
              (control:control-rollback-protocol control context)))
      (is (eq extension-protocol (kli:active-protocol context))))))

(test (kernel-recovery-restart-recovers-to-boot :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (extension-protocol (switch-to-extension-protocol context))
         (control (load-control-plane context))
         (boot (kli:find-live-object (kli:context-registry context)
                                     :boot-protocol))
         (broken (make-instance 'broken-dispatch-protocol
                                :id :restart-broken-protocol)))
    (declare (ignore extension-protocol control))
    (kli:register-live-object (kli:context-registry context) broken)
    (setf (kli:active-protocol context) broken)
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (invoke-restart 'control:recover-to-boot))))
      (control:with-kernel-recovery (context)
        (event:emit-event context
                        (event:make-event :restart-recovery
                                        :source :test))))
    (is (eq boot (kli:active-protocol context)))))

(test (recover-protocol-records-a-rollback-able-transaction :fixture interactive-authority)
  "Recovery is a recorded, reversible transition: boot's recover-protocol leaves a
transaction whose rollback restores the protocol that was active when recovery
ran, rather than nil-ing the slot."
  (let* ((context (kli:make-kernel-host))
         (extension-protocol (switch-to-extension-protocol context))
         (control (load-control-plane context))
         (boot (kli:find-live-object (kli:context-registry context)
                                     :boot-protocol)))
    (is (eq boot (control:control-recover-protocol control context)))
    (is (eq boot (kli:active-protocol context)))
    (let ((transaction (kli:boot-last-protocol-transaction boot)))
      (is (not (null transaction)) "recovery recorded a transaction")
      (is (eq extension-protocol
              (control:control-rollback-protocol control context))
          "rollback after recovery restores the previously-active protocol"))))

(test main-boots-the-default-interactive-profile
  (let* ((context (app:main))
         (protocol (kli:active-protocol context)))
    (is (eq context app:*current-context*))
    (is (null app:*current-app*))
    (is (typep context 'kli:kernel-context))
    (is (eq :extension-protocol (kli:object-id protocol)))
    (is (eq :interactive-terminal app:*default-profile*))
    (is (not (null (profiles:protocol-profile-activation
                    protocol app:*default-profile*)))
        "main records the default profile's activation")
    (dolist (id '(:standard-object :events :control :snapshot :history :introspection))
      (is (ext:extension-loaded-p protocol id)))
    (is (ext:extension-loaded-p protocol :tui-app)
        "the default profile is interactive and carries the terminal UI")
    (is (kli:find-live-object (kli:context-registry context) :control-plane))
    (is (kli:find-live-object (kli:context-registry context) :history-service))))

(test (headless-profile-installs-then-drains-baseline :fixture extension-load-authority)
  "Installing the headless profile loads every baseline extension behind one
   activation record. Deactivating it drains the protocol's installed
   contributions back to empty."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (profile (ext:install-manifest profiles:*headless-extension-manifest*
                                        protocol
                                        context)))
    (dolist (id '(:standard-object :events :control :snapshot :history :introspection))
      (is (ext:extension-loaded-p protocol id)))
    (is (plusp (length (ext:protocol-installed-contributions protocol))))
    (ext:deactivate-extension protocol profile context)
    (is (null (ext:protocol-installed-contributions protocol))
        "deactivating the headless profile must drain every contribution it installed")))

(test (every-baseline-extension-retracts-cleanly :fixture extension-load-authority)
  "Every baseline extension installs with a complete contribution manifest
   and retracts without residue: protocol-installed-contributions empties
   in lock-step with the manifest."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (loaded-extensions
           (mapcar (lambda (sym)
                     (install-extension context (symbol-value sym)))
                   profiles:*baseline-extension-manifests*)))
    (dolist (extension (reverse loaded-extensions))
      (let* ((expected-drop (length (ext:extension-contribution-list extension)))
             (count-before (length
                            (ext:protocol-installed-contributions protocol))))
        (ext:deactivate-extension protocol extension context)
        (let* ((count-after (length
                             (ext:protocol-installed-contributions protocol)))
               (actual-drop (- count-before count-after)))
          (is (= actual-drop expected-drop)
              "retracting ~A must remove its ~A contributions (saw ~A)"
              (kli:object-id extension) expected-drop actual-drop))))
    (is (null (ext:protocol-installed-contributions protocol))
        "after retracting every baseline extension, installed-contributions must be empty")))

(test (load-extension-manifest-installs-equivalent-state-into-distinct-protocols :fixture extension-load-authority)
  "Installing the same manifest into two protocols yields distinct extension
   instances with independent per-protocol state."
  (let* ((manifest (ext:load-extension-manifest
                    '(ext:defextension equivalent-manifest-probe
                       (:provides
                        (live-object equivalent-manifest-object
                          (obj:make-standard-live-object
                           :id :equivalent-manifest-object
                           :kind :counter
                           :state 0))))))
         (ctx-a (kli:make-kernel-host))
         (proto-a (switch-to-extension-protocol ctx-a))
         (ctx-b (kli:make-kernel-host))
         (proto-b (switch-to-extension-protocol ctx-b))
         (ext-a (ext:install-manifest manifest proto-a ctx-a))
         (ext-b (ext:install-manifest manifest proto-b ctx-b)))
    (is (functionp manifest)
        "load-extension-manifest returns a factory thunk")
    (is (not (eq ext-a ext-b))
        "two installs produce distinct extension instances")
    (is (eq :equivalent-manifest-probe (kli:object-id ext-a)))
    (is (eq :equivalent-manifest-probe (kli:object-id ext-b)))
    (let ((obj-a (kli:find-live-object (kli:context-registry ctx-a)
                                       :equivalent-manifest-object))
          (obj-b (kli:find-live-object (kli:context-registry ctx-b)
                                       :equivalent-manifest-object)))
      (is (not (eq obj-a obj-b))
          "per-protocol live objects are independent")
      (setf (obj:object-state obj-a) 42)
      (is (= 42 (obj:object-state obj-a)))
      (is (= 0 (obj:object-state obj-b))
          "mutation of one install's state does not leak to the other"))))

(defclass mc-test-target () ())

(defgeneric mc-test-op (target arg))

(defgeneric mc-leak-test-op (target))

(defun mc-find-installed-method (gf-name specializer-classes)
  (find-method (fdefinition gf-name) '() specializer-classes nil))

(test method-contribution-installs-and-retracts-symmetrically
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (specializers (list (find-class 'mc-test-target) (find-class 't)))
         (contrib (ext:make-method-contribution
                   :gf-name 'mc-test-op
                   :qualifiers '()
                   :specializer-names '(mc-test-target t)
                   :lambda-list '(target arg)
                   :body '((declare (ignore target))
                           (list :hit arg)))))
    (dotimes (cycle 3)
      (is (null (mc-find-installed-method 'mc-test-op specializers))
          "no method before install in cycle ~A" cycle)
      (ext:install-contribution protocol contrib context)
      (is (mc-find-installed-method 'mc-test-op specializers)
          "method present after install in cycle ~A" cycle)
      (is (equal (list :hit cycle)
                 (mc-test-op (make-instance 'mc-test-target) cycle))
          "dispatch in cycle ~A" cycle)
      (ext:retract-contribution protocol contrib context)
      (is (null (mc-find-installed-method 'mc-test-op specializers))
          "method absent after retract in cycle ~A" cycle))))

(test method-contribution-refcount-survives-overlapping-protocols
  (let* ((context (kli:make-kernel-host))
         (proto1 (switch-to-extension-protocol context))
         (proto2 (ext:make-extension-protocol :id :mc-coexist-protocol))
         (specializers (list (find-class 'mc-test-target) (find-class 't)))
         (body '((declare (ignore target))
                 (list :shared arg)))
         (contrib1 (ext:make-method-contribution
                    :gf-name 'mc-test-op
                    :qualifiers '()
                    :specializer-names '(mc-test-target t)
                    :lambda-list '(target arg)
                    :body body))
         (contrib2 (ext:make-method-contribution
                    :gf-name 'mc-test-op
                    :qualifiers '()
                    :specializer-names '(mc-test-target t)
                    :lambda-list '(target arg)
                    :body body)))
    (is (null (mc-find-installed-method 'mc-test-op specializers)))
    (ext:install-contribution proto1 contrib1 context)
    (is (mc-find-installed-method 'mc-test-op specializers)
        "method present after first install (refcount 0->1)")
    (ext:install-contribution proto2 contrib2 context)
    (is (mc-find-installed-method 'mc-test-op specializers)
        "method present after second install (refcount 1->2)")
    (is (equal (list :shared 7)
               (mc-test-op (make-instance 'mc-test-target) 7)))
    (ext:retract-contribution proto1 contrib1 context)
    (is (mc-find-installed-method 'mc-test-op specializers)
        "method survives first retract (refcount 2->1)")
    (is (equal (list :shared 8)
               (mc-test-op (make-instance 'mc-test-target) 8)))
    (ext:retract-contribution proto2 contrib2 context)
    (is (null (mc-find-installed-method 'mc-test-op specializers))
        "method removed after final retract (refcount 1->0)")))

(test method-contribution-recode-installs-new-body-into-existing-gf
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (instance (make-instance 'mc-test-target))
         (specializers (list (find-class 'mc-test-target) (find-class 't)))
         (v1 (ext:make-method-contribution
              :gf-name 'mc-test-op
              :qualifiers '()
              :specializer-names '(mc-test-target t)
              :lambda-list '(target arg)
              :body '((declare (ignore target arg))
                      :v1)))
         (v2 (ext:make-method-contribution
              :gf-name 'mc-test-op
              :qualifiers '()
              :specializer-names '(mc-test-target t)
              :lambda-list '(target arg)
              :body '((declare (ignore target arg))
                      :v2))))
    (ext:install-contribution protocol v1 context)
    (is (eq :v1 (mc-test-op instance 0)))
    (ext:retract-contribution protocol v1 context)
    (ext:install-contribution protocol v2 context)
    (is (eq :v2 (mc-test-op instance 0))
        "existing instance dispatches to the new body")
    (ext:retract-contribution protocol v2 context)
    (is (null (mc-find-installed-method 'mc-test-op specializers)))))

(test method-contribution-construction-leaves-no-residue-on-gf
  (let* ((specializers (list (find-class 'mc-test-target)))
         (contrib (ext:make-method-contribution
                   :gf-name 'mc-leak-test-op
                   :qualifiers '()
                   :specializer-names '(mc-test-target)
                   :lambda-list '(target)
                   :body '((declare (ignore target))
                           :leaked))))
    (is (null (mc-find-installed-method 'mc-leak-test-op specializers))
        "make-method-contribution must not call add-method")
    (is (eq 'mc-leak-test-op (ext:contribution-gf-name contrib)))
    (is (equal '(mc-test-target) (ext:contribution-specializer-names contrib)))
    (is (equal '(target) (ext:contribution-lambda-list contrib)))
    (is (equal '((declare (ignore target)) :leaked)
               (ext:contribution-body contrib)))))

(test defextension-rejects-unknown-contribution-kind
  (handler-case
      (progn (macroexpand-1
              '(ext:defextension test-unknown-contribution
                (:provides (totally-bogus-kind foo))))
             (fail "expected unknown-contribution-kind"))
    (ext:unknown-contribution-kind (condition)
      (is (eq :totally-bogus-kind
              (ext:unknown-contribution-kind-kind condition)))
      (is (equal '(totally-bogus-kind foo)
                 (ext:unknown-contribution-kind-form condition))))))

(test removed-dead-seams-stay-removed
  "Definitions the dead-code audit deleted stay deleted: a fresh image leaves
them uninterned or at least unbound, guarding against re-introduction."
  (dolist (entry '(("KLI/TUI/VIEWS" "VIEW-CURSOR-POSITION")
                   ("KLI/TUI/VIEWS" "VIEW-HANDLE-INPUT")
                   ("KLI/TUI/VIEWS" "VIEW-HANDLE-PASTE")
                   ("KLI/TUI/VIEWS" "INVALIDATE-VIEW")
                   ("KLI/TUI/VIEWS" "SET-VIEW-FOCUSED")
                   ("KLI/TUI/VIEWS" "VIEW-CHILDREN-EMPTY")
                   ("KLI/AUTH/CORE" "MAKE-AUTH-SCOPE")
                   ("KLI/AUTH/CORE" "AUTH-SCOPE-NAME")
                   ("KLI/EXT" "BUILDER-ADD-REQUIREMENT")
                   ("KLI/EXT" "EXTENSION-REQUIREMENTS-SATISFIED-P")
                   ("KLI/EXT" "DEFINE-METHOD-CONTRIBUTION")
                   ("KLI/EXT" "PARSE-METHOD-CONTRIBUTION-PARAMETERS")
                   ("KLI/PROFILES" "ACTIVE-PROFILE-IDS")
                   ("KLI/TUI/EDITOR" "MAKE-EDITOR-BUFFER")))
    (destructuring-bind (package name) entry
      (let ((symbol (find-symbol name package)))
        (is (or (null symbol) (not (fboundp symbol)))
            "~A::~A was deleted by the dead-code audit and must stay deleted"
            package name)))))
