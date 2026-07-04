(in-package #:kli/tests)

(in-suite all)

;;; Spawn gate and mediated install. The lift effect installer guards the spawn
;;; site with :extension/spawn-process, and the two-phase consent flow confers
;;; that authority only after both trust cards are confirmed. Reuses the sh
;;; fixture and the event recorder from t/mcp-client; lift-fixture-manifest from
;;; t/inbound-lift.

(defun spawn-install-spec ()
  (list :command "sh"
        :arguments (list "-c" (mcp-fixture-script))
        :id :fixture
        :timeout 5))

(test spawn-gate-denies-subject-without-spawn-capability
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let ((protocol (kli:active-protocol context))
           (ext:*call-subject*
             (ext:make-subject :capabilities '(:manifest/install))))
       ;; :manifest/install clears activate-extension's gate, so the only
       ;; remaining gate is the spawn site.
       (signals ext:capability-denied
         (ext:install-manifest (lift-fixture-manifest) protocol context))
       (is (null (ext:find-tool protocol "echo"))
           "a denied spawn installs no tool")))))

(test spawn-gate-admits-subject-with-spawn-capability
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let* ((protocol (kli:active-protocol context))
            (ext:*call-subject*
              (ext:make-subject
               :capabilities '(:manifest/install :manifest/retract
                               :extension/spawn-process)))
            (handle (ext:install-manifest (lift-fixture-manifest)
                                          protocol context)))
       (unwind-protect
            (is (not (null (ext:find-tool protocol "echo")))
                "a spawn-capable subject installs the lifted tool")
         (ext:retract-manifest handle protocol context))))))

(test spawn-consent-requires-capability-to-initiate
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let ((protocol (kli:active-protocol context))
           (ext:*call-subject*
             (ext:make-subject :capabilities '(:manifest/install))))
       (signals ext:capability-denied
         (app:install-isolated-extension
          (spawn-install-spec) protocol context
          :confirm-fn (lambda (stage card)
                        (declare (ignore stage card)) t)))
       (is (null (ext:find-tool protocol "echo"))
           "an unauthorized initiator spawns nothing")))))

(test (spawn-consent-cancels-at-a0 :fixture extension-load-authority)
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let ((protocol (kli:active-protocol context)))
       (multiple-value-bind (state detail)
           (app:install-isolated-extension
            (spawn-install-spec) protocol context
            :confirm-fn (lambda (stage card)
                          (declare (ignore card)) (eq stage :a1)))
         (is (eq :cancelled state))
         (is (eq :a0 detail)))
       (is (null (ext:find-tool protocol "echo"))
           "a cancel at A0 spawns nothing")))))

(test (spawn-consent-cancels-at-a1 :fixture extension-load-authority)
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let ((protocol (kli:active-protocol context)))
       (multiple-value-bind (state detail)
           (app:install-isolated-extension
            (spawn-install-spec) protocol context
            :confirm-fn (lambda (stage card)
                          (declare (ignore card)) (eq stage :a0)))
         (is (eq :cancelled state))
         (is (eq :a1 detail)))
       (is (null (ext:find-tool protocol "echo"))
           "a cancel at A1 spawns nothing")))))

(test (spawn-consent-installs-on-confirm :fixture lifted-tool-authority)
  (call-with-mcp-event-recorder
   (lambda (context recorded)
     (declare (ignore recorded))
     (let ((protocol (kli:active-protocol context)))
       (multiple-value-bind (state handle)
           (app:install-isolated-extension
            (spawn-install-spec) protocol context
            :confirm-fn (lambda (stage card)
                          (declare (ignore stage card)) t))
         (is (eq :installed state))
         (unwind-protect
              (progn
                (is (not (null (ext:find-tool protocol "echo")))
                    "a confirmed spawn lifts the server tool")
                (let ((result (ext:invoke-tool protocol "echo" nil context)))
                  (is (not (ext:tool-result-error-p result))
                      "the lifted tool is callable after the consent flow")))
           (ext:retract-manifest handle protocol context)))))))
