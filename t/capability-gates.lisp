(in-package #:kli/tests)

(in-suite all)

(defun run-test-gated-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool parameters context call-id on-update))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "ran"))))

(ext:defextension test-gated-tool
  (:provides
   (tool gated
     :label "Gated"
     :description "Tool requiring two capabilities for gate testing."
     :parameters '(:object ())
     :runner #'run-test-gated-tool
     :metadata '(:capabilities (:test/cap-a :test/cap-b)))))

(ext:defextension test-ungated-tool
  (:provides
   (tool ungated
     :label "Ungated"
     :description "Tool declaring no capabilities."
     :parameters '(:object ())
     :runner #'run-test-gated-tool)))

(test invoke-tool-rejects-subject-without-tool-capability
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-gated-tool-extension-manifest*)
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :gated '() context)))))

(test invoke-tool-rejects-on-any-missing-capability
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-gated-tool-extension-manifest*)
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:test/cap-a))))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :gated '() context)))))

(test invoke-tool-passes-subject-with-all-tool-capabilities
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-gated-tool-extension-manifest*)
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:test/cap-a :test/cap-b))))
      (let ((result (ext:invoke-tool protocol :gated '() context)))
        (is (not (ext:tool-result-error-p result)))
        (is (string= "ran"
                     (getf (first (ext:tool-result-content result))
                           :text)))))))

(test invoke-tool-rejects-default-narrow-subject
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-gated-tool-extension-manifest*)
    (signals ext:capability-denied
      (ext:invoke-tool protocol :gated '() context))))

(test default-agent-subject-denied-by-tool-gate
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-gated-tool-extension-manifest*)
    (let* ((agent (agents:make-agent nil nil nil nil nil :id :test-narrow-gate))
           (ext:*call-subject* (agents:agent-subject agent)))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :gated '() context)))))

(test invoke-tool-with-no-declared-capabilities-runs-under-empty-subject
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-ungated-tool-extension-manifest*)
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (let ((result (ext:invoke-tool protocol :ungated '() context)))
        (is (not (ext:tool-result-error-p result)))))))

(test activate-extension-rejects-subject-without-install-cap
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (ext:install-manifest *test-ungated-tool-extension-manifest*
                              protocol context)))))

(test activate-extension-accepts-subject-with-install-cap
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:manifest/install))))
      (finishes
        (ext:install-manifest *test-ungated-tool-extension-manifest*
                              protocol context)))))

(test deactivate-extension-rejects-subject-without-retract-cap
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (extension (install-extension context
                                       *test-ungated-tool-extension-manifest*)))
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:manifest/install))))
      (signals ext:capability-denied
        (ext:retract-manifest extension protocol context)))))

(test deactivate-extension-accepts-subject-with-retract-cap
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (extension (install-extension context
                                       *test-ungated-tool-extension-manifest*)))
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:manifest/retract))))
      (finishes
        (ext:retract-manifest extension protocol context)))))

(test recode-extension-rejects-subject-without-recode-cap
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (extension (install-extension context
                                       *test-ungated-tool-extension-manifest*)))
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:manifest/install
                                              :manifest/retract))))
      (signals ext:capability-denied
        (ext:recode-extension protocol extension extension context)))))

(test recode-extension-accepts-subject-with-image-recode
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (extension (install-extension context
                                       *test-ungated-tool-extension-manifest*)))
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:image/recode))))
      (finishes
        (ext:recode-extension protocol extension extension context)))))

(test image-recode-implies-install-and-retract
  (let ((subject (ext:make-subject :capabilities '(:image/recode))))
    (is (ext:check-capability subject :image/recode))
    (is (ext:check-capability subject :manifest/install))
    (is (ext:check-capability subject :manifest/retract))))

(test control-install-protocol-rejects-without-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((control (load-control-plane context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (control:control-install-protocol control
                                            :extension-protocol
                                            context))))))

(test control-install-protocol-accepts-with-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((control (load-control-plane context)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:protocol/create))))
        (finishes
          (control:control-install-protocol
           control
           (ext:make-extension-protocol :id :gate-test-protocol)
           context))))))

(test control-switch-protocol-rejects-without-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((control (load-control-plane context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (control:control-switch-protocol control :boot-protocol context))))))

(test control-switch-protocol-accepts-with-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((control (load-control-plane context)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:protocol/switch))))
        (finishes
          (control:control-switch-protocol control :boot-protocol context))))))

(test control-rollback-protocol-rejects-without-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((control (load-control-plane context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (control:control-rollback-protocol control context))))))

(test control-recover-protocol-rejects-without-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((control (load-control-plane context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (control:control-recover-protocol control context))))))

(test control-recover-protocol-accepts-with-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((control (load-control-plane context)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:protocol/recover))))
        (finishes
          (control:control-recover-protocol control context))))))

(test snapshot-context-rejects-subject-without-snapshot-cap
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (declare (ignore protocol))
    (install-extension context snapshot:*snapshot-extension-manifest*)
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (snapshot:snapshot-context context)))))

(test snapshot-context-accepts-subject-with-snapshot-cap
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (declare (ignore protocol))
    (install-extension context snapshot:*snapshot-extension-manifest*)
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:protocol/snapshot))))
      (finishes
        (snapshot:snapshot-context context)))))

(test (restore-active-protocol-rejects-subject-without-restore-cap :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extension context snapshot:*snapshot-extension-manifest*)
    (let ((snapshot (snapshot:snapshot-context context)))
      (kli:switch-protocol boot :boot-protocol context)
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (snapshot:restore-active-protocol context snapshot))))))

(test (restore-active-protocol-accepts-subject-with-restore-cap :fixture interactive-authority)
  (let* ((context (kli:make-kernel-host))
         (boot (kli:active-protocol context))
         (extension-protocol (kli:install-protocol boot
                                                   (ext:make-extension-protocol)
                                                   context)))
    (kli:switch-protocol boot (kli:object-id extension-protocol) context)
    (install-extension context snapshot:*snapshot-extension-manifest*)
    (let ((snapshot (snapshot:snapshot-context context)))
      (kli:switch-protocol boot :boot-protocol context)
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:protocol/restore))))
        (is (eq extension-protocol
                (snapshot:restore-active-protocol context snapshot)))))))

(test load-extension-source-rejects-without-image-eval
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (ext:load-extension-source
         context
         '(ext:defextension test-source-rejected
            (:provides)))))))

(test load-extension-source-accepts-with-image-eval-and-install
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((ext:*call-subject*
            (ext:make-subject
             :capabilities '(:image/eval :manifest/install))))
      (finishes
        (ext:load-extension-source
         context
         '(ext:defextension test-source-accepted
            (:provides)))))))

(test agent-defaults-to-narrow-subject
  (let ((agent (agents:make-agent nil nil nil nil nil :id :test-agent-default)))
    (is (not (typep (agents:agent-subject agent) 'ext:system-subject)))
    (is (null (ext:check-capability (agents:agent-subject agent) :file/write)))
    (is (null (ext:check-capability (agents:agent-subject agent)
                                    :process/exec)))))

(test agent-granted-capabilities-builds-restricted-subject
  (let ((agent (agents:make-agent nil nil nil nil nil
                                  :id :test-agent-granted
                                  :granted-capabilities
                                  '(:file/read :file/write))))
    (is (not (typep (agents:agent-subject agent) 'ext:system-subject)))
    (is (ext:check-capability (agents:agent-subject agent) :file/read))
    (is (ext:check-capability (agents:agent-subject agent) :file/write))
    (is (null (ext:check-capability (agents:agent-subject agent)
                                    :image/eval)))))

(test agent-explicit-subject-overrides-granted-capabilities
  (let* ((subject (ext:make-subject :capabilities '(:only)))
         (agent (agents:make-agent nil nil nil nil nil
                                   :id :test-agent-explicit
                                   :subject subject
                                   :granted-capabilities
                                   '(:ignored))))
    (is (eq subject (agents:agent-subject agent)))
    (is (ext:check-capability (agents:agent-subject agent) :only))
    (is (null (ext:check-capability (agents:agent-subject agent)
                                    :ignored)))))

(test capabilities-subject-absent-yields-nil
  "An absent key returns NIL with a NIL present flag, so the caller applies its
own fallback rather than an empty grant."
  (multiple-value-bind (subject present) (ext:capabilities-subject nil)
    (is (null subject))
    (is (null present))))

(test capabilities-subject-grants-exactly-the-listed-capabilities
  (multiple-value-bind (subject present)
      (ext:capabilities-subject #("file/read" "file/edit"))
    (is-true present)
    (is (not (null subject)))
    (is (not (typep subject 'ext:system-subject)))
    (is (ext:check-capability subject :file/read))
    (is (ext:check-capability subject :file/edit))
    (is (null (ext:check-capability subject :process/exec)))
    (is (null (ext:check-capability subject :image/eval)))))

(test capabilities-subject-empty-vector-denies-gated-tools
  "An empty array is present and grants nothing -- the bottom subject, denying
every gated tool."
  (multiple-value-bind (subject present) (ext:capabilities-subject #())
    (is-true present)
    (is (not (null subject)))
    (is (not (typep subject 'ext:system-subject)))
    (is (null (ext:check-capability subject :file/read)))))

(test capabilities-subject-scalar-string-is-singleton-shorthand
  "A scalar string is the one-capability shorthand: it grants exactly that
capability, marks the key present, and does not warn."
  (let ((warned nil))
    (multiple-value-bind (subject present)
        (handler-bind ((warning (lambda (w) (declare (ignore w))
                                  (setf warned t) (muffle-warning))))
          (ext:capabilities-subject "file/read"))
      (is (not warned) "a scalar string is valid and must not warn")
      (is-true present)
      (is (not (typep subject 'ext:system-subject)))
      (is (ext:check-capability subject :file/read))
      (is (null (ext:check-capability subject :process/exec))))))

(test capabilities-subject-case-folds-capability-strings
  "Capability strings normalize through the extension-id normalizer, so
\"file/read\" and \"FILE/READ\" denote the same atom."
  (dolist (spec '("file/read" "FILE/READ"))
    (let ((subject (ext:capabilities-subject spec)))
      (is (ext:check-capability subject :file/read)
          "~A grants :file/read" spec))))

(test capabilities-subject-all-invalid-warns-and-denies
  "A present value with no valid entry warns and yields the bottom subject:
presence opts into enforcement, so a bad value denies rather than disables."
  (signals warning (ext:capabilities-subject #(7)))
  (signals warning (ext:capabilities-subject 7))
  (multiple-value-bind (subject present)
      (handler-bind ((warning #'muffle-warning))
        (ext:capabilities-subject #(7)))
    (is-true present)
    (is (not (typep subject 'ext:system-subject)))
    (is (null (ext:check-capability subject :file/read))))
  (multiple-value-bind (subject present)
      (handler-bind ((warning #'muffle-warning))
        (ext:capabilities-subject 7))
    (is-true present)
    (is (null (ext:check-capability subject :file/read)))))

(test capabilities-subject-mixed-valid-and-invalid-keeps-valid-subset
  "Mixed entries warn and grant only the valid subset."
  (signals warning (ext:capabilities-subject #("file/read" 7)))
  (multiple-value-bind (subject present)
      (handler-bind ((warning #'muffle-warning))
        (ext:capabilities-subject #("file/read" 7)))
    (is-true present)
    (is (ext:check-capability subject :file/read))
    (is (null (ext:check-capability subject :process/exec)))))

(test capabilities-subject-nested-array-entry-is-invalid
  "A nested array is one invalid entry: it warns and is dropped while the flat
string entries still apply."
  (let ((value (vector "file/read" (vector "x"))))
    (signals warning (ext:capabilities-subject value))
    (multiple-value-bind (subject present)
        (handler-bind ((warning #'muffle-warning))
          (ext:capabilities-subject value))
      (is-true present)
      (is (ext:check-capability subject :file/read)))))

(test tools-standard-implies-the-tool-capabilities
  (let ((subject (ext:make-subject :capabilities '(:tools/standard))))
    (is (ext:check-capability subject :file/read))
    (is (ext:check-capability subject :file/write))
    (is (ext:check-capability subject :file/edit))
    (is (ext:check-capability subject :process/exec))
    (is (null (ext:check-capability subject :image/eval)))))

(test auth-register-rejects-subject-without-register-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extension context auth:*auth-extension-manifest*)
    (let* ((store (credential-store context))
           (provider (auth:make-auth-provider "test-no-cap"
                                              :display-name "No-cap"))
           (ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (auth:register-auth-provider store provider context)))))

(test auth-register-accepts-subject-with-register-cap
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extension context auth:*auth-extension-manifest*)
    (let* ((store (credential-store context))
           (provider (auth:make-auth-provider "test-with-cap"
                                              :display-name "With-cap"))
           (ext:*call-subject*
             (ext:make-subject
              :capabilities '(:auth/register-reference))))
      (finishes
        (auth:register-auth-provider store provider context)))))

(test auth-inspect-rejects-subject-without-read-metadata
  (let* ((context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (install-extension context auth:*auth-extension-manifest*)
    (let* ((store (credential-store context))
           (ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (auth:inspect-auth-store store)))))

(test context-inspect-rejects-subject-without-read-cap
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (ctx:inspect-agent-context agent-context))))))

(test context-stage-rejects-subject-without-stage-edit-cap
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context))
           (patch (ctx:make-append-message-patch
                   (sess:make-user-message "x"))))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (ctx:stage-context-patch agent-context patch))))))

(test context-abort-rejects-subject-without-stage-edit-cap
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (ctx:abort-context-patches agent-context))))))

(test (context-commit-rejects-subject-without-commit-edit-cap :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context))
           (patch (ctx:make-append-message-patch
                   (sess:make-user-message "x"))))
      (ctx:stage-context-patch agent-context patch)
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:context/stage-edit))))
        (signals ext:capability-denied
          (ctx:commit-context-patches agent-context context))))))

(test context-seal-rejects-subject-without-seal-cap
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (ctx:seal-context-projection agent-context context))))))

(test (context-commit-records-call-subject-as-patch-set-actor :fixture interactive-authority)
  (multiple-value-bind (context protocol)
      (context-lens-test-context)
    (declare (ignore protocol))
    (let* ((store (session-log-store context))
           (session (context-lens-test-session context))
           (agent-context (ctx:make-agent-context session store context))
           (patch (ctx:make-append-message-patch
                   (sess:make-user-message "x"))))
      (ctx:stage-context-patch agent-context patch)
      (let* ((subject (ext:make-subject
                       :capabilities '(:context/commit-edit)))
             (ext:*call-subject* subject)
             (patch-set (ctx:commit-context-patches agent-context context)))
        (is (eq subject (ctx:context-patch-actor patch-set)))))))

(test eval-capability-confers-first-party-image-authority
  "Holding :image/eval confers the full first-party gated authority, so the
lattice never advertises in-image eval as confined. It stops short of the
strictly stronger debug atom."
  (let ((subject (ext:make-subject :capabilities '(:image/eval))))
    (dolist (atom '(:file/write :file/edit :process/exec
                    :extension/spawn-process))
      (is (ext:check-capability subject atom)
          "an :image/eval grant covers ~A" atom))
    (is (null (ext:check-capability subject :image/debug))
        "eval does not silently confer the stronger debug authority")))

(test capabilities-overlay-granting-eval-cannot-withhold-first-party-authority
  "The settings/profile capability path can only grant eval together with the
first-party authority it implies, so no overlay confers eval while pretending
to withhold write/bash."
  (let ((subject (ext:capabilities-subject #("image/eval"))))
    (is (ext:check-capability subject :file/write))
    (is (ext:check-capability subject :process/exec)))
  (let ((reachable (ext:capabilities-subject #("manifest/install-remote"))))
    (is (ext:check-capability reachable :file/write)
        "a capability that reaches eval by implication also confers write")))

(test image-family-forms-a-strict-total-order
  "The :image/* atoms are a strict total order inspect <= eval <= debug: debug
confers eval and inspect, eval confers inspect but not debug, inspect confers
neither. This is the lattice basis attenuated debug delegation rests on -- confer
rejects a too-strong image conferral by grant-covers-p monotonicity. Asserted at
the grant algebra and carried through the eval-escalation coverage."
  (flet ((held (&rest atoms)
           (ext:make-grant :capabilities (ext:expand-implications atoms)))
         (want (atom)
           (ext:make-grant :capabilities (list atom))))
    (is (ext:grant-covers-p (held :image/debug) (want :image/eval)))
    (is (ext:grant-covers-p (held :image/debug) (want :image/inspect)))
    (is (ext:grant-covers-p (held :image/eval) (want :image/inspect)))
    (is (not (ext:grant-covers-p (held :image/eval) (want :image/debug))))
    (is (not (ext:grant-covers-p (held :image/inspect) (want :image/eval))))
    (is (not (ext:grant-covers-p (held :image/inspect) (want :image/debug)))))
  (let ((debug (ext:make-subject :capabilities '(:image/debug))))
    (dolist (atom '(:image/eval :image/inspect :file/write :process/exec))
      (is (ext:check-capability debug atom)
          "a :image/debug grant covers ~A" atom))))
