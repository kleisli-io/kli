(in-package #:kli/tests)

(in-suite all)

;;; Each dangerous chokepoint denies a restricted subject and admits the granted
;;; one. The ambient default is the narrow default subject, so every test binds
;;; the subject it exercises explicitly.

(test eval-tool-grants-on-image-eval-alone
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
      (let ((result (ext:invoke-tool protocol :eval '(:form "(+ 1 2)") context)))
        (is (not (ext:tool-result-error-p result)))
        (is (string= "3"
                     (getf (first (ext:tool-result-content result)) :text)))))
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (ext:invoke-tool protocol :eval '(:form "(+ 1 2)") context)))))

(test (compact-requires-compact-cap :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (bind-agent-session-mode context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (agent-session:compact-agent-session service :default-mode context)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:agent/session/compact))))
        (is (null (agent-session:compact-agent-session service :default-mode
                                                       context)))))))

(test (rename-requires-submit :fixture interactive-authority)
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (let ((service (bind-agent-session-mode context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (agent-session:rename-agent-session service :default-mode "x" context)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:agent/session/submit))))
        (is (string= "renamed"
                     (agent-session:rename-agent-session service :default-mode
                                                         "renamed" context)))))))

(test (forget-requires-forget-secret :fixture interactive-authority)
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (let ((store (credential-store context)))
        (auth:write-credential-record "fp" (auth:static-credential-record "sk-forget")
                                      path)
        (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
          (signals ext:capability-denied
            (auth:forget-credential store "fp" context path)))
        (is (auth:read-credential-record "fp" path))
        (let ((ext:*call-subject*
                (ext:make-subject :capabilities '(:auth/forget-secret))))
          (is (string= "fp" (auth:forget-credential store "fp" context path))))
        (is (null (auth:read-credential-record "fp" path)))))))

(test (set-static-gate-precedes-write :fixture interactive-authority)
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (with-temp-credentials (path)
      (let ((store (credential-store context)))
        (auth:set-static-credential store "regp" "sk-original" context path)
        (is (string= "sk-original"
                     (gethash "key" (auth:read-credential-record "regp" path))))
        (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
          (signals ext:capability-denied
            (auth:set-static-credential store "regp" "sk-overwrite" context path)))
        (is (string= "sk-original"
                     (gethash "key"
                              (auth:read-credential-record "regp" path))))))))

(test (credential-value-requires-resolve-secret-availability-stays-ungated :fixture interactive-authority)
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (let ((store (credential-store context))
          (static-ref (auth:make-static-credential-reference "sp" "sk-secret-value"))
          (env-ref (auth:make-env-credential-reference "ep" "PATH")))
      (auth:register-credential-reference store static-ref context)
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (is (auth::reference-available-p static-ref))
        (is (auth::reference-available-p env-ref))
        (is (auth:credential-available-p store "sp"))
        (signals ext:capability-denied
          (auth:credential-reference-value static-ref)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:auth/resolve-secret))))
        (is (string= "sk-secret-value"
                     (auth:credential-reference-value static-ref)))))))

(test recode-behavior-requires-hotpatch
  (let ((cell (tui-core:make-behavior-cell
               :id :gate-symmetry-cell
               :function (lambda (x) (list :v x)))))
    (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
      (signals ext:capability-denied
        (tui-core:recode-behavior cell :function (lambda (x) (list :v2 x)))))
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:behavior/hotpatch))))
      (signals ext:capability-denied
        (tui-core:recode-behavior cell :state '(:s 1))))
    (let ((ext:*call-subject*
            (ext:make-subject :capabilities '(:behavior/hotpatch
                                              :behavior/state))))
      (is (eq cell
              (tui-core:recode-behavior cell
                                        :function (lambda (x) (list :v3 x))
                                        :state '(:s 2)))))))

(test user-extension-load-requires-image-eval
  (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
    (signals ext:capability-denied
      (app::load-files-for-unit '(:single #p"/nonexistent-kli-ext.lisp")))))

(test recode-agent-loop-behavior-requires-hotpatch
  (multiple-value-bind (context protocol) (agent-loop-test-context)
    (declare (ignore protocol))
    (let* ((selection (agent-loop-register-model context
                                                 "agent-provider"
                                                 "agent-model"))
           (agent (make-agent-loop-session-agent context selection)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (signals ext:capability-denied
          (agents:recode-agent-loop-behavior agent
                                             :steering-delivery-policy :one-at-a-time)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:behavior/hotpatch))))
        (is (eq agent
                (agents:recode-agent-loop-behavior
                 agent :steering-delivery-policy :one-at-a-time)))))))

(test (oauth-secret-readable-only-through-the-gated-value-interface :fixture interactive-authority)
  ;; The raw access/refresh slot readers are not external, so the sole interface
  ;; read of an oauth secret is the gated credential-reference-value. A subject
  ;; can hold the live reference (find-credential-references stays open for
  ;; availability) yet cannot extract the secret without :auth/resolve-secret.
  (is (eq :internal
          (nth-value 1 (find-symbol "OAUTH-CREDENTIAL-REFERENCE-ACCESS"
                                    '#:kli/auth/core))))
  (is (eq :internal
          (nth-value 1 (find-symbol "OAUTH-CREDENTIAL-REFERENCE-REFRESH"
                                    '#:kli/auth/core))))
  (multiple-value-bind (context protocol) (auth-only-context)
    (declare (ignore protocol))
    (let ((store (credential-store context))
          (ref (auth:make-oauth-credential-reference
                "op" :access "oauth-secret-access" :refresh "oauth-secret-refresh"
                :expires (+ (get-universal-time) 3600))))
      (auth:register-credential-reference store ref context)
      (let ((ext:*call-subject* (ext:make-subject :capabilities '())))
        (is (member ref (auth:find-credential-references store "op")))
        (is (auth:credential-available-p store "op"))
        (signals ext:capability-denied
          (auth:credential-reference-value ref)))
      (let ((ext:*call-subject*
              (ext:make-subject :capabilities '(:auth/resolve-secret))))
        (is (string= "oauth-secret-access"
                     (auth:credential-reference-value ref)))))))
