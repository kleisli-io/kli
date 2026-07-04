(in-package #:kli/tests)

(in-suite all)

(defparameter *compatible-canned-json*
  (format nil "~{~A~%~}"
          '("{"
            "  \"groq\": {"
            "    \"base-url\": \"https://api.groq.com/openai/v1\","
            "    \"api\": \"openai-completions\","
            "    \"key-env\": \"KLI_TEST_GROQ_KEY\","
            "    \"options\": {"
            "      \"reasoning-effort\": {\"type\": \"enum\", \"values\": [\"off\", \"low\", \"high\"], \"default\": \"off\"}"
            "    },"
            "    \"models\": [{\"id\": \"llama-3.3-70b\", \"name\": \"Llama 3.3 70B\","
            "                  \"context-window\": 131072}]"
            "  },"
            "  \"together\": {"
            "    \"base-url\": \"https://api.together.xyz/v1\","
            "    \"api\": \"openai-responses\","
            "    \"url-path\": \"/responses\","
            "    \"key-env\": \"KLI_TEST_TOGETHER_KEY\","
            "    \"headers\": {\"x-org\": \"kli\"},"
            "    \"options\": {"
            "      \"reasoning-effort\": {\"type\": \"enum\", \"values\": [\"off\", \"minimal\", \"low\", \"medium\", \"high\", \"xhigh\"], \"default\": \"off\"}"
            "    },"
            "    \"models\": [{\"id\": \"deepseek-r1\"}]"
            "  }"
            "}"))
  "Two custom providers: groq (completions) and together (responses).")

(defmacro with-temp-providers ((path-var json-string) &body body)
  `(let ((,path-var (merge-pathnames
                     (format nil "kli-providers-test-~D/providers.json"
                             (incf *auth-test-seq*))
                     (uiop:temporary-directory))))
     (ensure-directories-exist ,path-var)
     (with-open-file (out ,path-var :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
       (write-string ,json-string out))
     (let ((compatible:*providers-path* ,path-var))
       (unwind-protect (progn ,@body)
         (uiop:delete-directory-tree
          (uiop:pathname-directory-pathname ,path-var)
          :validate (constantly t)
          :if-does-not-exist :ignore)))))

(defun compatible-schema (schemas option-id)
  (find option-id schemas
        :key #'models:model-option-schema-option-id
        :test #'string=))

(defmacro without-compatible-provider-warnings (&body body)
  `(handler-bind ((warning (lambda (warning)
                             (declare (ignore warning))
                             (muffle-warning))))
     ,@body))

(test compatible-parse-provider-configs-reads-two-specs
  (let* ((specs (compatible:parse-provider-configs *compatible-canned-json*))
         (groq (find "groq" specs :key (lambda (s) (getf s :provider-id)) :test #'string=))
         (together (find "together" specs :key (lambda (s) (getf s :provider-id)) :test #'string=)))
    (is (= 2 (length specs)))
    (is (eq :openai-completions (getf groq :api)))
    (is (string= "https://api.groq.com/openai/v1" (getf groq :base-url)))
    (is (string= "KLI_TEST_GROQ_KEY" (getf groq :key-env)))
    (is (null (getf groq :url-path)))
    (let* ((m (first (getf groq :models)))
           (reasoning (compatible-schema (getf m :option-schemas)
                                         "reasoning-effort")))
      (is (string= "llama-3.3-70b" (getf m :id)))
      (is (string= "Llama 3.3 70B" (getf m :name)))
      (is (= 131072 (getf m :context-window)))
      (is (not (null reasoning)))
      (is (equal '(:off :low :high)
                 (models:model-option-schema-values reasoning)))
      (is (eq :off (models:model-option-schema-default reasoning))))
    (is (eq :openai-responses (getf together :api)))
    (is (string= "/responses" (getf together :url-path)))
    (is (string= "KLI_TEST_TOGETHER_KEY" (getf together :key-env)))
    (is (string= "kli" (cdr (assoc "x-org" (getf together :headers) :test #'string=))))
    (let* ((m (first (getf together :models)))
           (reasoning (compatible-schema (getf m :option-schemas)
                                         "reasoning-effort")))
      (is (string= "deepseek-r1" (getf m :id)))
      (is (string= "deepseek-r1" (getf m :name)))
      (is (not (null reasoning))))))

(test compatible-provider-model-option-override-replaces-provider-option
  (let* ((json (format nil "~{~A~%~}"
                       '("{"
                         "  \"custom\": {"
                         "    \"base-url\": \"https://example.invalid/v1\","
                         "    \"options\": {"
                         "      \"reasoning-effort\": {\"type\": \"enum\", \"values\": [\"off\", \"low\", \"medium\", \"high\"], \"default\": \"off\"}"
                         "    },"
                         "    \"models\": [{"
                         "      \"id\": \"override-model\","
                         "      \"options\": {"
                         "        \"reasoning-effort\": {\"type\": \"enum\", \"values\": [\"off\", \"high\"], \"default\": \"high\"}"
                         "      }"
                         "    }]"
                         "  }"
                         "}")))
         (spec (first (compatible:parse-provider-configs json)))
         (model (first (getf spec :models)))
         (schema (compatible-schema (getf model :option-schemas)
                                   "reasoning-effort")))
    (is (equal '(:off :high) (models:model-option-schema-values schema)))
    (is (eq :high (models:model-option-schema-default schema)))))

(test compatible-provider-model-option-null-removes-inherited-option
  (let* ((json (format nil "~{~A~%~}"
                       '("{"
                         "  \"custom\": {"
                         "    \"base-url\": \"https://example.invalid/v1\","
                         "    \"options\": {"
                         "      \"reasoning-effort\": {\"type\": \"enum\", \"values\": [\"off\", \"low\"], \"default\": \"off\"},"
                         "      \"text-verbosity\": {\"type\": \"enum\", \"values\": [\"low\", \"medium\", \"high\"], \"default\": \"medium\"}"
                         "    },"
                         "    \"models\": [{\"id\": \"quiet-model\", \"options\": {\"text-verbosity\": null}}]"
                         "  }"
                         "}")))
         (spec (first (compatible:parse-provider-configs json)))
         (model (first (getf spec :models)))
         (schemas (getf model :option-schemas)))
    (is (not (null (compatible-schema schemas "reasoning-effort"))))
    (is (null (compatible-schema schemas "text-verbosity")))))

(test compatible-provider-invalid-option-schema-skips-only-that-provider
  (let* ((json (format nil "~{~A~%~}"
                       '("{"
                         "  \"bad\": {"
                         "    \"base-url\": \"https://bad.invalid/v1\","
                         "    \"options\": {"
                         "      \"reasoning-effort\": {\"type\": \"enum\", \"values\": [\"turbo\"], \"default\": \"turbo\"}"
                         "    },"
                         "    \"models\": [{\"id\": \"bad-model\"}]"
                         "  },"
                         "  \"good\": {"
                         "    \"base-url\": \"https://good.invalid/v1\","
                         "    \"options\": {"
                         "      \"reasoning-effort\": {\"type\": \"enum\", \"values\": [\"off\", \"high\"], \"default\": \"off\"}"
                         "    },"
                         "    \"models\": [{\"id\": \"good-model\"}]"
                         "  }"
                         "}")))
         (specs (without-compatible-provider-warnings
                  (compatible:parse-provider-configs json))))
    (is (= 1 (length specs)))
    (is (string= "good" (getf (first specs) :provider-id)))))

(test compatible-provider-legacy-thinking-is-ignored
  (let* ((json (format nil "~{~A~%~}"
                       '("{"
                         "  \"legacy\": {"
                         "    \"base-url\": \"https://legacy.invalid/v1\","
                         "    \"models\": [{\"id\": \"legacy-model\", \"thinking\": true}]"
                         "  }"
                         "}")))
         (spec (first (compatible:parse-provider-configs json)))
         (model (first (getf spec :models))))
    (is (null (getf model :option-schemas)))))

(test compatible-provider-options-reject-transport-profile-internals
  (let* ((json (format nil "~{~A~%~}"
                       '("{"
                         "  \"bad\": {"
                         "    \"base-url\": \"https://bad.invalid/v1\","
                         "    \"options\": {"
                         "      \"developer-role\": {\"type\": \"boolean\", \"default\": true}"
                         "    },"
                         "    \"models\": [{\"id\": \"bad-model\"}]"
                         "  }"
                         "}")))
         (specs (without-compatible-provider-warnings
                  (compatible:parse-provider-configs json))))
    (is (null specs))))

(test (compatible-provider-gates-each-model-on-its-key-env :fixture interactive-authority)
  "Each provider's catalogue stays hidden until its own key-env is set. With no
keys both are hidden. With the groq key only, groq appears and together stays
hidden. With both keys, both appear."
  (with-temp-providers (ppath *compatible-canned-json*)
    (multiple-value-bind (context protocol) (provider-baseline-context)
      (declare (ignore protocol))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv "KLI_TEST_GROQ_KEY")
               (sb-posix:unsetenv "KLI_TEST_TOGETHER_KEY")
               (install-extension context compatible:*compatible-provider-extension-manifest*)
               (let ((registry (model-registry context))
                     (store (credential-store context)))
                 (is (not (null (models:find-model-provider registry "groq"))))
                 (is (not (null (models:find-model-provider registry "together"))))
                 (is (not (null (rt:find-model-stream-adapter
                                 (model-runtime-service context) :openai-completions))))
                 (is (not (null (rt:find-model-stream-adapter
                                 (model-runtime-service context) :openai-responses))))
                 (is (zerop (count-provider-available "groq" registry store context)))
                 (is (zerop (count-provider-available "together" registry store context)))
                 (setf (uiop:getenv "KLI_TEST_GROQ_KEY") "sk-groq")
                 (is (= 1 (count-provider-available "groq" registry store context)))
                 (is (zerop (count-provider-available "together" registry store context)))
                 (setf (uiop:getenv "KLI_TEST_TOGETHER_KEY") "sk-together")
                 (is (= 1 (count-provider-available "groq" registry store context)))
                 (is (= 1 (count-provider-available "together" registry store context)))))
          (sb-posix:unsetenv "KLI_TEST_GROQ_KEY")
          (sb-posix:unsetenv "KLI_TEST_TOGETHER_KEY"))))))

(test (compatible-provider-restores-persisted-static :fixture interactive-authority)
  (with-temp-providers (ppath *compatible-canned-json*)
    (multiple-value-bind (context protocol) (provider-baseline-context)
      (declare (ignore protocol))
      (with-temp-credentials (cpath)
        (unwind-protect
             (progn
               (sb-posix:unsetenv "KLI_TEST_GROQ_KEY")
               (sb-posix:unsetenv "KLI_TEST_TOGETHER_KEY")
               (auth:write-credential-record
                "groq" (auth:static-credential-record "sk-groq-persist") cpath)
               (install-extension context compatible:*compatible-provider-extension-manifest*)
               (let ((registry (model-registry context))
                     (store (credential-store context)))
                 (is (= 1 (count-provider-available "groq" registry store context)))
                 (is (string= "sk-groq-persist"
                              (auth:resolved-credential-value
                               (auth:resolve-credential store "groq" context))))))
          (sb-posix:unsetenv "KLI_TEST_GROQ_KEY")
          (sb-posix:unsetenv "KLI_TEST_TOGETHER_KEY"))))))

(test (compatible-provider-deactivate-drains-every-spec :fixture interactive-authority)
  "Deactivating the compatible extension drains each spec's contribution-state:
both providers and their models leave the registry, both adapter references
release, and both auth-providers leave the store."
  (with-temp-providers (ppath *compatible-canned-json*)
    (multiple-value-bind (context protocol) (provider-baseline-context)
      (with-temp-credentials (cpath)
        (let* ((registry (model-registry context))
               (store (credential-store context))
               (runtime (model-runtime-service context))
               (handle (install-extension
                        context
                        compatible:*compatible-provider-extension-manifest*)))
          (is (not (null (models:find-model-provider registry "groq"))))
          (is (not (null (models:find-model-provider registry "together"))))
          (is (not (null (rt:find-model-stream-adapter runtime :openai-completions))))
          (is (not (null (rt:find-model-stream-adapter runtime :openai-responses))))
          (ext:deactivate-extension protocol handle context)
          (is (null (models:find-model-provider registry "groq")))
          (is (null (models:find-model-provider registry "together")))
          (is (null (models:find-model-definition registry "groq" "llama-3.3-70b")))
          (is (null (models:find-model-definition registry "together" "deepseek-r1")))
          (is (null (rt:find-model-stream-adapter runtime :openai-completions)))
          (is (null (rt:find-model-stream-adapter runtime :openai-responses)))
          (is (null (auth:find-auth-provider store "groq")))
          (is (null (auth:find-auth-provider store "together"))))))))
