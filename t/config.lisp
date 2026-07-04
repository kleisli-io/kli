(in-package #:kli/tests)
(in-suite all)

(defvar *config-test-counter* 0)

(defun temp-config-root ()
  ;; truename collapses macOS's /tmp -> /private/tmp symlink so derived paths
  ;; match locate-dominating's truenamed result (fails on darwin otherwise).
  (truename
   (ensure-directories-exist
    (merge-pathnames (format nil "kli-config-test-~D-~D/"
                             (get-universal-time)
                             (incf *config-test-counter*))
                     #p"/tmp/"))))

(defun make-config-test-dir (root &rest components)
  (let ((dir (if components
                 (merge-pathnames (format nil "~{~A/~}" components) root)
                 root)))
    (ensure-directories-exist dir)
    dir))

(defun write-config-test-file (path string)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (write-string string out))
  path)

(defun read-config-settings-quietly (path)
  (handler-bind ((warning #'muffle-warning))
    (config:read-settings-file path)))

(defun call-with-resource-root (key dir thunk)
  "Run THUNK with KEY resolving to DIR as an extra resource root, restoring
the global roots table on exit."
  (let ((roots (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k roots) v))
             buildlisp/resources::*resource-roots*)
    (setf (gethash key roots) (pathname dir))
    (let ((buildlisp/resources::*resource-roots* roots))
      (funcall thunk))))

(ext:defextension bundle-fixture
  (:metadata (:title "Bundled-resource test fixture.")))

(test config-directory-ancestors-walk-to-filesystem-root
  (let ((ancestors (config:directory-ancestors #p"/tmp/a/b/c/")))
    (is (equal #p"/tmp/a/b/c/" (first ancestors)))
    (is (member #p"/tmp/a/b/" ancestors :test #'equal))
    (is (member #p"/tmp/" ancestors :test #'equal))
    (is (equal #p"/" (first (last ancestors))))))

(test config-project-ancestors-stop-at-repo-root
  (let* ((root (temp-config-root))
         (repo (make-config-test-dir root "repo"))
         (leaf (make-config-test-dir root "repo" "sub" "leaf")))
    (make-config-test-dir root "repo" ".git")
    (let ((ancestors (config:project-ancestors :start leaf)))
      (is (= 3 (length ancestors)))
      (is (equal leaf (first ancestors)))
      (is (equal repo (first (last ancestors)))))
    (let ((unbounded (config:project-ancestors :start (make-config-test-dir
                                                       root "loose"))))
      (is (member #p"/" unbounded :test #'equal)))))

(test config-locate-dominating-finds-nearest-within-repo
  (let* ((root (temp-config-root))
         (leaf (make-config-test-dir root "repo" "sub" "leaf"))
         (near (make-config-test-dir root "repo" "sub" ".kli"))
         (far (make-config-test-dir root ".kli")))
    (make-config-test-dir root "repo" ".git")
    (is (equal near (config:locate-dominating ".kli/" :start leaf)))
    (is (null (config:locate-dominating "no-such-marker/" :start leaf))
        "a marker above the repo root must stay invisible")
    (is (equal far (config:locate-dominating ".kli/" :start root)))))

(test config-project-config-dir-resolves-nearest-kli
  (let* ((root (temp-config-root))
         (leaf (make-config-test-dir root "repo" "sub" "leaf"))
         (kli-dir (make-config-test-dir root "repo" ".kli")))
    (make-config-test-dir root "repo" ".git")
    (is (equal kli-dir (config:project-config-dir :start leaf)))
    (let* ((bare-root (temp-config-root))
           (bare (make-config-test-dir bare-root "repo" "bare")))
      (make-config-test-dir bare-root "repo" ".git")
      (is (null (config:project-config-dir :start bare))))))

(test config-read-settings-file-is-fail-soft
  (let ((root (temp-config-root)))
    (is (null (config:read-settings-file
               (merge-pathnames "absent.json" root))))
    (is (null (config:read-settings-file nil)))
    (is (null (read-config-settings-quietly
               (write-config-test-file
                (merge-pathnames "broken.json" root) "{not json"))))
    (is (null (read-config-settings-quietly
               (write-config-test-file
                (merge-pathnames "array.json" root) "[1, 2]"))))
    (let ((table (config:read-settings-file
                  (write-config-test-file
                   (merge-pathnames "good.json" root)
                   "{\"theme\": \"dark\"}"))))
      (is (hash-table-p table))
      (is (equal "dark" (gethash "theme" table))))))

(test config-merge-settings-follows-project-override-semantics
  (let ((global (com.inuoe.jzon:parse
                 "{\"theme\": \"dark\",
                   \"compaction\": {\"enabled\": true, \"reserveTokens\": 16384},
                   \"enabledModels\": [\"a\", \"b\"]}"))
        (project (com.inuoe.jzon:parse
                  "{\"compaction\": {\"reserveTokens\": 8192},
                    \"enabledModels\": [\"c\"]}")))
    (let ((merged (config:merge-settings global project)))
      (is (equal "dark" (gethash "theme" merged)))
      (is (eq t (gethash "enabled" (gethash "compaction" merged))))
      (is (= 8192 (gethash "reserveTokens" (gethash "compaction" merged))))
      (is (= 1 (length (gethash "enabledModels" merged)))
          "arrays replace wholesale, never merge")
      (is (= 16384 (gethash "reserveTokens" (gethash "compaction" global)))
          "merging must not mutate the inputs"))
    (is (eq global (config:merge-settings global nil)))
    (is (eq project (config:merge-settings nil project)))
    (is (null (config:merge-settings nil nil)))))

(test config-load-settings-merges-global-and-project
  (let* ((root (temp-config-root))
         (global-path (write-config-test-file
                       (merge-pathnames "global/settings.json" root)
                       "{\"theme\": \"dark\", \"editorPaddingX\": 1}"))
         (project-path (write-config-test-file
                        (merge-pathnames "project/settings.json" root)
                        "{\"editorPaddingX\": 2}")))
    (let ((merged (config:load-settings :global-path global-path
                                        :project-path project-path)))
      (is (equal "dark" (gethash "theme" merged)))
      (is (= 2 (gethash "editorPaddingX" merged))))
    (let ((global-only (config:load-settings :global-path global-path
                                             :project-path nil)))
      (is (= 1 (gethash "editorPaddingX" global-only))))
    (let ((empty (config:load-settings
                  :global-path (merge-pathnames "none-a.json" root)
                  :project-path (merge-pathnames "none-b.json" root))))
      (is (hash-table-p empty))
      (is (zerop (hash-table-count empty))))))

(test config-settings-value-walks-nested-keys
  (let ((settings (com.inuoe.jzon:parse
                   "{\"compaction\": {\"enabled\": false, \"reserveTokens\": 16384},
                     \"theme\": \"dark\"}")))
    (multiple-value-bind (value present)
        (config:settings-value settings "theme")
      (is (equal "dark" value))
      (is (eq t present)))
    (multiple-value-bind (value present)
        (config:settings-value settings "compaction" "reserveTokens")
      (is (= 16384 value))
      (is (eq t present)))
    (multiple-value-bind (value present)
        (config:settings-value settings "compaction" "enabled")
      (is (null value))
      (is (eq t present)
          "a stored false is present, distinct from a missing key"))
    (is (null (nth-value 1 (config:settings-value settings "missing"))))
    (is (null (nth-value 1 (config:settings-value
                            settings "compaction" "missing"))))
    (is (null (nth-value 1 (config:settings-value
                            settings "theme" "not-an-object"))))))

(test config-service-loads-and-reloads-settings
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "project" ".kli")))
    (write-config-test-file (merge-pathnames "settings.json" global-dir)
                            "{\"theme\": \"dark\"}")
    (let ((service (config:make-config-service :global-dir global-dir
                                               :project-dir project-dir)))
      (is (equal "dark" (config:settings-value
                         (config:config-service-settings service) "theme")))
      (write-config-test-file (merge-pathnames "settings.json" project-dir)
                              "{\"theme\": \"light\"}")
      (config:reload-settings service)
      (is (equal "light" (config:settings-value
                          (config:config-service-settings service) "theme"))))))

(test config-service-settings-overlay-is-a-third-layer
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "project" ".kli")))
    (write-config-test-file (merge-pathnames "settings.json" global-dir)
                            "{\"theme\": \"dark\", \"editorPaddingX\": 1}")
    (write-config-test-file (merge-pathnames "settings.json" project-dir)
                            "{\"theme\": \"light\"}")
    (let ((service (config:make-config-service :global-dir global-dir
                                               :project-dir project-dir))
          (overlay (com.inuoe.jzon:parse "{\"theme\": \"solarized\"}")))
      (is (equal "light" (config:settings-value
                          (config:config-service-settings service) "theme"))
          "project over global before any overlay")
      (is (null (config:set-settings-overlay service overlay))
          "no previous overlay on the first set")
      (is (equal "solarized"
                 (config:settings-value
                  (config:config-service-settings service) "theme"))
          "the overlay has the last word")
      (is (= 1 (config:settings-value
                (config:config-service-settings service) "editorPaddingX"))
          "keys absent from the overlay keep their file values")
      (config:reload-settings service)
      (is (equal "solarized"
                 (config:settings-value
                  (config:config-service-settings service) "theme"))
          "a reload re-reads the files but keeps the overlay layer")
      (let ((replacement (com.inuoe.jzon:parse "{\"theme\": \"grey\"}")))
        (is (eq overlay (config:set-settings-overlay service replacement))
            "a swap returns the previous overlay")
        (is (eq replacement (config:set-settings-overlay service nil))
            "clearing returns the replaced overlay"))
      (is (equal "light" (config:settings-value
                          (config:config-service-settings service) "theme"))
          "clearing the overlay restores the file view"))))

(test config-resource-paths-extend-global-with-project
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "project" ".kli"))
         (service (config:make-config-service :global-dir global-dir
                                              :project-dir project-dir)))
    (is (eq :prompts (config:register-resource-kind service :prompts "prompts/")))
    (is (null (config:resource-paths service :prompts)))
    (let ((global-prompts (make-config-test-dir global-dir "prompts")))
      (is (equal (list global-prompts)
                 (config:resource-paths service :prompts)))
      (let ((project-prompts (make-config-test-dir project-dir "prompts")))
        (is (equal (list global-prompts project-prompts)
                   (config:resource-paths service :prompts))
            "global first, project extends")))
    (is (= 2 (length (config:resource-paths service :prompts
                                            :existing-only nil))))
    (signals error (config:resource-paths service :unregistered))
    (let ((rootless (config:make-config-service :global-dir global-dir
                                                :project-dir nil)))
      (config:register-resource-kind rootless :prompts "prompts/")
      (is (= 1 (length (config:resource-paths rootless :prompts
                                              :existing-only nil)))
          "no project dir means only the global path"))))

(test config-extension-provides-capability-and-service
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (write-config-test-file (merge-pathnames "settings.json" global-dir)
                            "{\"theme\": \"dark\"}")
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extension context config:*config-extension-manifest*))
    (is (ext:capability-provided-p protocol :config))
    (let ((service (config:find-config-service context)))
      (is (typep service 'config:config-service))
      (is (equal global-dir (config:config-service-global-dir service)))
      (is (null (config:config-service-project-dir service)))
      (is (equal "dark" (config:settings-value
                         (config:config-service-settings service) "theme"))))
    (let ((provider (ext:require-capability-provider protocol :config
                                                     :contract :config/v1)))
      (is (equal "dark" (ext:provider-call provider :settings-value
                                           context "theme")))
      (is (hash-table-p (ext:provider-call provider :settings context))))))

(test config-settings-command-reports-paths-and-settings
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (write-config-test-file (merge-pathnames "settings.json" global-dir)
                            "{\"theme\": \"dark\"}")
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          config:*config-commands-extension-manifest*))
    (let ((text (command-result-text
                 context
                 (invoke-test-command context :settings))))
      (is (search (namestring global-dir) text))
      (is (search "theme" text))
      (is (search "dark" text)))))

;;; The :settings contribution kind.

(test settings-schema-validates-and-diagnoses
  (is (equal '(:string) (config:validate-settings-schema '(:string))))
  (signals error (config:validate-settings-schema '(:object ("k" (:strng)))))
  (signals error (config:validate-settings-schema '(:enum ("a") :default "b")))
  (signals error (config:validate-settings-schema '(:or (:string))))
  (let ((schema '(:object ("cap" (:integer :min 1 :default 2048))
                          ("ratio" (:number :min 0 :max 1))
                          ("level" (:enum ("stage" "run")))
                          ("binding" (:or (:enum ("current" "fork"))
                                          (:object ("task" (:string))))))))
    (is (null (config:settings-schema-diagnostics
               schema
               (com.inuoe.jzon:parse
                "{\"cap\": 2, \"binding\": {\"task\": \"t\"}, \"level\": \"run\"}")
               "x"))
        "a conforming subtree, union object arm included, is clean")
    (is (= 5 (length (config:settings-schema-diagnostics
                      schema
                      (com.inuoe.jzon:parse
                       "{\"cap\": 0, \"ratio\": 2, \"level\": \"leaf\",
                         \"binding\": \"pinned\", \"nope\": 1}")
                      "x")))
        "bound, type, enum, union, and unknown-key problems each diagnose")))

(ext:defextension settings-declaration-fixture
  (:provides
   (settings "skarlike"
     (:object ("toolResultCap" (:integer :min 1 :default 2048))
              ("cairn" (:object ("enabled" (:boolean :default t))
                                ("granularity" (:enum ("stage" "run")
                                                :default "stage"))))))))

(test settings-declaration-installs-validates-and-retracts
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (write-config-test-file
     (merge-pathnames "settings.json" global-dir)
     "{\"extensions\": {\"skarlike\": {\"toolResultCap\": 0}, \"mystery\": {}}}")
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj"))
          (extension nil)
          (warned '()))
      (install-extension context config:*config-extension-manifest*)
      (handler-bind ((warning (lambda (c)
                                (push (format nil "~A" c) warned)
                                (muffle-warning c))))
        (setf extension
              (install-extension
               context *settings-declaration-fixture-extension-manifest*)))
      (is (= 1 (length warned))
          "activation runs boot diagnostics over the loaded settings")
      (is (search "toolResultCap" (first warned)))
      (is (not (null (config:find-settings-declaration protocol "skarlike"))))
      (is (null (ext::protocol-grant-set protocol))
          "describes, never grants: installing a declaration confers nothing")
      (let ((report (config:extension-settings-report
                     protocol
                     (config:config-service-settings
                      (config:find-config-service context)))))
        (let ((entry (first (getf report :declarations))))
          (is (equal "skarlike" (getf entry :key)))
          (is (eq t (getf entry :present)))
          (is (= 1 (length (getf entry :diagnostics)))))
        (is (equal '("mystery") (getf report :undeclared))))
      (multiple-value-bind (value source)
          (config:declared-settings-value context "skarlike" "toolResultCap")
        (is (= 0 value))
        (is (eq :settings source)))
      (multiple-value-bind (value source)
          (config:declared-settings-value context "skarlike" "cairn" "granularity")
        (is (equal "stage" value))
        (is (eq :default source) "an absent key reads its declared default"))
      (is (null (nth-value 1 (config:declared-settings-value
                              context "skarlike" "cairn" "nope"))))
      (with-extension-load-authority
        (ext:retract-manifest extension (kli:active-protocol context) context))
      (is (null (config:find-settings-declaration protocol "skarlike"))))))

(test settings-command-reports-declared-extension-settings
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (write-config-test-file
     (merge-pathnames "settings.json" global-dir)
     "{\"extensions\": {\"skarlike\": {\"toolResultCap\": 64}}}")
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          config:*config-commands-extension-manifest*
                          *settings-declaration-fixture-extension-manifest*))
    (let ((text (command-result-text
                 context
                 (invoke-test-command context :settings))))
      (is (search "Extension settings:" text))
      (is (search "skarlike" text)))))

(test extension-settings-round-trip-merge-and-tiers
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "project" ".kli")))
    (write-config-test-file
     (merge-pathnames "settings.json" global-dir)
     "{\"extensions\": {\"skar\": {\"models\": {\"big\": \"p/one\"},
                        \"toolResultCap\": 1024,
                        \"cairn\": {\"enabled\": true}}}}")
    (write-config-test-file
     (merge-pathnames "settings.json" project-dir)
     "{\"extensions\": {\"skar\": {\"toolResultCap\": 4096}}}")
    (let ((service (config:make-config-service :global-dir global-dir
                                               :project-dir project-dir)))
      (flet ((skar-value (&rest keys)
               (apply #'config:settings-value
                      (config:config-service-settings service)
                      "extensions" "skar" keys)))
        (is (= 4096 (skar-value "toolResultCap"))
            "the project tier overrides key by key")
        (is (equal "p/one" (skar-value "models" "big"))
            "sibling keys the project does not name survive the merge")
        (is (eq t (skar-value "cairn" "enabled")))
        (config:set-settings-overlay
         service
         (com.inuoe.jzon:parse
          "{\"extensions\": {\"skar\": {\"cairn\": {\"granularity\": \"run\"}}}}"))
        (is (equal "run" (skar-value "cairn" "granularity"))
            "the overlay tier merges deepest-key-wins over the file tiers")
        (is (= 4096 (skar-value "toolResultCap")))
        (is (equal "p/one" (skar-value "models" "big")))))))
