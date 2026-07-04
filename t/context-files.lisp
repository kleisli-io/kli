(in-package #:kli/tests)
(in-suite all)

(defun write-context-test-file (dir name string)
  (write-config-test-file (merge-pathnames name dir) string))

(defun install-context-files-stack (context root)
  (let ((config:*global-config-dir* (make-config-test-dir root "global"))
        (config:*project-start-directory* (make-config-test-dir root "proj")))
    (install-extensions context
                        config:*config-extension-manifest*
                        context-files:*context-files-extension-manifest*)))

(defun context-files-session-agent (context)
  (let* ((service (kli:find-live-object (kli:context-registry context)
                                        :agent-session-service))
         (binding (gethash :default-mode
                           (agent-session:session-mode-bindings service))))
    (values service
            (kli:find-live-object
             (kli:context-registry context)
             (agent-session:mode-binding-agent-id binding)))))

(defun install-context-files-and-skills-stack (context root)
  "Install config, context-files, and skills over ROOT with an AGENTS.md and a
single installed skill, so both extensions contribute a system-prompt layer."
  (make-config-test-dir root "proj" ".git")
  (write-context-test-file (make-config-test-dir root "proj") "AGENTS.md"
                           "Project rules.")
  (write-installed-skill root)
  (let ((config:*global-config-dir* (make-config-test-dir root "global"))
        (config:*project-start-directory* (make-config-test-dir root "proj"))
        (skills:*user-agents-skills-directory*
          (make-config-test-dir root "user-agents-absent")))
    (install-extensions context
                        config:*config-extension-manifest*
                        context-files:*context-files-extension-manifest*
                        skills:*skills-extension-manifest*)))

(defun deactivate-extension-by-id (context id)
  (ext:deactivate-extension
   (kli:active-protocol context)
   (kli:find-live-object (kli:context-registry context) id)
   context))

(test context-files-candidate-precedence
  (let* ((root (temp-config-root))
         (both (make-config-test-dir root "both"))
         (claude-only (make-config-test-dir root "claude-only"))
         (empty (make-config-test-dir root "empty")))
    (write-context-test-file both "AGENTS.md" "agents body")
    (write-context-test-file both "CLAUDE.md" "claude body")
    (write-context-test-file claude-only "CLAUDE.md" "claude body")
    (let ((found (context-files:context-file-in-directory both)))
      (is (search "AGENTS.md" (getf found :path))
          "AGENTS.md outranks CLAUDE.md in the same directory")
      (is (equal "agents body" (getf found :content))))
    (let ((found (context-files:context-file-in-directory claude-only)))
      (is (search "CLAUDE.md" (getf found :path))))
    (is (null (context-files:context-file-in-directory empty)))))

(test context-files-oversized-file-treated-as-absent
  "A context file over *context-file-byte-limit* is treated as absent: the
   candidate walk falls through to the next name, and a directory whose only
   candidate is oversized contributes nothing."
  (let* ((root (temp-config-root))
         (both (make-config-test-dir root "fat-both"))
         (only (make-config-test-dir root "fat-only")))
    (write-context-test-file both "AGENTS.md"
                             (make-string 256 :initial-element #\x))
    (write-context-test-file both "CLAUDE.md" "claude body")
    (write-context-test-file only "AGENTS.md"
                             (make-string 256 :initial-element #\x))
    (let ((context-files::*context-file-byte-limit* 64))
      (let ((found (context-files:context-file-in-directory both)))
        (is (search "CLAUDE.md" (getf found :path))
            "an oversized AGENTS.md falls through to CLAUDE.md"))
      (is (null (context-files:context-file-in-directory only))
          "an oversized only candidate leaves the directory without context"))))

(test context-files-collection-order-and-dedup
  (let* ((root (temp-config-root))
         (global (make-config-test-dir root "global"))
         (repo (make-config-test-dir root "repo"))
         (mid (make-config-test-dir root "repo" "mid"))
         (start (make-config-test-dir root "repo" "mid" "start")))
    (make-config-test-dir root "repo" ".git")
    (write-context-test-file global "AGENTS.md" "global")
    (write-context-test-file repo "AGENTS.md" "root")
    (write-context-test-file mid "CLAUDE.md" "mid")
    (write-context-test-file start "AGENTS.md" "start")
    (let ((files (context-files:collect-context-files :global-dir global
                                                      :start start)))
      (is (equal '("global" "root" "mid" "start")
                 (mapcar (lambda (file) (getf file :content)) files))
          "global file first, then the walk outermost first, start last"))
    (let ((files (context-files:collect-context-files :global-dir mid
                                                      :start start)))
      (is (= 3 (length files))
          "a global dir inside the walk contributes exactly once"))))

(test context-files-walk-stops-at-repo-root
  (let* ((root (temp-config-root))
         (outside (make-config-test-dir root "outside"))
         (repo (make-config-test-dir root "outside" "repo"))
         (start (make-config-test-dir root "outside" "repo" "leaf")))
    (make-config-test-dir root "outside" "repo" ".git")
    (write-context-test-file outside "AGENTS.md" "above the repo")
    (write-context-test-file repo "AGENTS.md" "inside")
    (let ((files (context-files:collect-context-files :start start)))
      (is (equal '("inside")
                 (mapcar (lambda (file) (getf file :content)) files))
          "a file above the repo root stays invisible"))))

(test context-files-render-project-context
  (let ((files (list (list :path "/a/AGENTS.md" :content "First.")
                     (list :path "/b/CLAUDE.md" :content "Second."))))
    (is (equal (format nil "~%~%# Project Context~%~%Project-specific ~
                            instructions and guidelines:~%~%~
                            ## /a/AGENTS.md~%~%First.~%~%~
                            ## /b/CLAUDE.md~%~%Second.~%~%")
               (context-files:render-project-context files))))
  (is (equal "" (context-files:render-project-context '()))))

(test context-files-system-prompt-precedence
  (let* ((root (temp-config-root))
         (global (make-config-test-dir root "global"))
         (project (make-config-test-dir root "proj" ".kli"))
         (dirs (list global project)))
    (write-context-test-file global "SYSTEM.md" "global system")
    (is (equal "global system" (context-files:discover-system-prompt dirs)))
    (is (equal "global system"
               (context-files:discover-system-prompt (list global)))
        "a missing project directory falls back to the global file")
    (write-context-test-file project "SYSTEM.md" "project system")
    (is (equal "project system" (context-files:discover-system-prompt dirs))
        "the project file beats the global one")
    (write-context-test-file project "SYSTEM.md" (format nil " ~%  ~%"))
    (is (equal "global system" (context-files:discover-system-prompt dirs))
        "a whitespace-only file is treated as absent")
    (write-context-test-file global "SYSTEM.md" "")
    (is (null (context-files:discover-system-prompt dirs)))))

(test context-files-append-system-prompt-precedence
  (let* ((root (temp-config-root))
         (global (make-config-test-dir root "global"))
         (project (make-config-test-dir root "proj" ".kli"))
         (dirs (list global project)))
    (is (null (context-files:discover-append-system-prompt dirs)))
    (write-context-test-file global "APPEND_SYSTEM.md" "global append")
    (is (equal "global append"
               (context-files:discover-append-system-prompt dirs)))
    (write-context-test-file project "APPEND_SYSTEM.md" "project append")
    (is (equal "project append"
               (context-files:discover-append-system-prompt dirs)))))

(test (context-files-extension-adds-project-context-to-prompt :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (make-config-test-dir root "proj" ".git")
    (write-context-test-file (make-config-test-dir root "proj") "AGENTS.md"
                             "Always answer in haiku.")
    (install-context-files-stack context root)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (search "# Project Context" prompt))
        (is (search "## " prompt))
        (is (search "AGENTS.md" prompt))
        (is (search "Always answer in haiku." prompt)))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "again" context)
      (is (= 1 (count-substring "# Project Context"
                                (agents:agent-system-prompt agent)))
          "re-submitting never duplicates the context section"))))

(test (context-files-system-md-replaces-base-prompt :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (make-config-test-dir root "proj" ".git")
    (write-context-test-file (make-config-test-dir root "proj") "AGENTS.md"
                             "Project rules.")
    (write-context-test-file (make-config-test-dir root "proj" ".kli")
                             "SYSTEM.md" "You are a haiku engine.")
    (install-context-files-stack context root)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (not (search "You are kli" prompt))
            "SYSTEM.md discards the default base prompt")
        (is (eql 0 (search "You are a haiku engine." prompt)))
        (is (search "# Project Context" prompt)
            "the context section survives the replacement")
        (is (search "Project rules." prompt)))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "again" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (eql 0 (search "You are a haiku engine." prompt)))
        (is (= 1 (count-substring "# Project Context" prompt)))))))

(test (context-files-append-system-md-extends-base-prompt :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (make-config-test-dir root "proj" ".git")
    (write-context-test-file (make-config-test-dir root "proj" ".kli")
                             "APPEND_SYSTEM.md" "Trust the tests.")
    (install-context-files-stack context root)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let* ((prompt (agents:agent-system-prompt agent))
             (base-position (search "You are kli" prompt))
             (append-position (search "Trust the tests." prompt)))
        (is (not (null base-position)) "the default base prompt is retained")
        (is (not (null append-position)))
        (is (< base-position append-position)))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "again" context)
      (is (= 1 (count-substring "Trust the tests."
                                (agents:agent-system-prompt agent)))))))

(test (context-files-section-precedes-skills-advertisement :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (make-config-test-dir root "proj" ".git")
    (write-context-test-file (make-config-test-dir root "proj") "AGENTS.md"
                             "Project rules.")
    (write-installed-skill root)
    (let ((config:*global-config-dir* (make-config-test-dir root "global"))
          (config:*project-start-directory* (make-config-test-dir root "proj"))
          (skills:*user-agents-skills-directory*
            (make-config-test-dir root "user-agents-absent")))
      (install-extensions context
                          config:*config-extension-manifest*
                          context-files:*context-files-extension-manifest*
                          skills:*skills-extension-manifest*))
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (search "# Project Context" prompt))
        (is (search "<available_skills>" prompt))
        (is (< (search "# Project Context" prompt)
               (search "<available_skills>" prompt))
            "the context section lands before the skills advertisement")))))

(test (skills-advertisement-survives-context-files-retraction :fixture interactive-authority)
  "With both extensions active, retracting context-files first keeps the skills
advertisement. Layers retract by label, so dropping the earlier context layer
never strands the later advertisement layer."
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (install-context-files-and-skills-stack context root)
    (deactivate-extension-by-id context :context-files)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (search "<available_skills>" prompt)
            "the skills advertisement survives context-files retraction")
        (is (not (search "# Project Context" prompt))
            "the context section is gone")))))

(test (context-section-survives-skills-retraction :fixture interactive-authority)
  "Retracting skills first keeps the context section: the surviving context
layer still renders after the advertisement layer is dropped."
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (install-context-files-and-skills-stack context root)
    (deactivate-extension-by-id context :skills)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (search "# Project Context" prompt)
            "the context section survives skills retraction")
        (is (not (search "<available_skills>" prompt))
            "the skills advertisement is gone")))))

(test (retracting-both-layers-restores-clean-base :fixture interactive-authority)
  "Retracting both extensions returns a clean base prompt with neither block,
in either retraction order."
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (install-context-files-and-skills-stack context root)
    (deactivate-extension-by-id context :skills)
    (deactivate-extension-by-id context :context-files)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (search "You are kli" prompt) "the base prompt is restored")
        (is (not (search "# Project Context" prompt)))
        (is (not (search "<available_skills>" prompt)))))))

(test (context-and-skills-layers-re-render-without-duplication :fixture interactive-authority)
  "With both layers active, re-submitting renders each block exactly once and
keeps the context section ahead of the advertisement."
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (install-context-files-and-skills-stack context root)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "again" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (= 1 (count-substring "# Project Context" prompt))
            "the context section is not duplicated on re-render")
        (is (= 1 (count-substring "<available_skills>" prompt))
            "the skills advertisement is not duplicated on re-render")
        (is (< (search "# Project Context" prompt)
               (search "<available_skills>" prompt))
            "the section ordering is preserved across re-renders")))))

(test context-files-layers-are-additive-and-idempotent
  "The context-files contribution installs a SYSTEM.md :transform override and an
:append for the append and context blocks. Recomposing over an immutable base adds
each block once (idempotent), and a SYSTEM.md override replaces the base while the
append rides after it."
  (let* ((base "You are kli.")
         (context-block (format nil "~2%# Project Context~2%Project rules."))
         (append-block (format nil "~2%Trust the tests."))
         (append-layer (lambda () (concatenate 'string append-block context-block)))
         (stack (ext:install-layer (ext:make-layer-stack) '(:context-files :append)
                                   append-layer :kind :append))
         (once (ext:compose-layers stack base))
         (twice (ext:compose-layers stack base)))
    (is (search "# Project Context" once))
    (is (search "Trust the tests." once))
    (is (search base once) "the immutable base is preserved")
    (is (string= once twice)
        "recomposing from the base is a fixpoint -- no duplication")
    (is (= 1 (count-substring "# Project Context" once)))
    ;; A SYSTEM.md override is a :transform that replaces the running value; the
    ;; :append after it rides on top.
    (let* ((system-content "=== SYSTEM.md ===")
           (override-stack
             (ext:install-layer
              (ext:install-layer (ext:make-layer-stack) '(:context-files :system)
                                 (lambda (running) (or system-content running))
                                 :kind :transform)
              '(:context-files :append) append-layer :kind :append))
           (rendered (ext:compose-layers override-stack base)))
      (is (not (search base rendered)) "SYSTEM.md drops the base")
      (is (search system-content rendered) "SYSTEM.md replaces it")
      (is (search "# Project Context" rendered)
          "the append rides after the override"))))

(test (composed-system-prompt-is-byte-stable-across-resubmission
       :fixture interactive-authority)
  "With both shipped system-prompt layers active, re-submitting with no new
content leaves the system prompt byte-identical. Each layer strips its prior
block before reappending, so folding them over an already-composed prompt is a
fixpoint -- the model never perceives a changed system prompt between turns."
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (install-context-files-and-skills-stack context root)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((first (agents:agent-system-prompt agent)))
        (is (search "# Project Context" first)
            "the context layer contributed, so byte-stability is non-trivial")
        (is (search "<available_skills>" first)
            "the skills layer contributed too")
        (agent-session:submit-agent-session-prompt service :default-mode
                                                   "again" context)
        (is (string= first (agents:agent-system-prompt agent))
            "a second submission leaves the system prompt byte-identical")))))

(test (context-files-deactivation-restores-system-prompt :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (make-config-test-dir root "proj" ".git")
    (write-context-test-file (make-config-test-dir root "proj") "AGENTS.md"
                             "Project rules.")
    (install-context-files-stack context root)
    (let ((protocol (kli:active-protocol context))
          (extension (kli:find-live-object (kli:context-registry context)
                                           :context-files)))
      (ext:deactivate-extension protocol extension context))
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (is (not (search "# Project Context"
                       (agents:agent-system-prompt agent)))))))

(test (context-files-extension-without-files-leaves-prompt-alone :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (make-config-test-dir root "proj" ".git")
    (install-context-files-stack context root)
    (multiple-value-bind (service agent) (context-files-session-agent context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (search "You are kli" prompt))
        (is (not (search "# Project Context" prompt)))))
    (let ((service (config:find-config-service context)))
      (is (assoc :context-files
                 (getf (config:config-summary service) :resource-kinds))
          "the resource kind stays registered even with no files"))))
