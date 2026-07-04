(in-package #:kli/tests)
(in-suite all)

(defun write-skill-file (dir string)
  (write-config-test-file (merge-pathnames "SKILL.md" dir) string))

(defun skill-md (name description &key body extra)
  (format nil "---~%~@[name: ~A~%~]~@[description: ~A~%~]~@[~A~%~]---~%~A"
          name description extra (or body "Skill body.")))

(defun count-substring (needle haystack)
  (loop with step = (length needle)
        for start = (search needle haystack)
          then (search needle haystack :start2 (+ start step))
        while start count 1))

(test skills-ignore-matcher-gitignore-semantics
  (let ((matcher (skills:make-ignore-matcher)))
    (skills:add-ignore-lines matcher
                             '("*.log" "!keep.log" "build/" "/anchored.md"
                               "# comment" "" "docs/**")
                             :prefix "")
    (is (skills:path-ignored-p matcher "x.log"))
    (is (skills:path-ignored-p matcher "sub/deep/x.log")
        "a slashless pattern matches the basename at any depth")
    (is (not (skills:path-ignored-p matcher "keep.log"))
        "negation wins as the later rule")
    (is (skills:path-ignored-p matcher "build/"))
    (is (not (skills:path-ignored-p matcher "build"))
        "a trailing-slash pattern matches directories only")
    (is (skills:path-ignored-p matcher "build/nested/file.md")
        "everything under an ignored directory is ignored")
    (is (skills:path-ignored-p matcher "anchored.md"))
    (is (not (skills:path-ignored-p matcher "sub/anchored.md"))
        "a leading slash anchors to the root")
    (is (skills:path-ignored-p matcher "docs/a/b.md"))
    (is (not (skills:path-ignored-p matcher "comment"))))
  (let ((matcher (skills:make-ignore-matcher)))
    (skills:add-ignore-lines matcher '("*.tmp" "!special.tmp")
                             :prefix "nested/")
    (is (skills:path-ignored-p matcher "nested/x.tmp"))
    (is (not (skills:path-ignored-p matcher "nested/deep/x.tmp"))
        "prefixing anchors a slashless pattern to its directory, as pi does")
    (is (not (skills:path-ignored-p matcher "x.tmp")))
    (is (not (skills:path-ignored-p matcher "nested/special.tmp"))))
  (let ((matcher (skills:make-ignore-matcher)))
    (skills:add-ignore-lines matcher '("a?c.md" "lib/[ab].md") :prefix "")
    (is (skills:path-ignored-p matcher "abc.md"))
    (is (not (skills:path-ignored-p matcher "a/c.md"))
        "question mark never crosses a slash")
    (is (skills:path-ignored-p matcher "lib/a.md"))
    (is (not (skills:path-ignored-p matcher "lib/c.md")))))

(test skills-ignore-rules-read-from-directory-files
  (let* ((root (make-config-test-dir (temp-config-root)))
         (nested (make-config-test-dir root "nested"))
         (matcher (skills:make-ignore-matcher)))
    (write-config-test-file (merge-pathnames ".gitignore" root)
                            (format nil "skip-me/~%"))
    (write-config-test-file (merge-pathnames ".ignore" nested)
                            (format nil "*.draft~%"))
    (skills:add-ignore-rules matcher root root)
    (skills:add-ignore-rules matcher nested root)
    (is (skills:path-ignored-p matcher "skip-me/"))
    (is (skills:path-ignored-p matcher "nested/x.draft"))
    (is (not (skills:path-ignored-p matcher "x.draft"))
        "nested ignore files apply below their directory only")))

(test skills-validate-name-per-spec
  (is (null (skills:validate-skill-name "pdf-processing" "pdf-processing")))
  (is (= 1 (length (skills:validate-skill-name "pdf-processing" "other"))))
  (is (= 1 (length (skills:validate-skill-name "PDF-Processing"
                                               "PDF-Processing"))))
  (is (plusp (length (skills:validate-skill-name "-pdf" "-pdf"))))
  (is (plusp (length (skills:validate-skill-name "pdf-" "pdf-"))))
  (is (plusp (length (skills:validate-skill-name "pdf--processing"
                                                 "pdf--processing"))))
  (let ((long (make-string 65 :initial-element #\a)))
    (is (= 1 (length (skills:validate-skill-name long long))))))

(test skills-validate-description-per-spec
  (is (null (skills:validate-skill-description "Does a thing.")))
  (is (= 1 (length (skills:validate-skill-description nil))))
  (is (= 1 (length (skills:validate-skill-description "  "))))
  (let ((long (make-string 1025 :initial-element #\d)))
    (is (= 1 (length (skills:validate-skill-description long))))))

(test skills-load-skill-from-file
  (let ((dir (make-config-test-dir (temp-config-root) "my-skill")))
    (multiple-value-bind (skill diagnostics)
        (skills:load-skill
         (write-skill-file dir (skill-md "my-skill" "Does things.")))
      (is (equal "my-skill" (skills:skill-name skill)))
      (is (equal "Does things." (skills:skill-description skill)))
      (is (equal (namestring dir)
                 (namestring (skills:skill-base-dir skill))))
      (is (not (skills:skill-disable-model-invocation-p skill)))
      (is (null diagnostics))))
  (let ((dir (make-config-test-dir (temp-config-root) "fallback")))
    (multiple-value-bind (skill diagnostics)
        (skills:load-skill (write-skill-file dir (skill-md nil "Described.")))
      (is (equal "fallback" (skills:skill-name skill))
          "a missing name falls back to the parent directory")
      (is (null diagnostics))))
  (let ((dir (make-config-test-dir (temp-config-root) "renamed")))
    (multiple-value-bind (skill diagnostics)
        (skills:load-skill
         (write-skill-file dir (skill-md "other-name" "Described.")))
      (is (equal "other-name" (skills:skill-name skill)))
      (is (= 1 (length diagnostics))
          "a name and directory mismatch warns but still loads")
      (is (eq :warning (getf (first diagnostics) :type)))))
  (let ((dir (make-config-test-dir (temp-config-root) "no-description")))
    (multiple-value-bind (skill diagnostics)
        (skills:load-skill
         (write-skill-file dir (skill-md "no-description" nil)))
      (is (null skill) "a skill without a description never loads")
      (is (= 1 (length diagnostics)))))
  (let ((dir (make-config-test-dir (temp-config-root) "hidden")))
    (multiple-value-bind (skill diagnostics)
        (skills:load-skill
         (write-skill-file dir (skill-md "hidden" "Hides."
                                         :extra "disable-model-invocation: true")))
      (is (skills:skill-disable-model-invocation-p skill))
      (is (null diagnostics))))
  (multiple-value-bind (skill diagnostics)
      (skills:load-skill (merge-pathnames "absent/SKILL.md"
                                          (temp-config-root)))
    (is (null skill))
    (is (= 1 (length diagnostics)))))

(test skills-oversized-skill-file-skipped-and-refused
  "A SKILL.md over *skill-file-byte-limit* is skipped with a diagnostic at
   discovery, and a skill whose file grew over the limit errors at
   invocation instead of being read whole."
  (let* ((dir (make-config-test-dir (temp-config-root) "fat-skill"))
         (path (write-skill-file
                dir (skill-md "fat-skill" "Big."
                              :body (make-string 256 :initial-element #\x)))))
    (let ((skills::*skill-file-byte-limit* 64))
      (multiple-value-bind (skill diagnostics) (skills:load-skill path)
        (is (null skill) "an oversized skill never loads")
        (is (= 1 (length diagnostics)))
        (is (search "byte limit" (getf (first diagnostics) :message)))))
    (let ((skill (skills:load-skill path)))
      (is (not (null skill)) "within the default limit the skill loads")
      (let ((skills::*skill-file-byte-limit* 64))
        (signals error (skills::read-skill-body skill))))))

(test skills-oversized-ignore-file-skipped
  "An ignore file over *ignore-file-byte-limit* contributes no rules: a
   pathological file is treated as absent rather than read and split whole."
  (let ((dir (make-config-test-dir (temp-config-root) "ign")))
    (write-config-test-file
     (merge-pathnames ".gitignore" dir)
     (format nil "*.md~%#~A" (make-string 128 :initial-element #\x)))
    (let ((skills::*ignore-file-byte-limit* 64))
      (let ((matcher (skills:make-ignore-matcher)))
        (skills:add-ignore-rules matcher dir dir)
        (is (not (skills:path-ignored-p matcher "foo.md"))
            "an oversized ignore file contributes no rules")))
    (let ((matcher (skills:make-ignore-matcher)))
      (skills:add-ignore-rules matcher dir dir)
      (is (skills:path-ignored-p matcher "foo.md")
          "within the default limit the ignore rules apply"))))

(test skills-discovery-terminates-on-symlink-cycle
  "A directory symlink pointing back into the tree does not make discovery
   recurse without bound: the real skill is still found and the walk ends."
  (let* ((root (make-config-test-dir (temp-config-root) "cycle-root"))
         (real (make-config-test-dir root "real")))
    (write-skill-file real (skill-md "real" "A real skill."))
    (sb-posix:symlink (namestring (truename root))
                      (namestring (merge-pathnames "loop" root)))
    (let ((found (skills:discover-skills-in-directory root)))
      (is (equal '("real") (mapcar #'skills:skill-name found))
          "the real skill is found and the symlink cycle is bounded"))))

(test skills-discovery-recurses-and-stops-at-skill-roots
  (let ((root (make-config-test-dir (temp-config-root))))
    (write-skill-file (make-config-test-dir root "alpha")
                      (skill-md "alpha" "Alpha skill."))
    (write-skill-file (make-config-test-dir root "group" "beta")
                      (skill-md "beta" "Beta skill."))
    (write-skill-file (make-config-test-dir root "alpha" "inner")
                      (skill-md "inner" "Never found."))
    (write-skill-file (make-config-test-dir root ".hidden")
                      (skill-md "hidden" "Never found."))
    (write-skill-file (make-config-test-dir root "node_modules" "dep")
                      (skill-md "dep" "Never found."))
    (let ((skills (skills:discover-skills-in-directory root)))
      (is (equal '("alpha" "beta") (mapcar #'skills:skill-name skills))
          "a SKILL.md directory is a skill root with no deeper recursion"))))

(test skills-discovery-root-markdown-files
  (let ((root (make-config-test-dir (temp-config-root) "skills")))
    (write-config-test-file (merge-pathnames "loose.md" root)
                            (skill-md "loose" "Loose skill."))
    (write-config-test-file (merge-pathnames "notes.txt" root) "not a skill")
    (let ((with-root (skills:discover-skills-in-directory
                      root :root-files-p t))
          (without-root (skills:discover-skills-in-directory root)))
      (is (equal '("loose") (mapcar #'skills:skill-name with-root)))
      (is (null without-root)
          "root markdown files only load where pi loads them"))))

(test skills-discovery-honors-ignore-files
  (let ((root (make-config-test-dir (temp-config-root))))
    (write-config-test-file (merge-pathnames ".gitignore" root)
                            (format nil "skipped/~%"))
    (write-skill-file (make-config-test-dir root "kept")
                      (skill-md "kept" "Kept skill."))
    (write-skill-file (make-config-test-dir root "skipped")
                      (skill-md "skipped" "Never found."))
    (write-config-test-file
     (merge-pathnames ".gitignore" (make-config-test-dir root "group"))
     (format nil "filtered/~%"))
    (write-skill-file (make-config-test-dir root "group" "filtered")
                      (skill-md "filtered" "Never found."))
    (write-skill-file (make-config-test-dir root "group" "surviving")
                      (skill-md "surviving" "Found."))
    (let ((skills (skills:discover-skills-in-directory root)))
      (is (equal '("kept" "surviving")
                 (sort (mapcar #'skills:skill-name skills) #'string<))))))

(test skills-discover-skills-deduplicates-first-wins
  (let* ((root (temp-config-root))
         (first-dir (make-config-test-dir root "one"))
         (second-dir (make-config-test-dir root "two")))
    (write-skill-file (make-config-test-dir first-dir "dup")
                      (skill-md "dup" "First wins."))
    (write-skill-file (make-config-test-dir second-dir "dup")
                      (skill-md "dup" "Second loses."))
    (write-skill-file (make-config-test-dir second-dir "extra")
                      (skill-md "extra" "Extra skill."))
    (multiple-value-bind (skills diagnostics)
        (skills:discover-skills
         (list (list :directory first-dir :root-files-p t)
               (list :directory second-dir :root-files-p t)))
      (is (equal '("dup" "extra") (mapcar #'skills:skill-name skills)))
      (is (equal "First wins."
                 (skills:skill-description
                  (find "dup" skills :key #'skills:skill-name
                                     :test #'equal))))
      (let ((collisions (remove :collision diagnostics
                                :test-not #'eq
                                :key (lambda (d) (getf d :type)))))
        (is (= 1 (length collisions)))))))

(test skills-format-skills-for-prompt-xml
  (let ((skill (skills:make-skill
                :name "a-skill"
                :description "Handles <tags> & \"quotes\"."
                :path #p"/tmp/a-skill/SKILL.md"
                :base-dir #p"/tmp/a-skill/"))
        (hidden (skills:make-skill
                 :name "hidden"
                 :description "Hidden."
                 :path #p"/tmp/hidden/SKILL.md"
                 :base-dir #p"/tmp/hidden/"
                 :disable-model-invocation-p t)))
    (let ((text (skills:format-skills-for-prompt (list skill hidden))))
      (is (search "<available_skills>" text))
      (is (search "<name>a-skill</name>" text))
      (is (search "<description>Handles &lt;tags&gt; &amp; &quot;quotes&quot;.</description>"
                  text))
      (is (search "<location>/tmp/a-skill/SKILL.md</location>" text))
      (is (not (search "hidden" text))
          "disable-model-invocation skills never advertise")
      (is (search "Use the read tool" text)))
    (is (equal "" (skills:format-skills-for-prompt (list hidden))))
    (is (equal "" (skills:format-skills-for-prompt '())))))

(defun write-installed-skill (root &key (name "greet")
                                        (description "Greets the user.")
                                        (body "Say hello with enthusiasm.")
                                        extra)
  (write-skill-file
   (make-config-test-dir root "global" "skills" name)
   (skill-md name description :body body :extra extra)))

(defun install-skills-stack (context root)
  (let ((global-dir (make-config-test-dir root "global")))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj"))
          (skills:*user-agents-skills-directory*
            (make-config-test-dir root "user-agents-absent")))
      (install-extensions context
                          config:*config-extension-manifest*
                          skills:*skills-extension-manifest*))))

(test (skills-extension-registers-skill-commands :fixture interactive-authority)
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj" ".kli"))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (make-config-test-dir root "proj" ".git")
    (write-skill-file (make-config-test-dir global-dir "skills" "review")
                      (skill-md "review" "Global review."))
    (write-skill-file (make-config-test-dir project-dir "skills" "review")
                      (skill-md "review" "Project review."))
    (write-skill-file (make-config-test-dir root "proj" ".agents" "skills"
                                            "walked")
                      (skill-md "walked" "From the agents walk-up."))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (merge-pathnames "proj/" root))
          (skills:*user-agents-skills-directory*
            (make-config-test-dir root "user-agents" "skills")))
      (write-skill-file (make-config-test-dir root "user-agents" "skills"
                                              "homed")
                        (skill-md "homed" "From the user agents directory."))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          skills:*skills-extension-manifest*))
    (let ((provider (command-provider context)))
      (let ((review (ext:provider-call provider :find-command "skill:review")))
        (is (not (null review)))
        (is (equal "Project review." (commands:command-description review))
            "the project skill shadows the global one"))
      (is (not (null (ext:provider-call provider :find-command
                                        "skill:walked"))))
      (is (not (null (ext:provider-call provider :find-command
                                        "skill:homed")))))
    (let ((text (command-result-text
                 context
                 (invoke-test-command context "skill:review"
                                      '(:tail "focus on tests")))))
      (is (search "<skill name=\"review\"" text))
      (is (search "References are relative to" text))
      (is (search "Skill body." text))
      (is (search (format nil "</skill>~%~%focus on tests") text)
          "arguments append after the block, matching pi")
      (is (not (search "---" text)) "frontmatter never reaches the model"))
    (let ((extension (kli:find-live-object (kli:context-registry context)
                                           :skills)))
      (ext:deactivate-extension protocol extension context)
      (let ((provider (command-provider context)))
        (is (null (ext:provider-call provider :find-command
                                     "skill:review"))))
      (is (null (nth-value 1 (gethash :skills
                                      (config:config-service-resource-kinds
                                       (config:find-config-service context)))))
          "deactivation drains the :skills resource kind"))))

(test (skills-refresh-rediscovers-added-and-removed :fixture interactive-authority)
  "/skills re-discovers the skill directories live: a SKILL.md added after
install registers, one removed unregisters, and the refresh command's own
registration survives so it stays invokable."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (greet-dir (make-config-test-dir global-dir "skills" "greet"))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (write-skill-file greet-dir (skill-md "greet" "Greets."))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj"))
          (skills:*user-agents-skills-directory*
            (make-config-test-dir root "user-agents-absent")))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          skills:*skills-extension-manifest*)
      (let ((provider (command-provider context)))
        (is (not (null (ext:provider-call provider :find-command "skill:greet"))))
        (is (not (null (ext:provider-call provider :find-command "skills")))
            "the refresh command registers at install"))
      (uiop:delete-directory-tree greet-dir
                                  :validate (constantly t)
                                  :if-does-not-exist :ignore)
      (write-skill-file (make-config-test-dir global-dir "skills" "farewell")
                        (skill-md "farewell" "Says goodbye."))
      (let ((result (invoke-test-command context "skills")))
        (is (not (commands:command-result-error-p result)))
        (is (search "Skills refreshed." (command-result-text context result))))
      (let ((provider (command-provider context)))
        (is (null (ext:provider-call provider :find-command "skill:greet"))
            "a removed skill's command drains on refresh")
        (is (not (null (ext:provider-call provider :find-command
                                          "skill:farewell")))
            "an added skill registers on refresh")
        (is (not (null (ext:provider-call provider :find-command "skills")))
            "the refresh command survives a refresh")))))

(test skills-builtin-creating-extensions-loads-cleanly
  (multiple-value-bind (skill diagnostics)
      (skills:load-skill
       (buildlisp/resources:resource-path "kli/skills"
                                          "creating-extensions/SKILL.md"))
    (is (not (null skill)))
    (is (null diagnostics) "the builtin skill passes every validator")
    (is (equal "creating-extensions" (skills:skill-name skill)))))

(test skills-builtin-skill-registers-command
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj"))
          (skills:*user-agents-skills-directory*
            (make-config-test-dir root "user-agents-absent")))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          skills:*skills-extension-manifest*))
    (let ((command (ext:provider-call (command-provider context)
                                      :find-command
                                      "skill:creating-extensions")))
      (is (not (null command))
          "the builtin skill ships as a command with no user setup")
      (is (search "Author kli user extensions"
                  (commands:command-description command))))))

(test skills-builtin-skill-user-shadowable
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (write-skill-file (make-config-test-dir global-dir "skills"
                                            "creating-extensions")
                      (skill-md "creating-extensions" "User override."))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj"))
          (skills:*user-agents-skills-directory*
            (make-config-test-dir root "user-agents-absent")))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          skills:*skills-extension-manifest*))
    (let ((command (ext:provider-call (command-provider context)
                                      :find-command
                                      "skill:creating-extensions")))
      (is (equal "User override." (commands:command-description command))
          "a user skill with the builtin's name wins by precedence"))))

(test skills-per-extension-builtin-scoped
  "A skill under a loaded extension's declared root is discovered; the same
   root is invisible when that extension is absent; a same-named user skill
   shadows the builtin."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (builtin-dir (make-config-test-dir root "bundle-skills")))
    (write-skill-file (make-config-test-dir builtin-dir "scoped")
                      (skill-md "scoped" "Bundled scoped."))
    (write-skill-file (make-config-test-dir builtin-dir "shadowed")
                      (skill-md "shadowed" "Bundled shadowed."))
    (write-skill-file (make-config-test-dir global-dir "skills" "shadowed")
                      (skill-md "shadowed" "Filesystem shadowed."))
    (flet ((install (context extra)
             (switch-to-extension-protocol context)
             (let ((config:*global-config-dir* global-dir)
                   (config:*project-start-directory*
                     (make-config-test-dir root "proj"))
                   (config:*extension-resource-roots* '())
                   (skills:*user-agents-skills-directory*
                     (make-config-test-dir root "user-agents-absent")))
               (config:register-extension-resource-roots
                'bundle-fixture :skills "kli/bundle-fixture/skills")
               (call-with-resource-root
                "kli/bundle-fixture/skills" builtin-dir
                (lambda ()
                  (apply #'install-extensions context
                         (append
                          (list obj:*standard-object-extension-manifest*
                                event:*events-extension-manifest*
                                commands:*commands-extension-manifest*
                                config:*config-extension-manifest*)
                          extra
                          (list skills:*skills-extension-manifest*))))))))
      (let ((with (kli:make-kernel-host))
            (without (kli:make-kernel-host)))
        (install with (list *bundle-fixture-extension-manifest*))
        (install without '())
        (let ((provider (command-provider with)))
          (is (not (null (ext:provider-call provider :find-command
                                            "skill:scoped")))
              "a loaded extension's bundled skill is discovered")
          (is (equal "Filesystem shadowed."
                     (commands:command-description
                      (ext:provider-call provider :find-command
                                         "skill:shadowed")))
              "a same-named user skill shadows the builtin"))
        (is (null (ext:provider-call (command-provider without)
                                     :find-command "skill:scoped"))
            "the bundled skill is invisible when its extension is absent")))))

(test (skills-extension-advertises-in-system-prompt :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (write-installed-skill root)
    (write-installed-skill root :name "covert" :description "Covert skill."
                                :extra "disable-model-invocation: true")
    (install-skills-stack context root)
    (let* ((app (tui-app:make-tui-app :context context :columns 48))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (declare (ignore app))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (let ((prompt (agents:agent-system-prompt agent)))
        (is (= 1 (count-substring "<available_skills>" prompt)))
        (is (search "<name>greet</name>" prompt))
        (is (not (search "covert" prompt))
            "disable-model-invocation skills stay out of the prompt"))
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "again" context)
      (is (= 1 (count-substring "<available_skills>"
                                (agents:agent-system-prompt agent)))
          "re-submitting never duplicates the advertisement"))))

(test skills-advertisement-additive-is-idempotent
  "The skills advertisement is a pure :append layer. Recomposing the stack over an
immutable base appends the block exactly once, so resubmission is a fixpoint and
the base is never mutated."
  (let* ((base "You are kli. Base instructions.")
         (advertisement (format nil "~%~%<available_skills>~%<skill>greet</skill>~%</available_skills>"))
         (stack (ext:install-layer
                 (ext:make-layer-stack) :skills
                 (lambda () advertisement)
                 :kind :append))
         (once (ext:compose-layers stack base))
         (twice (ext:compose-layers stack base)))
    (is (search "<skill>greet</skill>" once)
        "the layer appends the advertisement block")
    (is (search base once)
        "the immutable base is preserved verbatim")
    (is (string= once twice)
        "recomposing from the base is a fixpoint -- no duplication")
    (is (= 1 (count-substring "<available_skills>" once))
        "exactly one advertisement block")))

(test (skills-extension-deactivation-restores-system-prompt :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (write-installed-skill root)
    (install-skills-stack context root)
    (let* ((protocol (kli:active-protocol context))
           (extension (kli:find-live-object (kli:context-registry context)
                                            :skills))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (ext:deactivate-extension protocol extension context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "hello" context)
      (is (not (search "<available_skills>"
                       (agents:agent-system-prompt agent)))))))

(test (skills-command-without-agent-reports-failure :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack))
        (root (temp-config-root)))
    (write-installed-skill root)
    (install-skills-stack context root)
    (let* ((app (tui-app:make-tui-app :context context :columns 48))
           (result (invoke-test-command context "skill:greet"
                                        (list :app app :tail "x"))))
      (is (commands:command-result-error-p result))
      (is (search "No agent bound" (command-result-text context result))))))

(defun sigil-test-skills (&rest names)
  (mapcar (lambda (name)
            (skills:make-skill :name name
                               :description "Sigil test."
                               :path (format nil "/tmp/~A/SKILL.md" name)
                               :base-dir (format nil "/tmp/~A/" name)))
          names))

(test skills-sigil-matching-known-names-only
  (let ((skills (sigil-test-skills "pdf" "pdf-tools" "greet")))
    (flet ((names (text)
             (mapcar #'skills:skill-name
                     (skills:find-sigil-skills text skills))))
      (is (equal '("pdf-tools") (names "use $pdf-tools now"))
          "the longest discovered name wins")
      (is (equal '("pdf") (names "use $pdf, then stop")))
      (is (equal '("pdf") (names "$pdf")))
      (is (equal '("pdf") (names "($pdf)")))
      (is (equal '("greet") (names "end with $greet")))
      (is (null (names "no sigils here")))
      (is (null (names "unknown $skills pass through")))
      (is (null (names "$pdf-toolsy is no skill"))
          "a partial name never matches across a boundary")
      (is (null (names "US$pdf stays prose"))
          "a sigil needs a non-alphanumeric character before it")
      (is (null (names "$$pdf stays prose")))
      (is (null (names "literal $HOME and $5 stay prose")))
      (is (equal '("greet" "pdf") (names "$greet then $pdf then $greet"))
          "references are distinct in first-appearance order"))))

(test skills-expand-sigils-prepends-blocks
  (let* ((root (temp-config-root))
         (alpha-dir (make-config-test-dir root "alpha"))
         (beta-dir (make-config-test-dir root "beta")))
    (write-skill-file alpha-dir (skill-md "alpha" "Alpha." :body "Alpha body."))
    (write-skill-file beta-dir (skill-md "beta" "Beta." :body "Beta body."))
    (let ((skills (list (skills:load-skill
                         (merge-pathnames "SKILL.md" alpha-dir))
                        (skills:load-skill
                         (merge-pathnames "SKILL.md" beta-dir)))))
      (is (equal "no references"
                 (skills:expand-skill-sigils "no references" skills))
          "text without sigils passes through unchanged")
      (let ((expanded (skills:expand-skill-sigils "run $beta then $alpha"
                                                  skills)))
        (is (uiop:string-suffix-p expanded "run $beta then $alpha")
            "the prose is never rewritten")
        (is (uiop:string-prefix-p "<skill name=\"beta\"" expanded)
            "blocks come first")
        (is (< (search "<skill name=\"beta\"" expanded)
               (search "<skill name=\"alpha\"" expanded))
            "blocks follow first-appearance order")
        (is (search "Referenced as $beta in the prompt that follows."
                    expanded))
        (is (search "Beta body." expanded))
        (is (not (search "---" expanded))
            "frontmatter never reaches the model"))
      (let ((expanded (skills:expand-skill-sigils "$alpha and $alpha again"
                                                  skills)))
        (is (= 1 (count-substring "<skill name=\"alpha\"" expanded))
            "repeated sigils for one skill yield one block")))))

(test skills-sigil-references-hidden-skills
  (let ((dir (make-config-test-dir (temp-config-root) "covert")))
    (write-skill-file dir (skill-md "covert" "Covert." :body "Covert body."
                                    :extra "disable-model-invocation: true"))
    (let ((skills (list (skills:load-skill
                         (merge-pathnames "SKILL.md" dir)))))
      (is (search "<skill name=\"covert\""
                  (skills:expand-skill-sigils "use $covert here" skills))
          "an explicit sigil reference works like the slash command"))))

(test (skills-sigil-expands-on-submit :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (write-installed-skill root)
    (install-skills-stack context root)
    (let* ((app (tui-app:make-tui-app :context context :columns 48))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (tui-app:tui-app-feed app "please $greet me" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (settle-tui-app app)
      (let ((delivered (first (agent-session-message-contents agent))))
        (is (uiop:string-prefix-p "<skill name=\"greet\"" delivered))
        (is (search "Referenced as $greet in the prompt that follows."
                    delivered))
        (is (search "Say hello with enthusiasm." delivered))
        (is (uiop:string-suffix-p delivered "please $greet me")
            "the typed prose closes the message untouched"))
      (let ((user-events (remove-if-not
                          (lambda (event)
                            (and (eq :message (tui-transcript:event-kind event))
                                 (eq :user (tui-transcript:event-role event))))
                          (tui-app:tui-app-transcript-events app))))
        (is (equal "please $greet me"
                   (tui-transcript:event-text (first user-events)))
            "the transcript keeps the typed text")))))

(test (skills-sigil-deactivation-restores-expander :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (write-installed-skill root)
    (install-skills-stack context root)
    (let* ((protocol (kli:active-protocol context))
           (extension (kli:find-live-object (kli:context-registry context)
                                            :skills))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (ext:deactivate-extension protocol extension context)
      (agent-session:submit-agent-session-prompt service :default-mode
                                                 "please $greet me" context)
      (is (equal "please $greet me"
                 (first (agent-session-message-contents agent)))
          "a sigil submits verbatim once the extension retracts"))))

(test (skills-command-submits-skill-block :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok")))
        (root (temp-config-root)))
    (write-installed-skill root)
    (install-skills-stack context root)
    (let* ((app (tui-app:make-tui-app :context context :columns 48))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (tui-app:tui-app-feed app "/skill:greet be brief" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (settle-tui-app app)
      (is (equal '(:user :assistant) (agent-session-message-roles agent)))
      (let ((delivered (first (agent-session-message-contents agent))))
        (is (search "<skill name=\"greet\"" delivered))
        (is (search "Say hello with enthusiasm." delivered))
        (is (search (format nil "</skill>~%~%be brief") delivered)))
      (let ((user-events (remove-if-not
                          (lambda (event)
                            (and (eq :message (tui-transcript:event-kind event))
                                 (eq :user (tui-transcript:event-role event))))
                          (tui-app:tui-app-transcript-events app))))
        (is (= 1 (length user-events)))
        (is (equal "/skill:greet be brief"
                   (tui-transcript:event-text (first user-events)))
            "the transcript keeps the typed command line")))))
