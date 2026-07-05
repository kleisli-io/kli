(in-package #:kli/tests)
(in-suite all)

(defun write-prompt-file (dir name string)
  (write-config-test-file (merge-pathnames name dir) string))

(test prompts-parse-command-arguments-respects-quotes
  (is (null (prompts:parse-command-arguments "")))
  (is (null (prompts:parse-command-arguments "   ")))
  (is (equal '("Button") (prompts:parse-command-arguments "Button")))
  (is (equal '("Button" "onClick handler" "disabled support")
             (prompts:parse-command-arguments
              "Button \"onClick handler\" \"disabled support\"")))
  (is (equal '("single quoted" "plain")
             (prompts:parse-command-arguments "'single quoted'  plain")))
  (is (equal '("ab cd") (prompts:parse-command-arguments "a\"b c\"d"))
      "adjacent quoted and bare text join into one argument")
  (is (equal '("a" "b") (prompts:parse-command-arguments
                         (format nil "a~Cb" #\Tab))))
  (is (equal '("a" "b c") (prompts:parse-command-arguments "a \"b c"))
      "an unterminated quote still yields the accumulated argument"))

(test prompts-substitute-arguments-positional-slices-and-raw
  (let ((args '("a" "b" "c")))
    (is (equal "make a with b"
               (prompts:substitute-arguments "make $1 with $2" args)))
    (is (equal "missing: "
               (prompts:substitute-arguments "missing: $9" args)))
    (is (equal "b c" (prompts:substitute-arguments "${@:2}" args)))
    (is (equal "b" (prompts:substitute-arguments "${@:2:1}" args)))
    (is (equal "a b c" (prompts:substitute-arguments "${@:0}" args))
        "position zero clamps to the first argument")
    (is (equal "" (prompts:substitute-arguments "${@:7}" args)))
    (is (equal "a b c" (prompts:substitute-arguments "$@" args)))
    (is (equal "a b c" (prompts:substitute-arguments "$ARGUMENTS" args))))
  (let ((args '("1" "2" "3" "4" "5" "6" "7" "8" "9" "ten")))
    (is (equal "ten" (prompts:substitute-arguments "$10" args))
        "multi-digit positions parse greedily"))
  (is (equal "raw: \"x\"  y"
             (prompts:substitute-arguments "raw: $RAW_ARGUMENTS"
                                           '("x" "y")
                                           :raw "\"x\"  y")))
  (is (equal "echo $1"
             (prompts:substitute-arguments "$RAW_ARGUMENTS"
                                           '("echo" "$1")
                                           :raw "echo $1"))
      "raw text substitutes last so placeholders inside it stay literal")
  (is (equal "x y" (prompts:substitute-arguments "$RAW_ARGUMENTS" '("x" "y")))
      "raw defaults to the parsed arguments joined"))

(test prompts-parse-frontmatter-extracts-flat-pairs
  (multiple-value-bind (frontmatter body)
      (prompts:parse-frontmatter
       (format nil "---~%description: Review staged~%argument-hint: \"<PR-URL>\"~%---~%~%Body line~%"))
    (is (equal "Review staged" (gethash "description" frontmatter)))
    (is (equal "<PR-URL>" (gethash "argument-hint" frontmatter))
        "surrounding quotes are stripped from values")
    (is (equal "Body line" body)))
  (multiple-value-bind (frontmatter body)
      (prompts:parse-frontmatter "no frontmatter here")
    (is (zerop (hash-table-count frontmatter)))
    (is (equal "no frontmatter here" body)))
  (multiple-value-bind (frontmatter body)
      (prompts:parse-frontmatter (format nil "---~%description: x~%no close"))
    (is (zerop (hash-table-count frontmatter))
        "an unterminated fence is body, not frontmatter")
    (is (search "description: x" body)))
  (multiple-value-bind (frontmatter body)
      (prompts:parse-frontmatter
       (format nil "---~C~%description: x~C~%---~C~%Body" #\Return #\Return #\Return))
    (is (equal "x" (gethash "description" frontmatter)))
    (is (equal "Body" body))))

(test prompts-load-template-derives-name-description-and-hint
  (let ((dir (make-config-test-dir (temp-config-root))))
    (let ((template (prompts:load-prompt-template
                     (write-prompt-file
                      dir "review.md"
                      (format nil "---~%description: Review changes~%argument-hint: \"<file>\"~%---~%Review $1")))))
      (is (equal "review" (prompts:prompt-template-name template)))
      (is (equal "Review changes" (prompts:prompt-template-description template)))
      (is (equal "<file>" (prompts:prompt-template-argument-hint template)))
      (is (equal "Review $1" (prompts:prompt-template-body template))))
    (let ((template (prompts:load-prompt-template
                     (write-prompt-file
                      dir "bare.md"
                      (format nil "~%First real line~%more")))))
      (is (equal "First real line" (prompts:prompt-template-description template))
          "description falls back to the first non-empty body line")
      (is (null (prompts:prompt-template-argument-hint template))))
    (let* ((long (make-string 70 :initial-element #\x))
           (template (prompts:load-prompt-template
                      (write-prompt-file dir "long.md" long))))
      (is (= 63 (length (prompts:prompt-template-description template))))
      (is (string= "..." (subseq (prompts:prompt-template-description template) 60))))
    (is (null (prompts:load-prompt-template
               (merge-pathnames "absent.md" dir))))))

(test prompts-oversized-template-treated-as-absent
  "A prompt template over *prompt-template-file-byte-limit* loads as NIL: its
   body would otherwise flow whole into the model context, so an oversized
   file is treated as absent."
  (let ((dir (make-config-test-dir (temp-config-root))))
    (let ((path (write-prompt-file dir "fat.md"
                                   (make-string 256 :initial-element #\x))))
      (let ((prompts::*prompt-template-file-byte-limit* 64))
        (is (null (prompts:load-prompt-template path))
            "an oversized template never loads"))
      (is (not (null (prompts:load-prompt-template path)))
          "within the default limit the template loads"))))

(test prompts-discover-templates-non-recursive-in-directory-order
  (let* ((root (temp-config-root))
         (global (make-config-test-dir root "global-prompts"))
         (project (make-config-test-dir root "project-prompts")))
    (write-prompt-file global "zeta.md" "zeta body")
    (write-prompt-file global "alpha.md" "alpha body")
    (write-prompt-file global "notes.txt" "not a template")
    (write-prompt-file (make-config-test-dir global "nested") "deep.md" "deep")
    (write-prompt-file project "beta.md" "beta body")
    (let ((templates (prompts:discover-prompt-templates (list global project))))
      (is (equal '("alpha" "zeta" "beta")
                 (mapcar #'prompts:prompt-template-name templates))
          "sorted within a directory, directory order preserved, non-recursive"))
    (is (null (prompts:discover-prompt-templates
               (list (merge-pathnames "absent/" root)))))))

(test (prompts-extension-registers-template-commands :fixture interactive-authority)
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (project-dir (make-config-test-dir root "proj" ".kli"))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (make-config-test-dir root "proj" ".git")
    (write-prompt-file (make-config-test-dir global-dir "prompts")
                       "review.md"
                       (format nil "---~%description: Global review~%---~%Global $1"))
    (write-prompt-file (make-config-test-dir global-dir "prompts")
                       "only-global.md"
                       "Only global body")
    (write-prompt-file (make-config-test-dir project-dir "prompts")
                       "review.md"
                       (format nil "---~%description: Project review~%argument-hint: \"<file>\"~%---~%Project $1 of $@ raw $RAW_ARGUMENTS"))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (merge-pathnames "proj/" root)))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          prompts:*prompt-templates-extension-manifest*))
    (let ((provider (command-provider context)))
      (let ((review (ext:provider-call provider :find-command :review)))
        (is (not (null review)))
        (is (equal "Project review" (commands:command-description review))
            "the project template shadows the global one")
        (is (equal "<file>" (commands:command-arguments review))))
      (let ((only-global (ext:provider-call provider :find-command :only-global)))
        (is (not (null only-global)))
        (is (equal "Only global body"
                   (commands:command-description only-global)))))
    (is (equal "Project a of a b c raw a \"b c\""
               (command-result-text
                context
                (invoke-test-command context :review
                                     '(:tail "a \"b c\""))))
        "without an app the expansion is the command result")
    (let ((extension (kli:find-live-object (kli:context-registry context)
                                           :prompt-templates)))
      (ext:deactivate-extension protocol extension context)
      (let ((provider (command-provider context)))
        (is (null (ext:provider-call provider :find-command :review)))
        (is (null (ext:provider-call provider :find-command :only-global))))
      (is (null (nth-value 1 (gethash :prompts
                                      (config:config-service-resource-kinds
                                       (config:find-config-service context)))))
          "deactivation drains the :prompts resource kind"))))

(test prompts-builtin-tier-discovered-and-shadowed
  "A prompt under the builtin tier registers with no filesystem entry; a
   same-named filesystem prompt shadows the builtin, filesystem winning."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (builtin-dir (make-config-test-dir root "builtin"))
         (context (kli:make-kernel-host)))
    (switch-to-extension-protocol context)
    (write-prompt-file builtin-dir "research.md"
                       (format nil "---~%description: Builtin research~%---~%Builtin $1"))
    (write-prompt-file builtin-dir "implement.md"
                       (format nil "---~%description: Builtin implement~%---~%Implement body"))
    (write-prompt-file (make-config-test-dir global-dir "prompts")
                       "research.md"
                       (format nil "---~%description: Filesystem research~%---~%Filesystem $1"))
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj"))
          (prompts::*builtin-prompt-roots* (list builtin-dir)))
      (install-extensions context
                          obj:*standard-object-extension-manifest*
                          event:*events-extension-manifest*
                          commands:*commands-extension-manifest*
                          config:*config-extension-manifest*
                          prompts:*prompt-templates-extension-manifest*))
    (let ((provider (command-provider context)))
      (let ((implement (ext:provider-call provider :find-command :implement)))
        (is (not (null implement))
            "a builtin-only prompt ships as a command with no filesystem entry")
        (is (equal "Builtin implement"
                   (commands:command-description implement))))
      (let ((research (ext:provider-call provider :find-command :research)))
        (is (not (null research)))
        (is (equal "Filesystem research"
                   (commands:command-description research))
            "a filesystem prompt with the builtin's name wins by precedence")))))

(test prompts-per-extension-builtin-scoped
  "A prompt under a loaded extension's declared root is discovered; the same
   root is invisible when that extension is absent; a same-named filesystem
   prompt shadows the builtin."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (builtin-dir (make-config-test-dir root "bundle-prompts")))
    (write-prompt-file builtin-dir "scoped.md"
                       (format nil "---~%description: Bundled scoped~%---~%Bundled $1"))
    (write-prompt-file builtin-dir "shadowed.md"
                       (format nil "---~%description: Bundled shadowed~%---~%Bundled body"))
    (write-prompt-file (make-config-test-dir global-dir "prompts")
                       "shadowed.md"
                       (format nil "---~%description: Filesystem shadowed~%---~%Filesystem $1"))
    (flet ((install (context extra)
             (switch-to-extension-protocol context)
             (let ((config:*global-config-dir* global-dir)
                   (config:*project-start-directory*
                     (make-config-test-dir root "proj"))
                   (config:*extension-resource-roots* '()))
               (config:register-extension-resource-roots
                'bundle-fixture :prompts "kli/bundle-fixture/prompts")
               (call-with-resource-root
                "kli/bundle-fixture/prompts" builtin-dir
                (lambda ()
                  (apply #'install-extensions context
                         (append
                          (list obj:*standard-object-extension-manifest*
                                event:*events-extension-manifest*
                                commands:*commands-extension-manifest*
                                config:*config-extension-manifest*)
                          extra
                          (list prompts:*prompt-templates-extension-manifest*))))))))
      (let ((with (kli:make-kernel-host))
            (without (kli:make-kernel-host)))
        (install with (list *bundle-fixture-extension-manifest*))
        (install without '())
        (let ((provider (command-provider with)))
          (is (not (null (ext:provider-call provider :find-command :scoped)))
              "a loaded extension's bundled prompt is discovered")
          (is (equal "Filesystem shadowed"
                     (commands:command-description
                      (ext:provider-call provider :find-command :shadowed)))
              "a same-named filesystem prompt shadows the builtin"))
        (is (null (ext:provider-call (command-provider without)
                                     :find-command :scoped))
            "the bundled prompt is invisible when its extension is absent")))))

(test prompts-refresh-rescans-late-loaded-extension
  "An extension installed after the prompt-templates effect surfaces its
   bundled prompt only through refresh-prompt-template-commands, and a
   retraction plus refresh withdraws it."
  (let* ((root (temp-config-root))
         (global-dir (make-config-test-dir root "global"))
         (builtin-dir (make-config-test-dir root "bundle-prompts")))
    (write-prompt-file builtin-dir "scoped.md"
                       (format nil "---~%description: Bundled scoped~%---~%Bundled $1"))
    (let ((context (kli:make-kernel-host)))
      (switch-to-extension-protocol context)
      (let ((config:*global-config-dir* global-dir)
            (config:*project-start-directory* (make-config-test-dir root "proj"))
            (config:*extension-resource-roots* '()))
        (config:register-extension-resource-roots
         'bundle-fixture :prompts "kli/bundle-fixture/prompts")
        (call-with-resource-root
         "kli/bundle-fixture/prompts" builtin-dir
         (lambda ()
           (install-extensions context
                               obj:*standard-object-extension-manifest*
                               event:*events-extension-manifest*
                               commands:*commands-extension-manifest*
                               config:*config-extension-manifest*
                               prompts:*prompt-templates-extension-manifest*)
           (let ((provider (command-provider context))
                 (protocol (kli:active-protocol context)))
             (is (null (ext:provider-call provider :find-command :scoped))
                 "the fixture's declared root is invisible before it loads")
             (let ((handle (install-extension
                            context *bundle-fixture-extension-manifest*)))
               (is (null (ext:provider-call provider :find-command :scoped))
                   "a late install alone does not re-scan prompt roots")
               (prompts:refresh-prompt-template-commands context)
               (is (not (null (ext:provider-call provider
                                                 :find-command :scoped)))
                   "refresh surfaces the late-loaded extension's prompt")
               (with-extension-load-authority
                 (ext:retract-manifest handle protocol context))
               (prompts:refresh-prompt-template-commands context)
               (is (null (ext:provider-call provider :find-command :scoped))
                   "refresh withdraws a retracted extension's prompt")))))))))

(defun install-prompt-stack-on-tui (context root body)
  (let ((global-dir (make-config-test-dir root "global")))
    (write-prompt-file (make-config-test-dir global-dir "prompts")
                       "greet.md" body)
    (let ((config:*global-config-dir* global-dir)
          (config:*project-start-directory* (make-config-test-dir root "proj")))
      (install-extensions context
                          config:*config-extension-manifest*
                          prompts:*prompt-templates-extension-manifest*))))

(test (prompts-template-command-without-agent-reports-failure :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack)))
    (install-prompt-stack-on-tui context (temp-config-root) "Hello $1")
    (let* ((app (tui-app:make-tui-app :context context :columns 48))
           (result (invoke-test-command context :greet
                                        (list :app app :tail "x"))))
      (is (commands:command-result-error-p result))
      (is (search "No agent bound"
                  (command-result-text context result))))))

(test (prompts-template-command-submits-expanded-prompt :fixture interactive-authority)
  (let ((context (ensure-tui-app-stack-with-session :deltas '("ok"))))
    (install-prompt-stack-on-tui context (temp-config-root)
                                 "Hello $1, raw $RAW_ARGUMENTS")
    (let* ((app (tui-app:make-tui-app :context context :columns 48))
           (service (kli:find-live-object (kli:context-registry context)
                                          :agent-session-service))
           (binding (gethash :default-mode
                             (agent-session:session-mode-bindings service)))
           (agent (kli:find-live-object
                   (kli:context-registry context)
                   (agent-session:mode-binding-agent-id binding))))
      (tui-app:tui-app-feed app "/greet world stranger" :render nil)
      (tui-app:tui-app-feed app (string #\Return) :render nil)
      (settle-tui-app app)
      (is (equal '(:user :assistant) (agent-session-message-roles agent)))
      (is (equal "Hello world, raw world stranger"
                 (first (agent-session-message-contents agent)))
          "the agent receives the expansion, not the typed command")
      (let ((user-events (remove-if-not
                          (lambda (event)
                            (and (eq :message (tui-transcript:event-kind event))
                                 (eq :user (tui-transcript:event-role event))))
                          (tui-app:tui-app-transcript-events app))))
        (is (= 1 (length user-events)))
        (is (equal "/greet world stranger"
                   (tui-transcript:event-text (first user-events)))
            "the transcript echoes the typed command line only")))))
