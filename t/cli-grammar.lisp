(in-package #:kli/tests)
(in-suite all)

;;;; Pin +cli-grammar+ against the parsers it must agree with, both directions:
;;;; a flag or route that drifts out of any surface fails here.

(test cli-grammar-routes-match-dispatch
  "Every command token routes to that command's :id; a token in no command's
:tokens falls through to the channel default and never a phantom id."
  (dolist (command app::+cli-grammar+)
    (dolist (token (app::cli-command-tokens command))
      (is (eq (app::cli-command-id command)
              (nth-value 0 (app::dispatch-command (list token) t))))))
  (is (eq :tui (nth-value 0 (app::dispatch-command '("frobnicate") t))))
  (is (eq :print (nth-value 0 (app::dispatch-command '("frobnicate") nil)))))

(test cli-grammar-ids-match-dispatch-main
  "The non-default grammar command ids are exactly the keys dispatch-main
switches on. Adding a command to the grammar without wiring its handler (or the
reverse) breaks this guard. The :tui id is the default arm and carries no token."
  (let ((grammar-ids (sort (loop for command in app::+cli-grammar+
                                 unless (app::cli-command-default-p command)
                                   collect (app::cli-command-id command))
                           #'string<))
        (handled (sort (list :version :update :docs :mcp-serve :install :help
                             :print :print-authority :complete :completion)
                       #'string<)))
    (is (equal handled grammar-ids))))

(test cli-grammar-derives-print-value-flags
  "+print-value-flags+ is the :print command's value-flag tokens, no more."
  (is (equal app::+print-value-flags+
             (app::cli-command-value-flag-tokens :print)))
  (is (equal '("--from-snapshot" "--grants" "--output-format" "--profile")
             (sort (copy-list app::+print-value-flags+) #'string<))))

(test cli-grammar-update-flags-match-parser
  "parse-update-args accepts exactly the :update flag tokens and signals on
anything outside the grammar (the prototype's invented `update -C` among them)."
  (flet ((accepts-p (token)
           (let* ((flag (app::cli-command-find-flag :update token))
                  (args (if (and flag (app::cli-flag-value-p flag))
                            (list token "v0")
                            (list token))))
             (handler-case (progn (app::parse-update-args args) t)
               (error () nil)))))
    (dolist (token (app::cli-command-flag-tokens :update))
      (is-true (accepts-p token)))
    (is-false (accepts-p "-C"))
    (is-false (accepts-p "--bogus"))))

(test cli-grammar-docs-subcommands-match-parser
  "Each :docs subcommand selects a literal branch in run-docs; a head outside the
subcommand list is fetched as a documentation page slug."
  (flet ((routes-as-page-p (args)
           (let* ((head (first args))
                  (seen nil)
                  (app::*docs-base-url* "https://example.test")
                  (app::*docs-http* (lambda (url) (setf seen url) (values "# x" 200)))
                  (out (make-string-output-stream))
                  (err (make-string-output-stream)))
             (let ((*standard-output* out) (*error-output* err))
               (app::run-docs args))
             (and seen (equal seen (app::docs-page-url head))))))
    (dolist (sub (app::cli-command-subcommands :docs))
      (is-false (routes-as-page-p (list sub "x"))))
    (is-true (routes-as-page-p '("extend/anatomy")))
    (is-true (routes-as-page-p '("guide")))))

(test cli-grammar-output-format-candidates-match-parser
  "Each --output-format candidate maps to its format keyword; a non-candidate
token falls back to :text."
  (dolist (candidate (app::cli-flag-candidates
                      (app::cli-command-find-flag :print "--output-format")))
    (is (eq (intern (string-upcase candidate) :keyword)
            (app::parse-output-format candidate))))
  (is (eq :text (app::parse-output-format "bogus"))))

(test cli-grammar-tui-passthrough-flags-match-constants
  "The TUI pass-through constants the launcher reads equal their grammar flags, so
renaming one surface fails here until the other follows."
  (is (string= app::+profile-arg+
               (app::cli-flag-name (app::cli-command-find-flag :tui "--profile"))))
  (is (equal app::+continue-args+
             (app::cli-flag-tokens (app::cli-command-find-flag :tui "-c"))))
  (is (string= app::+extension-arg+
               (app::cli-flag-name (app::cli-command-find-flag :tui "--extension")))))

;;;; The completion responder: complete-candidates answers from the grammar;
;;;; run-complete/run-completion are its boot-free, error-swallowing surface.

(test cli-grammar-hidden-commands-route-but-hide
  "__complete and completion dispatch like any command yet never appear in the
first-word candidate set."
  (is (eq :complete (nth-value 0 (app::dispatch-command '("__complete" "--") t))))
  (is (eq :completion (nth-value 0 (app::dispatch-command '("completion" "bash") t))))
  (let ((top (app::complete-candidates '())))
    (is-false (member "__complete" top :test #'string=))
    (is-false (member "completion" top :test #'string=))))

(test cli-grammar-responder-context-candidates
  "complete-candidates answers each context from the grammar: the first-word set,
a command's flags and subcommands, a value-flag's static candidates, the live
profile names, a path sentinel, or nothing."
  (flet ((c (ctx) (app::complete-candidates ctx)))
    (let ((top (c '())))
      (dolist (token '("version" "update" "docs" "mcp-serve" "install" "help"
                       "-p" "--print" "--print-authority" "--profile" "-c"
                       "--extension"))
        (is-true (member token top :test #'string=))))
    (is (equal '("--yes" "-y" "--version") (c '("update"))))
    (is (equal '("--yes" "-y") (c '("install"))))
    (is (equal '("index" "search" "help") (c '("docs"))))
    (is (equal '("text" "json" "stream-json") (c '("-p" "--output-format"))))
    ;; --profile is the live source: the builtins are always present, boot-free.
    (let ((profiles (c '("-p" "--profile"))))
      (is-true (member "headless" profiles :test #'string=))
      (is-true (member "interactive-terminal" profiles :test #'string=)))
    ;; Path positions answer with the sentinel alone.
    (is (equal (list app::+completion-file-sentinel+) (c '("-p" "--from-snapshot"))))
    (is (equal (list app::+completion-file-sentinel+) (c '("--extension"))))
    (is (eql (code-char 31) (char app::+completion-file-sentinel+ 0)))
    ;; Value-flags with neither candidates nor a source complete to nothing.
    (is (null (c '("update" "--version"))))
    (is (null (c '("-p" "--grants"))))
    ;; Once a hidden command is typed, its subcommands still complete.
    (is (equal '("bash") (c '("completion"))))))

(test run-complete-emits-candidates-and-sentinel
  "run-complete prints candidates one per line, a path sentinel alone, and exits
0; a no-completion context prints nothing."
  (flet ((out (args) (with-output-to-string (*standard-output*) (app::run-complete args))))
    (is (search "update" (out '("--"))))
    (is (equal app::+completion-file-sentinel+
               (string-right-trim '(#\Newline) (out '("--" "-p" "--from-snapshot")))))
    (is (equal "" (out '("--" "update" "--version"))))
    (is (eql 0 (app::run-complete '("--"))))))

(test run-complete-swallows-faults
  "A fault while computing candidates yields exit 0 and no output, never a
backtrace into the user's shell."
  (let ((original (symbol-function 'app::complete-candidates)))
    (unwind-protect
         (progn
           (setf (symbol-function 'app::complete-candidates)
                 (lambda (context) (declare (ignore context)) (error "boom")))
           (let* ((code nil)
                  (out (with-output-to-string (*standard-output*)
                         (setf code (app::run-complete '("--" "update"))))))
             (is (eql 0 code))
             (is (equal "" out))))
      (setf (symbol-function 'app::complete-candidates) original))))

(test run-completion-emits-bash-shim
  "kli completion bash writes the shim that registers _kli; an unsupported shell
is a diagnostic with a non-zero code."
  (let ((out (with-output-to-string (*standard-output*) (app::run-completion '("bash")))))
    (is (eql 0 (app::run-completion '("bash"))))
    (is (search "complete -F _kli kli" out))
    (is (search "__complete" out)))
  (is (search "complete -F _kli kli" app::+bash-completion-shim+))
  (let* ((err (make-string-output-stream))
         (code (let ((*error-output* err)) (app::run-completion '("zsh")))))
    (is (eql 1 code))
    (is (search "zsh" (get-output-stream-string err)))))
