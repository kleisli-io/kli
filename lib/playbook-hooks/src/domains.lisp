;;;; playbook-hooks - Domain Detection
;;;;
;;;; Detect programming domains from text content and file paths.

(in-package :playbook-hooks)

(defparameter *domain-keywords*
  '(("nix" . ("nix" "nixos" "nixpkgs" "flake" "derivation" "buildlisp" "mkderivation"))
    ("lisp" . ("lisp" "sbcl" "swank" "defun" "defmacro" "quicklisp" "asdf" "fiveam"))
    ("ace" . ("ace" "playbook" "pattern" "workflow" "reflection" "handoff" "observation"))
    ("python" . ("python" "pip" "pytest" "django" "flask" "venv"))
    ("rust" . ("rust" "cargo" "crate" "rustc" "tokio"))
    ("javascript" . ("javascript" "typescript" "node" "npm" "react" "vue"))
    ("shell" . ("bash" "shell" "zsh" "script" "shebang"))
    ("git" . ("git" "commit" "branch" "merge" "rebase" "pull request"))
    ("docker" . ("docker" "container" "dockerfile" "compose" "kubernetes" "k8s")))
  "Alist mapping domain names to keyword lists for text detection.")

(defparameter *extension-domains*
  '((".nix" . "nix")
    (".lisp" . "lisp")
    (".cl" . "lisp")
    (".asd" . "lisp")
    (".py" . "python")
    (".rs" . "rust")
    (".js" . "javascript")
    (".ts" . "javascript")
    (".tsx" . "javascript")
    (".jsx" . "javascript")
    (".sh" . "shell")
    (".bash" . "shell"))
  "Alist mapping file extensions to domain names.")

(defun detect-domains-from-text (text)
  "Detect domains mentioned in TEXT by keyword matching.
Returns a list of unique domain name strings, sorted alphabetically."
  (when text
    (let ((lower (string-downcase text))
          (found nil))
      (dolist (entry *domain-keywords*)
        (let ((domain (car entry))
              (keywords (cdr entry)))
          (when (some (lambda (kw) (search kw lower)) keywords)
            (pushnew domain found :test #'string=))))
      (sort found #'string<))))

(defun detect-domain-from-path (path)
  "Detect a single domain from a file PATH based on extension.
Returns a domain string or NIL."
  (when path
    (let ((lower (string-downcase path)))
      (loop for (ext . domain) in *extension-domains*
            when (and (>= (length lower) (length ext))
                      (string= ext (subseq lower (- (length lower) (length ext)))))
              return domain))))
