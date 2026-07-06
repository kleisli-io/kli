(in-package #:kli/app)

;;;; Login-shell completion for `kli`. `kli completion bash` prints a shim that
;;;; delegates every candidate to `kli __complete`, which answers from
;;;; +cli-grammar+ and the live profile set, boot-free and never faulting.

;; Unit-separator byte prefix: cannot collide with a literal candidate. The shim
;; expands #file with `compgen -f`, #dir with `compgen -d`.
(defparameter +completion-file-sentinel+
  (concatenate 'string (string (code-char 31)) "file"))

(defparameter +completion-dir-sentinel+
  (concatenate 'string (string (code-char 31)) "dir"))

(defun profile-completion-candidates ()
  "--profile names: builtins then data profiles from settings, downcased."
  (handler-bind ((warning #'muffle-warning))
    (let ((data-names (sort (loop for name being the hash-keys
                                    of (parse-profile-specs (load-settings))
                                  collect name)
                            #'string<)))
      (mapcar (lambda (name) (string-downcase (symbol-name name)))
              (append (known-profile-names) data-names)))))

(defun model-option-completion-candidates ()
  "KEY=VALUE candidates from global option definitions. Runtime still validates
support against the selected model."
  (sort
   (loop for definition in (registered-model-option-definitions)
         for id = (model-option-definition-id definition)
         append (case (model-option-definition-type definition)
                  (:enum
                   (loop for value in (model-option-definition-enum-values definition)
                         collect (format nil "~A=~(~A~)" id (symbol-name value))))
                  (t (list (format nil "~A=" id)))))
   #'string<))

(defun flag-value-completion (flag)
  "Value-FLAG's argument: static candidates, profile names, a path sentinel, or NIL."
  (cond ((cli-flag-candidates flag) (cli-flag-candidates flag))
        ((eq (cli-flag-source flag) :profiles) (profile-completion-candidates))
        ((eq (cli-flag-source flag) :model-options)
         (model-option-completion-candidates))
        ((eq (cli-flag-source flag) :file) (list +completion-file-sentinel+))
        ((eq (cli-flag-source flag) :dir) (list +completion-dir-sentinel+))
        (t nil)))

(defun top-level-completion ()
  "First-word set: every visible command's tokens plus the pass-through TUI flags."
  (append (loop for command in +cli-grammar+
                unless (cli-command-hidden-p command)
                  append (cli-command-tokens command))
          (cli-command-flag-tokens (cli-default-command))))

(defun complete-candidates (context)
  "Candidates for the left CONTEXT (tokens after `kli`, before the cursor word).
Empty -> first-word set. Else the command CONTEXT's head selects (or the TUI):
if CONTEXT ends on one of its value-flags, that flag's values; else its flag
tokens and subcommands."
  (if (null context)
      (top-level-completion)
      (let* ((command (or (cli-command-for-token (first context))
                          (cli-default-command)))
             (prev (car (last context)))
             (flag (cli-command-find-flag command prev)))
        (if (and flag (cli-flag-value-p flag))
            (flag-value-completion flag)
            (append (cli-command-flag-tokens command)
                    (cli-command-subcommands command))))))

(defun run-complete (args)
  "Answer one completion request and exit 0. ARGS is `-- <left-context>`.
Error-swallowing: a fault prints nothing rather than a backtrace into the shell.
Candidates print one per line; a path position prints its sentinel alone."
  (handler-case
      (let ((context (if (and args (string= (first args) "--")) (rest args) args)))
        (dolist (candidate (complete-candidates context))
          (write-line candidate)))
    (error () nil))
  0)

(defparameter +bash-completion-shim+
  "# kli bash completion: every candidate comes from `kli __complete`; this shim
# only expands the file/dir sentinel locally.
_kli() {
    local cur out
    cur=\"${COMP_WORDS[COMP_CWORD]}\"
    out=\"$(kli __complete -- \"${COMP_WORDS[@]:1:$((COMP_CWORD-1))}\" 2>/dev/null)\"
    [ -z \"$out\" ] && return 0
    case \"$out\" in
        $'\\x1f'file) compopt -o filenames; COMPREPLY=($(compgen -f -- \"$cur\")) ;;
        $'\\x1f'dir)  compopt -o filenames; COMPREPLY=($(compgen -d -- \"$cur\")) ;;
        *)            COMPREPLY=($(compgen -W \"$out\" -- \"$cur\")) ;;
    esac
}
complete -F _kli kli
"
  "The script `kli completion bash` emits; the single source for every consumer.")

(defun run-completion (args)
  "Emit a completion script. `kli completion bash` writes the shim; other shells error."
  (let ((shell (or (first args) "bash")))
    (cond ((string= shell "bash")
           (write-string +bash-completion-shim+)
           0)
          (t (format *error-output* "~&kli completion: unsupported shell ~A~%" shell)
             1))))
