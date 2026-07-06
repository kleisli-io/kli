(in-package #:kli/app)

;;;; The command-line grammar as one value: dispatch-command walks it, the print
;;;; driver derives its value-flag set from it, and shell completion answers from
;;;; it -- so a flag or route added here reaches all three and completion can
;;;; never offer what the parser rejects.
;;;;
;;;; Command: (:id kw :tokens (...) [:default t] [:hidden t] [:subcommands (...)]
;;;;           :flags (flag...)). :default has no :tokens; :hidden hides from
;;;; completion candidate sets.
;;;; Flag: (:name tok :aliases (...) [:value t] [:candidates (...static...)]
;;;;        [:source :profiles|:file|:dir]). :value t consumes the next token;
;;;; :source is dynamic (profiles live, file/dir via a shell sentinel).

(defparameter +cli-grammar+
  '((:id :version :tokens ("version" "--version" "-V"))
    (:id :update :tokens ("update")
     :flags ((:name "--yes" :aliases ("-y"))
             (:name "--version" :value t)))
    (:id :docs :tokens ("docs")
     :subcommands ("index" "search" "help"))
    (:id :mcp-serve :tokens ("mcp-serve"))
    (:id :install :tokens ("install")
     :flags ((:name "--yes" :aliases ("-y"))))
    (:id :help :tokens ("help" "--help" "-h"))
    (:id :print :tokens ("-p" "--print")
     :flags ((:name "--output-format" :value t
              :candidates ("text" "json" "stream-json"))
             (:name "--profile" :value t :source :profiles)
             (:name "--grants" :value t)
             (:name "--model-option" :aliases ("--option")
              :value t :source :model-options)
             (:name "--from-snapshot" :value t :source :file)
             (:name "--timings")
             (:name "--read-only")
             (:name "--no-bash")))
    (:id :print-authority :tokens ("--print-authority")
     :flags ((:name "--profile" :value t :source :profiles)
             (:name "--read-only")
             (:name "--no-bash")
             (:name "--json")))
    (:id :tui :default t
     :flags ((:name "--profile" :value t :source :profiles)
             (:name "-c" :aliases ("--continue"))
             (:name "--extension" :value t :source :file)))
    (:id :complete :tokens ("__complete") :hidden t)
    (:id :completion :tokens ("completion") :subcommands ("bash") :hidden t))
  "The kli command-line grammar.")

;;;; Command accessors. All but cli-command-id take an id OR a plist.

(defun cli-command-id (command) (getf command :id))

(defun cli-command-by-id (id)
  "The grammar command whose :id is ID, or NIL."
  (find id +cli-grammar+ :key #'cli-command-id))

(defun ensure-cli-command (id-or-command)
  "Resolve a keyword id to its command plist; pass a plist through unchanged."
  (if (keywordp id-or-command)
      (cli-command-by-id id-or-command)
      id-or-command))

(defun cli-command-tokens (id-or-command)
  (getf (ensure-cli-command id-or-command) :tokens))
(defun cli-command-flags (id-or-command)
  (getf (ensure-cli-command id-or-command) :flags))
(defun cli-command-subcommands (id-or-command)
  (getf (ensure-cli-command id-or-command) :subcommands))
(defun cli-command-default-p (id-or-command)
  (getf (ensure-cli-command id-or-command) :default))
(defun cli-command-hidden-p (id-or-command)
  (getf (ensure-cli-command id-or-command) :hidden))

(defun cli-command-for-token (token)
  "The command an exact leading TOKEN selects, or NIL. :default has no :tokens."
  (find-if (lambda (command)
             (member token (cli-command-tokens command) :test #'string=))
           +cli-grammar+))

(defun cli-default-command ()
  "The single fall-through command (the terminal UI)."
  (find-if #'cli-command-default-p +cli-grammar+))

;;;; Flag accessors.

(defun cli-flag-name (flag) (getf flag :name))
(defun cli-flag-aliases (flag) (getf flag :aliases))
(defun cli-flag-value-p (flag) (getf flag :value))
(defun cli-flag-candidates (flag) (getf flag :candidates))
(defun cli-flag-source (flag) (getf flag :source))

(defun cli-flag-tokens (flag)
  "FLAG's name then aliases, as a fresh list callers may nconc without mutating
the grammar literal."
  (cons (cli-flag-name flag) (copy-list (cli-flag-aliases flag))))

(defun cli-command-find-flag (id-or-command token)
  "The flag of the command whose name or an alias is TOKEN, or NIL."
  (find-if (lambda (flag)
             (member token (cli-flag-tokens flag) :test #'string=))
           (cli-command-flags id-or-command)))

(defun cli-command-flag-tokens (id-or-command)
  "Every flag token (names and aliases) the command accepts."
  (loop for flag in (cli-command-flags id-or-command)
        append (cli-flag-tokens flag)))

(defun cli-command-value-flag-tokens (id-or-command)
  "Tokens of the command's value-consuming flags; print derives +print-value-flags+ here."
  (loop for flag in (cli-command-flags id-or-command)
        when (cli-flag-value-p flag)
          append (cli-flag-tokens flag)))
