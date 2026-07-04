(in-package #:kli/app)

;;; Headless `kli install <url> <git-tree-sha1>`. Boot the headless profile,
;;; verify the pinned artifact, and place it durably WITHOUT activating it into
;;; the running image -- a later process rediscovers it from disk. The declared
;;; extension id is the machine output on stdout; the trust cards and the outcome
;;; line go to stderr, so a caller can capture the id cleanly.

(defun parse-install-argv (args)
  "Split the `install` ARGV into (values URL GIT-TREE-SHA1 AUTO-CONFIRM OK).
--yes/-y anywhere sets AUTO-CONFIRM; the two remaining positionals are the url and
the git-tree-sha1. OK is NIL on an unknown flag, or on other than exactly two
positionals, so the caller answers a malformed invocation with the usage code."
  (let ((auto nil) (bad nil) (positionals '()))
    (dolist (arg args)
      (cond ((member arg '("--yes" "-y") :test #'string=) (setf auto t))
            ((and (plusp (length arg)) (char= (char arg 0) #\-)) (setf bad t))
            (t (push arg positionals))))
    (setf positionals (nreverse positionals))
    (if (and (not bad) (= 2 (length positionals)))
        (values (first positionals) (second positionals) auto t)
        (values nil nil auto nil))))

(defun prompt-install-stage (stage card)
  "Interactive per-stage consent: prompt on stderr and read one line from stdin,
defaulting to no. The stage's card text has already been reported, so this asks
only the question."
  (declare (ignore stage card))
  (format *error-output* "~&Proceed? [y/N] ")
  (force-output *error-output*)
  (let ((line (read-line *standard-input* nil "")))
    (and (plusp (length line)) (char-equal (char line 0) #\y))))

(defun install-confirm-fn (auto)
  "The (stage card) consent predicate for a headless install. AUTO auto-confirms
every stage; an interactive terminal prompts per stage; a non-terminal without
--yes denies, so a piped install refuses rather than blocking on a prompt no one
can answer."
  (cond (auto (constantly t))
        ((terminal-input-tty-p) #'prompt-install-stage)
        (t (constantly nil))))

(defun report-install-phase (event)
  "Surface an install phase EVENT to stderr: the A0/A1 trust cards, so the operator
sees what they are consenting to. Other events carry no card and are silent here;
the final outcome line is written by run-install."
  (let ((card (getf event :card)))
    (when card
      (format *error-output* "~&~A~%" (getf card :text)))))

(defun run-install (args)
  "Run `kli install <url> <git-tree-sha1> [--yes]` and return a process exit code.
Boots the headless profile, mints a bounded install subject, verifies the pinned
artifact through the two-phase consent flow, and places it durably without
activating it into the running image. On success the declared id prints to stdout
and `Installed <id>.` to stderr; exit 0. A malformed invocation exits 2; a refusal,
cancellation, or verification failure exits 3, having placed nothing."
  (multiple-value-bind (url sha auto ok) (parse-install-argv args)
    (unless ok
      (format *error-output* "Usage: kli install <url> <git-tree-sha1> [--yes]~%")
      (return-from run-install 2))
    (let* ((settings (load-settings))
           (context (main :profile :headless :settings settings))
           (protocol (active-protocol context)))
      (report-boot-diagnostics context)
      (let ((kli/ext:*call-subject*
              (kli/ext:make-subject
               :capabilities '(:manifest/install-remote :image/eval
                               :image/load-native))))
        (multiple-value-bind (state detail)
            (install-remote-extension
             (list :url url :git-tree-sha1 sha) protocol context
             :confirm-fn (install-confirm-fn auto)
             :on-phase #'report-install-phase
             :commit (lambda (meta artifact)
                       (commit-remote-install-no-activate meta artifact protocol nil)))
          (case state
            (:installed
             (format *standard-output* "~A~%" detail)
             (format *error-output* "~A~%" (install-result-text detail :installed nil))
             0)
            (t
             (format *error-output* "~A~%"
                     (install-result-text (url-derived-id url) state detail))
             3)))))))
