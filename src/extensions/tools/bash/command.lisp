(in-package #:kli/tools/bash)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun command-result-from-tool-result (result)
  (make-command-result
   :content (kli/ext:tool-result-content result)
   :details (kli/ext:tool-result-details result)
   :error-p (kli/ext:tool-result-error-p result)))

(defun bash-command-string (arguments)
  (let ((command (or (getf arguments :tail)
                     (getf arguments :command))))
    (unless (and command (plusp (length command)))
      (error "Bash command requires a command."))
    command))

(defun maybe-put-tool-parameter (parameters arguments name)
  (let ((missing (list :missing-parameter)))
    (let ((value (getf arguments name missing)))
      (if (eq value missing)
          parameters
          (list* name value parameters)))))

(defun bash-command-parameters (arguments)
  (let ((parameters (list :command (bash-command-string arguments))))
    (dolist (name '(:directory :input :shell :timeout) parameters)
      (setf parameters
            (maybe-put-tool-parameter parameters arguments name)))))

(defun run-bash-command (command arguments context &key call-id on-update)
  ;; The interactive subject withholds :process/exec; a user-driven /bash
  ;; elevates locally to confer it, still bounded by the capabilities policy.
  (declare (ignore command))
  (let ((result (with-operator-capability (context :process/exec)
                  (invoke-tool (active-protocol context)
                               :bash
                               (bash-command-parameters arguments)
                               context
                               :call-id call-id
                               :on-update on-update))))
    (command-result-from-tool-result result)))

;; Complete the bash tail's last word against $PATH executables (command
;; position) or the working directory. The word is found by a shell-aware scan
;; that honours quotes, backslash escapes, and the operators that start a fresh
;; command, so `cat "my f`, `a; b`, and `a | b` tokenize the way a shell reads
;; them. Candidates are re-quoted in the word's own style; directories keep a
;; trailing slash so accepting chains into them. The shell expands ~, $VAR, and
;; globs at run time, so the completer only resolves a leading ~/.

(defparameter +bash-completion-limit+ 50)

(defparameter +bash-word-special-characters+
  '(#\Space #\Tab #\Newline #\" #\' #\\ #\$ #\` #\| #\& #\; #\< #\> #\( #\) #\{ #\})
  "Characters a bare (unquoted) word must backslash-escape to survive shell word
splitting. Glob metacharacters and ~ are left intact so completing never defeats
an intended expansion.")

(defun bash-command-separator-p (char)
  "Whether CHAR ends the current command, so the next word is a command name."
  (member char '(#\; #\| #\& #\( #\) #\Newline)))

(defun bash-tail-token (tail)
  "Scan TAIL as a shell line and describe the final word under completion.
Returns (values START LITERAL QUOTE COMMAND-POSITION-P): START is where the word
begins (everything before it is the untouched prefix), LITERAL its dequoted text
so far, QUOTE the open quote character enclosing it or NIL, and COMMAND-POSITION-P
whether the word sits where a command name is expected. A blank LITERAL means the
cursor rests on whitespace or an operator with no word to complete."
  (let ((start nil)
        (literal (make-string-output-stream))
        (quote nil)
        (escaped nil)
        (word-command nil)
        (next-command t))
    (flet ((open-word (index)
             (unless start
               (setf start index
                     word-command next-command
                     next-command nil)))
           (close-word ()
             (setf start nil
                   quote nil
                   literal (make-string-output-stream))))
      (loop for index from 0
            for char across tail do
              (cond
                (escaped
                 (open-word (1- index))
                 (write-char char literal)
                 (setf escaped nil))
                (quote
                 (if (char= char quote)
                     (setf quote nil)
                     (progn (open-word index) (write-char char literal))))
                ((char= char #\\)
                 (open-word index)
                 (setf escaped t))
                ((or (char= char #\") (char= char #\'))
                 (open-word index)
                 (setf quote char))
                ((whitespace-char-p char)
                 (close-word)
                 (when (char= char #\Newline) (setf next-command t)))
                ((bash-command-separator-p char)
                 (close-word)
                 (setf next-command t))
                ((or (char= char #\<) (char= char #\>))
                 (close-word)
                 (setf next-command nil))
                (t
                 (open-word index)
                 (write-char char literal))))
      (if start
          (values start (get-output-stream-string literal) quote word-command)
          (values (length tail) "" nil next-command)))))

(defun bash-dir-entry-name (path)
  (if (uiop:directory-pathname-p path)
      (car (last (pathname-directory path)))
      (file-namestring path)))

(defun bash-hidden-name-p (name)
  (and (plusp (length name)) (char= (char name 0) #\.)))

(defun bash-encode-token (text quote dir-p)
  "Re-encode the completed word TEXT in the quoting style QUOTE the user opened
(#\\\" , #\\' , or NIL for bare). A directory token keeps its open quote so the
editor chains another segment after the trailing slash; a file closes it."
  (case quote
    (#\"
     (concatenate 'string "\""
                  (with-output-to-string (out)
                    (loop for char across text do
                      (when (member char '(#\" #\\ #\$ #\`)) (write-char #\\ out))
                      (write-char char out)))
                  (if dir-p "" "\"")))
    (#\'
     (concatenate 'string "'"
                  (with-output-to-string (out)
                    (loop for char across text do
                      (if (char= char #\')
                          (write-string "'\\''" out)
                          (write-char char out))))
                  (if dir-p "" "'")))
    (t
     (with-output-to-string (out)
       (loop for char across text do
         (when (member char +bash-word-special-characters+) (write-char #\\ out))
         (write-char char out))))))

(defun bash-path-completions (literal cwd quote)
  "Entries under the directory LITERAL points into, each re-encoded with QUOTE.
A leading ~/ resolves against home; hidden entries surface only when the typed
basename is itself dotted."
  (let* ((slash (position #\/ literal :from-end t))
         (dir-part (if slash (subseq literal 0 (1+ slash)) ""))
         (base (if slash (subseq literal (1+ slash)) literal))
         (directory (if (and (plusp (length dir-part))
                             (char= (char dir-part 0) #\~))
                        (merge-pathnames (subseq dir-part 2)
                                         (user-homedir-pathname))
                        (merge-pathnames dir-part
                                         (uiop:ensure-directory-pathname cwd)))))
    (when (uiop:directory-exists-p directory)
      (loop for entry in (append (uiop:subdirectories directory)
                                 (uiop:directory-files directory))
            for name = (bash-dir-entry-name entry)
            for dir-p = (uiop:directory-pathname-p entry)
            when (and (string-prefix-p base name)
                      (or (plusp (length base)) (not (bash-hidden-name-p name))))
              collect (bash-encode-token
                       (concatenate 'string dir-part name (if dir-p "/" ""))
                       quote dir-p)))))

(defun bash-executable-file-p (path)
  "Whether PATH is accessible with the execute bit, per access(2) X_OK."
  (and (ignore-errors
         (sb-unix:unix-access (uiop:native-namestring path) sb-unix:x_ok))
       t))

(defvar *bash-path-executable-cache* nil
  "Memo of (PATH-string . sorted executable names) so the $PATH walk and its
stat-per-file cost run once per distinct PATH rather than on every keystroke.")

(defun bash-path-executable-names ()
  "Executable basenames across $PATH, earliest directory winning on collisions.
Cached on the PATH string; a changed PATH rebuilds it."
  (let ((path (or (uiop:getenv "PATH") "")))
    (unless (equal (car *bash-path-executable-cache*) path)
      (let ((seen (make-hash-table :test 'equal))
            (names '()))
        (dolist (dir (uiop:split-string path :separator '(#\:)))
          (when (plusp (length dir))
            (ignore-errors
              (dolist (file (uiop:directory-files
                             (uiop:ensure-directory-pathname dir)))
                (let ((name (file-namestring file)))
                  (when (and (plusp (length name))
                             (not (gethash name seen))
                             (bash-executable-file-p file))
                    (setf (gethash name seen) t)
                    (push name names)))))))
        (setf *bash-path-executable-cache* (cons path (nreverse names)))))
    (cdr *bash-path-executable-cache*)))

(defun bash-path-executables (prefix quote)
  "PATH executables matching PREFIX, re-encoded with QUOTE."
  (loop for name in (bash-path-executable-names)
        when (string-prefix-p prefix name)
          collect (bash-encode-token name quote nil)))

(defun bash-command-completer (command tail)
  ;; Candidates replace the whole tail; a blank word => hint only, not a dump.
  (declare (ignore command))
  (multiple-value-bind (start literal quote command-position-p)
      (bash-tail-token tail)
    (if (zerop (length literal))
        (list :hint "<shell command>")
        (let* ((prefix (subseq tail 0 start))
               (executables-p (and command-position-p
                                   (not (find #\/ literal))
                                   (not (member (char literal 0) '(#\. #\~)))))
               (candidates
                 (append (when executables-p
                           (bash-path-executables literal quote))
                         (bash-path-completions literal (uiop:getcwd) quote)))
               (capped (subseq candidates 0
                               (min +bash-completion-limit+
                                    (length candidates)))))
          (list :candidates
                (mapcar (lambda (c) (concatenate 'string prefix c)) capped)
                :hint "<shell command>")))))

(defun make-bash-command ()
  (make-command :name :bash
                :label "Bash"
                :description "Run a shell command."
                :arguments '(:tail :command)
                :runner #'run-bash-command
                :completer #'bash-command-completer
                :metadata '(:tool bash)))

(defun register-bash-command (protocol contribution context)
  (declare (ignore protocol))
  (provider-call (commands-provider context)
                 :register-command
                 context
                 :bash
                 (make-bash-command)
                 :source (contribution-extension contribution)
                 :tier :core))

(defun unregister-bash-command (protocol contribution context)
  (declare (ignore protocol))
  (provider-call (commands-provider context)
                 :unregister-command
                 context
                 (contribution-state contribution)))

(defun resolve-bash-exec-provider-id (word)
  "Bash-exec provider id named by WORD: `persistent` for the persistent shell,
`local` for the built-in per-command backend. NIL when unrecognized."
  (cond
    ((or (string-equal word "persistent") (string-equal word "shell"))
     +persistent-shell-bash-exec-provider-id+)
    ((or (string-equal word "local") (string-equal word "default"))
     +local-bash-exec-provider-id+)
    (t nil)))

(defun bash-exec-provider-label (id)
  (cond
    ((eql id +persistent-shell-bash-exec-provider-id+) "persistent shell")
    ((eql id +local-bash-exec-provider-id+) "local")
    (t (princ-to-string id))))

(defun run-bash-shell-command (command arguments context &key call-id on-update)
  "Select the bash execution backend for the session, or report the current one
when called bare. `persistent` routes the Bash tool through one long-lived shell
so cd and export persist across commands; `local` restores the per-command
backend."
  (declare (ignore command call-id on-update))
  (let* ((protocol (active-protocol context))
         (word (string-trim '(#\Space #\Tab #\Newline)
                            (or (getf arguments :tail) ""))))
    (if (zerop (length word))
        (make-command-result
         :content (list (make-command-text-content
                         (format nil "Bash backend: ~A (persistent | local)."
                                 (bash-exec-provider-label
                                  (active-bash-exec-provider-id protocol))))))
        (let ((id (resolve-bash-exec-provider-id word)))
          (if id
              (progn
                (setf (active-bash-exec-provider-id protocol) id)
                (make-command-result
                 :content (list (make-command-text-content
                                 (format nil "Bash backend: ~A."
                                         (bash-exec-provider-label id))))))
              (make-command-result
               :content (list (make-command-text-content
                               (format nil "Unknown backend ~S (use persistent or local)."
                                       word)))
               :error-p t))))))

(defun make-bash-shell-command ()
  (make-command :name :bash-shell
                :label "Bash backend"
                :description "Select the bash execution backend: persistent or local."
                :arguments '(:tail)
                :runner #'run-bash-shell-command))

(defun register-bash-shell-command (protocol contribution context)
  (declare (ignore protocol))
  (provider-call (commands-provider context)
                 :register-command
                 context
                 :bash-shell
                 (make-bash-shell-command)
                 :source (contribution-extension contribution)
                 :tier :core))

(defun unregister-bash-shell-command (protocol contribution context)
  (declare (ignore protocol))
  (provider-call (commands-provider context)
                 :unregister-command
                 context
                 (contribution-state contribution)))
