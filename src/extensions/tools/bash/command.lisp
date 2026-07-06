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

(defun command-tail-skip-space (tail index)
  (loop while (and (< index (length tail))
                   (whitespace-char-p (char tail index)))
        do (incf index))
  index)

(defun command-tail-token (tail index)
  (let ((start index))
    (loop while (and (< index (length tail))
                     (not (whitespace-char-p (char tail index))))
          do (incf index))
    (values (subseq tail start index) index)))

(defun command-tail-integer-option (name value)
  (handler-case
      (let ((integer (parse-integer value)))
        (unless (plusp integer)
          (error "~A must be positive." name))
        integer)
    (error ()
      (error "~A expects a positive integer, got ~S." name value))))

(defparameter +bash-command-option-candidates+
  '("--timeout " "-t " "--background " "-b "))

(defun parse-bash-command-tail (tail)
  "Parse human-facing /bash options from TAIL, preserving the command text."
  (let ((index (command-tail-skip-space tail 0))
        (timeout nil)
        (background nil))
    (loop
      (setf index (command-tail-skip-space tail index))
      (when (>= index (length tail))
        (error "Bash command requires a command."))
      (multiple-value-bind (token token-end)
          (command-tail-token tail index)
        (cond
          ((string= token "--")
           (setf index (command-tail-skip-space tail token-end))
           (return))
          ((or (string= token "--background") (string= token "-b"))
           (setf background t
                 index token-end))
          ((or (string= token "--timeout") (string= token "-t"))
           (let ((value-start (command-tail-skip-space tail token-end)))
             (when (>= value-start (length tail))
               (error "~A expects a positive integer." token))
             (multiple-value-bind (value value-end)
                 (command-tail-token tail value-start)
               (setf timeout (command-tail-integer-option token value)
                     index value-end))))
          ((string-prefix-p "--timeout=" token)
           (setf timeout
                 (command-tail-integer-option
                  "--timeout"
                  (subseq token (length "--timeout=")))
                 index token-end))
          (t
           (return)))))
    (let ((command (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                     (subseq tail index))))
      (unless (plusp (length command))
        (error "Bash command requires a command."))
      (values command timeout background))))

(defun maybe-put-tool-parameter (parameters arguments name)
  (let ((missing (list :missing-parameter)))
    (let ((value (getf arguments name missing)))
      (if (eq value missing)
          parameters
          (list* name value parameters)))))

(defun bash-command-parameters (arguments)
  (let ((parameters
          (multiple-value-bind (command timeout background)
              (let ((tail (getf arguments :tail)))
                (if tail
                    (parse-bash-command-tail tail)
                    (values (bash-command-string arguments)
                            (getf arguments :timeout)
                            (getf arguments :run_in_background))))
            (append (list :command command)
                    (when timeout (list :timeout timeout))
                    (when background (list :run_in_background t))))))
    (dolist (name '(:directory :input :shell :timeout :run_in_background) parameters)
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

(defun bash-job-command-id (arguments)
  (let ((job-id (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (or (getf arguments :tail)
                                 (getf arguments :job_id)
                                 ""))))
    (unless (plusp (length job-id))
      (error "Bash job command requires a job id."))
    job-id))

(defun run-bash-output-command (command arguments context &key call-id on-update)
  (declare (ignore command))
  (let ((result (with-operator-capability (context :process/exec)
                  (invoke-tool (active-protocol context)
                               :bash-output
                               (list :job_id (bash-job-command-id arguments))
                               context
                               :call-id call-id
                               :on-update on-update))))
    (command-result-from-tool-result result)))

(defun run-bash-kill-command (command arguments context &key call-id on-update)
  (declare (ignore command))
  (let ((result (with-operator-capability (context :process/exec)
                  (invoke-tool (active-protocol context)
                               :bash-kill
                               (list :job_id (bash-job-command-id arguments))
                               context
                               :call-id call-id
                               :on-update on-update))))
    (command-result-from-tool-result result)))

(defun run-bash-jobs-command (command arguments context &key call-id on-update)
  (declare (ignore command arguments))
  (let ((result (with-operator-capability (context :process/exec)
                  (invoke-tool (active-protocol context)
                               :bash-jobs
                               '()
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

(defparameter +bash-programmable-completion-timeout-seconds+ 1.0)

(defparameter +bash-programmable-completion-script+
  "set +e
line=$1
point=$2
shift 2
COMP_LINE=$line
COMP_POINT=$point
COMP_WORDS=(\"$@\")
COMP_CWORD=$(($# - 1))
if (( COMP_CWORD < 0 )); then exit 0; fi
cmd=${COMP_WORDS[0]}
cur=${COMP_WORDS[COMP_CWORD]}
prev=
if (( COMP_CWORD > 0 )); then prev=${COMP_WORDS[COMP_CWORD-1]}; fi
for f in \"$KLI_BASH_COMPLETION_FILE\" /usr/share/bash-completion/bash_completion /etc/bash_completion; do
  if [[ -n $f && -r $f ]]; then source \"$f\" >/dev/null 2>/dev/null || true; fi
done
compopt() { return 0; }
spec=$(complete -p -- \"$cmd\" 2>/dev/null || true)
if [[ -z $spec ]] && declare -F _completion_loader >/dev/null; then
  _completion_loader \"$cmd\" >/dev/null 2>/dev/null || true
  spec=$(complete -p -- \"$cmd\" 2>/dev/null || true)
fi
if [[ $spec =~ (^|[[:space:]])-F[[:space:]]+([^[:space:]]+) ]]; then
  fn=${BASH_REMATCH[2]}
  if declare -F \"$fn\" >/dev/null; then
    COMPREPLY=()
    \"$fn\" \"$cmd\" \"$cur\" \"$prev\" >/dev/null 2>/dev/null || true
    if (( ${#COMPREPLY[@]} )); then
      printf '%s\\n' \"${COMPREPLY[@]}\"
      exit 0
    fi
  fi
fi
if [[ $spec =~ (^|[[:space:]])-W[[:space:]]+([^[:space:]]+) ]]; then
  for word in ${BASH_REMATCH[2]}; do
    case \"$word\" in \"$cur\"*) printf '%s\\n' \"$word\" ;; esac
  done
  exit 0
fi
if [[ $cur == --* ]]; then
  words=$(\"$cmd\" --help 2>/dev/null | sed -n 's/.*\\(--[A-Za-z0-9][A-Za-z0-9_-]*\\).*/\\1/p' | sort -u)
  while IFS= read -r word; do
    case \"$word\" in \"$cur\"*) printf '%s\\n' \"$word\" ;; esac
  done <<< \"$words\"
fi
")

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

(defun bash-completion-words (tail)
  "Words from the current simple command in TAIL, including a blank current word
when the cursor rests after whitespace."
  (let ((words '())
        (word (make-string-output-stream))
        (quote nil)
        (escaped nil)
        (open nil)
        (ended-on-gap t))
    (flet ((open-word ()
             (setf open t
                   ended-on-gap nil))
           (emit-word ()
             (when open
               (push (get-output-stream-string word) words)
               (setf word (make-string-output-stream)
                     open nil))
             (setf ended-on-gap t))
           (reset-command ()
             (setf words '()
                   word (make-string-output-stream)
                   open nil
                   quote nil
                   ended-on-gap t)))
      (loop for char across tail do
        (cond
          (escaped
           (open-word)
           (write-char char word)
           (setf escaped nil))
          (quote
           (if (char= char quote)
               (setf quote nil)
               (progn (open-word) (write-char char word))))
          ((char= char #\\)
           (open-word)
           (setf escaped t))
          ((or (char= char #\") (char= char #\'))
           (open-word)
           (setf quote char))
          ((whitespace-char-p char)
           (emit-word))
          ((bash-command-separator-p char)
           (emit-word)
           (reset-command))
          ((or (char= char #\<) (char= char #\>))
           (emit-word))
          (t
           (open-word)
           (write-char char word))))
      (cond
        (open
         (push (get-output-stream-string word) words))
        ((and words ended-on-gap)
         (push "" words)))
      (nreverse words))))

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

(defun read-bash-completion-lines (path)
  (ignore-errors
   (with-open-file (stream path :if-does-not-exist nil)
     (loop for line = (read-line stream nil nil)
           while line
           unless (zerop (length line))
             collect line))))

(defun wait-for-bash-completion-process (process)
  (let ((deadline (+ (get-internal-real-time)
                     (round (* +bash-programmable-completion-timeout-seconds+
                               internal-time-units-per-second)))))
    (loop
      (unless (sb-ext:process-alive-p process)
        (ignore-errors (sb-ext:process-wait process))
        (return t))
      (when (> (get-internal-real-time) deadline)
        (ignore-errors (sb-ext:process-kill process 9))
        (ignore-errors (sb-ext:process-wait process))
        (return nil))
      (sleep 0.01))))

(defun bash-programmable-completion-replies (tail words)
  (let ((bash (bash-completion-program)))
    (when (and bash words)
      (uiop:with-temporary-file (:pathname stdout :keep t
                                 :prefix "kli-bash-completion" :type "out")
        (uiop:with-temporary-file (:pathname stderr :keep t
                                   :prefix "kli-bash-completion" :type "err")
          (unwind-protect
               (let ((process
                       (sb-ext:run-program
                        bash
                        (append (list "--noprofile" "--norc" "-c"
                                      +bash-programmable-completion-script+
                                      "kli-bash-completion"
                                      tail
                                      (princ-to-string (length tail)))
                                words)
                        :wait nil
                        :output stdout
                        :if-output-exists :supersede
                        :error stderr
                        :if-error-exists :supersede)))
                 (when (wait-for-bash-completion-process process)
                   (read-bash-completion-lines stdout)))
            (ignore-errors (uiop:delete-file-if-exists stdout))
            (ignore-errors (uiop:delete-file-if-exists stderr))))))))

(defun bash-programmable-completions (tail start quote)
  (declare (ignore start))
  (let* ((words (bash-completion-words tail))
         (replies (bash-programmable-completion-replies tail words)))
    (when replies
      (loop for reply in (remove-duplicates replies :test #'string=)
            collect (bash-encode-token reply quote nil)))))

(defun bash-help-token-char-p (char)
  (or (alphanumericp char)
      (member char '(#\- #\_ #\.))))

(defun bash-help-command-chain (words)
  (when words
    (cons (first words)
          (loop for word in (rest words)
                while (and (plusp (length word))
                           (not (char= (char word 0) #\-))
                           (not (find #\/ word)))
                collect word))))

(defun bash-help-lines (chain)
  (when chain
    (uiop:with-temporary-file (:pathname stdout :keep t
                               :prefix "kli-bash-help" :type "out")
      (uiop:with-temporary-file (:pathname stderr :keep t
                                 :prefix "kli-bash-help" :type "err")
        (unwind-protect
             (flet ((try (flag)
                      (ignore-errors
                       (let ((process
                               (sb-ext:run-program
                                (first chain)
                                (append (rest chain) (list flag))
                                :search t
                                :wait nil
                                :output stdout
                                :if-output-exists :supersede
                                :error stderr
                                :if-error-exists :supersede)))
                         (when (wait-for-bash-completion-process process)
                           (append (read-bash-completion-lines stdout)
                                   (read-bash-completion-lines stderr)))))))
               (or (try "--help") (try "-h")))
          (ignore-errors (uiop:delete-file-if-exists stdout))
          (ignore-errors (uiop:delete-file-if-exists stderr)))))))

(defun bash-help-options (lines)
  (remove-duplicates
   (loop for line in lines append
     (let ((tokens '())
           (index 0)
           (length (length line)))
       (loop while (< index length) do
         (if (and (char= (char line index) #\-)
                  (< (1+ index) length)
                  (or (char= (char line (1+ index)) #\-)
                      (alphanumericp (char line (1+ index)))))
             (let ((start index))
               (incf index (if (char= (char line (1+ index)) #\-) 2 1))
               (loop while (and (< index length)
                                (bash-help-token-char-p (char line index)))
                     do (incf index))
               (let ((token (subseq line start index)))
                 (when (and (> (length token) 1)
                            (not (string= token "--")))
                   (push token tokens))))
             (incf index)))
       (nreverse tokens)))
   :test #'string=))

(defun bash-help-command-row (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (if (and (>= (length trimmed) (length "usage:"))
             (string-equal "usage:" trimmed :end2 (length "usage:")))
        (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                          (subseq trimmed (length "usage:")))
        trimmed)))

(defun bash-help-next-token-after-chain (line chain-text)
  (let* ((row (bash-help-command-row line))
         (start (length chain-text)))
    (when (and (< start (length row))
               (string= chain-text row :end2 start)
               (whitespace-char-p (char row start)))
      (loop while (and (< start (length row))
                       (whitespace-char-p (char row start)))
            do (incf start))
      (let ((end start))
        (loop while (and (< end (length row))
                         (bash-help-token-char-p (char row end)))
              do (incf end))
        (when (> end start)
          (subseq row start end))))))

(defun bash-help-subcommands (lines chain)
  (let ((chain-text (format nil "~{~A~^ ~}" chain)))
    (remove-duplicates
     (loop for line in lines
           for after-chain = (bash-help-next-token-after-chain line chain-text)
           for token = after-chain
           when (and token
                     (plusp (length token))
                     (not (char= (char token 0) #\-)))
             collect token)
     :test #'string=)))

(defun bash-generic-help-completions (tail literal quote)
  (let* ((words (bash-completion-words tail))
         (prior (butlast words))
         (chain (bash-help-command-chain prior))
         (lines (bash-help-lines chain))
         (tokens (if (and (plusp (length literal))
                          (char= (char literal 0) #\-))
                     (bash-help-options lines)
                     (bash-help-subcommands lines chain))))
    (loop for token in tokens
          when (string-prefix-p literal token)
            collect (bash-encode-token token quote nil))))

(defun bash-command-option-completions (prefix token)
  (let ((matches
          (loop for candidate in +bash-command-option-candidates+
                when (string-prefix-p token candidate)
                  collect (concatenate 'string prefix candidate))))
    (list :candidates matches
          :hint "[-t SECONDS] [-b] <shell command>")))

(defun bash-command-completion-tail (tail)
  (let ((index (command-tail-skip-space tail 0))
        (parsed-option-p nil))
    (loop
      (setf index (command-tail-skip-space tail index))
      (when (>= index (length tail))
        (return (if parsed-option-p
                    (values :command (subseq tail 0 index) "")
                    (values :options (subseq tail 0 index) ""))))
      (let ((token-start index))
        (multiple-value-bind (token token-end)
            (command-tail-token tail index)
          (cond
            ((and (= token-end (length tail))
                  (or (zerop (length token))
                      (char= (char token 0) #\-)))
             (return (values :options (subseq tail 0 token-start) token)))
            ((string= token "--")
             (setf index (command-tail-skip-space tail token-end))
             (return (values :command (subseq tail 0 index) (subseq tail index))))
            ((or (string= token "--background") (string= token "-b"))
             (setf index token-end
                   parsed-option-p t))
            ((or (string= token "--timeout") (string= token "-t"))
             (let ((value-start (command-tail-skip-space tail token-end)))
               (when (>= value-start (length tail))
                 (return (values :timeout-value (subseq tail 0 value-start) "")))
               (multiple-value-bind (value value-end)
                   (command-tail-token tail value-start)
                 (declare (ignore value))
                 (if (= value-end (length tail))
                     (return (values :timeout-value
                                     (subseq tail 0 value-start)
                                     (subseq tail value-start)))
                     (setf index value-end
                           parsed-option-p t)))))
            ((string-prefix-p "--timeout=" token)
             (setf index token-end
                   parsed-option-p t))
            ((char= (char token 0) #\-)
             (return (values :options (subseq tail 0 token-start) token)))
            (t
             (return (values :command (subseq tail 0 index) (subseq tail index))))))))))

(defun bash-shell-command-completer (tail)
  ;; Candidates replace the whole tail; a blank word => hint only, not a dump.
  (multiple-value-bind (start literal quote command-position-p)
      (bash-tail-token tail)
    (let* ((programmable (bash-programmable-completions tail start quote))
           (generic (and (null programmable)
                         (bash-generic-help-completions tail literal quote))))
      (if (and (zerop (length literal)) (not programmable) (not generic))
          (list :hint "<shell command>")
          (let* ((prefix (subseq tail 0 start))
                 (executables-p (and (plusp (length literal))
                                     command-position-p
                                     (not (find #\/ literal))
                                     (not (member (char literal 0) '(#\. #\~)))))
                 (path-completions (and (plusp (length literal))
                                        (bash-path-completions literal
                                                               (uiop:getcwd)
                                                               quote)))
                 (candidates (append programmable
                                     generic
                                     (when executables-p
                                       (bash-path-executables literal quote))
                                     path-completions))
                 (capped (subseq candidates 0
                                 (min +bash-completion-limit+
                                      (length candidates)))))
            (list :candidates
                  (mapcar (lambda (c) (concatenate 'string prefix c)) capped)
                  :hint "<shell command>"))))))

(defun bash-command-completer (command tail)
  (declare (ignore command))
  (multiple-value-bind (mode prefix rest)
      (bash-command-completion-tail tail)
    (case mode
      (:options
       (bash-command-option-completions prefix rest))
      (:timeout-value
       (list :hint "<seconds> <shell command>"))
      (t
       (if (eq *command-completion-mode* :manual)
           (let ((completion (bash-shell-command-completer rest)))
             (if (getf completion :candidates)
                 (list :candidates
                       (mapcar (lambda (candidate)
                                 (concatenate 'string prefix candidate))
                               (getf completion :candidates))
                       :hint (getf completion :hint))
                 completion))
           (list :hint "<shell command>"))))))

(defun make-bash-command ()
  (make-command :name :bash
                :label "Bash"
                :description "Run a shell command. Options: --timeout SECONDS, --background."
                :arguments '(:tail :command)
                :runner #'run-bash-command
                :completer #'bash-command-completer
                :metadata '(:tool bash)))

(defun make-bash-output-command ()
  (make-command :name :bash-output
                :label "Bash output"
                :description "Read new output from a background bash job."
                :arguments '(:tail :job_id)
                :runner #'run-bash-output-command
                :metadata '(:tool bash-output)))

(defun make-bash-kill-command ()
  (make-command :name :bash-kill
                :label "Bash kill"
                :description "Stop a background bash job."
                :arguments '(:tail :job_id)
                :runner #'run-bash-kill-command
                :metadata '(:tool bash-kill)))

(defun make-bash-jobs-command ()
  (make-command :name :bash-jobs
                :label "Bash jobs"
                :description "List background bash jobs."
                :runner #'run-bash-jobs-command
                :metadata '(:tool bash-jobs)))

(defun register-bash-command (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context))
        (source (contribution-extension contribution)))
    (loop for (name command) in
          (list (list :bash (make-bash-command))
                (list :bash-output (make-bash-output-command))
                (list :bash-kill (make-bash-kill-command))
                (list :bash-jobs (make-bash-jobs-command)))
          collect (provider-call commands
                                 :register-command
                                 context
                                 name
                                 command
                                 :source source
                                 :tier :core))))

(defun unregister-bash-command (protocol contribution context)
  (declare (ignore protocol))
  (dolist (registration (contribution-state contribution))
    (provider-call (commands-provider context)
                   :unregister-command
                   context
                   registration))
  (contribution-state contribution))

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
