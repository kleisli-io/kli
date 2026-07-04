(in-package #:kli/tui/completion)

(defparameter +file-walk-limit+ 1000
  "Upper bound on entries collected by the file walk.")

(defun registered-commands (protocol)
  "Commands visible on PROTOCOL, or none when the capability is absent.
File and path completion keep working without a commands provider."
  (let ((provider (find-capability-provider protocol
                                            :commands
                                            :contract :commands/v1)))
    (if provider
        (provider-call provider :list-commands)
        '())))

(defun command-hint (command)
  (command-signature (command-arguments command)))

(defun argument-help (protocol name tail)
  "Argument help for the command named NAME with TAIL typed after it, as
a plist (:candidates entries :hint string), or NIL. The command's
completer owns the answer when set — it receives (command tail) and its
candidates replace the whole tail, each a string or a
(string . description) cons. Without one, the argument spec renders as a
hint while the tail is still blank."
  (let* ((provider (find-capability-provider protocol
                                             :commands
                                             :contract :commands/v1))
         (command (and provider
                       (plusp (length name))
                       (provider-call provider :find-command name))))
    (when command
      (let ((completer (command-completer command)))
        (cond
          (completer (funcall completer command tail))
          ((blank-string-p tail)
           (let ((signature (command-signature
                             (command-arguments command))))
             (and signature (list :hint signature)))))))))

(defun contested-command-table (protocol)
  (let ((provider (find-capability-provider protocol
                                            :commands
                                            :contract :commands/v1))
        (table (make-hash-table :test 'equal)))
    (when provider
      (dolist (entry (provider-call provider :command-collisions))
        (setf (gethash (getf entry :name) table) entry)))
    table))

(defun command-candidate (name command)
  (make-completion-candidate
   :insert (format nil "/~A" name)
   :match name
   :hint (command-hint command)
   :description (command-description command)))

(defun resolve-command-for-completion (protocol name)
  (let* ((provider (find-capability-provider protocol :commands :contract :commands/v1))
         (resolution (and provider (provider-call provider :resolve-command name))))
    (getf resolution :command)))

(defun same-completion-command-p (left right)
  (and left right
       (eq (kli/interaction/commands:command-runner left)
           (kli/interaction/commands:command-runner right))
       (equal (command-arguments left) (command-arguments right))
       (equal (command-description left) (command-description right))
       (equal (kli/interaction/commands:command-metadata left)
              (kli/interaction/commands:command-metadata right))))

(defun qualified-shadow-candidates (protocol collision bare-command)
  (loop with winner = (getf collision :winner)
        for qualified in (getf collision :sources)
        for command = (resolve-command-for-completion protocol qualified)
        unless (or (and winner (string= qualified winner))
                   (same-completion-command-p command bare-command))
          collect (command-candidate qualified command)))

(defun command-candidates (protocol)
  (let ((contested (contested-command-table protocol)))
    (loop for command in (registered-commands protocol)
          for name = (command-name command)
          for collision = (gethash name contested)
          if collision
            append (append (unless (eq (getf collision :status) :ambiguous)
                             (list (command-candidate name command)))
                           (qualified-shadow-candidates protocol collision command))
          else
            collect (command-candidate name command))))

(defun skill-candidates (protocol)
  (loop for command in (registered-commands protocol)
        for name = (command-name command)
        when (string-prefix-p "skill:" name)
          collect (make-completion-candidate
                   :insert (format nil "$~A" (subseq name 6))
                   :match (subseq name 6)
                   :description (command-description command))))

(defun hidden-name-p (name)
  (and (plusp (length name)) (char= (char name 0) #\.)))

(defun directory-entry-name (path)
  (if (uiop:directory-pathname-p path)
      (car (last (pathname-directory path)))
      (file-namestring path)))

(defun walk-files (root)
  "Relative paths under ROOT in breadth-first order, directories carrying
a trailing slash. Ignore rules accumulate per directory, hidden entries
are skipped, and the walk stops at +FILE-WALK-LIMIT+ entries."
  (let ((matcher (make-ignore-matcher))
        (queue (list (cons (uiop:ensure-directory-pathname root) "")))
        (results '())
        (count 0))
    (loop while (and queue (< count +file-walk-limit+))
          do (destructuring-bind (dir . prefix) (pop queue)
               (add-ignore-rules matcher dir root)
               (dolist (entry (append (uiop:subdirectories dir)
                                      (uiop:directory-files dir)))
                 (when (>= count +file-walk-limit+)
                   (return))
                 (let* ((dir-p (uiop:directory-pathname-p entry))
                        (name (directory-entry-name entry))
                        (relative (concatenate 'string
                                               prefix
                                               name
                                               (if dir-p "/" ""))))
                   (unless (or (hidden-name-p name)
                               (path-ignored-p matcher relative))
                     (incf count)
                     (push relative results)
                     (when dir-p
                       (setf queue
                             (nconc queue (list (cons entry relative))))))))))
    (nreverse results)))

(defun file-candidates (protocol)
  (declare (ignore protocol))
  (loop for relative in (walk-files (uiop:getcwd))
        collect (make-completion-candidate
                 :insert (concatenate 'string "@" relative)
                 :match relative)))

(defun split-path-query (query)
  "QUERY split at its last slash into directory part and basename."
  (let ((slash (position #\/ query :from-end t)))
    (if slash
        (values (subseq query 0 (1+ slash)) (subseq query (1+ slash)))
        (values "" query))))

(defun directory-path-strings (query cwd)
  "Path strings naming the entries of the directory QUERY points into,
relative to CWD: the typed directory part plus each entry name, a trailing
slash marking directories. Hidden entries surface only when QUERY's
basename itself starts with a dot."
  (multiple-value-bind (dir-part base) (split-path-query query)
    (let ((directory (merge-pathnames dir-part cwd)))
      (when (uiop:directory-exists-p directory)
        (loop for entry in (append (uiop:subdirectories directory)
                                   (uiop:directory-files directory))
              for dir-p = (uiop:directory-pathname-p entry)
              for name = (directory-entry-name entry)
              unless (and (hidden-name-p name) (not (hidden-name-p base)))
                collect (concatenate 'string dir-part name (if dir-p "/" "")))))))

(defun path-candidates (protocol query)
  "Entries of the directory QUERY points into, matched on the full path
so editor-side ranking sees the query verbatim. Hidden entries surface
only when the query basename itself starts with a dot."
  (declare (ignore protocol))
  (loop for path in (directory-path-strings query (uiop:getcwd))
        collect (make-completion-candidate :insert path :match path)))
