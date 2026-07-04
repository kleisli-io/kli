(in-package #:kli/interaction/commands)

(defclass command-contribution (contribution)
  ((command-name
    :initarg :command-name
    :reader contribution-command-name)
   (label
    :initarg :label
    :initform nil
    :reader contribution-command-label)
   (description
    :initarg :description
    :initform ""
    :reader contribution-command-description)
   (arguments
    :initarg :arguments
    :initform nil
    :reader contribution-command-arguments)
   (runner
    :initarg :runner
    :reader contribution-command-runner)
   (completer
    :initarg :completer
    :initform nil
    :reader contribution-command-completer)
   (metadata
    :initarg :metadata
    :initform '()
    :reader contribution-command-metadata)
   (tier
    :initarg :tier
    :initform nil
    :reader contribution-command-tier)
   (handle
    :initform nil
    :accessor contribution-command-handle))
  (:documentation
   "Backs the command author clause. Registers a slash command against the
per-protocol :commands provider on install and unregisters it on retract, so a
declared command is as installable, retractable, and introspectable as any
other contribution."))

(defun make-command-contribution (&key name command-name label description
                                       arguments runner completer metadata
                                       tier source)
  (make-instance 'command-contribution
                 :kind :command
                 :name (normalize-extension-id name)
                 :command-name (or command-name (command-display-name name))
                 :label label
                 :description (or description "")
                 :arguments arguments
                 :runner runner
                 :completer completer
                 :metadata metadata
                 :tier tier
                 :source source))

(defmethod install-contribution ((protocol extension-protocol)
                                 (contribution command-contribution)
                                 context)
  (let* ((commands (require-capability-provider protocol :commands
                                                :contract :commands/v1))
         (command (make-command
                   :name (contribution-command-name contribution)
                   :label (contribution-command-label contribution)
                   :description (contribution-command-description contribution)
                   :arguments (contribution-command-arguments contribution)
                   :runner (contribution-command-runner contribution)
                   ;; A clause completer receives (command tail context) --
                   ;; the provider contract passes only (command tail), so
                   ;; the install context is closed over here.
                   :completer (let ((completer (contribution-command-completer
                                                contribution)))
                                (and completer
                                     (lambda (command tail)
                                       (funcall completer command tail
                                                context))))
                   :metadata (contribution-command-metadata contribution)))
         (handle (provider-call commands :register-command context
                                (contribution-command-name contribution)
                                command
                                :source (contribution-extension contribution)
                                :tier (contribution-command-tier contribution))))
    (setf (contribution-command-handle contribution) handle)
    (push contribution (protocol-installed-contributions protocol))
    contribution))

(defmethod retract-contribution ((protocol extension-protocol)
                                 (contribution command-contribution)
                                 context)
  (provider-call (require-capability-provider protocol :commands
                                              :contract :commands/v1)
                 :unregister-command context
                 (contribution-command-handle contribution))
  (setf (protocol-installed-contributions protocol)
        (remove contribution (protocol-installed-contributions protocol)))
  contribution)

(defcontribution-kind :command (extension-id form)
  (destructuring-bind (_ name &key label description arguments handler
                              completer metadata tier)
      form
    (declare (ignore _))
    `(make-command-contribution
      :name ,(if (stringp name) name `',name)
      :label ,label
      :description ,description
      :arguments ,arguments
      :runner ,handler
      :completer ,completer
      :metadata ,metadata
      :tier ,tier
      :source ',extension-id)))

(register-author-clause-requirements
 :command '((capability commands :contract commands/v1)))

(defun reply (text)
  "Build a command runner's result from TEXT shown to the user."
  (make-command-result :content (list (make-command-text-content text))))

(defun rest-arg (arguments)
  "The free-text tail of a parsed command's ARGUMENTS plist, or NIL if empty."
  (let ((tail (getf arguments :tail)))
    (and tail (plusp (length tail)) tail)))
