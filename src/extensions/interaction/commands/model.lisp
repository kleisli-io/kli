(in-package #:kli/interaction/commands)

(defvar *command-completion-mode* :passive
  "How a command completer is being queried: :passive for live hints/popups,
:manual for an explicit completion request.")

(defclass command-service (live-object)
  ((commands
    :initform (make-hash-table :test #'equal)
    :accessor command-service-commands)))

(defclass command (live-object)
  ((name
    :initarg :name
    :reader command-name)
   (label
    :initarg :label
    :initform nil
    :accessor command-label)
   (description
    :initarg :description
    :initform ""
    :accessor command-description)
   (arguments
    :initarg :arguments
    :initform nil
    :accessor command-arguments)
   (runner
    :initarg :runner
    :accessor command-runner)
   (completer
    :initarg :completer
    :initform nil
    :accessor command-completer)
   (renderer
    :initarg :renderer
    :initform nil
    :accessor command-renderer)
   (metadata
    :initarg :metadata
    :initform '()
    :accessor command-metadata)))

(defclass command-result ()
  ((content
    :initarg :content
    :initform '()
    :reader command-result-content)
   (details
    :initarg :details
    :initform nil
    :reader command-result-details)
   (error-p
    :initarg :error-p
    :initform nil
    :reader command-result-error-p)))

;; Lower binds tighter. A bare name resolves to the lowest-priority
;; registration; ties between distinct sources at that priority are ambiguous,
;; never silently broken. Built-in classification (which sources earn
;; +core-priority+) is the registrar's job -- the model only compares.
(defconstant +alias-priority+ 0)
(defconstant +override-priority+ 10)
(defconstant +core-priority+ 20)
(defconstant +extension-priority+ 30)
;; User templates override extension prompts by name but yield to any command.
(defconstant +user-prompt-priority+ 35)
(defconstant +prompt-priority+ 40)

(defun tier-priority (tier)
  "Priority for a named precedence TIER, lower binding tighter: :core
first-party, :extension the default, :user-prompt the user's own templates,
:prompt extension-shipped templates that never shadow a command, :alias and
:override the user and deliberate-shadow tiers."
  (ecase tier
    (:alias +alias-priority+)
    (:override +override-priority+)
    (:core +core-priority+)
    (:extension +extension-priority+)
    (:user-prompt +user-prompt-priority+)
    (:prompt +prompt-priority+)))

(defclass command-registration ()
  ((command
    :initarg :command
    :reader registration-command)
   (source
    :initarg :source
    :initform nil
    :reader registration-source)
   (override
    :initarg :override
    :initform nil
    :reader registration-override)
   (priority
    :initarg :priority
    :initform +extension-priority+
    :reader registration-priority)))

(defun command-name-string (name)
  (etypecase name
    (symbol (symbol-name name))
    (string name)))

(defun strip-command-prefix (name)
  (let ((string (command-name-string name)))
    (if (and (plusp (length string))
             (char= #\/ (char string 0)))
        (subseq string 1)
        string)))

(defun normalize-command-name (name)
  (let ((string (strip-command-prefix name)))
    (when (zerop (length string))
      (error "Empty command name: ~S" name))
    (intern (string-upcase string) :keyword)))

(defun command-display-name (name)
  (string-downcase (strip-command-prefix name)))

(defun make-command-service (&key (id :commands-service))
  (make-instance 'command-service
                 :id id))

(defun make-command-result (&key content details error-p)
  (make-instance 'command-result
                 :content content
                 :details details
                 :error-p error-p))

(defun make-command-text-content (text)
  (list :type :text :text (princ-to-string text)))

(defun argument-words (arguments)
  (or (getf arguments :words)
      (split-on-whitespace (or (getf arguments :tail) ""))))

(defun normalize-command-result (result)
  (cond
    ((typep result 'command-result)
     result)
    ((stringp result)
     (make-command-result :content (list (make-command-text-content result))))
    (t
     (make-command-result
      :content (list (make-command-text-content (prin1-to-string result)))
      :details result))))

(defun command-error-result (condition)
  (make-command-result
   :content (list (make-command-text-content (format nil "~A" condition)))
   :details (list :condition-type (type-of condition))
   :error-p t))

(defun make-command (&key id name label description arguments runner completer
                       renderer metadata)
  (let* ((id (or id (list :command (normalize-command-name name))))
         (name (command-display-name (or name id))))
    (make-instance 'command
                   :id id
                   :name name
                   :label (or label name)
                   :description (or description "")
                   :arguments arguments
                   :runner (or runner
                               (lambda (command arguments context
                                        &key call-id on-update)
                                 (declare (ignore command arguments context
                                                  call-id on-update))
                                 (make-command-result
                                  :content
                                  (list (make-command-text-content
                                         "Command has no runner."))
                                  :error-p t)))
                   :completer completer
                   :renderer renderer
                   :metadata metadata)))

(defun clone-command (command &key name)
  "A copy of COMMAND under NAME (its own name by default), sharing the runner,
metadata, and display slots so the copy behaves identically under a second
name. The vehicle for an alias: one command reachable by another name, with the
alias name carried for events, listing, and completion."
  (make-command
   :name (or name (command-name command))
   :label (command-label command)
   :description (command-description command)
   :arguments (command-arguments command)
   :runner (command-runner command)
   :completer (command-completer command)
   :renderer (command-renderer command)
   :metadata (command-metadata command)))

(defun make-command-registration (&key command source override priority)
  (make-instance 'command-registration
                 :command command
                 :source source
                 :override override
                 :priority (or priority
                               (if override +override-priority+
                                   +extension-priority+))))

(defun command-signature (arguments)
  "Display form of an argument spec: template strings verbatim,
(:tail name ...) as <name> placeholders, anything else nothing."
  (cond
    ((stringp arguments)
     (and (plusp (length arguments)) arguments))
    ((and (consp arguments)
          (eq (first arguments) :tail)
          (rest arguments))
     (format nil "~{<~(~A~)>~^ ~}" (rest arguments)))))

(defun command-usage-text (command)
  "Usage help for COMMAND, or NIL. Tiers: :usage metadata verbatim, the
completer's blank-tail hint, then the argument spec signature. A completer
is called with (command tail) and returns a plist whose :candidates are
full-tail replacements — strings or (string . description) conses — and
whose :hint describes the position being typed."
  (let ((usage (getf (command-metadata command) :usage)))
    (or usage
        (let* ((completer (command-completer command))
               (line (or (and completer
                              (getf (funcall completer command "") :hint))
                         (command-signature (command-arguments command)))))
          (and line
               (format nil "Usage: /~A ~A" (command-name command) line))))))

(defun call-with-operator-capability (context capability thunk)
  "Run THUNK with *call-subject* elevated to confer CAPABILITY, still bounded by
the session's \"capabilities\" policy. A user-driven command (e.g. /bash, /eval)
needs a model actuator the narrow interactive subject withholds; this confers
exactly that atom, then meets it against the declared policy so a lockdown still
attenuates. An absent policy key is unrestricted, so the capability is conferred
outright; a present key that omits the atom meets to nothing, denying it. The
ambient subject is untouched outside the dynamic extent."
  (let* ((provider (find-capability-provider (active-protocol context)
                                             :config :contract :config/v1))
         (value (and provider
                     (provider-call provider :settings-value context
                                    "capabilities"))))
    (multiple-value-bind (subject present) (capabilities-subject value)
      (let* ((policy (if present subject (make-unrestricted-subject)))
             (elevated (subject-meet
                        (make-subject :capabilities (list capability))
                        policy))
             (*call-subject* elevated))
        (funcall thunk)))))

(defmacro with-operator-capability ((context capability) &body body)
  "Run BODY with *call-subject* elevated to confer CAPABILITY under the session's
capabilities policy. See `call-with-operator-capability`."
  `(call-with-operator-capability ,context ,capability (lambda () ,@body)))

(defun command-key (name)
  (normalize-command-name name))

(defun register-command (service name command &key source override priority tier)
  (unless (typep command 'command)
    (error "Not a command: ~S" command))
  (let* ((name (command-key name))
         (priority (cond (priority priority)
                         (tier (tier-priority tier))
                         (override +override-priority+)
                         (t +extension-priority+)))
         (registration (make-command-registration :command command
                                                  :source source
                                                  :override override
                                                  :priority priority)))
    (push registration
          (gethash name (command-service-commands service)))
    registration))

(defun unregister-command (service registration)
  (let ((names '()))
    (maphash (lambda (name registrations)
               (declare (ignore registrations))
               (push name names))
             (command-service-commands service))
    (dolist (name names)
      (let ((remaining (remove registration
                               (gethash name
                                        (command-service-commands service)))))
        (if remaining
            (setf (gethash name (command-service-commands service))
                  remaining)
            (remhash name (command-service-commands service))))))
  registration)

(defun find-command (service name)
  "The command NAME resolves to, or NIL. A bare name yields its
precedence winner and an ambiguous bare name yields NIL (no single
winner); a qualified SOURCE:NAME selects its source exactly. Callers that
need to tell ambiguity from absence use RESOLVE-COMMAND."
  (values (resolve-command service name)))

(defun list-commands (service)
  "One command per bare name: its precedence winner, matching FIND-COMMAND. An
ambiguous name has no winner, so it falls back to a representative registration."
  (let ((commands '()))
    (maphash (lambda (key registrations)
               (when registrations
                 (push (or (resolve-bare-command service key)
                           (registration-command (first registrations)))
                       commands)))
             (command-service-commands service))
    (sort commands #'string< :key #'command-name)))

(defun source-keyword (string)
  (intern (string-upcase string) :keyword))

(defun split-qualified-name (string)
  "Split SOURCE:NAME at the first colon. Returns (values source name), or
(values NIL string) when there is no interior colon to split on."
  (let ((colon (position #\: string)))
    (if (and colon (plusp colon) (< (1+ colon) (length string)))
        (values (subseq string 0 colon) (subseq string (1+ colon)))
        (values nil string))))

(defun registration-qualified-name (registration)
  "SOURCE:NAME display form, or the bare name when no source is recorded."
  (let ((source (registration-source registration))
        (name (command-name (registration-command registration))))
    (if source
        (format nil "~(~A~):~A" source name)
        name)))

(defun find-qualified-registration (service source name)
  "The registration of NAME by SOURCE, or NIL. When SOURCE defines NAME at
several tiers its tightest-binding one answers, so a qualified name selects a
source's strongest entry, not whichever registered last."
  (let ((key (ignore-errors (command-key name)))
        (source (source-keyword source))
        (winner nil))
    (when key
      (dolist (registration (gethash key (command-service-commands service)))
        (when (and (eq source (registration-source registration))
                   (or (null winner)
                       (< (registration-priority registration)
                          (registration-priority winner))))
          (setf winner registration))))
    winner))

(defun distinct-by-source (registrations)
  "REGISTRATIONS thinned to one per source, keeping each source's most recent
\(the head of its push order)."
  (let ((seen '()) (kept '()))
    (dolist (registration registrations (nreverse kept))
      (let ((source (registration-source registration)))
        (unless (member source seen :test #'eql)
          (push source seen)
          (push registration kept))))))

(defun resolve-bare-command (service key)
  (let ((registrations (gethash key (command-service-commands service))))
    (if (null registrations)
        (values nil :not-found nil)
        (let* ((top (reduce #'min registrations :key #'registration-priority))
               (winners (distinct-by-source
                         (remove top registrations
                                 :key #'registration-priority :test-not #'=))))
          (cond
            ((rest winners) (values nil :ambiguous winners))
            ((rest registrations)
             (values (registration-command (first winners))
                     :resolved-by-precedence winners))
            (t (values (registration-command (first winners))
                       :unique winners)))))))

(defun resolve-command (service name)
  "Resolve NAME to a command. Returns (values command status matches).

NAME is bare or qualified SOURCE:NAME; a leading slash is ignored. A
qualified name selects its source exactly. A bare name resolves by
precedence across every source defining it: the lowest-priority
registration wins, and a tie between distinct sources is :ambiguous with no
winner. STATUS is one of :unique, :resolved-by-precedence, :ambiguous,
:not-found. MATCHES holds the registrations the status speaks to."
  (let ((string (strip-command-prefix name)))
    (if (zerop (length string))
        (values nil :not-found nil)
        (multiple-value-bind (source bare) (split-qualified-name string)
          (let ((hit (and source
                          (find-qualified-registration service source bare))))
            (if hit
                (values (registration-command hit) :unique (list hit))
                (resolve-bare-command service
                                      (normalize-command-name string))))))))

(defun command-collisions (service)
  "Every bare name contested by more than one source, each a plist
(:name :status :winner :sources). STATUS is :shadowed when precedence picks
a winner, or :ambiguous when distinct sources tie at the top tier (no
winner). A pure read of the registry, computable at any time rather than a
register-time side effect, so it catches every contest including ones a
later load introduces."
  (let ((collisions '()))
    (maphash
     (lambda (key registrations)
       (let ((distinct (distinct-by-source registrations)))
         (when (rest distinct)
           (multiple-value-bind (command status matches)
               (resolve-bare-command service key)
             (declare (ignore command))
             (push (list :name (command-display-name key)
                         :status (if (eq status :ambiguous)
                                     :ambiguous :shadowed)
                         :winner (unless (eq status :ambiguous)
                                   (registration-qualified-name
                                    (first matches)))
                         :sources (mapcar #'registration-qualified-name
                                          distinct))
                   collisions)))))
     (command-service-commands service))
    (sort collisions #'string< :key (lambda (entry) (getf entry :name)))))

(defun format-command-collisions (collisions)
  "Human diagnostic line for COLLISIONS, or NIL when there are none. A
shadowed name names its winner; an ambiguous one asks the reader to qualify."
  (when collisions
    (with-output-to-string (out)
      (format out "Command name~P contested by more than one source:"
              (length collisions))
      (dolist (entry collisions)
        (ecase (getf entry :status)
          (:shadowed
           (format out "~%  /~A -> ~A (shadows ~{~A~^, ~})"
                   (getf entry :name) (getf entry :winner)
                   (remove (getf entry :winner) (getf entry :sources)
                           :test #'string=)))
          (:ambiguous
           (format out "~%  /~A is ambiguous: ~{~A~^, ~} -- qualify to choose"
                   (getf entry :name) (getf entry :sources))))))))

(defun maybe-emit-command-event (service context type payload source)
  (declare (ignore service))
  (let* ((protocol (active-protocol context))
         (events (and protocol
                      (find-capability-provider protocol
                                                :events
                                                :contract :events/v1))))
    (when events
      (provider-call events
                     :emit-event
                     context
                     (provider-call events
                                    :make-event
                                    type
                                    :payload payload
                                    :source source)))))

(defun invoke-command (service name arguments context &key call-id on-update)
  (let ((command (or (and (typep name 'command) name)
                     (find-command service name))))
    (unless command
      (error "No command named ~S." name))
    (let ((call-id (or call-id (gensym "COMMAND-CALL-"))))
      (maybe-emit-command-event service
                                context
                                :command/call
                                (list :command (command-name command)
                                      :call-id call-id
                                      :arguments arguments)
                                (object-id command))
      (let ((result
              (handler-case
                  (normalize-command-result
                   (funcall (command-runner command)
                            command
                            arguments
                            context
                            :call-id call-id
                            :on-update on-update))
                (error (condition)
                  (command-error-result condition)))))
        ;; :tail, :content, and :model-visible are plain data so handlers in
        ;; other extensions can consume the payload without importing this
        ;; package's classes. Visibility defaults on -- a command hides its
        ;; invocation from the model only by declaring :model-visible nil.
        (maybe-emit-command-event service
                                  context
                                  :command/result
                                  (list :command (command-name command)
                                        :call-id call-id
                                        :result result
                                        :tail (getf arguments :tail)
                                        :content (command-result-content result)
                                        :model-visible
                                        (getf (command-metadata command)
                                              :model-visible t)
                                        :error-p (command-result-error-p result))
                                  (object-id command))
        result))))

(defun recode-command (command &key runner label description arguments completer
                                 renderer metadata)
  (when runner
    (setf (command-runner command) runner))
  (when label
    (setf (command-label command) label))
  (when description
    (setf (command-description command) description))
  (when arguments
    (setf (command-arguments command) arguments))
  (when completer
    (setf (command-completer command) completer))
  (when renderer
    (setf (command-renderer command) renderer))
  (when metadata
    (setf (command-metadata command) metadata))
  command)
