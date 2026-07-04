(in-package #:kli/context/commands)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun agent-session-service (context)
  (or (find-live-object (context-registry context) :agent-session-service)
      (error "No agent-session service is loaded.")))

(defun resolve-agent-context (context arguments)
  (let* ((mode-id (or (getf arguments :mode-id) :default-mode))
         (service (agent-session-service context))
         (agent-context (agent-session-context service mode-id context)))
    (unless agent-context
      (error "No agent-context bound for mode ~S." mode-id))
    agent-context))

(defun command-result (text &key details error-p)
  (make-command-result
   :content (list (make-command-text-content text))
   :details details
   :error-p error-p))

(defun strip-leading-word (tail word)
  "Returns TAIL with the first occurrence of WORD and any following whitespace
removed. Returns TAIL unchanged when WORD is absent."
  (let ((position (search word tail :test #'char=)))
    (if position
        (let ((cursor (+ position (length word)))
              (length (length tail)))
          (loop while (and (< cursor length)
                           (whitespace-char-p (char tail cursor)))
                do (incf cursor))
          (subseq tail cursor))
        tail)))

(defun parse-integer-or-nil (string)
  (when (and string (plusp (length string)))
    (handler-case (parse-integer string)
      (error () nil))))

(defun patch-summary-line (patch)
  (let ((payload (context-patch-payload patch)))
    (case (context-patch-kind patch)
      (:append-message
       (let ((message (getf payload :message)))
         (format nil "  append: ~A"
                 (and message (message-content message)))))
      (:remove-message
       (format nil "  remove: index=~S message-id=~S"
               (getf payload :index)
               (getf payload :message-id)))
      (:replace-message
       (let ((message (getf payload :message)))
         (format nil "  replace: ~A"
                 (and message (message-content message)))))
      (otherwise
       (format nil "  ~A: ~S"
               (context-patch-kind patch)
               payload)))))

(defun format-staged-patches (patches)
  (format nil "~{~A~^~%~}" (mapcar #'patch-summary-line patches)))

(defun run-inspect-context (command arguments context
                            &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((agent-context (resolve-agent-context context arguments))
         (staged (context-staged-patches agent-context))
         (projected (context-projected-messages agent-context))
         (epoch (context-epoch agent-context))
         (header (format nil "Context epoch ~D, ~D message~:P, ~D staged."
                         epoch (length projected) (length staged))))
    (command-result
     (if staged
         (format nil "~A~%Staged:~%~A" header (format-staged-patches staged))
         header)
     :details (list :epoch epoch
                    :projected-count (length projected)
                    :staged-count (length staged)))))

(defun stage-usage-result ()
  (command-result
   "Usage: /context stage append <text> | remove <index> | replace <index> <text>."
   :error-p t))

(defun stage-and-summarize (agent-context patch kind-label)
  (stage-context-patch agent-context patch)
  (command-result
   (format nil "Staged ~A patch (~D total)."
           kind-label
           (length (context-staged-patches agent-context)))
   :details (list :patch-id (object-id patch)
                  :kind (context-patch-kind patch)
                  :staged-count
                  (length (context-staged-patches agent-context)))))

(defun run-stage-append (agent-context tail)
  (let ((text (trim-whitespace tail)))
    (if (blank-string-p text)
        (stage-usage-result)
        (stage-and-summarize agent-context
                             (make-append-message-patch
                              (make-user-message text))
                             "append-message"))))

(defun run-stage-remove (agent-context tail)
  (let* ((text (trim-whitespace tail))
         (index (parse-integer-or-nil text)))
    (cond
      ((blank-string-p text)
       (stage-usage-result))
      ((null index)
       (command-result
        "Usage: /context stage remove <index>."
        :error-p t))
      (t
       (stage-and-summarize agent-context
                            (make-remove-message-patch :index index)
                            "remove-message")))))

(defun run-stage-replace (agent-context tail)
  (let* ((words (split-on-whitespace tail))
         (index (parse-integer-or-nil (first words)))
         (rest-text (when index
                      (trim-whitespace
                       (strip-leading-word tail (first words))))))
    (cond
      ((null index)
       (command-result
        "Usage: /context stage replace <index> <text>."
        :error-p t))
      ((blank-string-p rest-text)
       (command-result
        "Usage: /context stage replace <index> <text>."
        :error-p t))
      (t
       (stage-and-summarize agent-context
                            (make-replace-message-patch
                             (make-user-message rest-text)
                             :index index)
                            "replace-message")))))

(defun run-stage-context (command arguments context
                          &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((words (argument-words arguments))
         (sub-word (first words))
         (tail (getf arguments :tail "")))
    (when (or (null sub-word) (blank-string-p sub-word))
      (return-from run-stage-context (stage-usage-result)))
    (let ((rest-tail (strip-leading-word tail sub-word))
          (agent-context (resolve-agent-context context arguments)))
      (cond
        ((string-equal sub-word "append")
         (run-stage-append agent-context rest-tail))
        ((string-equal sub-word "remove")
         (run-stage-remove agent-context rest-tail))
        ((string-equal sub-word "replace")
         (run-stage-replace agent-context rest-tail))
        (t
         (command-result
          (format nil "Unknown sub-command ~S. ~
Usage: /context stage append <text> | remove <index> | replace <index> <text>."
                  sub-word)
          :error-p t))))))

(defun run-commit-context (command arguments context
                           &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((agent-context (resolve-agent-context context arguments))
         (before (length (context-staged-patches agent-context)))
         (patch-set (commit-context-patches agent-context context)))
    (if patch-set
        (command-result
         (format nil "Committed ~D patch~:P (epoch ~D)."
                 before (context-epoch agent-context))
         :details (list :epoch (context-epoch agent-context)
                        :committed-count before))
        (command-result "No staged patches to commit."))))

(defun run-revert-context (command arguments context
                           &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((agent-context (resolve-agent-context context arguments))
         (cleared (abort-context-patches agent-context)))
    (command-result
     (if cleared
         (format nil "Reverted ~D staged patch~:P." (length cleared))
         "No staged patches to revert."))))

(defun run-diff-context (command arguments context
                         &key call-id on-update)
  (declare (ignore command call-id on-update))
  (let* ((agent-context (resolve-agent-context context arguments))
         (staged (context-staged-patches agent-context)))
    (command-result
     (if staged
         (format nil "Pending changes (~D):~%~A"
                 (length staged)
                 (format-staged-patches staged))
         "No pending context changes."))))

(defun context-command-specs ()
  (list (list :context/inspect
              "Inspect context"
              "Show context projection and staged patches."
              '()
              #'run-inspect-context)
        (list :context/stage
              "Stage context patch"
              "Stage an append, remove, or replace patch (use append|remove|replace as first word)."
              '(:tail :text)
              #'run-stage-context)
        (list :context/commit
              "Commit context patches"
              "Apply all staged patches to the context."
              '()
              #'run-commit-context)
        (list :context/diff
              "Diff context patches"
              "Show pending staged patches without committing."
              '()
              #'run-diff-context)
        (list :context/revert
              "Revert staged patches"
              "Discard all staged context patches."
              '()
              #'run-revert-context)))

(defun register-context-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context))
        (source (contribution-extension contribution)))
    (loop for (name label description arguments runner)
            in (context-command-specs)
          collect (provider-call commands
                                 :register-command
                                 context
                                 name
                                 (make-command :name name
                                               :label label
                                               :description description
                                               :arguments arguments
                                               :runner runner)
                                 :source source
                                 :tier :core))))

(defun unregister-context-commands (protocol contribution context)
  (declare (ignore protocol))
  (let ((commands (commands-provider context)))
    (dolist (registration (contribution-state contribution))
      (provider-call commands
                     :unregister-command
                     context
                     registration))))
