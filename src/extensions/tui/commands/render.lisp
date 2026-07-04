(in-package #:kli/tui/commands)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun make-response-event (text &key status)
  "Direct command output as a :response, distinct from an ambient :notice, so a
   command's answer reads as a reply rather than as background activity."
  (make-transcript-event :response nil text :status status))

(defun text-content-value (content)
  (cond
    ((and (listp content)
          (eq (getf content :type) :text))
     (getf content :text))
    (content
     (prin1-to-string content))
    (t "")))

(defun command-result-text (result)
  (let ((content (command-result-content result)))
    (format nil "~{~A~^~%~}"
            (remove "" (mapcar #'text-content-value content)
                    :test #'string=))))

(defun render-command-result (command-name result)
  (let ((text (command-result-text result))
        (error-p (command-result-error-p result)))
    (cond
      ((plusp (length text))
       ;; No /cmd: prefix: the command is echoed directly above as the user line,
       ;; so the response shows only its body.
       (list (make-response-event text :status (and error-p :error))))
      (error-p
       (list (make-response-event (format nil "/~(~A~) failed."
                                        command-name)
                                :status :error)))
      (t nil))))

(defun missing-command-name-events ()
  (list (make-response-event "Command name required.")))

(defun unknown-command-events (name)
  (list (make-response-event (format nil "Unknown command: /~(~A~)."
                                   name))))

(defun ambiguous-command-events (name matches)
  (list (make-response-event
         (format nil "/~(~A~) is ambiguous: ~{/~A~^, ~}. Qualify to choose."
                 name
                 (mapcar #'registration-qualified-name matches)))))

(defun hierarchical-command-name (name-text first-word)
  (when (and name-text
             (plusp (length name-text))
             first-word
             (plusp (length first-word)))
    (intern (string-upcase (concatenate 'string name-text "/" first-word))
            :keyword)))

(defun strip-leading-word (tail word)
  (let ((position (search word tail :test #'char=)))
    (if position
        (let ((cursor (+ position (length word)))
              (length (length tail)))
          (loop while (and (< cursor length)
                           (whitespace-char-p (char tail cursor)))
                do (incf cursor))
          (subseq tail cursor))
        tail)))

(defun resolve-hierarchical-command (commands parsed)
  "Prefer a hierarchical match (e.g. :context/inspect) when registered.
Returns (values name updated-parsed). Falls through to the bare name when
no hierarchical command matches."
  (let* ((words (getf parsed :words))
         (first-word (first words))
         (hierarchical (hierarchical-command-name
                        (getf parsed :name-text) first-word)))
    (if (and hierarchical
             (provider-call commands :find-command hierarchical))
        (let ((updated (copy-list parsed)))
          (setf (getf updated :name) hierarchical
                (getf updated :name-text)
                (string-downcase (symbol-name hierarchical))
                (getf updated :tail)
                (strip-leading-word (getf parsed :tail) first-word)
                (getf updated :words) (rest words))
          (values hierarchical updated))
        (values (getf parsed :name) parsed))))

(defun dispatch-slash-command (context input &key app mode-id worker-spawner)
  (unless (slash-command-input-p input)
    (return-from dispatch-slash-command (values nil nil)))
  (let* ((parsed (parse-slash-command input))
         (commands (commands-provider context)))
    (unless (getf parsed :name)
      (return-from dispatch-slash-command
        (values t (missing-command-name-events))))
    (multiple-value-bind (name resolved-parsed)
        (resolve-hierarchical-command commands parsed)
      (let* ((arguments (append (when app (list :app app))
                                (when mode-id (list :mode-id mode-id))
                                (when worker-spawner
                                  (list :worker-spawner worker-spawner))
                                resolved-parsed))
             (resolution (provider-call commands :resolve-command name))
             (command (getf resolution :command)))
        (cond
          ((eq (getf resolution :status) :ambiguous)
           (values t (ambiguous-command-events name (getf resolution :matches))))
          ((null command)
           (values t (unknown-command-events name)))
          (t
           (values t
                   (render-command-result
                    name
                    (provider-call commands
                                   :invoke-command
                                   command
                                   arguments
                                   context)))))))))
