(in-package #:kli/app)

;;;; One-shot print driver: boot the print profile, bind a private session,
;;;; register a stdout formatter, read the prompt from argv or stdin, submit it
;;;; synchronously on the one engine, and exit with a state-derived code. The
;;;; machine-consumable reply goes to stdout via the formatter; every diagnostic
;;;; goes to stderr, so a caller can pipe stdout cleanly.

(defparameter +print-mode-id+ :print
  "Session mode the print driver binds its lone agent under.")

(defparameter +print-value-flags+
  (cli-command-value-flag-tokens :print)
  "Value-flags skipped when scanning argv for the prompt; derived from the :print
grammar. Boolean flags consume no value and are skipped directly.")

(defparameter +read-only-drop-atoms+ '(:file/write :file/edit :process/exec)
  "Capability atoms --read-only removes from the run subject.")

(defparameter +no-bash-drop-atoms+ '(:process/exec)
  "Capability atoms --no-bash removes from the run subject.")

(defun print-flag-value (args flag)
  "The token following FLAG in ARGS, or NIL when FLAG is absent."
  (loop for (a v) on args when (string= a flag) do (return v)))

(defun print-flag-present-p (args flag)
  "True when boolean FLAG appears anywhere in ARGS."
  (and (member flag args :test #'string=) t))

(defun print-positional-prompt (args)
  "The first non-flag token in ARGS, skipping value-flags and their values, or
NIL when ARGS carries no positional."
  (loop with skip = nil
        for a in args
        do (cond (skip (setf skip nil))
                 ((member a +print-value-flags+ :test #'string=) (setf skip t))
                 ((and (plusp (length a)) (char= (char a 0) #\-)) nil)
                 (t (return a)))))

(defun print-prompt (args stdin)
  "The prompt: the inline positional argv token wins, else the trimmed STDIN
text. NIL when neither yields a non-empty string."
  (or (print-positional-prompt args)
      (let ((text (and stdin (string-right-trim '(#\Newline) stdin))))
        (and text (plusp (length text)) text))))

(defun slurp-stream (stream)
  "All of STREAM as one string."
  (with-output-to-string (out)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof) do (write-line line out))))

(defun print-exit-code (agent-state)
  "Process exit code for a terminal AGENT-STATE: completed is success, aborted
is its own code, anything else is a failure."
  (case agent-state (:idle 0) (:aborted 2) (t 1)))

(defun headless-drop-atoms (read-only no-bash)
  "Capability atoms the headless flags remove: --read-only drops write, edit,
and process exec; --no-bash drops process exec."
  (remove-duplicates
   (append (when read-only (copy-list +read-only-drop-atoms+))
           (when no-bash (copy-list +no-bash-drop-atoms+)))))

(defun attenuate-run-subject (base &key read-only no-bash)
  "BASE narrowed by the headless permission flags. The result is always covered
by BASE (the meet only descends), so escalation is unrepresentable by
construction. With no flag set, BASE is returned unchanged."
  (let ((drop (headless-drop-atoms read-only no-bash)))
    (if drop
        (make-subject :grant (grant-without-atoms (subject-grant base) drop))
        base)))

(defun headless-grants-request (args)
  "The --grants NAME value in ARGS, or NIL. No named grant-set source exists in
this release, so any request fails closed at the call site."
  (print-flag-value args "--grants"))

(defun attenuate-bound-agent (binding context &key read-only no-bash)
  "Narrow the freshly bound print agent's run subject by the headless flags. The
model's tool calls execute under the agent's own subject, so attenuating it here
-- after setup, before submit -- bounds every actuator the run can reach."
  (when (or read-only no-bash)
    (let ((agent (find-live-object (context-registry context)
                                   (mode-binding-agent-id binding))))
      (when agent
        (setf (kli/agent/loop:agent-subject agent)
              (attenuate-run-subject (kli/agent/loop:agent-subject agent)
                                     :read-only read-only :no-bash no-bash))))))

(defun read-snapshot-file (path)
  "Read a durable snapshot datum, with read-time eval disabled since the file
is external input."
  (with-open-file (stream path)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (read stream)))))

(defun restore-from-snapshot (context path)
  "Re-establish the protocol PATH holds as CONTEXT's active protocol, through
the runtime/snapshot provider. Runs under *call-subject*, which carries the
:protocol/restore authority the provider gates on."
  (provider-call (require-capability-provider (active-protocol context)
                                              :runtime/snapshot
                                              :contract :runtime/snapshot/v1)
                 :restore-active-protocol context (read-snapshot-file path)))

(defun run-print-session (context format prompt stream
                          &key (mode-id +print-mode-id+) read-only no-bash
                               from-snapshot)
  "Drive one synchronous turn on CONTEXT's agent-session service: bind a fresh
session, register the FORMAT formatter onto STREAM, submit PROMPT, and return a
state-derived exit code. Setup runs under the first-party interactive subject,
which carries the session switch and listen authority it needs; READ-ONLY and
NO-BASH then attenuate the bound agent's own run subject so the model's tool
calls are gated more tightly than setup. FROM-SNAPSHOT restores a saved image
under that same interactive subject before the prompt is submitted. A signaled
run failure degrades to a stderr note and a failure code; the structured reply
has already reached STREAM through the formatter."
  (let ((kli/ext:*call-subject* *ui-subject*)
        (service (find-live-object (context-registry context)
                                   :agent-session-service)))
    (let ((binding (reset-agent-session service mode-id context)))
      (register-output-formatter context format stream)
      (attenuate-bound-agent binding context :read-only read-only :no-bash no-bash))
    (when from-snapshot
      (restore-from-snapshot context from-snapshot))
    (handler-case
        (let ((agent (submit-agent-session-prompt service mode-id prompt context)))
          (print-exit-code (kli/agent/loop:agent-state-value
                            (kli/agent/loop:agent-state agent))))
      (error (condition)
        (format *error-output* "kli: ~A~%" condition)
        1))))

(defun run-print (&optional args)
  "Boot the print profile and run one prompt to stdout, then exit. The prompt is
the inline positional argument or, absent that, stdin. With neither, report the
miss on stderr and exit non-zero."
  (with-fatal-error-handler ()
    (let* ((settings (load-settings))
           (context (main :profile :print :settings settings)))
      (boot-user-extensions context)
      (report-boot-diagnostics context)
      (with-headless-io ()
        (let ((grants (headless-grants-request args)))
          (when grants
            (format *error-output* "kli: unknown grant set ~S~%" grants)
            (uiop:quit 2)))
        (let* ((format (parse-output-format (print-flag-value args "--output-format")))
               (read-only (print-flag-present-p args "--read-only"))
               (no-bash (print-flag-present-p args "--no-bash"))
               (from-snapshot (print-flag-value args "--from-snapshot"))
               (prompt (print-prompt args (and (not (terminal-input-tty-p))
                                               (slurp-stream *standard-input*)))))
          (if (and prompt (plusp (length prompt)))
              (uiop:quit (run-print-session context format prompt *standard-output*
                                            :read-only read-only :no-bash no-bash
                                            :from-snapshot from-snapshot))
              (progn
                (format *error-output*
                        "kli: no prompt (pass one as an argument or on stdin)~%")
                (uiop:quit 2))))))))

;;;; Authority introspection: resolve the subject a run would hold -- the
;;;; configured capabilities under the resolved profile, narrowed by the same
;;;; --read-only/--no-bash drops a print run applies -- and report its atoms and
;;;; constraints so an operator can size the confinement they run kli inside.
;;;; This reads no prompt and runs no agent; it only describes authority.

(defun authority-report-text (report)
  "Human-readable lines for a grant REPORT (from grant-report)."
  (with-output-to-string (out)
    (let ((caps (getf report :capabilities)))
      (ecase (getf report :scope)
        (:universal
         (format out "authority: universal (every capability, unconstrained)~%"))
        (:universal-except
         (format out "authority: universal except (every capability but those listed)~%")
         (dolist (atom (getf report :excluded))
           (format out "  ~A~%" atom)))
        (:none
         (format out "authority: none (no capability is granted)~%"))
        (:bounded
         (format out "authority:~%")
         (dolist (entry caps)
           (let ((constraint (getf entry :constraint)))
             (format out "  ~A~@[  [~A]~]~%"
                     (getf entry :atom)
                     (unless (string= constraint "any") constraint)))))))))

(defun authority-report-json (report)
  "A jzon object mirroring a grant REPORT: the scope plus the atoms the scope
carries. A bounded report lists {atom, constraint} capability entries; a
universal-except report lists the excluded atom names under \"excluded\"."
  (if (eq (getf report :scope) :universal-except)
      (fmt-obj "type" "authority"
               "scope" "universal-except"
               "excluded" (coerce (getf report :excluded) 'vector))
      (fmt-obj "type" "authority"
               "scope" (string-downcase (symbol-name (getf report :scope)))
               "capabilities"
               (coerce (mapcar (lambda (entry)
                                 (fmt-obj "atom" (getf entry :atom)
                                          "constraint" (getf entry :constraint)))
                               (getf report :capabilities))
                       'vector))))

(defun run-print-authority (args)
  "Boot the would-be run profile, resolve its agent subject the way a real run
does, apply the --read-only/--no-bash drops, and print the resulting authority,
then return an exit code. Defaults to the print profile (mirroring -p); an
explicit --profile inspects that profile instead. --json emits one object."
  (with-fatal-error-handler ()
    (let* ((settings (load-settings))
           (requested (profile-arg-value args))
           (profile (if requested (normalize-extension-id requested) :print))
           (context (main :profile profile :settings settings)))
      (boot-user-extensions context)
      (report-boot-diagnostics context)
      (let* ((read-only (print-flag-present-p args "--read-only"))
             (no-bash (print-flag-present-p args "--no-bash"))
             (base (root-agent-subject context))
             (subject (attenuate-run-subject base :read-only read-only
                                                  :no-bash no-bash))
             (report (grant-report (subject-grant subject))))
        (if (print-flag-present-p args "--json")
            (write-json-line (authority-report-json report) *standard-output*)
            (write-string (authority-report-text report) *standard-output*))
        (finish-output *standard-output*)
        0))))
