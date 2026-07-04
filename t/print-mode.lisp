(in-package #:kli/tests)
(in-suite all)

;;;; The one-shot print driver: -p/--print dispatch routing, prompt resolution
;;;; from argv-or-stdin, terminal-state exit codes, and the synchronous
;;;; run-print-session core driven against a fake model.

(test dispatch-routes-print-flags
  "-p and --print force print mode under both an interactive and a piped channel,
carrying the remaining args; an explicit command still wins over a later flag."
  (flet ((route (argv interactive)
           (multiple-value-list (app::dispatch-command argv interactive))))
    (is (equal '(:print ("2+2")) (route '("-p" "2+2") t)))
    (is (equal '(:print ("2+2")) (route '("-p" "2+2") nil)))
    (is (equal '(:print ("hi")) (route '("--print" "hi") t)))
    (is (equal '(:print ()) (route '("-p") t)))
    (is (eq :version (nth-value 0 (app::dispatch-command '("version" "-p") t))))))

(test print-prompt-resolves-positional-then-stdin
  "The inline positional token wins; value-flags and their values are skipped;
boolean flags consume no value; stdin (newline-trimmed) is the fallback."
  (is (equal "say hi" (app::print-positional-prompt '("say hi"))))
  (is (equal "explain"
             (app::print-positional-prompt
              '("-c" "--output-format" "stream-json" "explain"))))
  (is (equal "edit the file"
             (app::print-positional-prompt '("--read-only" "edit the file"))))
  (is (null (app::print-positional-prompt '("--output-format" "json"))))
  (is (null (app::print-positional-prompt '("-c"))))
  (is (equal "inline" (app::print-prompt '("inline") "piped text")))
  (is (equal "piped text"
             (app::print-prompt '("--output-format" "json")
                                (format nil "piped text~%~%"))))
  (is (null (app::print-prompt '() "")))
  (is (null (app::print-prompt '() nil))))

(test print-exit-code-maps-terminal-states
  "Completed is success, aborted is its own code, every other terminal state is
a failure."
  (is (eql 0 (app::print-exit-code :idle)))
  (is (eql 2 (app::print-exit-code :aborted)))
  (is (eql 1 (app::print-exit-code :error)))
  (is (eql 1 (app::print-exit-code :streaming))))

(test run-print-session-text-prints-final-text-and-exits-zero
  "The text formatter writes exactly the final assistant text to the result
stream and a completed run yields exit code 0."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (agent-loop-register-model context "print-provider" "print-model"
                               :metadata (list :fake-deltas '("Hello from print.")))
    (let* ((stream (make-string-output-stream))
           (code (app::run-print-session context :text "say hi" stream)))
      (is (eql 0 code))
      (is (equal "Hello from print."
                 (string-right-trim '(#\Newline)
                                    (get-output-stream-string stream)))))))

(test run-print-session-json-emits-one-result-object
  "The json formatter writes one terminal result object carrying the final text
and the completed run state."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (agent-loop-register-model context "print-provider" "print-model"
                               :metadata (list :fake-deltas '("the answer")))
    (let* ((stream (make-string-output-stream))
           (code (app::run-print-session context :json "q" stream))
           (object (com.inuoe.jzon:parse
                    (string-right-trim '(#\Newline)
                                       (get-output-stream-string stream)))))
      (is (eql 0 code))
      (is (equal "result" (gethash "type" object)))
      (is (equal "the answer" (gethash "text" object)))
      (is (equal "completed" (gethash "state" object))))))

(test run-print-session-failed-run-exits-nonzero-with-stderr-note
  "A non-retryable provider failure degrades to exit code 1 and a stderr note,
and leaks no answer onto the result stream -- the failure signal is the exit
code plus stderr, not stdout content."
  (multiple-value-bind (context protocol) (agent-session-test-context)
    (declare (ignore protocol))
    (agent-loop-register-model context "print-provider" "print-model"
                               :metadata (list :fake-deltas '("unused")))
    (rt:register-model-stream-adapter
     (model-runtime-service context) :fake
     (lambda (provider request ctx &key emit)
       (declare (ignore provider request ctx emit))
       (error "simulated provider failure"))
     context)
    (let* ((stream (make-string-output-stream))
           (err (make-string-output-stream))
           (code (let ((*error-output* err))
                   (app::run-print-session context :text "q" stream))))
      (is (eql 1 code))
      (is (search "simulated provider failure" (get-output-stream-string err)))
      (is (zerop (length (string-trim '(#\Newline #\Space)
                                      (get-output-stream-string stream))))
          "no answer reaches the result stream on failure"))))

(defun print-actuator-base ()
  "A base run subject carrying the standard read plus write/edit/exec actuators."
  (kli/ext:make-subject
   :capabilities '(:file/read :file/write :file/edit :process/exec)))

(test attenuate-run-subject-read-only-drops-write-edit-bash-keeps-read
  "--read-only removes write, edit, and bash from the run subject while leaving
read intact."
  (let ((subject (app::attenuate-run-subject (print-actuator-base) :read-only t)))
    (is (kli/ext:check-capability subject :file/read))
    (is (not (kli/ext:check-capability subject :file/write)))
    (is (not (kli/ext:check-capability subject :file/edit)))
    (is (not (kli/ext:check-capability subject :process/exec)))))

(test attenuate-run-subject-no-bash-drops-exec-only
  "--no-bash removes process exec and nothing else."
  (let ((subject (app::attenuate-run-subject (print-actuator-base) :no-bash t)))
    (is (kli/ext:check-capability subject :file/read))
    (is (kli/ext:check-capability subject :file/write))
    (is (kli/ext:check-capability subject :file/edit))
    (is (not (kli/ext:check-capability subject :process/exec)))))

(test attenuate-run-subject-only-narrows
  "A flag-attenuated subject is always covered by its base, and absent any flag
the base is returned unchanged."
  (let ((base (print-actuator-base)))
    (flet ((narrows-p (subject)
             (kli/ext:grant-covers-p (kli/ext:subject-grant base)
                                     (kli/ext:subject-grant subject))))
      (is (narrows-p (app::attenuate-run-subject base :read-only t)))
      (is (narrows-p (app::attenuate-run-subject base :no-bash t)))
      (is (eq base (app::attenuate-run-subject base))))))

(test attenuate-run-subject-on-unrestricted-base-keeps-read-drops-actuators
  "--read-only against an unrestricted (universal) base yields a co-finite subject
that still reads but withholds write/edit/exec, rather than collapsing to none."
  (let ((subject (app::attenuate-run-subject (kli/ext:make-unrestricted-subject)
                                             :read-only t)))
    (is (kli/ext:check-capability subject :file/read))
    (is (kli/ext:check-capability subject :cairn/read))
    (is (not (kli/ext:check-capability subject :file/write)))
    (is (not (kli/ext:check-capability subject :file/edit)))
    (is (not (kli/ext:check-capability subject :process/exec)))))

(test attenuate-run-subject-no-bash-on-unrestricted-base-drops-exec-only
  "--no-bash against an unrestricted base withholds only process exec; every
other capability, including write, survives."
  (let ((subject (app::attenuate-run-subject (kli/ext:make-unrestricted-subject)
                                             :no-bash t)))
    (is (kli/ext:check-capability subject :file/read))
    (is (kli/ext:check-capability subject :file/write))
    (is (kli/ext:check-capability subject :file/edit))
    (is (not (kli/ext:check-capability subject :process/exec)))))

(test attenuate-run-subject-on-unrestricted-base-only-narrows
  "A flag-attenuated unrestricted subject is still covered by the top, so the
flag can never widen authority even from the universal grant."
  (let ((top (kli/ext:subject-grant (kli/ext:make-unrestricted-subject))))
    (is (kli/ext:grant-covers-p
         top
         (kli/ext:subject-grant
          (app::attenuate-run-subject (kli/ext:make-unrestricted-subject)
                                      :read-only t))))))

(test authority-report-universal-except-text-lists-excluded-atoms
  "The human report of a co-finite grant names the universal-except scope and
lists the withheld atoms."
  (let* ((grant (kli/ext:grant-without-atoms (kli/ext:grant-top)
                                             '(:file/write :process/exec)))
         (text (app::authority-report-text (kli/ext:grant-report grant))))
    (is (search "universal except" text))
    (is (search "file/write" text))
    (is (search "process/exec" text))))

(test authority-report-universal-except-json-lists-excluded-atoms
  "The JSON form of a co-finite grant carries the universal-except scope and the
excluded atom names."
  (let* ((grant (kli/ext:grant-without-atoms (kli/ext:grant-top)
                                             '(:file/write :process/exec)))
         (object (com.inuoe.jzon:parse
                  (com.inuoe.jzon:stringify
                   (app::authority-report-json (kli/ext:grant-report grant)))))
         (excluded (gethash "excluded" object)))
    (is (equal "authority" (gethash "type" object)))
    (is (equal "universal-except" (gethash "scope" object)))
    (is (eql 2 (length excluded)))
    (is (find "file/write" excluded :test #'equal))
    (is (find "process/exec" excluded :test #'equal))))

(test headless-grants-request-reports-the-name-to-fail-closed
  "--grants surfaces its name so the driver fails closed; without it the request
is absent."
  (is (equal "team" (app::headless-grants-request '("--grants" "team"))))
  (is (null (app::headless-grants-request '("--read-only" "do it"))))
  (is (null (app::headless-grants-request '()))))

(test dispatch-routes-print-authority
  "--print-authority routes to its own command regardless of channel, carrying
the remaining flags; an explicit command still wins over a later occurrence."
  (flet ((route (argv interactive)
           (multiple-value-list (app::dispatch-command argv interactive))))
    (is (equal '(:print-authority ("--json")) (route '("--print-authority" "--json") t)))
    (is (equal '(:print-authority ("--read-only"))
               (route '("--print-authority" "--read-only") nil)))
    (is (eq :version (nth-value 0 (app::dispatch-command
                                   '("version" "--print-authority") t))))))

(test grant-report-classifies-scope-and-sorts-atoms
  "A grant report tags the universal grant, the empty grant, and a bounded grant,
listing a bounded grant's atoms downcased and name-sorted."
  (is (eq :universal
          (getf (kli/ext:grant-report
                 (kli/ext:subject-grant (kli/ext:make-system-subject)))
                :scope)))
  (let ((none (kli/ext:grant-report
               (kli/ext:subject-grant (kli/ext:make-default-subject)))))
    (is (eq :none (getf none :scope)))
    (is (null (getf none :capabilities))))
  (let* ((subject (kli/ext:make-subject
                   :capabilities '(:process/exec :file/read)))
         (report (kli/ext:grant-report (kli/ext:subject-grant subject))))
    (is (eq :bounded (getf report :scope)))
    ;; file/read and process/exec each confer result/read (page back the output
    ;; they produce), so the closure carries it too, name-sorted last.
    (is (equal '("file/read" "process/exec" "result/read")
               (mapcar (lambda (e) (getf e :atom)) (getf report :capabilities))))))

(test grant-report-renders-constraints-human-readably
  "A bounded atom's constraint renders as a short string; :any reads as \"any\"."
  (is (equal "any" (kli/ext:constraint->string (kli/ext:constraint-any))))
  (is (equal "path under /srv/data"
             (kli/ext:constraint->string
              (kli/ext:path-prefix-constraint "/srv/data"))))
  (let* ((grant (kli/ext:make-grant
                 :atoms (list (cons :file/read
                                    (kli/ext:path-prefix-constraint "/srv/data")))))
         (entry (first (getf (kli/ext:grant-report grant) :capabilities))))
    (is (equal "file/read" (getf entry :atom)))
    (is (equal "path under /srv/data" (getf entry :constraint)))))

(test authority-report-reflects-headless-flag-drops
  "The report of a flag-attenuated subject omits the dropped atoms, so the printed
authority tracks what --read-only/--no-bash actually remove."
  (let* ((base (print-actuator-base))
         (no-bash (kli/ext:grant-report
                   (kli/ext:subject-grant
                    (app::attenuate-run-subject base :no-bash t))))
         (read-only (kli/ext:grant-report
                     (kli/ext:subject-grant
                      (app::attenuate-run-subject base :read-only t)))))
    (flet ((atoms (report) (mapcar (lambda (e) (getf e :atom))
                                   (getf report :capabilities))))
      (is (not (member "process/exec" (atoms no-bash) :test #'string=)))
      (is (member "file/write" (atoms no-bash) :test #'string=))
      (is (not (member "process/exec" (atoms read-only) :test #'string=)))
      (is (not (member "file/write" (atoms read-only) :test #'string=)))
      (is (member "file/read" (atoms read-only) :test #'string=)))))

(test authority-report-json-mirrors-scope-and-capabilities
  "The JSON form carries the scope string and one object per capability atom."
  (let* ((subject (kli/ext:make-subject :capabilities '(:file/read :process/exec)))
         (report (kli/ext:grant-report (kli/ext:subject-grant subject)))
         (object (com.inuoe.jzon:parse
                  (com.inuoe.jzon:stringify (app::authority-report-json report))))
         (caps (gethash "capabilities" object)))
    (is (equal "authority" (gethash "type" object)))
    (is (equal "bounded" (gethash "scope" object)))
    ;; file/read and process/exec each confer result/read, so the closure is three
    ;; atoms, each rendered as one capability object.
    (is (eql 3 (length caps)))
    (is (equal "any" (gethash "constraint" (aref caps 0))))))

(test authority-report-text-lists-atoms-and-marks-scope
  "The human report names the scope and lists each bounded atom; a non-:any
constraint rides in brackets while :any is left bare."
  (is (search "authority: none"
              (app::authority-report-text
               (kli/ext:grant-report
                (kli/ext:subject-grant (kli/ext:make-default-subject))))))
  (let* ((grant (kli/ext:make-grant
                 :atoms (list (cons :file/read
                                    (kli/ext:path-prefix-constraint "/srv/data"))
                              (cons :process/exec (kli/ext:constraint-any)))))
         (text (app::authority-report-text (kli/ext:grant-report grant))))
    (is (search "file/read" text))
    (is (search "[path under /srv/data]" text))
    (is (search "process/exec" text))
    (is (not (search "[any]" text)))))
