(in-package #:kli/tests)

(defun run-test-echo-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool context call-id on-update))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content
                   (or (ext:tool-parameter parameters :message)
                       "")))))

(ext:defextension test-echo-tool
  (:provides
   (tool echo
     :label "Echo"
     :description "Echo a message."
     :parameters '(:object (:message :string))
     :runner #'run-test-echo-tool)))

(test tool-contribution-registers-in-extension-protocol
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-echo-tool-extension-manifest*)
    (let* ((tool (ext:find-tool protocol :echo))
           (result (ext:invoke-tool protocol
                                    :echo
                                    '(:message "hello")
                                    context)))
      (is (typep tool 'ext:tool))
      (is (equal '(:test-echo-tool :tool :echo)
                 (kli:object-id tool)))
      (is (not (ext:tool-result-error-p result)))
      (is (string= "hello"
                   (getf (first (ext:tool-result-content result))
                         :text))))))

(test tool-can-be-recoded-without-losing-identity
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context *test-echo-tool-extension-manifest*)
    (let ((tool (ext:find-tool protocol :echo)))
      (ext:recode-tool
       tool
       :runner (lambda (tool parameters context &key call-id on-update)
                 (declare (ignore tool parameters context call-id on-update))
                 (ext:make-tool-result
                  :content (list (ext:make-tool-text-content "recoded")))))
      (is (equal '(:test-echo-tool :tool :echo)
                 (kli:object-id tool)))
      (is (string= "recoded"
                   (getf (first (ext:tool-result-content
                                 (ext:invoke-tool protocol
                                                  :echo
                                                  '(:message "hello")
                                                  context)))
                         :text))))))

(test (eval-tool-evaluates-in-live-image :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((result (ext:invoke-tool protocol
                                   :eval
                                   '(:form "(+ 20 22)")
                                   context)))
      (is (not (ext:tool-result-error-p result)))
      (is (string= "42"
                   (getf (first (ext:tool-result-content result))
                         :text)))
      (is (equal '("42")
                 (getf (ext:tool-result-details result) :values))))))

(test (eval-tool-surfaces-structured-condition-on-error :fixture tool-authority)
  "A signaling form returns an error result whose :details carry the
condition-type/message/category, the active restarts, the captured backtrace,
and the offending form as :source -- the structured fields the wire forwards
to the model."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool protocol
                                    :eval
                                    '(:form "(/ 1 0)")
                                    context))
           (details (ext:tool-result-details result)))
      (is (ext:tool-result-error-p result))
      (is (string= "DIVISION-BY-ZERO" (getf details :condition-type)))
      (is (stringp (getf details :message)))
      (is (keywordp (getf details :category)))
      (is (string= "(/ 1 0)" (getf details :source)))
      (is (plusp (length (getf details :backtrace))))
      (is (every #'stringp (getf details :backtrace)))
      (is (every #'stringp (getf details :restarts))))))

(test (eval-command-invokes-eval-tool :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*
                        tools-eval:*eval-command-extension-manifest*)
    (let* ((commands (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (result (ext:provider-call commands
                                      :invoke-command
                                      :eval
                                      '(:tail "(+ 40 2)")
                                      context)))
      (is (ext:provider-call commands :find-command :eval))
      (is (not (commands:command-result-error-p result)))
      (is (string= "42"
                   (getf (first (commands:command-result-content result))
                         :text))))))

;;; Interactive (parking) eval: an :on-error "interactive" eval parks on the live
;;; restarts; eval-continue resumes, eval-abort unwinds, and an unresolved park
;;; drains when the eval extension is torn down. Resume requires :image/debug.

(defun install-interactive-eval-tools (context)
  "The eval tool and its resume companions; returns the eval tool's manifest
handle so a test can retract it and observe a park drain."
  (install-extension context event:*events-extension-manifest*)
  (install-extension context tools-eval:*eval-continue-tool-extension-manifest*)
  (install-extension context tools-eval:*eval-abort-tool-extension-manifest*)
  (install-extension context tools-eval:*eval-tool-extension-manifest*))

(test eval-interactive-parks-on-error-then-continue-resumes
  "An :on-error \"interactive\" eval of an erroring form parks on the live
restarts without unwinding; eval-continue invoking USE-VALUE resumes the parked
computation to its value and the park drains from the registry."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-interactive-eval-tools context)
    (with-granted-authority (:image/debug)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(+ 1000 interactive-park-undef)"
                        :on-error "interactive")
                      context))
             (details (ext:tool-result-details parked))
             (park-id (getf details :park)))
        (is (getf details :parked-p))
        (is (stringp park-id))
        (is (member "USE-VALUE" (getf details :restart-names) :test #'string=))
        (let* ((resumed (ext:invoke-tool
                         protocol :eval-continue
                         (list :park park-id :restart "USE-VALUE" :arg "337")
                         context))
               (rdetails (ext:tool-result-details resumed)))
          (is (not (ext:tool-result-error-p resumed)))
          (is (getf rdetails :resumed-p))
          (is (equal '("1337") (getf rdetails :values)))
          (is (null (kli:find-live-object (kli:context-registry context) park-id))
              "the resumed park drained from the registry"))))))

(test eval-interactive-abort-unwinds-the-parked-thread
  "eval-abort unwinds a parked eval through its abort restart, leaving no parked
thread and no registry entry."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-interactive-eval-tools context)
    (with-granted-authority (:image/debug)
      (let* ((parked (ext:invoke-tool protocol :eval
                                      '(:form "(/ 1 0)" :on-error "interactive")
                                      context))
             (park-id (getf (ext:tool-result-details parked) :park))
             (park (kli:find-live-object (kli:context-registry context) park-id))
             (thread (tools-eval:eval-park-thread park)))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (let ((aborted (ext:invoke-tool protocol :eval-abort
                                        (list :park park-id) context)))
          (is (getf (ext:tool-result-details aborted) :aborted-p))
          (is (not (sb-thread:thread-alive-p thread))
              "the aborted park's thread unwound")
          (is (null (kli:find-live-object (kli:context-registry context) park-id))))))))

(test eval-interactive-park-drains-on-teardown
  "An unresolved park is a per-protocol live object whose thread is reaped and
whose registry entry is gone once the eval extension is torn down."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (handle (install-interactive-eval-tools context)))
    (with-granted-authority (:image/debug)
      (let* ((parked (ext:invoke-tool protocol :eval
                                      '(:form "(/ 1 0)" :on-error "interactive")
                                      context))
             (park-id (getf (ext:tool-result-details parked) :park))
             (park (kli:find-live-object (kli:context-registry context) park-id))
             (thread (tools-eval:eval-park-thread park)))
        (is (sb-thread:thread-alive-p thread) "the unresumed park stays parked")
        (with-extension-load-authority
          (ext:retract-manifest handle protocol context))
        (is (not (sb-thread:thread-alive-p thread)) "teardown reaped the park thread")
        (is (null (kli:find-live-object (kli:context-registry context) park-id))
            "teardown deregistered the park")))))

(test eval-interactive-resume-tools-require-debug
  "eval-continue and eval-abort require :image/debug; an :image/eval holder can
start an interactive eval and read its restarts but is denied resume and abort."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-interactive-eval-tools context)
    (let ((park-id nil))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/eval))))
        (let ((parked (ext:invoke-tool protocol :eval
                                       '(:form "(/ 1 0)" :on-error "interactive")
                                       context)))
          (is (getf (ext:tool-result-details parked) :parked-p)
              "an :image/eval holder can start an interactive eval")
          (setf park-id (getf (ext:tool-result-details parked) :park)))
        (signals ext:capability-denied
          (ext:invoke-tool protocol :eval-continue
                           (list :park park-id :restart "ABORT-EVAL") context))
        (signals ext:capability-denied
          (ext:invoke-tool protocol :eval-abort (list :park park-id) context)))
      (let ((ext:*call-subject* (ext:make-subject :capabilities '(:image/debug))))
        (ext:invoke-tool protocol :eval-abort (list :park park-id) context)))))

(test eval-interactive-clean-eval-carries-no-park-metadata
  "An :on-error \"interactive\" eval that completes without ever signaling a
condition never surfaced a park to the model, so its result is byte-identical to
a plain success: it carries the value but no :park, :resumed-p, or :parked-p."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-interactive-eval-tools context)
    (with-granted-authority (:image/debug)
      (let* ((result (ext:invoke-tool protocol :eval
                                      '(:form "(+ 20 22)" :on-error "interactive")
                                      context))
             (details (ext:tool-result-details result)))
        (is (not (ext:tool-result-error-p result)))
        (is (equal '("42") (getf details :values)) "the value is returned")
        (is (null (getf details :park)) "a never-parked eval carries no park id")
        (is (null (getf details :resumed-p)) "nothing was resumed")
        (is (null (getf details :parked-p)) "nothing parked")))))

(test (recompile-rerun-redefines-then-reruns :fixture tool-authority)
  "Redefining a buggy defun to a correct one re-runs a previously failing form to
success; :details carry an :ok redefinition, an :ok re-run, and the restorable
flag for the prior definition."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*
                        tools-eval:*recompile-rerun-tool-extension-manifest*)
    (ext:invoke-tool protocol :eval
                     '(:form "(defun rr-demo (x) (/ x 0))")
                     context)
    (let* ((result (ext:invoke-tool
                    protocol :recompile-rerun
                    '(:definition "(defun rr-demo (x) (* x 2))"
                      :form "(rr-demo 21)")
                    context))
           (details (ext:tool-result-details result)))
      (is (not (ext:tool-result-error-p result)))
      (is (eq :ok (getf (getf details :definition) :status)))
      (is (eq :ok (getf (getf details :rerun) :status)))
      (is (equal '("42") (getf (getf details :rerun) :values)))
      (is (getf details :prior-fdefinition-restorable)))))

(test recompile-rerun-snapshots-and-restores-prior-definition
  "recompile-and-rerun returns the prior fdefinition as a live function object;
restoring it reverts the redefinition so the original behavior returns."
  (let ((*package* (find-package :cl-user)))
    (eval '(defun cl-user::rr-restore (x) (/ x 0)))
    (let* ((info (tools-eval:recompile-and-rerun
                  "(defun rr-restore (x) (* x 2))"
                  "(rr-restore 10)"))
           (prior (getf info :prior-fdefinition)))
      (is (functionp prior))
      (is (getf info :prior-fdefinition-restorable))
      (is (= 20 (funcall 'cl-user::rr-restore 10)))
      (setf (fdefinition 'cl-user::rr-restore) prior)
      (is (eq :division-by-zero
              (handler-case (funcall 'cl-user::rr-restore 10)
                (division-by-zero () :division-by-zero)))))))

(test (recompile-rerun-still-buggy-surfaces-condition :fixture tool-authority)
  "A still-buggy redefinition re-runs to an error whose structured condition-state
-- type, offending form, backtrace -- rides :details under :rerun."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*recompile-rerun-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool
                    protocol :recompile-rerun
                    '(:definition "(defun rr-buggy (x) (/ x 0))"
                      :form "(rr-buggy 3)")
                    context))
           (details (ext:tool-result-details result))
           (rerun (getf details :rerun)))
      (is (ext:tool-result-error-p result))
      (is (eq :ok (getf (getf details :definition) :status)))
      (is (eq :error (getf rerun :status)))
      (is (string= "DIVISION-BY-ZERO" (getf rerun :condition-type)))
      (is (string= "(RR-BUGGY 3)" (getf rerun :source)))
      (is (plusp (length (getf rerun :backtrace)))))))

(test (recompile-rerun-captures-compiler-notes :fixture tool-authority)
  "A defun whose body triggers a compiler note redefines successfully with the
note captured into :compiler-notes rather than aborting the redefinition."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*recompile-rerun-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool
                    protocol :recompile-rerun
                    '(:definition "(defun rr-noted (x) (declare (optimize speed)) (the fixnum (+ x \"no\")))"
                      :form "42")
                    context))
           (details (ext:tool-result-details result)))
      (is (eq :ok (getf (getf details :definition) :status)))
      (is (plusp (length (getf details :compiler-notes))))
      (is (every #'stringp (getf details :compiler-notes))))))

(test (bash-command-invokes-bash-tool :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*
                        tools-bash:*bash-command-extension-manifest*)
    (let* ((commands (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (result (ext:provider-call commands
                                      :invoke-command
                                      :bash
                                      '(:tail "printf command-ok")
                                      context)))
      (is (ext:provider-call commands :find-command :bash))
      (is (not (commands:command-result-error-p result)))
      (is (string= "command-ok"
                   (getf (first (commands:command-result-content result))
                         :text))))))

(test (bash-command-forwards-timeout-option :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*
                        tools-bash:*bash-command-extension-manifest*)
    (let* ((commands (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (result (ext:provider-call commands
                                      :invoke-command
                                      :bash
                                      '(:tail "--timeout 1 sleep 30")
                                      context)))
      (is (commands:command-result-error-p result))
      (is (= 1 (getf (commands:command-result-details result)
                     :timeout-seconds)))
      (is (search "timed out"
                  (getf (first (commands:command-result-content result))
                        :text))))))

(test (bash-command-forwards-background-option :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*
                        tools-bash:*bash-command-extension-manifest*)
    (let* ((commands (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (result (ext:provider-call commands
                                      :invoke-command
                                      :bash
                                      '(:tail "--background printf bg-ok")
                                      context))
           (details (commands:command-result-details result)))
      (is (not (commands:command-result-error-p result)))
      (is (getf details :background-p))
      (is (string= "printf bg-ok" (getf details :command)))
      (is (search "bash-" (getf details :job-id))))))

(test (bash-job-commands-poll-list-and-kill-background-jobs :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*
                        tools-bash:*bash-command-extension-manifest*)
    (let* ((commands (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (short (ext:provider-call commands
                                     :invoke-command
                                     :bash
                                     '(:tail "--background printf command-bg")
                                     context))
           (short-id (getf (commands:command-result-details short) :job-id))
           (long (ext:provider-call commands
                                    :invoke-command
                                    :bash
                                    '(:tail "--background sleep 30")
                                    context))
           (long-id (getf (commands:command-result-details long) :job-id)))
      (is (ext:provider-call commands :find-command :bash-output))
      (is (ext:provider-call commands :find-command :bash-kill))
      (is (ext:provider-call commands :find-command :bash-jobs))
      (loop repeat 20
            for poll = (ext:provider-call commands
                                          :invoke-command
                                          :bash-output
                                          (list :tail short-id)
                                          context)
            for text = (getf (first (commands:command-result-content poll)) :text)
            until (search "command-bg" text)
            finally (is (search "command-bg" text)
                        "bash-output reads background output"))
      (let ((listed (ext:provider-call commands
                                       :invoke-command
                                       :bash-jobs
                                       '()
                                       context)))
        (let ((text (getf (first (commands:command-result-content listed)) :text)))
          (is (search short-id text) "bash-jobs lists the finished job")
          (is (search long-id text) "bash-jobs lists the running job")))
      (let* ((killed (ext:provider-call commands
                                        :invoke-command
                                        :bash-kill
                                        (list :tail long-id)
                                        context))
             (details (commands:command-result-details killed)))
        (is (getf details :killed-p) "bash-kill stops the running job")))))

(test (bash-command-reports-shell-failure :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*
                        tools-bash:*bash-command-extension-manifest*)
    (let* ((commands (ext:require-capability-provider protocol
                                                      :commands
                                                      :contract :commands/v1))
           (result (ext:provider-call commands
                                      :invoke-command
                                      :bash
                                      '(:tail "exit 7")
                                      context)))
      (is (commands:command-result-error-p result))
      (is (= 7 (getf (commands:command-result-details result)
                     :exit-code))))))

(test (bash-tool-runs-command-named-like-interactive-program :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (root (ensure-directories-exist
                (merge-pathnames
                 (format nil "kli-fake-nvim-~D-~D/"
                         #+sbcl (sb-posix:getpid)
                         #-sbcl 0
                         (get-internal-real-time))
                 (uiop:temporary-directory))))
         (nvim (merge-pathnames "nvim" root))
         (saved-path (uiop:getenv "PATH")))
    (install-extensions context tools-bash:*bash-tool-extension-manifest*)
    (unwind-protect
         (progn
           (with-open-file (stream nvim :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
             (write-line "#!/bin/sh" stream)
             (write-line "printf fake-nvim-ran" stream))
           (sb-posix:chmod (uiop:native-namestring nvim) #o755)
           (sb-posix:setenv
            "PATH"
            (if (and saved-path (plusp (length saved-path)))
                (format nil "~A:~A" (uiop:native-namestring root) saved-path)
                (uiop:native-namestring root))
            1)
           (let ((result (ext:invoke-tool protocol
                                          :bash
                                          '(:command "nvim")
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "fake-nvim-ran" (tool-result-text result)))
             (is (null (getf (ext:tool-result-details result)
                             :interactive-command)))))
      (if saved-path
          (sb-posix:setenv "PATH" saved-path 1)
          (sb-posix:unsetenv "PATH"))
      (uiop:delete-directory-tree root :validate (constantly t)
                                       :if-does-not-exist :ignore))))

(test (bash-tool-times-out-long-running-command :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context tools-bash:*bash-tool-extension-manifest*)
    (let ((result (ext:invoke-tool protocol
                                   :bash
                                   '(:command "sleep 2" :timeout 1)
                                   context)))
      (is (ext:tool-result-error-p result))
      (is (getf (ext:tool-result-details result) :timed-out-p))
      (is (= 1 (getf (ext:tool-result-details result)
                     :timeout-seconds)))
      (is (search "timed out" (tool-result-text result))))))

(test (bash-timeout-kill-actually-terminates-the-command :fixture tool-authority)
  "killpg on a non-leader child is ESRCH inside ignore-errors, after which
the final unbounded wait blocked until the runaway command exited on its own
-- a timed-out sleep 30 wedged the agent worker for the full 30 seconds. The
kill must take effect (the process group when setsid is available, the direct
child otherwise) and the reap must be bounded."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (start (get-internal-real-time)))
    (install-extensions context tools-bash:*bash-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool protocol
                                    :bash
                                    '(:command "sleep 30" :timeout 1)
                                    context))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
      (is (ext:tool-result-error-p result))
      (is (getf (ext:tool-result-details result) :timed-out-p))
      (is (< elapsed 10)
          "the kill must take effect instead of waiting out the command"))))

(test (bash-tool-windows-captured-output :fixture tool-authority)
  "Output over the capture limit keeps its head and its tail joined by an
elision marker, so a terminal error survives an otherwise oversized blob while
one chatty command still cannot exhaust the heap; output under the limit passes
through untouched."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context tools-bash:*bash-tool-extension-manifest*)
    (let ((tools-bash:*bash-output-character-limit* 16))
      (let* ((result (ext:invoke-tool protocol
                                      :bash
                                      '(:command "printf 0123456789abcdefGHIJ")
                                      context))
             (text (tool-result-text result))
             (details (ext:tool-result-details result)))
        (is (not (ext:tool-result-error-p result)))
        (is (search "0123456789" text) "the head is kept")
        (is (search "efGHIJ" text)
            "the tail survives so a terminal error is not lost")
        (is (not (search "abcd" text)) "the elided middle is dropped")
        (is (search "truncated" text) "the cut is surfaced, not silent")
        (is (getf details :stdout-truncated-p))
        (is (not (getf details :stderr-truncated-p)))))
    (let ((result (ext:invoke-tool protocol
                                   :bash
                                   '(:command "printf small-ok")
                                   context)))
      (is (string= "small-ok" (tool-result-text result))
          "output under the cap passes through with no note"))))

(test (bash-default-cap-is-a-model-context-budget :fixture tool-authority)
  "The default capture cap is a model-context budget, not a heap guard: a command
that emits hundreds of kilobytes across many short lines -- a recursive find, a
verbose log -- is windowed to the cap before it reaches the model, so one chatty
command cannot overflow the context window in a single turn even while it fits in
memory."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context tools-bash:*bash-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool protocol
                                    :bash
                                    '(:command "yes ABCDEFGH | head -c 300000")
                                    context))
           (text (tool-result-text result)))
      (is (getf (ext:tool-result-details result) :stdout-truncated-p)
          "a 300 KB output is windowed to the cap, not relayed whole")
      (is (search "truncated" text) "the cut is surfaced to the model, not silent")
      (is (<= (length text)
              (+ tools-bash:*bash-output-character-limit* 1000))
          "the model-facing text is bounded by the cap, not the megabyte heap guard"))))

(test (bash-windowed-reader-resyncs-a-multibyte-tail :fixture tool-authority)
  "The windowed reader seeks near the end for the tail and resyncs onto a UTF-8
lead byte, so a character split by the seek degrades to a replacement rather
than corrupting the surviving tail, and the head is kept too."
  (uiop:with-temporary-file (:pathname path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :external-format :utf-8)
      (dotimes (i 400) (write-string "omega-Ωμέγα-😀-héllo " stream))
      (write-string "TAILMARK-Ω😀" stream))
    (multiple-value-bind (text truncated)
        (tools-bash::read-windowed-output-file path 64)
      (is (eq t truncated))
      (is (search "omega" text) "the head is kept")
      (is (search "TAILMARK" text) "the tail marker survives the seek and resync")
      (is (search "truncated" text)))))

(test (bash-incremental-decoder-reproduces-the-full-decode :fixture tool-authority)
  "Feeding bytes to the incremental decoder in arbitrary chunk sizes, including
splits inside multi-byte characters, yields pieces that concatenate to the exact
full decode."
  (let* ((text (with-output-to-string (stream)
                 (dotimes (i 80) (format stream "héllo-😀-Ωμέγα-~D " i))))
         (bytes (sb-ext:string-to-octets text :external-format :utf-8)))
    (dolist (chunk-size '(1 2 3 5 7 64))
      (let ((decode (tools-bash::make-incremental-decoder))
            (out (make-string-output-stream)))
        (loop for start from 0 below (length bytes) by chunk-size
              for end = (min (length bytes) (+ start chunk-size))
              do (write-string (funcall decode (subseq bytes start end)) out))
        (is (string= (get-output-stream-string out) text)
            "chunk size ~D must round-trip exactly" chunk-size)))))

(test (bash-read-leader-pid-round-trips :fixture tool-authority)
  "The group leader pid the spawned child records is read back as an integer,
and a missing or empty record reads as NIL so teardown falls back cleanly to the
direct child."
  (uiop:with-temporary-file (:pathname path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (princ 31415 stream))
    (is (= 31415 (tools-bash::read-leader-pid path))))
  (uiop:with-temporary-file (:pathname path)
    (is (null (tools-bash::read-leader-pid path)) "an empty record reads as NIL"))
  (is (null (tools-bash::read-leader-pid
             #p"/nonexistent/kli-bash-leader-absent"))
      "an absent record reads as NIL"))

(test (bash-streams-captured-output-incrementally :fixture tool-authority)
  "The local provider funcalls on-update as captured output grows, and the
streamed pieces concatenate to the full capture."
  (let* ((pieces '())
         (result (tools-bash:local-bash-run
                  (list :command
                        "for i in 1 2 3 4 5; do printf 'line-%d\\n' \"$i\"; done"
                        :shell "sh"
                        :timeout-seconds 10
                        :on-update (lambda (text) (push text pieces)))
                  nil)))
    (is (zerop (getf result :exit-code)))
    (is (plusp (length pieces)) "at least one incremental payload is delivered")
    (is (string= (apply #'concatenate 'string (reverse pieces))
                 (getf result :stdout))
        "streamed pieces reproduce the full capture")))

(test (bash-foreground-spills-truncated-output-to-a-handle :fixture tool-authority)
  "A foreground command whose stdout overflows the cap keeps its window but also
retains the whole output behind a handle: the footer names the handle, and
read-result / search-result reach the lines the window dropped -- including the
middle, the part a head+tail window loses."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-bash:*bash-tool-extension-manifest*
                               spill:*output-spill-extension-manifest*)
           (let ((tools-bash:*bash-output-character-limit* 2000))
             (let* ((result (ext:invoke-tool
                             protocol :bash
                             '(:command "for i in $(seq 1 1000); do echo \"line $i\"; done")
                             context))
                    (text (tool-result-text result))
                    (handle (getf (ext:tool-result-details result) :stdout-handle)))
               (is (getf (ext:tool-result-details result) :stdout-truncated-p))
               (is (stringp handle) "a handle is minted for the spilled stdout")
               (is (search "read-result" text) "the footer names the retrieval tool")
               (is (search handle text) "the footer carries the handle token")
               (let ((page (ext:invoke-tool protocol :read-result
                                            (list :handle handle :start 995 :limit 10)
                                            context)))
                 (is (search "line 999" (tool-result-text page))
                     "read-result reaches lines past the inline window"))
               (let ((hit (ext:invoke-tool protocol :search-result
                                           (list :handle handle :pattern "^line 500$")
                                           context)))
                 (is (search "line 500" (tool-result-text hit))
                     "search-result finds a line the window dropped")))))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test (bash-foreground-degrades-without-a-handle-when-spilling-disabled
       :fixture tool-authority)
  "With spilling disabled the cap still windows the output, but the footer carries
no handle -- the model is never pointed at a backing that was not retained."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context tools-bash:*bash-tool-extension-manifest*)
           (let ((tools-bash:*bash-output-character-limit* 2000)
                 (spill:*output-spill-enabled* nil))
             (let* ((result (ext:invoke-tool
                             protocol :bash
                             '(:command "for i in $(seq 1 1000); do echo \"line $i\"; done")
                             context))
                    (text (tool-result-text result)))
               (is (getf (ext:tool-result-details result) :stdout-truncated-p))
               (is (null (getf (ext:tool-result-details result) :stdout-handle))
                   "no handle when spilling is disabled")
               (is (search "truncated" text) "the cut is still surfaced")
               (is (not (search "read-result" text))
                   "but the model is not pointed at an absent backing"))))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test (bash-background-handle-is-stable-pinned-and-reaches-the-live-log
       :fixture tool-authority)
  "A background job's output handle is registered once over the live log and cached,
so repeated polls surface one stable token; it is pinned -- never evicted, its file
never unlinked, budget-neutral -- because the job machinery owns the file, and the
handle still pages and searches it."
  (with-spill-store (protocol)
    (uiop:with-temporary-file (:pathname outf :prefix "kli-bgjob-test")
      (with-open-file (s outf :direction :output :if-exists :supersede)
        (dotimes (i 1000) (format s "job ~D~%" i)))
      (let* ((job (tools-bash::%make-bash-job :id "bash-1" :outf outf))
             (h1 (tools-bash::ensure-bash-job-spill-handle protocol job))
             (h2 (tools-bash::ensure-bash-job-spill-handle protocol job))
             (entry (spill:find-spill-entry protocol h1)))
        (is (stringp h1) "a handle is registered for the job log")
        (is (string= h1 h2) "the handle is cached on the job, stable across polls")
        (is (null (spill:spill-entry-owned entry)) "the live log is pinned, not owned")
        (is (zerop (spill:spill-entry-bytes entry)) "and budget-neutral")
        (multiple-value-bind (matches next eof)
            (spill:search-within (spill:spill-entry-path entry) "^job 750$")
          (declare (ignore next eof))
          (is (= 750 (caar matches)) "the handle searches the live log"))))))

(test (bash-background-output-surfaces-a-handle-over-the-live-log
       :fixture tool-authority)
  "Polling a finished background job whose backlog exceeds the cap windows the
output and surfaces a handle over the live log, so the model can search the part the
poll dropped."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-bash:*bash-tool-extension-manifest*
                               tools-bash:*bash-jobs-extension-manifest*
                               spill:*output-spill-extension-manifest*)
           (let ((tools-bash:*bash-output-character-limit* 2000))
             (let* ((launch (ext:invoke-tool
                             protocol :bash
                             '(:command "for i in $(seq 1 2000); do echo \"bg $i\"; done"
                               :run_in_background t)
                             context))
                    (job-id (getf (ext:tool-result-details launch) :job-id)))
               (is (stringp job-id) "the job launches and returns an id")
               (loop repeat 200
                     for done = (getf (first (getf (ext:tool-result-details
                                                    (ext:invoke-tool protocol :bash-jobs
                                                                     nil context))
                                                   :jobs))
                                      :done-p)
                     until done do (sleep 0.05))
               (let* ((poll (ext:invoke-tool protocol :bash-output
                                             (list :job_id job-id) context))
                      (text (tool-result-text poll))
                      (handle (getf (ext:tool-result-details poll) :result-handle)))
                 (is (getf (ext:tool-result-details poll) :truncated)
                     "the finished job's full backlog overflows the cap")
                 (is (stringp handle) "a handle over the live log is surfaced")
                 (is (search "read-result" text) "the footer names the retrieval tool")
                 (let ((hit (ext:invoke-tool protocol :search-result
                                             (list :handle handle :pattern "^bg 1500$")
                                             context)))
                   (is (search "bg 1500" (tool-result-text hit))
                       "search-result reaches a line the poll window dropped"))))))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test (eval-tool-bounds-printed-values :fixture tool-authority)
  "Value printing binds the printer limits, so a huge value cannot exhaust the heap and a circular value cannot hang the turn."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((result (ext:invoke-tool protocol
                                   :eval
                                   '(:form "(make-list 10000 :initial-element :x)")
                                   context)))
      (is (not (ext:tool-result-error-p result)))
      (is (search "..." (tool-result-text result)))
      (is (< (length (tool-result-text result)) 2000)))
    (let ((result (ext:invoke-tool protocol
                                   :eval
                                   '(:form "(let ((x (list 1 2))) (setf (cddr x) x) x)")
                                   context)))
      (is (not (ext:tool-result-error-p result)))
      (is (search "#1=" (tool-result-text result))
          "a circular value prints with circle labels instead of looping"))))

(test (bash-tool-abort-cuts-the-wait-short :fixture tool-authority)
  "An abort request must cut the process wait short and kill the command.
Abort was previously only checked between tool calls, so an aborted agent
kept executing the current command for up to its full timeout while the UI
already said aborted."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (start (get-internal-real-time)))
    (install-extensions context tools-bash:*bash-tool-extension-manifest*)
    (let* ((ext:*tool-abort-predicate* (lambda () t))
           (result (ext:invoke-tool protocol
                                    :bash
                                    '(:command "sleep 30")
                                    context))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
      (is (ext:tool-result-error-p result))
      (is (getf (ext:tool-result-details result) :aborted-p))
      (is (not (getf (ext:tool-result-details result) :timed-out-p)))
      (is (search "aborted" (tool-result-text result)))
      (is (< elapsed 10)
          "the abort must take effect instead of waiting out the command"))))

(defun run-fake-sandbox-bash (spec context)
  (declare (ignore context))
  (list :exit-code 0
        :stdout (format nil "sandboxed:~A" (getf spec :command))
        :stderr "" :timed-out-p nil :aborted-p nil
        :stdout-truncated-p nil :stderr-truncated-p nil))

(ext:defextension fake-sandbox-bash-exec
  (:provides
   (capability bash-exec
     (ext:make-provider
      :id :fake-sandbox
      :capability :bash-exec
      :contracts '(:bash-exec/v1)
      :entries (list :run #'run-fake-sandbox-bash)))))

(test (bash-exec-defaults-to-the-local-provider :fixture tool-authority)
  "Installing the bash tool registers the local bash-exec provider and selects
it by default, so the tool resolves to local exec with no configuration."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (is (eq tools-bash:+local-bash-exec-provider-id+
            (tools-bash:active-bash-exec-provider-id protocol)))
    (let ((result (ext:invoke-tool protocol :bash '(:command "printf via-local")
                                   context)))
      (is (not (ext:tool-result-error-p result)))
      (is (string= "via-local" (tool-result-text result))))))

(test (bash-exec-selection-shadows-local-with-another-provider :fixture tool-authority)
  "Selecting another bash-exec provider id routes the tool through it; the local
provider stays installed and the tool reverts to it when the selection clears."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tools-bash:*bash-tool-extension-manifest*
                        *fake-sandbox-bash-exec-extension-manifest*)
    (is (string= "local"
                 (tool-result-text
                  (ext:invoke-tool protocol :bash '(:command "printf local")
                                   context))))
    (setf (tools-bash:active-bash-exec-provider-id protocol) :fake-sandbox)
    (is (string= "sandboxed:printf in-box"
                 (tool-result-text
                  (ext:invoke-tool protocol :bash '(:command "printf in-box")
                                   context))))
    (setf (tools-bash:active-bash-exec-provider-id protocol)
          tools-bash:+local-bash-exec-provider-id+)
    (is (string= "back"
                 (tool-result-text
                  (ext:invoke-tool protocol :bash '(:command "printf back")
                                   context))))))

(test (persistent-shell-persists-state-across-commands :fixture tool-authority)
  "Selecting the persistent-shell backend routes the Bash tool through one
long-lived shell, so a cd or export in one command is still in effect in the
next -- the property the per-command local backend cannot have."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*persistent-shell-extension-manifest*)
    (setf (tools-bash:active-bash-exec-provider-id protocol)
          tools-bash:+persistent-shell-bash-exec-provider-id+)
    (unwind-protect
         (progn
           (ext:invoke-tool protocol :bash '(:command "cd /tmp") context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :bash
                                         '(:command "pwd") context))))
             (is (string= "/tmp"
                          (subseq text 0 (or (position #\Newline text)
                                             (length text))))
                 "pwd reports the persisted cwd on its first line"))
           (ext:invoke-tool protocol :bash
                            '(:command "export PERSH_MARK=alive") context)
           (is (search "alive"
                       (tool-result-text
                        (ext:invoke-tool protocol :bash
                                         '(:command "printf '%s' \"$PERSH_MARK\"")
                                         context)))))
      (tools-bash::teardown-session-persistent-shell protocol))))

(test (persistent-shell-streams-captured-output :fixture tool-authority)
  "The persistent-shell provider funcalls on-update as captured output grows,
and the streamed pieces concatenate to the full capture -- the same contract the
local provider honors."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (pieces '()))
    (unwind-protect
         (let ((result (tools-bash::persistent-shell-bash-run
                        (list :command
                              "for i in 1 2 3 4 5; do printf 'line-%d\\n' \"$i\"; done"
                              :timeout-seconds 10
                              :on-update (lambda (text) (push text pieces)))
                        context)))
           (is (zerop (getf result :exit-code)))
           (is (search "line-5" (getf result :stdout)))
           (is (plusp (length pieces)) "at least one streamed payload is delivered")
           (is (string= (apply #'concatenate 'string (reverse pieces))
                        (getf result :stdout))
               "streamed pieces reproduce the full capture"))
      (tools-bash::teardown-session-persistent-shell protocol))))

(test (persistent-shell-teardown-reaps-the-shell-on-rollback :fixture tool-authority)
  "Retracting the persistent-shell extension reaps the session's shell, so a
protocol rollback leaves no live process behind."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (let ((handle (install-extension
                   context tools-bash:*persistent-shell-extension-manifest*)))
      (setf (tools-bash:active-bash-exec-provider-id protocol)
            tools-bash:+persistent-shell-bash-exec-provider-id+)
      (ext:invoke-tool protocol :bash '(:command "echo spawned") context)
      (let* ((shell (tools-bash::session-persistent-shell protocol))
             (process (tools-bash::persistent-shell-process shell)))
        (is (sb-ext:process-alive-p process) "the shell is live after a command")
        (with-extension-load-authority
          (ext:retract-manifest handle protocol context))
        (loop repeat 200
              while (sb-ext:process-alive-p process)
              do (sleep 0.02))
        (is (not (sb-ext:process-alive-p process))
            "rollback reaped the shell -- no orphan survives")
        (is (null (tools-bash::session-persistent-shell protocol))
            "the storage slot is cleared")))))

(test (bash-shell-command-selects-the-backend :fixture tool-authority)
  "/bash-shell persistent routes the Bash tool through the persistent shell and
/bash-shell local restores the default -- the user-facing path into the
per-session backend selection."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        commands:*commands-extension-manifest*
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*persistent-shell-extension-manifest*
                        tools-bash:*persistent-shell-command-extension-manifest*)
    (let ((commands (ext:require-capability-provider protocol
                                                     :commands
                                                     :contract :commands/v1)))
      (ext:provider-call commands :invoke-command
                         :bash-shell '(:tail "persistent") context)
      (is (eq tools-bash:+persistent-shell-bash-exec-provider-id+
              (tools-bash:active-bash-exec-provider-id protocol)))
      (ext:provider-call commands :invoke-command
                         :bash-shell '(:tail "local") context)
      (is (eq tools-bash:+local-bash-exec-provider-id+
              (tools-bash:active-bash-exec-provider-id protocol))))))

(test (persistent-shell-footer-surfaces-cwd-in-result-text :fixture tool-authority)
  "The persistent-shell backend appends an absolute cwd footer to the model-facing
result text, read authoritatively off the live shell, so a cd that silently
changes where the next relative path resolves stays legible in the transcript."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*persistent-shell-extension-manifest*)
    (setf (tools-bash:active-bash-exec-provider-id protocol)
          tools-bash:+persistent-shell-bash-exec-provider-id+)
    (unwind-protect
         (let ((text (tool-result-text
                      (ext:invoke-tool protocol :bash
                                       '(:command "cd /tmp") context))))
           (is (search "[cwd: /tmp]" text)
               "the footer surfaces the directory the next command runs in"))
      (tools-bash::teardown-session-persistent-shell protocol))))

(test (persistent-shell-footer-reflects-respawn-after-death :fixture tool-authority)
  "A command that ends the shell respawns it in the seed directory, and the next
command's footer reflects that reset cwd rather than the dead shell's -- the
footer never reports a directory the live shell is not in."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*persistent-shell-extension-manifest*)
    (setf (tools-bash:active-bash-exec-provider-id protocol)
          tools-bash:+persistent-shell-bash-exec-provider-id+)
    (unwind-protect
         (progn
           (is (search "[cwd: /tmp]"
                       (tool-result-text
                        (ext:invoke-tool protocol :bash '(:command "cd /tmp")
                                         context)))
               "the footer follows the live shell into /tmp")
           (ext:invoke-tool protocol :bash '(:command "exit") context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :bash
                                         '(:command "pwd" :directory "/")
                                         context))))
             (is (search "[cwd: /]" text)
                 "the respawned shell seeds at the given directory")
             (is (not (search "[cwd: /tmp]" text))
                 "the dead shell's /tmp does not leak into the respawn")))
      (tools-bash::teardown-session-persistent-shell protocol))))

(test (local-backend-omits-the-cwd-footer :fixture tool-authority)
  "The local per-command backend has no cross-command cwd to surface, so it
appends no footer -- the footer's presence is itself the signal that the
persistent backend is in effect."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (let ((text (tool-result-text
                 (ext:invoke-tool protocol :bash '(:command "pwd") context))))
      (is (not (search "[cwd:" text))
          "the local backend emits no cwd footer"))))

(test (bash-run-in-background-launches-polls-lists-and-kills :fixture tool-authority)
  "run_in_background returns a job id at once; bash-output reads a job's captured
output and exit status; bash-jobs lists the jobs; bash-kill stops one."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*)
    (unwind-protect
         (let* ((short (ext:invoke-tool
                        protocol :bash
                        '(:command "printf 'bg-line\\n'" :run_in_background t)
                        context))
                (short-id (getf (ext:tool-result-details short) :job-id)))
           (is (not (ext:tool-result-error-p short)))
           (is (getf (ext:tool-result-details short) :background-p))
           (is (stringp short-id) "the launch returns a job id")
           (is (search short-id (tool-result-text short))
               "the launch names the job in its text")
           (let ((output "") (done nil) (exit :unset))
             (loop repeat 300 until done
                   do (let ((r (ext:invoke-tool protocol :bash-output
                                                (list :job_id short-id) context)))
                        (setf output (concatenate 'string output
                                                  (getf (ext:tool-result-details r)
                                                        :output))
                              done (getf (ext:tool-result-details r) :done-p)
                              exit (getf (ext:tool-result-details r) :exit-code))
                        (unless done (sleep 0.02))))
             (is (not (null done)) "the short job completes")
             (is (search "bg-line" output) "bash-output returns the job's output")
             (is (eql 0 exit) "the completed job's exit status is harvested"))
           (let* ((long (ext:invoke-tool
                         protocol :bash
                         '(:command "sleep 30" :run_in_background t) context))
                  (long-id (getf (ext:tool-result-details long) :job-id))
                  (listed (tool-result-text
                           (ext:invoke-tool protocol :bash-jobs '() context))))
             (is (search short-id listed) "bash-jobs lists the finished job")
             (is (search long-id listed) "bash-jobs lists the running job")
             (is (search "running" listed) "a live job shows as running")
             (let ((killed (ext:invoke-tool protocol :bash-kill
                                            (list :job_id long-id) context))
                   (job (tools-bash::find-bash-job protocol long-id)))
               (is (getf (ext:tool-result-details killed) :killed-p))
               (is (not (sb-ext:process-alive-p (tools-bash::bash-job-process job)))
                   "bash-kill terminated the job"))))
      (tools-bash::teardown-session-bash-jobs protocol))))

(test (bash-output-on-an-unknown-job-is-an-error :fixture tool-authority)
  "bash-output, bash-kill against an id with no job report an error rather than
silently succeed."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*)
    (is (ext:tool-result-error-p
         (ext:invoke-tool protocol :bash-output '(:job_id "bash-404") context)))
    (is (ext:tool-result-error-p
         (ext:invoke-tool protocol :bash-kill '(:job_id "bash-404") context)))))

(test (bash-jobs-drain-reaps-live-jobs-on-rollback :fixture tool-authority)
  "Retracting the bash-jobs extension SIGKILLs every live background job, so a
protocol rollback leaves no detached process behind and clears the table."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (let ((handle (install-extension
                   context tools-bash:*bash-jobs-extension-manifest*)))
      (ext:invoke-tool protocol :bash
                       '(:command "sleep 300" :run_in_background t) context)
      (let* ((job (first (tools-bash::bash-jobs-table-jobs
                          (tools-bash::session-bash-jobs protocol))))
             (process (tools-bash::bash-job-process job)))
        (is (sb-ext:process-alive-p process) "the job is live after launch")
        (with-extension-load-authority
          (ext:retract-manifest handle protocol context))
        (loop repeat 200
              while (sb-ext:process-alive-p process)
              do (sleep 0.02))
        (is (not (sb-ext:process-alive-p process))
            "rollback reaped the job -- no orphan survives")
        (is (null (tools-bash::session-bash-jobs protocol))
            "the jobs table is cleared")))))

(test (bash-kill-stops-a-job-and-keeps-its-output :fixture tool-authority)
  "A detached job's abort control is bash-kill; killing it stops the process and
its captured output survives the kill, deleted only when the session drains."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        tools-bash:*bash-tool-extension-manifest*
                        tools-bash:*bash-jobs-extension-manifest*)
    (unwind-protect
         (let* ((launch (ext:invoke-tool
                         protocol :bash
                         '(:command "yes ABCDEFGH | head -c 300000; sleep 300"
                           :run_in_background t)
                         context))
                (job-id (getf (ext:tool-result-details launch) :job-id))
                (job (tools-bash::find-bash-job protocol job-id)))
           (loop repeat 300
                 until (plusp (tools-bash::bash-job-output-bytes job))
                 do (sleep 0.02))
           (is (plusp (tools-bash::bash-job-output-bytes job))
               "the job flushed output to disk while running")
           (is (sb-ext:process-alive-p (tools-bash::bash-job-process job))
               "and is still running")
           (ext:invoke-tool protocol :bash-kill (list :job_id job-id) context)
           (is (not (sb-ext:process-alive-p (tools-bash::bash-job-process job)))
               "bash-kill stopped the job")
           (is (probe-file (tools-bash::bash-job-outf job))
               "the captured output survives the kill")
           (is (plusp (tools-bash::bash-job-output-bytes job))
               "and is still readable"))
      (tools-bash::teardown-session-bash-jobs protocol))))

(test (bash-policy-restricts-the-child-environment :fixture tool-authority)
  "An :env-allowlist restricts the child to the named parent vars plus the PATH
floor, so an unlisted (secret-shaped) parent var does not leak while command
lookups inside the child still resolve."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (sb-posix:setenv "KLI_BASH_KEPT" "kept" 1)
    (sb-posix:setenv "KLI_BASH_SECRET" "leaked" 1)
    (unwind-protect
         (progn
           (setf (tools-bash:bash-policy protocol)
                 '(:env-allowlist ("KLI_BASH_KEPT")))
           (let ((text (tool-result-text
                        (ext:invoke-tool
                         protocol :bash
                         '(:command "printf 'K=[%s] S=[%s] P=%s L=%s' \"$KLI_BASH_KEPT\" \"$KLI_BASH_SECRET\" \"${PATH:+set}\" \"$(ls / >/dev/null 2>&1 && echo ok)\"")
                         context))))
             (is (search "K=[kept]" text) "an allowlisted parent var is present")
             (is (search "S=[]" text) "an unlisted parent var does not leak")
             (is (search "P=set" text) "the PATH floor is present though unlisted")
             (is (search "L=ok" text) "command lookups resolve under the floor")))
      (sb-posix:unsetenv "KLI_BASH_KEPT")
      (sb-posix:unsetenv "KLI_BASH_SECRET"))))

(test (bash-default-environment-reaches-the-child :fixture tool-authority)
  "With no :env-allowlist the child inherits the parent environment unchanged, so
even a credential-shaped parent var reaches the shell-out. Keeping such a var out
of the environment is the process sandbox's job, not the tool's."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (sb-posix:setenv "KLI_BASH_GH_TOKEN" "sentinel" 1)
    (unwind-protect
         (let ((text (tool-result-text
                      (ext:invoke-tool
                       protocol :bash
                       '(:command "printf 'T=[%s]' \"$KLI_BASH_GH_TOKEN\"")
                       context))))
           (is (search "T=[sentinel]" text)
               "a credential-shaped parent var reaches the no-allowlist child"))
      (sb-posix:unsetenv "KLI_BASH_GH_TOKEN"))))

(test bash-restricted-environment-inherits-by-default
  "With no allowlist, restricted-environment returns NIL so the child inherits the
parent environment unchanged -- no per-tool scrub. An explicit allowlist switches
to strict floor+listed, admitting only the named parent vars on top of PATH."
  (let ((getenv (lambda (name)
                  (cond ((string= name "PATH") "/usr/bin")
                        ((string= name "GITHUB_TOKEN") "secret")
                        ((string= name "EDITOR") "vi")
                        (t nil)))))
    (is (null (tools-bash::restricted-environment nil getenv))
        "no allowlist inherits the parent environment unchanged")
    (let ((env (tools-bash::restricted-environment '("GITHUB_TOKEN") getenv)))
      (is (member "PATH=/usr/bin" env :test #'string=)
          "the PATH floor passes in strict mode")
      (is (member "GITHUB_TOKEN=secret" env :test #'string=)
          "an explicitly allowlisted name passes")
      (is (not (member "EDITOR=vi" env :test #'string=))
          "an unlisted plain var does not pass in strict mode"))))

(test (bash-policy-applies-cwd-and-timeout-defaults :fixture tool-authority)
  "With no per-call directory or timeout, the policy's :cwd is the spawn
directory and its :default-timeout bounds the run."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extension context tools-bash:*bash-tool-extension-manifest*)
    (setf (tools-bash:bash-policy protocol) '(:cwd "/tmp" :default-timeout 1))
    (let* ((here (tool-result-text
                  (ext:invoke-tool protocol :bash '(:command "pwd") context)))
           (line (subseq here 0 (or (position #\Newline here) (length here)))))
      ;; The spawned process reports getcwd(), which resolves symlinks —
      ;; on darwin /tmp is a symlink to /private/tmp — so compare resolved
      ;; directories rather than namestrings.
      (is (equal (truename "/tmp/")
                 (truename (concatenate 'string line "/")))
          "a bare pwd reports the policy cwd"))
    (let ((slow (ext:invoke-tool protocol :bash '(:command "sleep 5") context)))
      (is (ext:tool-result-error-p slow)
          "the policy default timeout bounds the run")
      (is (search "timed out" (tool-result-text slow))))))

(test (bash-policy-and-backend-ride-snapshot-restore :fixture tool-authority)
  "The session's bash policy and selected exec backend live in protocol storage,
so they ride snapshot and restore as durable data. A snapshot taken with a policy
set and a non-default backend selected, carried through print/read and restored
into a fresh image, yields the same policy plist and backend id."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (policy '(:cwd "/tmp/locked"
                   :env-allowlist ("PATH" "HOME")
                   :default-timeout 45)))
    (install-extensions context
                        snapshot:*snapshot-extension-manifest*
                        tools-bash:*bash-tool-extension-manifest*)
    (setf (tools-bash:bash-policy protocol) policy)
    (setf (tools-bash:active-bash-exec-provider-id protocol)
          tools-bash:+persistent-shell-bash-exec-provider-id+)
    (let* ((datum (let ((ext:*call-subject* ext:*ui-subject*))
                    (snapshot:snapshot-context context)))
           (read-back (with-standard-io-syntax
                        (let ((*read-eval* nil))
                          (read-from-string (prin1-to-string datum)))))
           (fresh (kli:make-kernel-host))
           (restored (let ((ext:*call-subject* ext:*ui-subject*))
                       (snapshot:restore-active-protocol fresh read-back))))
      (is (equal datum read-back)
          "the snapshot datum carrying the policy is print/read-safe whole")
      (is (equal policy (tools-bash:bash-policy restored))
          "the bash policy plist rehydrates unchanged")
      (is (eql tools-bash:+persistent-shell-bash-exec-provider-id+
               (tools-bash:active-bash-exec-provider-id restored))
          "the selected non-default backend id rehydrates unchanged"))))

(test (eval-tool-caps-captured-output :fixture tool-authority)
  "Output over the window is kept inline up to the cap and the rest retained whole
behind a handle: the head shows, the tail is dropped from the inline view but
read-result reaches it, and output under the cap passes through with no handle."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context
                               event:*events-extension-manifest*
                               tools-eval:*eval-tool-extension-manifest*
                               spill:*output-spill-extension-manifest*)
           (let ((tools-eval:*eval-output-character-limit* 16))
             (let* ((result (ext:invoke-tool protocol
                                             :eval
                                             '(:form "(progn (princ \"0123456789abcdefGHIJ\") (values))")
                                             context))
                    (text (tool-result-text result))
                    (details (ext:tool-result-details result))
                    (handle (getf details :stdout-handle)))
               (is (not (ext:tool-result-error-p result)))
               (is (search "0123456789abcdef" text))
               (is (not (search "GHIJ" text)) "output beyond the window is dropped from the inline view")
               (is (getf details :stdout-truncated-p))
               (is (search "truncated" text) "the cut is surfaced, not silent")
               (is (stringp handle) "the full output is retained behind a handle")
               (is (search handle text) "the footer carries the handle token")
               (let ((page (ext:invoke-tool protocol :read-result
                                            (list :handle handle) context)))
                 (is (search "GHIJ" (tool-result-text page))
                     "read-result reaches the tail the inline window dropped"))))
           (let ((result (ext:invoke-tool protocol
                                          :eval
                                          '(:form "(progn (princ \"small-ok\") (values))")
                                          context)))
             (is (string= "small-ok" (tool-result-text result))
                 "output under the cap passes through with no handle")))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test (eval-tool-times-out-runaway-form :fixture tool-authority)
  "A form running past the deadline is interrupted with the timeout surfaced
-- bash got a deadline, eval got none, so a slow form wedged the agent
worker for its natural lifetime."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (start (get-internal-real-time)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool protocol
                                    :eval
                                    '(:form "(sleep 30)" :timeout 1)
                                    context))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second))
           (details (ext:tool-result-details result)))
      (is (ext:tool-result-error-p result))
      (is (getf details :timed-out-p))
      (is (= 1 (getf details :timeout-seconds)))
      (is (search "timed out" (tool-result-text result)))
      (is (< elapsed 10)
          "the interrupt must take effect instead of waiting out the form"))))

(test (eval-tool-interrupts-a-tight-loop :fixture tool-authority)
  "The audit case: an infinite loop wedges the agent worker beyond even
quit's reach. The interrupt must take effect on a pure computation loop,
not just on a sleeping form."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (start (get-internal-real-time)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool protocol
                                    :eval
                                    '(:form "(loop)" :timeout 1)
                                    context))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second))
           (text (tool-result-text result)))
      (is (ext:tool-result-error-p result))
      (is (getf (ext:tool-result-details result) :timed-out-p))
      (is (not (search "output unavailable" text))
          "the interrupt must actually stop the loop")
      (is (< elapsed 10)))))

(test (eval-tool-reads-forms-in-the-requested-package :fixture tool-authority)
  "The reader must run under the :package binding. The read previously
happened on the runner thread before the binding, interning unqualified
symbols into the runner's ambient package -- so a form naming an
extension-defined function failed with undefined-function in CL-USER
despite the correct eval-time package."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((probe (or (find-package "KLI-EVAL-READ-PROBE")
                     (make-package "KLI-EVAL-READ-PROBE"
                                   :use '("COMMON-LISP")))))
      (unwind-protect
           (let ((result (ext:invoke-tool
                          protocol
                          :eval
                          '(:form "(symbol-package 'read-probe-symbol)"
                            :package "KLI-EVAL-READ-PROBE")
                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "KLI-EVAL-READ-PROBE" (tool-result-text result))
                 "the value names the requested package, not the ambient one")
             (is (not (null (find-symbol "READ-PROBE-SYMBOL" probe)))
                 "the unqualified symbol interned in the requested package"))
        (delete-package probe)))))

(test (eval-tool-read-time-evaluation-stays-under-the-deadline :fixture tool-authority)
  "Sharp-dot runs at read time. Reading on the runner thread put it outside
the deadline and abort guards, so a #.(sleep ...) wedged the agent worker
exactly like the pre-deadline eval did. Reading on the eval thread puts it
under both."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (start (get-internal-real-time)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool protocol
                                    :eval
                                    '(:form "#.(sleep 30)" :timeout 1)
                                    context))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
      (is (ext:tool-result-error-p result))
      (is (getf (ext:tool-result-details result) :timed-out-p))
      (is (< elapsed 10)
          "the read-time sleep must hit the deadline, not run to completion"))))

(test (eval-tool-abort-cuts-eval-short :fixture tool-authority)
  "An abort request must interrupt a running form just like the deadline
does, so Esc reaches a long eval instead of waiting it out."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (start (get-internal-real-time)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((ext:*tool-abort-predicate* (lambda () t))
           (result (ext:invoke-tool protocol
                                    :eval
                                    '(:form "(sleep 30)")
                                    context))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second))
           (details (ext:tool-result-details result)))
      (is (ext:tool-result-error-p result))
      (is (getf details :aborted-p))
      (is (not (getf details :timed-out-p)))
      (is (search "aborted" (tool-result-text result)))
      (is (< elapsed 10)
          "the abort must take effect instead of waiting out the form"))))

(defun tool-test-path (name)
  (make-pathname :directory '(:absolute "tmp")
                 :name (format nil "kli-tool-test-~D-~A"
                               #+sbcl (sb-posix:getpid)
                               #-sbcl 0
                               name)))

(defun tool-test-typed-path (name type)
  (make-pathname :type type :defaults (tool-test-path name)))

(defun tool-result-text (result)
  (getf (first (ext:tool-result-content result)) :text))

(test (base-tools-register-read-write-edit-and-bash :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "file")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-bash:*bash-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*edit-tool-extension-manifest*)
           (dolist (name '(:bash :read :write :edit))
             (is (typep (ext:find-tool protocol name) 'ext:tool)))
           (let ((bash-result (ext:invoke-tool protocol
                                               :bash
                                               '(:command "printf bash-ok")
                                               context)))
             (is (not (ext:tool-result-error-p bash-result)))
             (is (string= "bash-ok" (tool-result-text bash-result))))
           (let ((write-result (ext:invoke-tool protocol
                                                :write
                                                (list :path (namestring path)
                                                      :content "alpha beta")
                                                context)))
             (is (not (ext:tool-result-error-p write-result))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path)
                                                     :raw t)
                                               context)))
             (is (string= "alpha beta" (tool-result-text read-result))))
           (let ((edit-result (ext:invoke-tool protocol
                                               :edit
                                               (list :input
                                                     (format nil "@@ ~A~%= 1:~A..1:~A~%~~alpha gamma"
                                                             (namestring path)
                                                             (tools-filesystem:line-hash "alpha beta")
                                                             (tools-filesystem:line-hash "alpha beta")))
                                               context)))
             (is (not (ext:tool-result-error-p edit-result))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path)
                                                     :raw t)
                                               context)))
             (is (string= "alpha gamma" (tool-result-text read-result)))))
      (ignore-errors (delete-file path)))))

(defun tree-contains-key-p (tree key)
  (cond ((atom tree) nil)
        ((eq (car tree) key) t)
        (t (or (tree-contains-key-p (car tree) key)
               (tree-contains-key-p (cdr tree) key)))))

(defun assert-private-diff-presentation (result path &key (require-hunks t))
  (let* ((term (ext:tool-result-presentation result))
         (updates (and (consp term) (getf term :updates)))
         (update (first updates)))
    (is (eq :diff (getf term :kind)))
    (is (= 1 (length updates)))
    (is (string= path (getf update :path)))
    (when require-hunks
      (is (consp (getf update :hunks))))
    (dolist (key '(:old :new :preview-old :preview-new :patched :repaired))
      (is (not (tree-contains-key-p term key))
          (format nil "private presentation must not contain bulk key ~S" key)))))

(test (write-tool-details-are-compact :fixture tool-authority)
  "Write details expose model-facing mutation facts, not whole file bodies."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "write-details")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*)
           (let* ((result (ext:invoke-tool protocol
                                           :write
                                           (list :path (namestring path)
                                                 :content "alpha")
                                           context))
                  (details (ext:tool-result-details result)))
             (is (string= (namestring path) (getf details :path)))
             (is (getf details :created-p))
             (is (= 5 (getf details :characters)))
             (is (= 1 (getf details :added)))
             (is (= 0 (getf details :removed)))
             (is (null (getf details :old)))
             (is (null (getf details :new)))
             (is (stringp (getf details :new-sha256)))
             (assert-private-diff-presentation result (namestring path))
             (assert-compact-file-result-wire
              (tool-result-responses-wire "write" result)
              (namestring path)))
           (let* ((result (ext:invoke-tool protocol
                                           :write
                                           (list :path (namestring path)
                                                 :content "beta")
                                           context))
                  (details (ext:tool-result-details result)))
             (is (getf details :overwritten-p))
             (is (= 1 (getf details :added)))
             (is (= 1 (getf details :removed)))
             (is (null (getf details :old)))
             (is (null (getf details :new)))
             (is (stringp (getf details :old-sha256)))
             (is (stringp (getf details :new-sha256)))
             (assert-private-diff-presentation result (namestring path))
             (assert-compact-file-result-wire
              (tool-result-responses-wire "write" result)
              (namestring path))))
      (ignore-errors (delete-file path)))))

(test line-hash-fixtures
  "Contract fixtures for the line hash. Changing the hash function breaks these on purpose. Raw content is hashed, so whitespace and case drift change the anchor."
  (is (string= "c5" (tools-filesystem:line-hash "")))
  (is (string= "21" (tools-filesystem:line-hash "(defun hello ())")))
  (is (not (string= "21" (tools-filesystem:line-hash "(defun hello ()) "))))
  (is (not (string= "21" (tools-filesystem:line-hash "(Defun Hello ())"))))
  (let ((hash (tools-filesystem:line-hash "λ → α")))
    (is (= 2 (length hash)))
    (is (every (lambda (ch) (find ch "0123456789abcdef")) hash))))

(test split-file-lines-semantics
  (let ((lines (tools-filesystem:split-file-lines
                (format nil "alpha~%beta~%gamma~%"))))
    (is (typep lines 'simple-vector))
    (is (equalp #("alpha" "beta" "gamma") lines)))
  (is (equalp #("alpha" "" "beta")
              (tools-filesystem:split-file-lines (format nil "alpha~%~%beta"))))
  (is (equalp #("alpha" "")
              (tools-filesystem:split-file-lines (format nil "alpha~%~%"))))
  (is (zerop (length (tools-filesystem:split-file-lines "")))))

(test clamp-range-defaults-clamps-and-rejects
  (multiple-value-bind (start end)
      (tools-filesystem:clamp-range 3 nil nil "f.lisp")
    (is (= 1 start))
    (is (= 3 end)))
  (multiple-value-bind (start end)
      (tools-filesystem:clamp-range 3 2 99 "f.lisp")
    (is (= 2 start))
    (is (= 3 end)))
  (multiple-value-bind (start end)
      (tools-filesystem:clamp-range 3 0 nil "f.lisp")
    (is (= 1 start))
    (is (= 3 end)))
  (signals error (tools-filesystem:clamp-range 3 5 nil "f.lisp"))
  (signals error (tools-filesystem:clamp-range 3 2 1 "f.lisp")))

(test (read-tool-emits-anchored-lines-and-arms-cache :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "anchored-read"))
         (other (tool-test-path "anchored-unread")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%beta~%gamma~%"))
                            context)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring other)
                                  :content "unread")
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :read
                                          (list :path (namestring path))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "1:~A|alpha~%2:~A|beta~%3:~A|gamma"
                                  (tools-filesystem:line-hash "alpha")
                                  (tools-filesystem:line-hash "beta")
                                  (tools-filesystem:line-hash "gamma"))
                          (tool-result-text result)))
             (let ((details (ext:tool-result-details result)))
               (is (= 3 (getf details :lines)))
               (is (= 1 (getf details :start)))
               (is (= 3 (getf details :end)))
               (is (not (getf details :raw)))))
           (is (eq :valid
                   (tools-filesystem:anchor-known-p
                    protocol path 1 (tools-filesystem:line-hash "alpha"))))
           (is (eq :stale (tools-filesystem:anchor-known-p protocol path 1 "zz")))
           (is (eq :stale
                   (tools-filesystem:anchor-known-p
                    protocol path 99 (tools-filesystem:line-hash "alpha"))))
           (is (eq :unread
                   (tools-filesystem:anchor-known-p protocol other 1 "aa"))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file other)))))

(test (read-tool-pipe-delimiter-disambiguates-indentation :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "anchored-read-delimiter" "lisp"))
         (lines (list "" " " "  (form)" "|symbol name|"))
         (content (format nil "~{~A~%~}" lines)))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path) :content content)
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :read
                                          (list :path (namestring path))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "1:~A|~%2:~A| ~%3:~A|  (form)~%4:~A||symbol name|"
                                  (tools-filesystem:line-hash "")
                                  (tools-filesystem:line-hash " ")
                                  (tools-filesystem:line-hash "  (form)")
                                  (tools-filesystem:line-hash "|symbol name|"))
                          (tool-result-text result)))))
      (ignore-errors (delete-file path)))))

(test (read-tool-range-slices-and-rejects-start-past-eof :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "anchored-range")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%beta~%gamma~%"))
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :read
                                          (list :path (namestring path)
                                                :start 2
                                                :end 99)
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "2:~A|beta~%3:~A|gamma"
                                  (tools-filesystem:line-hash "beta")
                                  (tools-filesystem:line-hash "gamma"))
                          (tool-result-text result)))
             (let ((details (ext:tool-result-details result)))
               (is (= 2 (getf details :start)))
               (is (= 3 (getf details :end)))))
           (let ((result (ext:invoke-tool protocol
                                          :read
                                          (list :path (namestring path)
                                                :start 5)
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "only 3 lines" (tool-result-text result)))))
      (ignore-errors (delete-file path)))))

(test (read-tool-raw-bypasses-anchors-and-still-arms-cache :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "anchored-raw")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%beta~%gamma~%"))
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :read
                                          (list :path (namestring path)
                                                :raw t)
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "alpha~%beta~%gamma")
                          (tool-result-text result)))
             (is (getf (ext:tool-result-details result) :raw)))
           (is (eq :valid
                   (tools-filesystem:anchor-known-p
                    protocol path 3 (tools-filesystem:line-hash "gamma")))))
      (ignore-errors (delete-file path)))))

(test (read-tool-reports-empty-file :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "anchored-empty")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path) :content "")
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :read
                                          (list :path (namestring path))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "(empty file)" (tool-result-text result)))
             (is (zerop (getf (ext:tool-result-details result) :lines)))))
      (ignore-errors (delete-file path)))))

(test (anchor-cache-keys-by-truename :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "anchored-truename")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%"))
                            context)
           (ext:invoke-tool protocol
                            :read
                            (list :path (format nil "/tmp/./~A"
                                                (file-namestring path)))
                            context)
           (is (eq :valid
                   (tools-filesystem:anchor-known-p
                    protocol path 1 (tools-filesystem:line-hash "alpha")))))
      (ignore-errors (delete-file path)))))

(test (session-switch-clears-anchor-cache :fixture tool-authority)
  "Read-before-edit is conversation-scoped. A session switch starts a fresh
conversation with none of the prior session's reads in the model's context, so
the anchor cache must reset -- an edit in the new session must re-read rather
than trust a read that happened in a session the user switched away from."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "anchored-session-switch")))
    (unwind-protect
         (progn
           (install-extensions
            context
            event:*events-extension-manifest*
            tools-filesystem:*write-tool-extension-manifest*
            tools-filesystem:*read-tool-extension-manifest*
            tools-filesystem:*filesystem-anchor-lifecycle-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%"))
                            context)
           (ext:invoke-tool protocol :read
                            (list :path (namestring path)) context)
           (is (eq :valid
                   (tools-filesystem:anchor-known-p
                    protocol path 1 (tools-filesystem:line-hash "alpha"))))
           (event:emit-event context
                             (event:make-event :session-switch :source :test))
           (is (eq :unread
                   (tools-filesystem:anchor-known-p
                    protocol path 1 (tools-filesystem:line-hash "alpha")))
               "a session switch clears the read-before-edit anchor cache"))
      (ignore-errors (delete-file path)))))

(defun hashline-fixture (context protocol path content)
  "Write CONTENT to PATH and read it so the anchor cache is armed."
  (ext:invoke-tool protocol
                   :write
                   (list :path (namestring path) :content content)
                   context)
  (ext:invoke-tool protocol
                   :read
                   (list :path (namestring path))
                   context))

(defun install-file-tools (context)
  (install-extensions context
                      tools-filesystem:*write-tool-extension-manifest*
                      tools-filesystem:*read-tool-extension-manifest*
                      tools-filesystem:*edit-tool-extension-manifest*))

(test hashline-parse-ops-and-payload
  (let ((sections (tools-filesystem:parse-hashline-patch
                   (format nil "@@ /tmp/a.txt~%~
                                + 2:ab~%~
                                ~~  indented~%~
                                ~~~~tilde~%~
                                +~%~
                                ~~eof-line~%~
                                <~%~
                                ~~bof-line~%~
                                ~%~
                                @@ /tmp/b.txt~%~
                                - 1:aa..2:bb~%~
                                = 3:cc..3:cc"))))
    (is (= 2 (length sections)))
    (destructuring-bind ((path-a . ops-a) (path-b . ops-b)) sections
      (is (string= "/tmp/a.txt" path-a))
      (is (string= "/tmp/b.txt" path-b))
      (is (= 3 (length ops-a)))
      (let ((op (first ops-a)))
        (is (eq :insert-after (tools-filesystem:patch-op-kind op)))
        (is (= 2 (tools-filesystem:patch-op-start-line op)))
        (is (string= "ab" (tools-filesystem:patch-op-start-hash op)))
        (is (equal '("  indented" "~tilde")
                   (tools-filesystem:patch-op-payload op))))
      (let ((op (second ops-a)))
        (is (eq :insert-after (tools-filesystem:patch-op-kind op)))
        (is (null (tools-filesystem:patch-op-start-line op)))
        (is (equal '("eof-line") (tools-filesystem:patch-op-payload op))))
      (let ((op (third ops-a)))
        (is (eq :insert-before (tools-filesystem:patch-op-kind op)))
        (is (null (tools-filesystem:patch-op-start-line op)))
        (is (equal '("bof-line") (tools-filesystem:patch-op-payload op))))
      (let ((op (first ops-b)))
        (is (eq :delete (tools-filesystem:patch-op-kind op)))
        (is (= 1 (tools-filesystem:patch-op-start-line op)))
        (is (string= "aa" (tools-filesystem:patch-op-start-hash op)))
        (is (= 2 (tools-filesystem:patch-op-end-line op)))
        (is (string= "bb" (tools-filesystem:patch-op-end-hash op))))
      (let ((op (second ops-b)))
        (is (eq :replace (tools-filesystem:patch-op-kind op)))
        (is (null (tools-filesystem:patch-op-payload op)))))))

(test hashline-parse-accepts-copied-pipe-anchors
  (let ((sections (tools-filesystem:parse-hashline-patch
                   (format nil "@@ /tmp/a.txt~%~
                                + 2:ab|~%~
                                ~~after~%~
                                = 3:cc|..4:dd|~%~
                                ~~replacement~%~
                                - 5:ee|..6:ff|"))))
    (destructuring-bind ((path . ops)) sections
      (is (string= "/tmp/a.txt" path))
      (is (= 3 (length ops)))
      (let ((op (first ops)))
        (is (eq :insert-after (tools-filesystem:patch-op-kind op)))
        (is (= 2 (tools-filesystem:patch-op-start-line op)))
        (is (string= "ab" (tools-filesystem:patch-op-start-hash op))))
      (let ((op (second ops)))
        (is (eq :replace (tools-filesystem:patch-op-kind op)))
        (is (= 3 (tools-filesystem:patch-op-start-line op)))
        (is (string= "cc" (tools-filesystem:patch-op-start-hash op)))
        (is (= 4 (tools-filesystem:patch-op-end-line op)))
        (is (string= "dd" (tools-filesystem:patch-op-end-hash op))))
      (let ((op (third ops)))
        (is (eq :delete (tools-filesystem:patch-op-kind op)))
        (is (= 5 (tools-filesystem:patch-op-start-line op)))
        (is (string= "ee" (tools-filesystem:patch-op-start-hash op)))
        (is (= 6 (tools-filesystem:patch-op-end-line op)))
        (is (string= "ff" (tools-filesystem:patch-op-end-hash op)))))))

(test hashline-parse-rejects-malformed
  (signals error (tools-filesystem:parse-hashline-patch "= 1:aa..1:aa"))
  (signals error (tools-filesystem:parse-hashline-patch
                  (format nil "@@ /tmp/a~%bogus")))
  (signals error (tools-filesystem:parse-hashline-patch
                  (format nil "@@ /tmp/a~%+ 2:xyz")))
  (signals error (tools-filesystem:parse-hashline-patch
                  (format nil "@@ /tmp/a~%+ two:ab")))
  (signals error (tools-filesystem:parse-hashline-patch
                  (format nil "@@ /tmp/a~%- 3:aa..2:bb")))
  (signals error (tools-filesystem:parse-hashline-patch
                  (format nil "@@ /tmp/a~%~~stray")))
  (signals error (tools-filesystem:parse-hashline-patch
                  (format nil "@@ /tmp/a~%- 1:aa..1:aa~%~~nope")))
  (signals error (tools-filesystem:parse-hashline-patch
                  (format nil "@@ /tmp/a~%- 1:aa"))))

(test hashline-parse-diagnoses-indented-markers-and-missing-payload-tilde
  (flet ((message (input)
           (handler-case
               (progn (tools-filesystem:parse-hashline-patch input) nil)
             (error (condition) (princ-to-string condition)))))
    (let ((indented-replace (message (format nil "@@ /tmp/a~% = 1:aa..1:aa")))
          (indented-header (message " @@ /tmp/a"))
          (indented-payload (message (format nil "@@ /tmp/a~%+~% ~~payload")))
          (missing-payload (message (format nil "@@ /tmp/a~%+~%literal")))
          (valid-payload (tools-filesystem:parse-hashline-patch
                          (format nil "@@ /tmp/a~%+~%~~literal"))))
      (dolist (text (list indented-replace indented-header indented-payload))
        (is (search "column 1" text))
        (is (search "leading whitespace" text))
        (is (search "file content" text))
        (is (search "~content" text)))
      (is (search "\"=\"" indented-replace))
      (is (search "\"@@\"" indented-header))
      (is (search "\"~\"" indented-payload))
      (is (search "as payload" missing-payload))
      (is (search "must start with \"~\"" missing-payload))
      (is (equal '("literal")
                 (tools-filesystem:patch-op-payload
                  (first (cdar valid-payload))))))))

(test (hashline-edit-replaces-range-with-fresh-anchors :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-replace")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path
                             (format nil "alpha~%beta~%gamma~%delta-tail~%"))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 2:~A..3:~A~%~~BETA-GAMMA FUSED"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "beta")
                                                        (tools-filesystem:line-hash "gamma")))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "Edited ~A (+1 -2)~%2:~A|BETA-GAMMA FUSED"
                                  (namestring path)
                                  (tools-filesystem:line-hash "BETA-GAMMA FUSED"))
                          (tool-result-text result)))
             (let* ((details (ext:tool-result-details result))
                    (file (first (getf details :files))))
               (is (= 1 (length (getf details :files))))
               (is (string= (namestring path) (getf file :path)))
               (is (= 1 (getf file :added)))
               (is (= 2 (getf file :removed)))
               (is (equal '((:start 2 :end 2))
                          (getf file :changed-ranges)))
               (is (null (getf file :old)))
               (is (null (getf file :new)))
               (is (stringp (getf file :old-sha256)))
               (is (stringp (getf file :new-sha256)))
               (assert-private-diff-presentation result (namestring path))
               (assert-compact-file-result-wire
                (tool-result-responses-wire "edit" result)
                (namestring path))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path)
                                                     :raw t)
                                               context)))
             (is (string= (format nil "alpha~%BETA-GAMMA FUSED~%delta-tail")
                          (tool-result-text read-result))))
           (is (eq :valid
                   (tools-filesystem:anchor-known-p
                    protocol path 2
                    (tools-filesystem:line-hash "BETA-GAMMA FUSED")))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-chains-on-fresh-anchors :fixture tool-authority)
  "A second edit consumes the fresh anchor returned by the first result, with no intervening read."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-chain")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path
                             (format nil "alpha~%beta~%gamma~%"))
           (ext:invoke-tool protocol
                            :edit
                            (list :input
                                  (format nil "@@ ~A~%= 2:~A..2:~A~%~~fused"
                                          (namestring path)
                                          (tools-filesystem:line-hash "beta")
                                          (tools-filesystem:line-hash "beta")))
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 2:~A..2:~A~%~~final"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "fused")
                                                        (tools-filesystem:line-hash "fused")))
                                          context)))
             (is (not (ext:tool-result-error-p result))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path)
                                                     :raw t)
                                               context)))
             (is (string= (format nil "alpha~%final~%gamma")
                          (tool-result-text read-result)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-bof-eof-renumber :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-bof-eof")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path (format nil "a~%b~%"))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%<~%~~prepended~%+~%~~appended"
                                                        (namestring path)))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "Edited ~A (+2 -0)~%1:~A|prepended~%4:~A|appended"
                                  (namestring path)
                                  (tools-filesystem:line-hash "prepended")
                                  (tools-filesystem:line-hash "appended"))
                          (tool-result-text result))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path)
                                                     :raw t)
                                               context)))
             (is (string= (format nil "prepended~%a~%b~%appended")
                          (tool-result-text read-result)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-insert-and-delete :fixture tool-authority)
  "An insert and a delete in one patch. Delete ops contribute no region lines to the result preview, the insert does."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-insert-delete")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path
                             (format nil "alpha~%beta~%gamma~%"))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%+ 1:~A~%~~inserted~%- 3:~A..3:~A"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "alpha")
                                                        (tools-filesystem:line-hash "gamma")
                                                        (tools-filesystem:line-hash "gamma")))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "Edited ~A (+1 -1)~%2:~A|inserted"
                                  (namestring path)
                                  (tools-filesystem:line-hash "inserted"))
                          (tool-result-text result))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path)
                                                     :raw t)
                                               context)))
             (is (string= (format nil "alpha~%inserted~%beta")
                          (tool-result-text read-result)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-delete-everything :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-delete-all")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path (format nil "x~%y~%"))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%- 1:~A..2:~A"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "x")
                                                        (tools-filesystem:line-hash "y")))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "Edited ~A (+0 -2)" (namestring path))
                          (tool-result-text result)))
             (let ((file (first (getf (ext:tool-result-details result) :files))))
               (is (= 0 (getf file :added)))
               (is (= 2 (getf file :removed)))
               (is (null (getf file :new)))
               (is (stringp (getf file :new-sha256)))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path))
                                               context)))
             (is (string= "(empty file)" (tool-result-text read-result)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-rejects-stale-anchor-with-guidance :fixture tool-authority)
  "A stale anchor is rejected with guidance to re-read around the line. The write tool does not refresh the anchor cache, so writing after a read drifts the file behind the cache."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-stale")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path (format nil "one~%two~%"))
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "ONE~%TWO~%"))
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 2:~A..2:~A~%~~patched"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "two")
                                                        (tools-filesystem:line-hash "two")))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "is stale (replace start)" (tool-result-text result)))
             (is (search "Re-read" (tool-result-text result)))
             (is (search "around line 2" (tool-result-text result))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path)
                                                     :raw t)
                                               context)))
             (is (string= (format nil "ONE~%TWO")
                          (tool-result-text read-result)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-rejects-unread-file-without-stale-cascade :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-unread")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "one~%two~%"))
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 1:aa..1:aa~%~~x~%- 2:bb..2:bb"
                                                        (namestring path)))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "has not been read this session" (tool-result-text result)))
             (is (search "Read it first to obtain anchors" (tool-result-text result)))
             (is (not (search "stale" (tool-result-text result))))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-rejects-overlapping-ops :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-overlap")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path
                             (format nil "a~%b~%c~%d~%"))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 2:~A..3:~A~%~~X~%- 3:~A..4:~A"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "b")
                                                        (tools-filesystem:line-hash "c")
                                                        (tools-filesystem:line-hash "c")
                                                        (tools-filesystem:line-hash "d")))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "Ops overlap on lines 2..3 and 3..4"
                         (tool-result-text result)))
             (is (search "Split into separate edits" (tool-result-text result))))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 2:~A..3:~A~%~~X~%+ 2:~A~%~~Y"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "b")
                                                        (tools-filesystem:line-hash "c")
                                                        (tools-filesystem:line-hash "b")))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "Split into separate edits" (tool-result-text result)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-dedupes-twin-anchor-problems :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-dedupe")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path (format nil "a~%b~%"))
           (let* ((real (tools-filesystem:line-hash "a"))
                  (stale (if (string= real "00") "11" "00"))
                  (result (ext:invoke-tool protocol
                                           :edit
                                           (list :input
                                                 (format nil "@@ ~A~%= 1:~A..1:~A~%~~new"
                                                         (namestring path)
                                                         stale stale))
                                           context))
                  (text (tool-result-text result))
                  (first-hit (search "is stale" text)))
             (is (ext:tool-result-error-p result))
             (is (not (null first-hit)))
             (is (not (search "is stale" text :start2 (1+ first-hit))))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-multi-file-atomicity :fixture tool-authority)
  "A multi-file patch is atomic. When one file has drifted behind the cache, its section is stale and the valid section of the other file is not written."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path-a (tool-test-path "hashline-atomic-a"))
         (path-b (tool-test-path "hashline-atomic-b")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path-a (format nil "a1~%a2~%"))
           (hashline-fixture context protocol path-b (format nil "b1~%b2~%"))
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path-b)
                                  :content (format nil "b1x~%b2x~%"))
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 1:~A..1:~A~%~~A1-NEW~%@@ ~A~%= 1:~A..1:~A~%~~B1-NEW"
                                                        (namestring path-a)
                                                        (tools-filesystem:line-hash "a1")
                                                        (tools-filesystem:line-hash "a1")
                                                        (namestring path-b)
                                                        (tools-filesystem:line-hash "b1")
                                                        (tools-filesystem:line-hash "b1")))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "is stale" (tool-result-text result))))
           (let ((read-result (ext:invoke-tool protocol
                                               :read
                                               (list :path (namestring path-a)
                                                     :raw t)
                                               context)))
             (is (string= (format nil "a1~%a2")
                          (tool-result-text read-result)))))
      (ignore-errors (delete-file path-a))
      (ignore-errors (delete-file path-b)))))

(test (hashline-edit-rejects-insert-without-payload :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-no-payload")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path (format nil "a~%"))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%+ 1:~A"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "a")))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "has no payload" (tool-result-text result)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-rejects-anchor-beyond-eof :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "hashline-beyond-eof")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path
                             (format nil "a~%b~%c~%"))
           (let ((result (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 9:aa..9:aa~%~~x"
                                                        (namestring path)))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "only 3 lines" (tool-result-text result)))))
      (ignore-errors (delete-file path)))))

(defun first-repair-preview (result)
  (first (getf (ext:tool-result-details result) :repair-previews)))

(defun two-defun-content ()
  (format nil "(defun a ()~%  (+ 1 2))~%(defun b ()~%  (+ 3 4))~%"))

(test (hashline-edit-unsafe-lisp-preview-writes-nothing :fixture tool-authority)
  "Default safe repair previews an unsafe Lisp repair without changing disk."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-unsafe-preview" "lisp"))
         (original (two-defun-content)))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let* ((result (ext:invoke-tool
                           protocol
                           :edit
                           (list :input
                                 (format nil "@@ ~A~%= 3:~A..3:~A~%~~((defun b ()"
                                         (namestring path)
                                         (tools-filesystem:line-hash "(defun b ()")
                                         (tools-filesystem:line-hash "(defun b ()")))
                           context))
                  (preview (first-repair-preview result)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "No files were written" (tool-result-text result)))
             (is (stringp (getf preview :candidate-id)))
             (is (null (getf (getf preview :classification) :safe-p)))
             (is (string= original (uiop:read-file-string path)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-accepts-preview-once :fixture tool-authority)
  "A repair preview can be accepted exactly once when the file is unchanged."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-accept-preview" "lisp"))
         (original (two-defun-content)))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let* ((preview-result
                    (ext:invoke-tool
                     protocol
                     :edit
                     (list :input
                           (format nil "@@ ~A~%= 3:~A..3:~A~%~~((defun b ()"
                                   (namestring path)
                                   (tools-filesystem:line-hash "(defun b ()")
                                   (tools-filesystem:line-hash "(defun b ()")))
                     context))
                  (preview (first-repair-preview preview-result))
                  (candidate-id (getf preview :candidate-id))
                  (accepted (ext:invoke-tool
                             protocol
                             :edit
                             (list :accept_repair candidate-id
                                   :path (namestring path))
                             context))
                  (second-accept (ext:invoke-tool
                                  protocol
                                  :edit
                                  (list :accept_repair candidate-id
                                        :path (namestring path))
                                  context))
                  (canonical-path
                    (tools-filesystem::canonical-file-namestring
                     (namestring path))))
             (is (not (ext:tool-result-error-p preview-result)))
             (assert-private-diff-presentation preview-result (namestring path))
             (let ((wire (tool-result-responses-wire "edit" preview-result)))
               (is (search "\"repair-previews\":" wire))
               (is (search "\"status\":\"repair-preview\"" wire))
               (is (search "\"candidate-id\":" wire))
               (is (search "\"patched-sha256\":" wire))
               (is (search "\"repaired-sha256\":" wire))
               (assert-no-bulk-file-result-wire wire))
             (is (not (ext:tool-result-error-p accepted)))
             (assert-private-diff-presentation accepted canonical-path)
             (assert-compact-file-result-wire
              (tool-result-responses-wire "edit" accepted)
              canonical-path)
             (is (search "Accepted repair" (tool-result-text accepted)))
             (is (search "|" (tool-result-text accepted))
                 "repair accept returns pipe-delimited fresh anchors")
             (is (string= (getf (first (getf (ext:tool-result-details accepted)
                                             :files))
                                :new-sha256)
                          (tools-filesystem::file-content-sha256
                           (uiop:read-file-string path))))
             (is (ext:tool-result-error-p second-accept))
             (is (search "No pending edit repair candidate"
                         (tool-result-text second-accept)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-accepts-preview-through-symlink-path :fixture tool-authority)
  "Accepting a repair through an alias path reports the canonical file path."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (real-path (tool-test-typed-path "hashline-accept-symlink-real" "lisp"))
         (link-path (tool-test-typed-path "hashline-accept-symlink-link" "lisp"))
         (original (two-defun-content)))
    (unwind-protect
         (progn
           (install-file-tools context)
           (ignore-errors (delete-file link-path))
           (with-open-file (s real-path :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
             (write-string original s))
           (sb-posix:symlink (namestring real-path) (namestring link-path))
           (ext:invoke-tool protocol
                            :read
                            (list :path (namestring link-path))
                            context)
           (let* ((preview-result
                    (ext:invoke-tool
                     protocol
                     :edit
                     (list :input
                           (format nil "@@ ~A~%= 3:~A..3:~A~%~~((defun b ()"
                                   (namestring link-path)
                                   (tools-filesystem:line-hash "(defun b ()")
                                   (tools-filesystem:line-hash "(defun b ()")))
                     context))
                  (preview (first-repair-preview preview-result))
                  (candidate-id (getf preview :candidate-id))
                  (accepted (ext:invoke-tool
                             protocol
                             :edit
                             (list :accept_repair candidate-id
                                   :path (namestring link-path))
                             context))
                  (canonical-path
                    (tools-filesystem::canonical-file-namestring
                     (namestring link-path))))
             (is (not (string= (namestring link-path) canonical-path))
                 "the fixture must exercise a path alias")
             (is (not (ext:tool-result-error-p preview-result)))
             (assert-private-diff-presentation preview-result
                                               (namestring link-path))
             (is (not (ext:tool-result-error-p accepted)))
             (assert-private-diff-presentation accepted canonical-path)
             (assert-compact-file-result-wire
              (tool-result-responses-wire "edit" accepted)
              canonical-path)))
      (ignore-errors (delete-file link-path))
      (ignore-errors (delete-file real-path)))))

(test (hashline-edit-accept-rejects-disk-drift :fixture tool-authority)
  "Accepting a preview re-checks the file hash and refuses to write after drift."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-accept-drift" "lisp"))
         (original (two-defun-content)))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let* ((preview-result
                    (ext:invoke-tool
                     protocol
                     :edit
                     (list :input
                           (format nil "@@ ~A~%= 3:~A..3:~A~%~~((defun b ()"
                                   (namestring path)
                                   (tools-filesystem:line-hash "(defun b ()")
                                   (tools-filesystem:line-hash "(defun b ()")))
                     context))
                  (candidate-id (getf (first-repair-preview preview-result)
                                      :candidate-id)))
             (ext:invoke-tool protocol
                              :write
                              (list :path (namestring path)
                                    :content (format nil "(defun drift () :changed)~%"))
                              context)
             (let ((accepted (ext:invoke-tool
                              protocol
                              :edit
                              (list :accept_repair candidate-id
                                    :path (namestring path))
                              context)))
               (is (ext:tool-result-error-p accepted))
               (is (search "changed after preview" (tool-result-text accepted)))
               (is (string= (format nil "(defun drift () :changed)~%")
                            (uiop:read-file-string path))))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-safe-missing-paren-repair-auto-writes :fixture tool-authority)
  "A local missing-paren repair in a replaced form is safe and writes immediately."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-safe-repair" "lisp"))
         (original (format nil "(defun one ()~%  (+ 1 2))~%"))
         (expected (format nil "(defun one ()~%  (* 3 4))~%")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let ((result (ext:invoke-tool
                          protocol
                          :edit
                          (list :input
                                (format nil "@@ ~A~%= 1:~A..2:~A~%~~(defun one ()~%~~  (* 3 4"
                                        (namestring path)
                                        (tools-filesystem:line-hash "(defun one ()")
                                        (tools-filesystem:line-hash "  (+ 1 2))")))
                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (null (getf (ext:tool-result-details result) :repair-previews)))
             (is (search "auto-repaired" (tool-result-text result)))
             (is (string= expected (uiop:read-file-string path)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-repair-reject-policy-errors-before-writing :fixture tool-authority)
  "repair=reject refuses unbalanced Lisp output and leaves disk untouched."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-repair-reject" "lisp"))
         (original (format nil "(defun one ()~%  (+ 1 2))~%")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let ((result (ext:invoke-tool
                          protocol
                          :edit
                          (list :repair "reject"
                                :input
                                (format nil "@@ ~A~%= 1:~A..2:~A~%~~(defun one ()~%~~  (* 3 4"
                                        (namestring path)
                                        (tools-filesystem:line-hash "(defun one ()")
                                        (tools-filesystem:line-hash "  (+ 1 2))")))
                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "unbalanced" (tool-result-text result)))
             (is (string= original (uiop:read-file-string path)))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-writes-reader-indeterminate-balanced-lisp :fixture tool-authority)
  "A delimiter-balanced Lisp file that fails scratch-package reading is still editable."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-reader-indeterminate" "lisp"))
         (original (format nil "(in-package #:kli/tests)~%(ext:foo)~%"))
         (expected (format nil "(in-package #:kli/tests)~%(ext:bar)~%")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let ((result (ext:invoke-tool
                          protocol
                          :edit
                          (list :input
                                (format nil "@@ ~A~%= 2:~A..2:~A~%~~(ext:bar)"
                                        (namestring path)
                                        (tools-filesystem:line-hash "(ext:foo)")
                                        (tools-filesystem:line-hash "(ext:foo)")))
                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "Edited" (tool-result-text result)))
             (is (null (getf (ext:tool-result-details result) :repair-previews)))
             (is (null (getf (first (getf (ext:tool-result-details result) :files))
                             :repair)))
             (is (string= expected (uiop:read-file-string path)))))
      (ignore-errors (delete-file path)))))


(test (hashline-edit-writes-cst-valid-under-every-policy :fixture tool-authority)
  "A CST-valid Lisp patch writes identically under repair reject, preview, and safe."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-valid-policies" "lisp"))
         (expected (format nil "(defun a ()~%  (* 9 9))~%(defun b ()~%  (+ 3 4))~%")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (dolist (policy '("reject" "preview" "safe"))
             (hashline-fixture context protocol path (two-defun-content))
             (let ((result (ext:invoke-tool
                            protocol
                            :edit
                            (list :repair policy
                                  :input
                                  (format nil "@@ ~A~%= 2:~A..2:~A~%~~  (* 9 9))"
                                          (namestring path)
                                          (tools-filesystem:line-hash "  (+ 1 2))")
                                          (tools-filesystem:line-hash "  (+ 1 2))")))
                            context)))
               (is (not (ext:tool-result-error-p result)) "policy ~A errored" policy)
               (is (search "Edited" (tool-result-text result)))
               (is (null (first-repair-preview result)) "policy ~A previewed" policy)
               (is (string= expected (uiop:read-file-string path))
                   "policy ~A wrote wrong content" policy))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-rejects-cst-invalid-balanced-under-every-policy
       :fixture tool-authority)
  "A delimiter-balanced but CST-invalid Lisp patch is rejected under every policy,
writing nothing and offering no repair preview."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-invalid-balanced" "lisp"))
         (original (format nil "(list (a b))~%")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (dolist (policy '("reject" "preview" "safe"))
             (hashline-fixture context protocol path original)
             (let ((result (ext:invoke-tool
                            protocol
                            :edit
                            (list :repair policy
                                  :input
                                  (format nil "@@ ~A~%= 1:~A..1:~A~%~~(list #z(1 2))"
                                          (namestring path)
                                          (tools-filesystem:line-hash "(list (a b))")
                                          (tools-filesystem:line-hash "(list (a b))")))
                            context)))
               (is (ext:tool-result-error-p result) "policy ~A did not error" policy)
               (is (search "not valid Common Lisp source" (tool-result-text result)))
               (is (null (first-repair-preview result)) "policy ~A previewed" policy)
               (is (string= original (uiop:read-file-string path))
                   "policy ~A wrote" policy))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-repairable-safe-by-policy :fixture tool-authority)
  "A locally-repairable missing paren errors under reject, previews under preview
even though safe-classified, and auto-writes under safe."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-safe-by-policy" "lisp"))
         (original (format nil "(defun one ()~%  (+ 1 2))~%"))
         (expected (format nil "(defun one ()~%  (* 3 4))~%")))
    (flet ((apply-policy (policy)
             (hashline-fixture context protocol path original)
             (ext:invoke-tool
              protocol
              :edit
              (list :repair policy
                    :input
                    (format nil "@@ ~A~%= 1:~A..2:~A~%~~(defun one ()~%~~  (* 3 4"
                            (namestring path)
                            (tools-filesystem:line-hash "(defun one ()")
                            (tools-filesystem:line-hash "  (+ 1 2))")))
              context)))
      (unwind-protect
           (progn
             (install-file-tools context)
             (let ((rej (apply-policy "reject")))
               (is (ext:tool-result-error-p rej))
               (is (search "unbalanced" (tool-result-text rej)))
               (is (string= original (uiop:read-file-string path))))
             (let* ((prev (apply-policy "preview"))
                    (preview (first-repair-preview prev)))
               (is (not (ext:tool-result-error-p prev)))
               (is (search "No files were written" (tool-result-text prev)))
               (is (getf (getf preview :classification) :safe-p))
               (is (string= original (uiop:read-file-string path))))
             (let ((saf (apply-policy "safe")))
               (is (not (ext:tool-result-error-p saf)))
               (is (search "auto-repaired" (tool-result-text saf)))
               (is (string= expected (uiop:read-file-string path)))))
        (ignore-errors (delete-file path))))))

(test (hashline-edit-repairable-unsafe-by-policy :fixture tool-authority)
  "An unsafe repair errors under reject and previews under both preview and safe,
never auto-writing."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-unsafe-by-policy" "lisp"))
         (original (two-defun-content)))
    (flet ((apply-policy (policy)
             (hashline-fixture context protocol path original)
             (ext:invoke-tool
              protocol
              :edit
              (list :repair policy
                    :input
                    (format nil "@@ ~A~%= 3:~A..3:~A~%~~((defun b ()"
                            (namestring path)
                            (tools-filesystem:line-hash "(defun b ()")
                            (tools-filesystem:line-hash "(defun b ()")))
              context)))
      (unwind-protect
           (progn
             (install-file-tools context)
             (let ((rej (apply-policy "reject")))
               (is (ext:tool-result-error-p rej))
               (is (search "unbalanced" (tool-result-text rej)))
               (is (string= original (uiop:read-file-string path))))
             (dolist (policy '("preview" "safe"))
               (let* ((res (apply-policy policy))
                      (preview (first-repair-preview res)))
                 (is (not (ext:tool-result-error-p res)) "policy ~A errored" policy)
                 (is (search "No files were written" (tool-result-text res)))
                 (is (null (getf (getf preview :classification) :safe-p))
                     "policy ~A classified safe" policy)
                 (is (string= original (uiop:read-file-string path))
                     "policy ~A wrote" policy))))
        (ignore-errors (delete-file path))))))

(test (hashline-edit-accept-revalidates-cst :fixture tool-authority)
  "accept_repair refuses a candidate whose repaired content is no longer valid
Common Lisp source, leaves disk untouched, and keeps the candidate."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-typed-path "hashline-accept-revalidate" "lisp"))
         (original (format nil "(list (a b))~%"))
         (repaired (format nil "(list #z(1 2))~%")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let ((candidate
                   (list :id "revalidate-test"
                         :path (tools-filesystem::canonical-file-namestring
                                (namestring path))
                         :display-path (namestring path)
                         :base-hash (tools-filesystem::file-content-sha256
                                     (uiop:read-file-string path))
                         :repaired repaired
                         :repaired-hash (tools-filesystem::file-content-sha256 repaired)
                         :timestamp (get-universal-time))))
             (tools-filesystem::store-repair-candidate protocol candidate)
             (let ((result (ext:invoke-tool
                            protocol
                            :edit
                            (list :accept_repair "revalidate-test"
                                  :path (namestring path))
                            context)))
               (is (ext:tool-result-error-p result))
               (is (search "valid Common Lisp source" (tool-result-text result)))
               (is (string= original (uiop:read-file-string path)))
               (is (gethash "revalidate-test"
                            (tools-filesystem::repair-candidates protocol))))))
      (ignore-errors (delete-file path)))))

(test (hashline-edit-multi-file-preview-is-atomic :fixture tool-authority)
  "If any file needs an unsafe preview, no file in the multi-file patch is written."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path-a (tool-test-path "hashline-preview-atomic-a.txt"))
         (path-b (tool-test-typed-path "hashline-preview-atomic-b" "lisp"))
         (content-a (format nil "alpha~%"))
         (content-b (two-defun-content)))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path-a content-a)
           (hashline-fixture context protocol path-b content-b)
           (let ((result (ext:invoke-tool
                          protocol
                          :edit
                          (list :input
                                (format nil "@@ ~A~%= 1:~A..1:~A~%~~ALPHA~%@@ ~A~%= 3:~A..3:~A~%~~((defun b ()"
                                        (namestring path-a)
                                        (tools-filesystem:line-hash "alpha")
                                        (tools-filesystem:line-hash "alpha")
                                        (namestring path-b)
                                        (tools-filesystem:line-hash "(defun b ()")
                                        (tools-filesystem:line-hash "(defun b ()")))
                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (search "No files were written" (tool-result-text result)))
             (is (string= content-a (uiop:read-file-string path-a)))
             (is (string= content-b (uiop:read-file-string path-b)))))
      (ignore-errors (delete-file path-a))
      (ignore-errors (delete-file path-b)))))

(defun tool-test-directory (name)
  (make-pathname :directory (list :absolute "tmp"
                                  (format nil "kli-tool-test-~D-~A"
                                          #+sbcl (sb-posix:getpid)
                                          #-sbcl 0
                                          name))))

(defun tool-test-file (dir name content)
  (let ((path (merge-pathnames name dir)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string content s))
    path))

(test (find-tool-globs-paths :fixture tool-authority)
  "find lists glob matches one path per line, sorted, with no content load. ** descends subdirectories including zero levels."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "find-glob")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*find-tool-extension-manifest*)
           (dolist (name '("a.lisp" "b.lisp" "c.txt" "sub/d.lisp"))
             (tool-test-file dir name "x"))
           (let* ((result (ext:invoke-tool protocol
                                           :find
                                           (list :pattern
                                                 (format nil "~A*.lisp"
                                                         (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (lines (text:split-lines text)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 2 (getf (ext:tool-result-details result) :count)))
             (is (= 2 (length lines)))
             (is (search "a.lisp" (first lines)))
             (is (search "b.lisp" (second lines)))
             (is (not (search "c.txt" text))))
           (let ((result (ext:invoke-tool protocol
                                          :find
                                          (list :pattern
                                                (format nil "~A**/*.lisp"
                                                        (namestring dir)))
                                          context)))
             (is (= 3 (getf (ext:tool-result-details result) :count)))
             (is (search "d.lisp" (tool-result-text result))))
           (let ((result (ext:invoke-tool protocol
                                          :find
                                          (list :pattern
                                                (format nil "~A*.nope"
                                                        (namestring dir)))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= "(no matches)" (tool-result-text result)))
             (is (= 0 (getf (ext:tool-result-details result) :count)))))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (find-tool-caps-reported-paths :fixture tool-authority)
  "A glob with more matches than *find-result-limit* reports the first limit
   paths sorted, counts the overflow honestly in the text, and flags the
   truncation in details. The dropped paths are retained behind a handle."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "find-cap")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*find-tool-extension-manifest*
                               spill:*output-spill-extension-manifest*)
           (dolist (name '("a.lisp" "b.lisp" "c.lisp" "d.lisp"))
             (tool-test-file dir name "x"))
           (let* ((tools-filesystem::*find-result-limit* 2)
                  (result (ext:invoke-tool protocol
                                           :find
                                           (list :pattern
                                                 (format nil "~A*.lisp"
                                                         (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (lines (text:split-lines text))
                  (details (ext:tool-result-details result))
                  (handle (getf details :result-handle)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 4 (length lines)) "two paths, the overflow line, and the handle marker")
             (is (search "a.lisp" (first lines)))
             (is (search "b.lisp" (second lines)))
             (is (string= "(and 2 more)" (third lines)))
             (is (search "read-result" (fourth lines)))
             (is (not (search "c.lisp" text)))
             (is (= 4 (getf details :count)) "count stays the full match count")
             (is (eq t (getf details :truncated)))
             (is (stringp handle) "the full path list is retained")
             (let ((page (ext:invoke-tool protocol :read-result
                                          (list :handle handle :start 2 :limit 4)
                                          context)))
               (is (search "c.lisp" (tool-result-text page))
                   "read-result reaches dropped paths"))
             (let ((hit (ext:invoke-tool protocol :search-result
                                         (list :handle handle :pattern "d\\.lisp")
                                         context)))
               (is (search "d.lisp" (tool-result-text hit))
                   "search-result reaches dropped paths")))
           (let ((result (ext:invoke-tool protocol
                                          :find
                                          (list :pattern
                                                (format nil "~Aa.lisp"
                                                        (namestring dir)))
                                          context)))
             (is (null (getf (ext:tool-result-details result) :truncated))
                 "an uncapped listing carries no truncation flag")))
      (ignore-errors (sb-ext:delete-directory dir :recursive t))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test (search-tool-anchors-matches-and-arms-cache :fixture tool-authority)
  "Match lines carry *LINE:HH| anchors and a matching file arms its whole anchor vector, so a search hit is directly editable."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-anchors")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%beta~%alpha beta~%gamma~%"))
                            context)
           (let* ((result (ext:invoke-tool protocol
                                           :search
                                           (list :pattern "alpha"
                                                 :path (namestring path))
                                           context))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "~A~%*1:~A|alpha~%*3:~A|alpha beta"
                                  (namestring (truename path))
                                  (tools-filesystem:line-hash "alpha")
                                  (tools-filesystem:line-hash "alpha beta"))
                          (tool-result-text result)))
             (is (= 2 (getf details :matches)))
             (is (= 1 (getf details :file-count))))
           (is (eq :valid
                   (tools-filesystem:anchor-known-p
                    protocol path 2 (tools-filesystem:line-hash "beta")))
               "the whole matched file arms the cache, not only match lines"))
      (ignore-errors (delete-file path)))))

(test (search-tool-context-lines-space-prefixed :fixture tool-authority)
  "Context lines around a match are anchored and space-prefixed. The regex is anchored matching, not substring search."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-context")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%beta~%alpha beta~%gamma~%"))
                            context)
           (let ((result (ext:invoke-tool protocol
                                          :search
                                          (list :pattern "^beta"
                                                :path (namestring path)
                                                :context 1)
                                          context)))
             (is (string= (format nil "~A~% 1:~A|alpha~%*2:~A|beta~% 3:~A|alpha beta"
                                  (namestring (truename path))
                                  (tools-filesystem:line-hash "alpha")
                                  (tools-filesystem:line-hash "beta")
                                  (tools-filesystem:line-hash "alpha beta"))
                          (tool-result-text result)))))
      (ignore-errors (delete-file path)))))

(test (search-tool-glob-spans-files-and-reports-no-matches :fixture tool-authority)
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "search-glob")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*search-tool-extension-manifest*)
           (tool-test-file dir "one.txt" (format nil "needle here~%plain~%"))
           (tool-test-file dir "two.txt" (format nil "nothing~%"))
           (tool-test-file dir "three.txt" (format nil "needle again~%"))
           (let* ((result (ext:invoke-tool protocol
                                           :search
                                           (list :pattern "needle"
                                                 :path (format nil "~A*.txt"
                                                               (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 2 (getf details :file-count)))
             (is (= 2 (getf details :matches)))
             (is (search "one.txt" text))
             (is (search "three.txt" text))
             (is (not (search "two.txt" text)))
             (is (search (format nil "*1:~A|needle here"
                                 (tools-filesystem:line-hash "needle here"))
                         text)))
           (let ((result (ext:invoke-tool protocol
                                          :search
                                          (list :pattern "absent"
                                                :path (format nil "~A*.txt"
                                                              (namestring dir)))
                                          context)))
             (is (string= "(no matches)" (tool-result-text result)))
             (is (= 0 (getf (ext:tool-result-details result) :matches)))
             (is (= 0 (getf (ext:tool-result-details result) :file-count)))))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (search-tool-streams-past-file-byte-limit :fixture tool-authority)
  "Search streams files in bounded batches, so a match in a file above
*file-byte-limit* is found instead of being silently skipped."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "search-oversized")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (install-extensions context
                               tools-filesystem:*search-tool-extension-manifest*)
           (tool-test-file dir "small.txt" (format nil "needle~%"))
           (tool-test-file dir "huge.txt"
                           (format nil "prefix~%~A~%needle in huge~%"
                                   (make-string 128 :initial-element #\x)))
           (let ((tools-filesystem:*file-byte-limit* 64))
             (let* ((result (ext:invoke-tool protocol
                                             :search
                                             (list :pattern "needle"
                                                   :path (format nil "~A*.txt"
                                                                 (namestring dir)))
                                             context))
                    (text (tool-result-text result))
                    (details (ext:tool-result-details result)))
               (is (not (ext:tool-result-error-p result)))
               (is (= 2 (getf details :file-count)))
               (is (= 2 (getf details :matches)))
               (is (zerop (getf details :skipped)))
               (is (search "small.txt" text))
               (is (search "huge.txt" text)
                   "the formerly oversized file is searched")
               (is (search "needle in huge" text))
               (let ((edited (ext:invoke-tool
                              protocol :edit
                              (list :input
                                    (format nil "@@ ~A~%= 3:~A..3:~A~%~~FOUND"
                                            (namestring (merge-pathnames "huge.txt" dir))
                                            (tools-filesystem:line-hash "needle in huge")
                                            (tools-filesystem:line-hash "needle in huge")))
                              context)))
                 (is (not (ext:tool-result-error-p edited))
                     "the search result still arms anchors for edit")))))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (find-tool-matches-partial-wildcard-directories :fixture tool-authority)
  "A directory component with an embedded * matches by glob, so su*/a.lisp finds sub1/a.lisp and not other/a.lisp."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "find-partial-wild")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*find-tool-extension-manifest*)
           (tool-test-file dir "sub1/a.lisp" "x")
           (tool-test-file dir "other/a.lisp" "x")
           (let* ((result (ext:invoke-tool protocol
                                           :find
                                           (list :pattern
                                                 (format nil "~Asu*/a.lisp"
                                                         (namestring dir)))
                                           context))
                  (text (tool-result-text result)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 1 (getf (ext:tool-result-details result) :count)))
             (is (search "sub1" text))
             (is (not (search "other" text)))))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (find-tool-walk-stops-at-entry-limit :fixture tool-authority)
  "A glob walk over more filesystem entries than *walk-entry-limit* stops early with the partial listing, a visible stop notice, and the truncation flagged, instead of walking a store-scale tree unboundedly."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "find-entry-budget")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*find-tool-extension-manifest*)
           (dolist (name '("a.lisp" "b.lisp" "c.lisp" "d.lisp"))
             (tool-test-file dir name "x"))
           (let* ((tools-filesystem::*walk-entry-limit* 2)
                  (result (ext:invoke-tool protocol
                                           :find
                                           (list :pattern
                                                 (format nil "~A**/*.lisp"
                                                         (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 2 (getf details :count)))
             (is (eq t (getf details :truncated)))
             (is (search "a.lisp" text))
             (is (search "b.lisp" text))
             (is (not (search "c.lisp" text)))
             (is (search "(walk stopped at the 2-entry limit" text)
                 "the stop is surfaced in the result, not silent")))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (find-tool-abort-stops-the-walk :fixture tool-authority)
  "An abort request stops a glob walk at the next entry, so Esc cuts even a
bounded walk short instead of letting it run to its entry or deadline cap."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "find-abort")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*find-tool-extension-manifest*)
           (dolist (name '("a.lisp" "b.lisp"))
             (tool-test-file dir name "x"))
           (let* ((ext:*tool-abort-predicate* (lambda () t))
                  (result (ext:invoke-tool protocol
                                           :find
                                           (list :pattern
                                                 (format nil "~A**/*.lisp"
                                                         (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 0 (getf details :count)))
             (is (eq t (getf details :truncated)))
             (is (search "(walk stopped -- abort was requested" text)
                 "the stop is surfaced in the result, not silent")))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (search-tool-walk-stops-at-entry-limit-and-deadline :fixture tool-authority)
  "A search glob walk stops at *walk-entry-limit* entries or at the *walk-deadline-seconds* wall clock, surfacing the stop both times."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "search-entry-budget")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*search-tool-extension-manifest*)
           (dolist (name '("a.txt" "b.txt" "c.txt"))
             (tool-test-file dir name (format nil "needle~%")))
           (let* ((tools-filesystem::*walk-entry-limit* 1)
                  (result (ext:invoke-tool protocol
                                           :search
                                           (list :pattern "needle"
                                                 :path (format nil "~A*.txt"
                                                               (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 1 (getf details :file-count)))
             (is (eq t (getf details :truncated)))
             (is (search "(walk stopped at the 1-entry limit" text)))
           (let* ((tools-filesystem::*walk-deadline-seconds* 0)
                  (result (ext:invoke-tool protocol
                                           :search
                                           (list :pattern "needle"
                                                 :path (format nil "~A*.txt"
                                                               (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (eq t (getf details :truncated)))
             (is (search "(walk stopped after 0 seconds" text))))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (search-tool-caps-matching-files :fixture tool-authority)
  "A search rendering *search-file-limit* matching files stops there with a visible notice instead of reading every further match whole."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "search-file-cap")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*search-tool-extension-manifest*)
           (dolist (name '("a.txt" "b.txt" "c.txt"))
             (tool-test-file dir name (format nil "needle~%")))
           (let* ((tools-filesystem::*search-file-limit* 1)
                  (result (ext:invoke-tool protocol
                                           :search
                                           (list :pattern "needle"
                                                 :path (format nil "~A*.txt"
                                                               (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 1 (getf details :file-count)))
             (is (eq t (getf details :truncated)))
             (is (search "a.txt" text))
             (is (not (search "b.txt" text)))
             (is (search "(stopped at the 1 matching-file limit" text))))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (search-tool-caps-rendered-matches :fixture tool-authority)
  "Match lines beyond *search-match-limit* are not rendered inline, but the full
search block is retained behind a handle."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-match-cap")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*
                               spill:*output-spill-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "needle one~%needle two~%needle three~%"))
                            context)
           (let* ((tools-filesystem::*search-match-limit* 2)
                  (result (ext:invoke-tool protocol
                                           :search
                                           (list :pattern "needle"
                                                 :path (namestring path))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result))
                  (handle (getf details :result-handle)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 3 (getf details :matches))
                 "the found count stays the full count")
             (is (eq t (getf details :truncated)))
             (is (search "needle one" text))
             (is (search "needle two" text))
             (is (not (search "needle three" text)))
             (is (search "(stopped at the 2 rendered-match limit" text))
             (is (search "read-result" text) "the marker names the retrieval tool")
             (is (stringp handle) "the full rendered search block is retained")
             (let ((hit (ext:invoke-tool protocol :search-result
                                         (list :handle handle :pattern "needle three")
                                         context)))
               (is (search "needle three" (tool-result-text hit))
                   "search-result reaches the match dropped from the inline window"))))
      (ignore-errors (delete-file path))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test (search-tool-completes-across-symlink-cycle :fixture tool-authority)
  "A symlinked directory is never descended, so a link cycle cannot loop the walk, while a symlink to a matching file still matches."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (tool-test-directory "search-symlink-cycle")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*search-tool-extension-manifest*)
           (tool-test-file dir "sub/x.txt" (format nil "needle~%"))
           (sb-posix:symlink (namestring dir)
                             (namestring (merge-pathnames "loop" dir)))
           (sb-posix:symlink (namestring (merge-pathnames "sub/x.txt" dir))
                             (namestring (merge-pathnames "y.txt" dir)))
           (let* ((result (ext:invoke-tool protocol
                                           :search
                                           (list :pattern "needle"
                                                 :path (format nil "~A**/*.txt"
                                                               (namestring dir)))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (ext:tool-result-error-p result)))
             (is (= 2 (getf details :file-count))
                 "the real file and the file symlink, the cycle adds nothing")
             (is (null (getf details :truncated))
                 "the walk completes, it does not hit a budget")
             (is (search "x.txt" text))
             (is (search "y.txt" text))))
      (ignore-errors (sb-ext:delete-directory dir :recursive t)))))

(test (read-tool-pages-past-file-byte-limit :fixture tool-authority)
  "Reading a file over *file-byte-limit* pages and anchors it instead of failing
or allocating the whole content."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "read-oversized")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (with-open-file (s path :direction :output :if-exists :supersede)
             (format s "first~%~A~%last~%" (make-string 256 :initial-element #\x)))
           (let ((tools-filesystem:*file-byte-limit* 64)
                 (tools-filesystem:*read-line-limit* 2))
             (let* ((result (ext:invoke-tool protocol
                                             :read
                                             (list :path (namestring path))
                                             context))
                    (text (tool-result-text result))
                    (details (ext:tool-result-details result)))
               (is (not (ext:tool-result-error-p result)))
               (is (= 3 (getf details :lines)))
               (is (eq t (getf details :truncated)))
               (is (search "1:" text))
               (is (search "2:" text))
               (is (not (search "3:" text)))
               (is (search "showing lines 1..2 of 3" text))
               (let ((page (ext:invoke-tool protocol
                                            :read
                                            (list :path (namestring path)
                                                  :start 3)
                                            context)))
                 (is (search "3:" (tool-result-text page))))
               (let ((edited (ext:invoke-tool
                              protocol :edit
                              (list :input
                                    (format nil "@@ ~A~%= 1:~A..1:~A~%~~FIRST"
                                            (namestring path)
                                            (tools-filesystem:line-hash "first")
                                            (tools-filesystem:line-hash "first")))
                              context)))
                 (is (not (ext:tool-result-error-p edited))
                     "read still arms anchors for edit")))))
      (ignore-errors (delete-file path)))))

(test (write-tool-skips-oversized-old-content :fixture tool-authority)
  "Overwriting a file too large to diff still writes. The pre-supersede capture is dropped rather than read whole."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "write-oversized")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*)
           (with-open-file (s path :direction :output :if-exists :supersede)
             (write-string (make-string 256 :initial-element #\x) s))
           (let ((tools-filesystem:*file-byte-limit* 64))
             (let* ((result (ext:invoke-tool protocol
                                             :write
                                             (list :path (namestring path)
                                                   :content "beta")
                                             context))
                    (details (ext:tool-result-details result)))
               (is (not (ext:tool-result-error-p result)))
               (is (null (getf details :old)))
               (is (null (getf details :new)))
               (is (null (getf details :old-sha256)))
               (is (stringp (getf details :new-sha256)))
               (assert-private-diff-presentation result (namestring path)
                                                 :require-hunks nil)
               (is (search "re-read"
                           (getf (first (getf (ext:tool-result-presentation result)
                                              :updates))
                                 :notice)))))
           (with-open-file (s path)
             (is (string= "beta" (read-line s)))))
      (ignore-errors (delete-file path)))))

(test (search-then-edit-needs-no-read :fixture tool-authority)
  "An edit consumes an anchor straight from a search result with no intervening read."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-edit")))
    (unwind-protect
         (progn
           (install-file-tools context)
           (install-extensions context
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol
                            :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%beta~%gamma~%"))
                            context)
           (let ((found (ext:invoke-tool protocol
                                         :search
                                         (list :pattern "beta"
                                               :path (namestring path))
                                         context)))
             (is (search (format nil "*2:~A|beta"
                                 (tools-filesystem:line-hash "beta"))
                         (tool-result-text found))))
           (let ((edited (ext:invoke-tool protocol
                                          :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 2:~A..2:~A~%~~BETA"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "beta")
                                                        (tools-filesystem:line-hash "beta")))
                                          context)))
             (is (not (ext:tool-result-error-p edited))))
           (let ((readback (ext:invoke-tool protocol
                                            :read
                                            (list :path (namestring path)
                                                  :raw t)
                                            context)))
             (is (string= (format nil "alpha~%BETA~%gamma")
                          (tool-result-text readback)))))
      (ignore-errors (delete-file path)))))

;;; Argument-aware coordinates on the most dangerous first-party tools. Each is
;;; installed for real, then driven through the production gate (mediate-tool-call,
;;; which invoke-tool runs before any runner) under an explicitly bounded subject,
;;; so a confined grant actually bites on filesystem and process authority while a
;;; coarse grant keeps working.

(defun install-and-find-tool (context manifest name)
  (let ((protocol (switch-to-extension-protocol context)))
    (install-extensions context manifest)
    (ext:find-tool protocol name)))

(test write-tool-coordinate-confines-and-canonicalizes-path
  "The write tool carries a path-prefix coordinate on its :path: a confined grant
admits a path under the prefix, denies one outside it, and denies a .. that would
string-match the prefix yet resolve outside it once normalized."
  (let* ((context (kli:make-kernel-host))
         (tool (install-and-find-tool
                context tools-filesystem:*write-tool-extension-manifest* :write))
         (under-tmp (ext:make-subject
                     :grant (ext:make-grant
                             :atoms (list (cons :file/write
                                                (ext:path-prefix-constraint "/tmp/")))))))
    (is (delegation-admits-p under-tmp tool '(:path "/tmp/x"))
        "a path under the confined prefix is admitted")
    (is (null (delegation-admits-p under-tmp tool '(:path "/etc/x")))
        "a path outside the prefix is denied")
    (is (null (delegation-admits-p under-tmp tool '(:path "/tmp/../etc/x")))
        "a .. escaping the prefix is denied after normalization")
    (is (delegation-admits-p under-tmp tool '(:path "/tmp/sub/../x"))
        "a .. that stays under the prefix is admitted")))

(test write-tool-coarse-grant-admits-any-path
  (let* ((context (kli:make-kernel-host))
         (tool (install-and-find-tool
                context tools-filesystem:*write-tool-extension-manifest* :write))
         (coarse (ext:make-subject :capabilities '(:file/write))))
    (is (delegation-admits-p coarse tool '(:path "/etc/anywhere"))
        "a coarse grant covers any derived path constraint")))

(test edit-tool-coordinate-confines-each-patched-path
  "The edit tool derives one path-prefix request per file its patch targets, and
accept_repair derives the same request from its required path. A confined grant
admits in-prefix paths, denies out-of-prefix paths before the runner writes, and
denies a multi-file patch when any file falls outside the prefix."
  (let* ((context (kli:make-kernel-host))
         (tool (install-and-find-tool
                context tools-filesystem:*edit-tool-extension-manifest* :edit))
         (under-tmp (ext:make-subject
                     :grant (ext:make-grant
                             :atoms (list (cons :file/edit
                                                (ext:path-prefix-constraint "/tmp/"))))))
         (coarse (ext:make-subject :capabilities '(:file/edit)))
         (in-prefix (list :input (format nil "@@ /tmp/a~%+~%~~hi")))
         (out-of-prefix (list :input (format nil "@@ /etc/b~%+~%~~hi")))
         (mixed (list :input (format nil "@@ /tmp/a~%+~%~~hi~%@@ /etc/b~%+~%~~yo")))
         (accept-in-prefix '(:accept_repair "abc" :path "/tmp/a"))
         (accept-out-of-prefix '(:accept_repair "abc" :path "/etc/b"))
         (accept-missing-path '(:accept_repair "abc")))
    (is (delegation-admits-p under-tmp tool in-prefix)
        "an in-prefix patch is admitted")
    (is (null (delegation-admits-p under-tmp tool out-of-prefix))
        "an out-of-prefix patch is denied at the gate, before any write")
    (is (null (delegation-admits-p under-tmp tool mixed))
        "a multi-file patch with one out-of-prefix file is denied whole")
    (is (delegation-admits-p under-tmp tool accept-in-prefix)
        "an accept_repair call with an in-prefix path is admitted")
    (is (null (delegation-admits-p under-tmp tool accept-out-of-prefix))
        "an accept_repair call with an out-of-prefix path is denied")
    (is (null (delegation-admits-p under-tmp tool accept-missing-path))
        "an accept_repair call without a path is not silently admitted")
    (is (delegation-admits-p coarse tool mixed)
        "a coarse grant admits any patch")
    (is (delegation-admits-p coarse tool accept-out-of-prefix)
        "a coarse grant admits an accept_repair call with a path")))

(test bash-tool-gates-on-binary-process-exec
  "The bash tool gates on a plain :process/exec capability -- exec is a binary
atom, not a per-command enumeration. A subject holding it runs any command; a
subject lacking it is denied."
  (let* ((context (kli:make-kernel-host))
         (tool (install-and-find-tool
                context tools-bash:*bash-tool-extension-manifest* :bash))
         (exec (ext:make-subject :capabilities '(:process/exec)))
         (no-exec (ext:make-subject :capabilities '(:file/read))))
    (is (delegation-admits-p exec tool '(:command "ls"))
        "a process/exec grant admits a command")
    (is (delegation-admits-p exec tool '(:command "rm -rf /"))
        "and admits any other command -- exec is not enumerable")
    (is (null (delegation-admits-p no-exec tool '(:command "ls")))
        "a subject without process/exec is denied")))

(test tool-helpers-resolve-to-the-canonical-ext-definitions
  "The bash and filesystem extensions share kli/ext's required-tool-parameter and
tool-text-result rather than carrying private copies."
  (is (eq 'ext:required-tool-parameter
          (find-symbol "REQUIRED-TOOL-PARAMETER" '#:kli/tools/bash)))
  (is (eq 'ext:tool-text-result
          (find-symbol "TOOL-TEXT-RESULT" '#:kli/tools/bash)))
  (is (eq 'ext:required-tool-parameter
          (find-symbol "REQUIRED-TOOL-PARAMETER" '#:kli/tools/filesystem)))
  (is (eq 'ext:tool-text-result
          (find-symbol "TOOL-TEXT-RESULT" '#:kli/tools/filesystem)))
  (is (string= "x" (ext:required-tool-parameter (list :name "x") :name)))
  (signals error (ext:required-tool-parameter (list) :name))
  (let ((result (ext:tool-text-result "hi" :error-p t)))
    (is (ext:tool-result-error-p result))
    (is (equal (list :type :text :text "hi")
               (first (ext:tool-result-content result))))))

;;; Bash command-line completion.

(defun touch-test-file (path)
  (with-open-file (stream path :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (declare (ignore stream)))
  path)

(defun call-with-bash-completion-fixture (thunk)
  "A throwaway directory tree for completion tests: two files, a dotfile, a
subdirectory, and a name carrying a space, deleted afterwards."
  (let ((root (ensure-directories-exist
               (merge-pathnames "kli-bash-completion-test/"
                                (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "config-lib/" root))
           (ensure-directories-exist (merge-pathnames "my dir/" root))
           (dolist (name '("config.json" "command.lisp" ".hidden" "my file.txt"))
             (touch-test-file (merge-pathnames name root)))
           (funcall thunk root))
      (uiop:delete-directory-tree root :validate (constantly t)
                                       :if-does-not-exist :ignore))))

(defmacro with-bash-completion-fixture ((root-var) &body body)
  `(call-with-bash-completion-fixture (lambda (,root-var) ,@body)))

(defun call-with-bash-programmable-completion-fixture (thunk)
  (with-bash-completion-fixture (root)
    (let ((bash (merge-pathnames "bash" root))
          (saved (uiop:getenv "KLI_BASH_COMPLETION_BASH")))
      (with-open-file (stream bash :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-line "#!/bin/sh" stream)
        (write-line "case \"$6\" in" stream)
        (write-line "  'ls --') printf '%s\\n' --all --almost-all ;;" stream)
        (write-line "  'ls ') printf '%s\\n' --author ;;" stream)
        (write-line "esac" stream))
      (sb-posix:chmod (uiop:native-namestring bash) #o755)
      (unwind-protect
           (progn
             (sb-posix:setenv "KLI_BASH_COMPLETION_BASH"
                              (uiop:native-namestring bash)
                              1)
             (funcall thunk root))
        (if saved
            (sb-posix:setenv "KLI_BASH_COMPLETION_BASH" saved 1)
            (sb-posix:unsetenv "KLI_BASH_COMPLETION_BASH"))))))

(defmacro with-bash-programmable-completion-fixture ((root-var) &body body)
  `(call-with-bash-programmable-completion-fixture
    (lambda (,root-var) ,@body)))

(defun call-with-real-bash-programmable-completion-fixture (thunk)
  (with-bash-completion-fixture (root)
    (let ((completion-file (merge-pathnames "bash_completion" root))
          (bash (tools-bash::program-on-path "bash"))
          (saved-bash (uiop:getenv "KLI_BASH_COMPLETION_BASH"))
          (saved-file (uiop:getenv "KLI_BASH_COMPLETION_FILE")))
      (unless bash
        (error "bash is required for the programmable completion regression test."))
      (with-open-file (stream completion-file :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
        (write-line "_kli_test_ls_completion() {" stream)
        (write-line "  compopt -o nospace" stream)
        (write-line "  COMPREPLY=()" stream)
        (write-line "  for word in --all --almost-all --author; do" stream)
        (write-line "    case \"$word\" in \"$2\"*) COMPREPLY+=(\"$word\") ;; esac" stream)
        (write-line "  done" stream)
        (write-line "}" stream)
        (write-line "complete -F _kli_test_ls_completion ls" stream))
      (unwind-protect
           (progn
             (sb-posix:setenv "KLI_BASH_COMPLETION_BASH" bash 1)
             (sb-posix:setenv "KLI_BASH_COMPLETION_FILE"
                              (uiop:native-namestring completion-file)
                              1)
             (funcall thunk root))
        (if saved-bash
            (sb-posix:setenv "KLI_BASH_COMPLETION_BASH" saved-bash 1)
            (sb-posix:unsetenv "KLI_BASH_COMPLETION_BASH"))
        (if saved-file
            (sb-posix:setenv "KLI_BASH_COMPLETION_FILE" saved-file 1)
            (sb-posix:unsetenv "KLI_BASH_COMPLETION_FILE"))))))

(defmacro with-real-bash-programmable-completion-fixture ((root-var) &body body)
  `(call-with-real-bash-programmable-completion-fixture
    (lambda (,root-var) ,@body)))

(test bash-completion-tokenizer-finds-the-trailing-word
  "The scanner returns the last word's start, its dequoted text, the open quote,
and whether it sits in command position."
  (flet ((token (tail)
           (multiple-value-list (tools-bash::bash-tail-token tail))))
    (is (equal '(3 "foo" nil nil) (token "ls foo"))
        "a plain argument is found unquoted and out of command position")
    (is (equal '(4 "my fi" #\" nil) (token "cat \"my fi"))
        "a double-quoted word reports its quote and dequoted text")
    (is (equal '(4 "my fi" nil nil) (token "cat my\\ fi"))
        "a backslash-escaped space joins the word")
    (is (equal '(4 "qu" #\' nil) (token "cat 'qu"))
        "a single-quoted word reports its quote"))
  (flet ((command-p (tail)
           (nth 3 (multiple-value-list (tools-bash::bash-tail-token tail)))))
    (is (command-p "ls foo;b") "a word after ; is a command word")
    (is (command-p "a | b") "a word after | is a command word")
    (is (command-p "a && b") "a word after && is a command word")
    (is (command-p "git") "the first word is a command word")
    (is (not (command-p "git sta")) "a later word is an argument")
    (is (not (command-p "echo hi > out"))
        "a word after a redirection is an argument, not a command"))
  (is (equal '("ls" "--")
             (tools-bash::bash-completion-words "echo x | ls --"))
      "programmable completion receives the current simple command only"))

(test bash-completion-tokenizer-blank-word-yields-no-completion
  "A cursor resting on whitespace or just past an operator has no word to
complete, so the completer answers with the hint alone."
  (is (equal "" (nth-value 1 (tools-bash::bash-tail-token "ls ")))
      "trailing whitespace leaves a blank word")
  (is (equal "" (nth-value 1 (tools-bash::bash-tail-token "a; ")))
      "an operator then a space leaves a blank word")
  (is (equal '(:hint "<shell command>")
             (tools-bash::bash-command-completer nil "ls "))
      "the completer answers a blank word with the hint and no candidates"))

(test bash-completion-suggests-human-facing-options
  "Before the shell command begins, completion advertises /bash's command-level
options. Once the shell command begins, it shows a hint and leaves path
completion to the editor's explicit Tab path."
  (let ((candidates (getf (tools-bash::bash-command-completer nil "")
                          :candidates)))
    (is (member "--timeout " candidates :test #'string=))
    (is (member "--background " candidates :test #'string=)))
  (let ((candidates (getf (tools-bash::bash-command-completer nil "--back")
                          :candidates)))
    (is (equal '("--background ") candidates)))
  (is (equal '(:hint "<seconds> <shell command>")
             (tools-bash::bash-command-completer nil "--timeout ")))
  (is (equal '(:hint "<shell command>")
             (tools-bash::bash-command-completer nil "-b ")))
  (is (equal '(:hint "<shell command>")
             (tools-bash::bash-command-completer
              nil "--timeout 9 --background run-o")))
  (with-bash-completion-fixture (root)
    (let* ((bin (ensure-directories-exist (merge-pathnames "bin/" root)))
           (runnable (touch-test-file (merge-pathnames "run-option" bin)))
           (saved (uiop:getenv "PATH")))
      (sb-posix:chmod (uiop:native-namestring runnable) #o755)
      (unwind-protect
           (let ((commands:*command-completion-mode* :manual))
             (sb-posix:setenv "PATH" (uiop:native-namestring bin) 1)
             (setf tools-bash::*bash-path-executable-cache* nil)
             (let ((candidates
                     (getf (tools-bash::bash-command-completer
                            nil "--timeout 9 --background run-o")
                           :candidates)))
               (is (member "--timeout 9 --background run-option"
                           candidates :test #'string=))))
        (sb-posix:setenv "PATH" (or saved "") 1)
        (setf tools-bash::*bash-path-executable-cache* nil)))))

(test bash-completion-encoder-requotes-in-the-word-style
  "Completed words are re-encoded in the quote style the user opened: bare words
backslash-escape word-breaking characters, quoted words stay wrapped, and a
directory keeps its quote open so accepting chains another segment."
  (is (equal "my\\ file" (tools-bash::bash-encode-token "my file" nil nil))
      "a bare word escapes the embedded space")
  (is (equal "my\\ dir/" (tools-bash::bash-encode-token "my dir/" nil t))
      "a bare directory escapes the space but keeps the trailing slash")
  (is (equal "a\\$b\\;c" (tools-bash::bash-encode-token "a$b;c" nil nil))
      "bare words escape shell metacharacters")
  (is (equal "*.lisp" (tools-bash::bash-encode-token "*.lisp" nil nil))
      "glob metacharacters survive so an intended expansion is not defeated")
  (is (equal "\"my file\"" (tools-bash::bash-encode-token "my file" #\" nil))
      "a double-quoted file closes its quote")
  (is (equal "\"my dir/" (tools-bash::bash-encode-token "my dir/" #\" t))
      "a double-quoted directory leaves the quote open to chain")
  (is (equal "'it'\\''s'" (tools-bash::bash-encode-token "it's" #\' nil))
      "a single-quoted word escapes an embedded apostrophe the bash way"))

(test bash-completion-completes-paths-with-trailing-slash-and-hidden-rules
  "Path completion lists matching entries under the directory the word points
into, marks directories with a trailing slash, and reveals dotfiles only when
the typed basename is itself dotted."
  (with-bash-completion-fixture (root)
    (let ((matches (tools-bash::bash-path-completions "co" root nil)))
      (is (member "config-lib/" matches :test #'string=)
          "a subdirectory completes with a trailing slash")
      (is (member "config.json" matches :test #'string=)
          "a file completes without one")
      (is (member "command.lisp" matches :test #'string=)))
    (is (not (member ".hidden" (tools-bash::bash-path-completions "" root nil)
                     :test #'string=))
        "a blank basename hides dotfiles")
    (is (member ".hidden" (tools-bash::bash-path-completions "." root nil)
                :test #'string=)
        "a dotted basename reveals them")))

(test bash-completion-requotes-path-candidates
  "Path candidates carry the word's quoting through, whether bare or quoted."
  (with-bash-completion-fixture (root)
    (let ((bare (tools-bash::bash-path-completions "my" root nil))
          (quoted (tools-bash::bash-path-completions "my" root #\")))
      (is (member "my\\ dir/" bare :test #'string=)
          "a bare directory with a space escapes it and keeps the slash")
      (is (member "my\\ file.txt" bare :test #'string=)
          "a bare file with a space escapes it")
      (is (member "\"my dir/" quoted :test #'string=)
          "a double-quoted directory leaves the quote open")
      (is (member "\"my file.txt\"" quoted :test #'string=)
          "a double-quoted file closes the quote"))))

(test bash-shell-completion-replaces-the-whole-tail
  "The shell completer splices each candidate over the entire tail, keeping the
typed prefix verbatim, and offers cwd entries even at command position."
  (with-bash-completion-fixture (root)
    (uiop:with-current-directory (root)
      (let ((candidates (getf (tools-bash::bash-shell-command-completer
                               "ls config")
                              :candidates)))
        (is (member "ls config-lib/" candidates :test #'string=)
            "the prefix `ls ` is preserved and the directory chained")
        (is (member "ls config.json" candidates :test #'string=)))
      (let ((candidates (getf (tools-bash::bash-shell-command-completer
                               "config")
                              :candidates)))
        (is (member "config-lib/" candidates :test #'string=)
            "a command-position word still falls back to cwd entries")))))

(test bash-programmable-completion-completes-command-options
  "Manual /bash completion asks Bash's programmable completion first, so command
argument options such as `ls --` can complete before the path fallback runs."
  (with-bash-programmable-completion-fixture (root)
    (declare (ignore root))
    (let ((candidates (getf (tools-bash::bash-shell-command-completer "ls --")
                            :candidates)))
      (is (member "ls --all" candidates :test #'string=))
      (is (member "ls --almost-all" candidates :test #'string=)))
    (let ((candidates (getf (tools-bash::bash-shell-command-completer "ls ")
                            :candidates)))
      (is (member "ls --author" candidates :test #'string=)))))

(test bash-programmable-completion-sources-the-configured-file
  "The real helper sources KLI_BASH_COMPLETION_FILE and survives completion
functions that call compopt outside Bash's native completion callback."
  (with-real-bash-programmable-completion-fixture (root)
    (declare (ignore root))
    (let ((candidates (getf (tools-bash::bash-shell-command-completer "ls --")
                            :candidates)))
      (is (member "ls --all" candidates :test #'string=))
      (is (member "ls --almost-all" candidates :test #'string=)))))

(test bash-programmable-completion-falls-back-to-help-options
  "When no programmable spec is loaded, the helper still offers long options
from COMMAND --help so `ls --` does not degrade to literal tab insertion."
  (let ((bash (tools-bash::program-on-path "bash"))
        (saved-bash (uiop:getenv "KLI_BASH_COMPLETION_BASH"))
        (saved-file (uiop:getenv "KLI_BASH_COMPLETION_FILE")))
    (unless bash
      (error "bash is required for the long-option completion regression test."))
    (unwind-protect
         (progn
           (sb-posix:setenv "KLI_BASH_COMPLETION_BASH" bash 1)
           (sb-posix:setenv "KLI_BASH_COMPLETION_FILE"
                            "/missing/kli/bash_completion"
                            1)
           (let ((candidates (getf (tools-bash::bash-shell-command-completer
                                    "ls --")
                                   :candidates)))
             (is (member "ls --all" candidates :test #'string=))
             (is (member "ls --almost-all" candidates :test #'string=))))
      (if saved-bash
          (sb-posix:setenv "KLI_BASH_COMPLETION_BASH" saved-bash 1)
          (sb-posix:unsetenv "KLI_BASH_COMPLETION_BASH"))
      (if saved-file
          (sb-posix:setenv "KLI_BASH_COMPLETION_FILE" saved-file 1)
          (sb-posix:unsetenv "KLI_BASH_COMPLETION_FILE")))))

(test bash-generic-help-completion-offers-options-and-subcommands
  "When programmable completion has no spec, manual /bash completion falls back
to generic help output: short options, long options, top-level subcommands, and
nested subcommands are all derived from COMMAND [SUBCOMMAND...] --help."
  (with-bash-completion-fixture (root)
    (let ((command (merge-pathnames "cmdtree" root))
          (saved-path (uiop:getenv "PATH"))
          (saved-file (uiop:getenv "KLI_BASH_COMPLETION_FILE")))
      (with-open-file (stream command :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
        (write-line "#!/bin/sh" stream)
        (write-line "case \"$*\" in" stream)
        (write-line "  '--help'|'-h')" stream)
        (write-line "    cat <<'EOF'" stream)
        (write-line "usage: cmdtree [option...] subcommand" stream)
        (write-line "  -a, --all" stream)
        (write-line "  -v, --verbose" stream)
        (write-line "  cmdtree build - build a target" stream)
        (write-line "  cmdtree store - store commands" stream)
        (write-line "EOF" stream)
        (write-line "    ;;" stream)
        (write-line "  'store --help'|'store -h')" stream)
        (write-line "    cat <<'EOF'" stream)
        (write-line "usage: cmdtree store [option...] subcommand" stream)
        (write-line "  --json" stream)
        (write-line "  cmdtree store verify - verify paths" stream)
        (write-line "  cmdtree store gc - collect garbage" stream)
        (write-line "EOF" stream)
        (write-line "    ;;" stream)
        (write-line "esac" stream))
      (sb-posix:chmod (uiop:native-namestring command) #o755)
      (unwind-protect
           (progn
             (sb-posix:setenv "PATH"
                              (format nil "~A:~A"
                                      (uiop:native-namestring root)
                                      (or saved-path ""))
                              1)
             (sb-posix:setenv "KLI_BASH_COMPLETION_FILE"
                              "/missing/kli/bash_completion"
                              1)
             (setf tools-bash::*bash-path-executable-cache* nil)
             (let ((candidates (getf (tools-bash::bash-shell-command-completer
                                      "cmdtree -")
                                     :candidates)))
               (is (member "cmdtree -a" candidates :test #'string=))
               (is (member "cmdtree --all" candidates :test #'string=)))
             (let ((candidates (getf (tools-bash::bash-shell-command-completer
                                      "cmdtree buil")
                                     :candidates)))
               (is (member "cmdtree build" candidates :test #'string=)))
             (let ((candidates (getf (tools-bash::bash-shell-command-completer
                                      "cmdtree store ver")
                                     :candidates)))
               (is (member "cmdtree store verify" candidates :test #'string=))))
        (sb-posix:setenv "PATH" (or saved-path "") 1)
        (if saved-file
            (sb-posix:setenv "KLI_BASH_COMPLETION_FILE" saved-file 1)
            (sb-posix:unsetenv "KLI_BASH_COMPLETION_FILE"))
        (setf tools-bash::*bash-path-executable-cache* nil)))))

(test bash-generic-help-completion-does-not-treat-prose-as-subcommands
  "Generic help fallback only accepts subcommands from lines that repeat the
current command chain, so ordinary help prose from non-subcommand tools does not
turn into bogus candidates."
  (let ((saved-file (uiop:getenv "KLI_BASH_COMPLETION_FILE")))
    (unwind-protect
         (progn
           (sb-posix:setenv "KLI_BASH_COMPLETION_FILE"
                            "/missing/kli/bash_completion"
                            1)
           (let ((candidates (getf (tools-bash::bash-shell-command-completer
                                    "ls ")
                                   :candidates)))
             (is (not (member "ls home" candidates :test #'string=)))
             (is (not (member "ls invocation" candidates :test #'string=)))))
      (if saved-file
          (sb-posix:setenv "KLI_BASH_COMPLETION_FILE" saved-file 1)
          (sb-posix:unsetenv "KLI_BASH_COMPLETION_FILE")))))

(test bash-completion-path-scan-keeps-only-executables
  "The $PATH scan returns executable files only, dropping plain files, and the
result is cached against the PATH string."
  (with-bash-completion-fixture (root)
    (let* ((bin (ensure-directories-exist (merge-pathnames "bin/" root)))
           (runnable (touch-test-file (merge-pathnames "runme" bin)))
           (plain (touch-test-file (merge-pathnames "data.txt" bin)))
           (saved (uiop:getenv "PATH")))
      (sb-posix:chmod (uiop:native-namestring runnable) #o755)
      (is (tools-bash::bash-executable-file-p runnable)
          "the execute bit reads as executable")
      (is (not (tools-bash::bash-executable-file-p plain))
          "a plain file is not executable")
      (unwind-protect
           (progn
             (sb-posix:setenv "PATH" (uiop:native-namestring bin) 1)
             (setf tools-bash::*bash-path-executable-cache* nil)
             (let ((names (tools-bash::bash-path-executable-names)))
               (is (member "runme" names :test #'string=)
                   "an executable on PATH is offered")
               (is (not (member "data.txt" names :test #'string=))
                   "a non-executable file is withheld")
               (is (equal (uiop:native-namestring bin)
                          (car tools-bash::*bash-path-executable-cache*))
                   "the scan is memoized on the PATH string")))
        (sb-posix:setenv "PATH" (or saved "") 1)
        (setf tools-bash::*bash-path-executable-cache* nil)))))

;;;; Context-pollution bounds: per-line render truncation, the read line-count
;;;; clamp, search windowing/budget/timeout, and the edit replace gate. All
;;;; truncation is render-only -- the LINE:HH anchor always hashes the full disk
;;;; line, which these tests pin from both the read and the edit side.

(test render-truncate-front-marks-the-elision
  "A line over the limit keeps its head and gains a quantified tail marker; a
line within the limit passes through untouched."
  (let ((text:*render-line-limit* 10))
    (is (string= "hello" (text:render-truncate-front "hello")))
    (is (string= "0123456789" (text:render-truncate-front "0123456789"))
        "a line exactly at the limit is not truncated")
    (is (string= "0123456789[+1 chars]"
                 (text:render-truncate-front "0123456789A"))
        "one char over the limit elides exactly one char")
    (is (string= "0123456789[+10 chars]"
                 (text:render-truncate-front "0123456789ABCDEFGHIJ")))))

(test (read-tool-front-truncates-a-wide-line :fixture tool-authority)
  "A wide line renders head + [+N chars] and the LINE:HH anchor still hashes the
full disk line, so the truncation is render-only."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "wide-line"))
         (text:*render-line-limit* 10)
         (line "0123456789ABCDEFGHIJ"))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "~A~%" line))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :read
                                         (list :path (namestring path))
                                         context))))
             (is (string= (format nil "1:~A|0123456789[+10 chars]"
                                  (tools-filesystem:line-hash line))
                          text)
                 "head kept, tail quantified, anchor hashes the full line")
             (is (not (search "ABCDEFGHIJ" text))
                 "the elided tail is absent from the output")))
      (ignore-errors (delete-file path)))))

(test (read-tool-leaves-normal-lines-unmarked :fixture tool-authority)
  "Lines within the limit render whole, with no elision marker."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "normal-lines"))
         (text:*render-line-limit* 10))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "alpha~%beta~%gamma~%"))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :read
                                         (list :path (namestring path))
                                         context))))
             (is (not (search "[+" text)) "no marker on within-limit lines")
             (is (search "gamma" text))))
      (ignore-errors (delete-file path)))))

(test (read-tool-raw-front-truncates-without-anchor :fixture tool-authority)
  "Raw read drops the anchor prefix but still front-truncates a wide line."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "wide-raw"))
         (text:*render-line-limit* 10)
         (line "0123456789ABCDEFGHIJ"))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "~A~%" line))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :read
                                         (list :path (namestring path) :raw t)
                                         context))))
             (is (string= "0123456789[+10 chars]" text)
                 "head + marker, no LINE:HH prefix")))
      (ignore-errors (delete-file path)))))

(test (edit-deletes-a-truncated-line-via-its-full-line-anchor :fixture tool-authority)
  "A line shown truncated still carries the full-line hash as its anchor, so a
delete referencing it validates against disk and applies."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "edit-truncated-delete"))
         (text:*render-line-limit* 10)
         (wide "0123456789ABCDEFGHIJ"))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path
                             (format nil "~A~%keeper~%" wide))
           (let ((result (ext:invoke-tool protocol :edit
                                          (list :input
                                                (format nil "@@ ~A~%- 1:~A..1:~A"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash wide)
                                                        (tools-filesystem:line-hash wide)))
                                          context)))
             (is (not (ext:tool-result-error-p result))
                 "the full-line anchor validates though the line was shown truncated")
             (is (string= (format nil "keeper~%")
                          (uiop:read-file-string path))
                 "the wide line is gone, the rest intact")))
      (ignore-errors (delete-file path)))))

(test (anchor-cache-holds-the-full-hash-for-a-truncated-line :fixture tool-authority)
  "Reading a wide line arms the cache with the full-line hash, not the truncated
render, so the full-line anchor reads back :valid and the head's hash does not."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "anchor-truncated"))
         (text:*render-line-limit* 10)
         (wide "0123456789ABCDEFGHIJ"))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "~A~%" wide))
                            context)
           (ext:invoke-tool protocol :read (list :path (namestring path)) context)
           (is (eq :valid
                   (tools-filesystem:anchor-known-p
                    protocol path 1 (tools-filesystem:line-hash wide)))
               "the full-line hash is :valid")
           (is (eq :stale
                   (tools-filesystem:anchor-known-p
                    protocol path 1 (tools-filesystem:line-hash "0123456789")))
               "the truncated head's hash is not what the cache holds"))
      (ignore-errors (delete-file path)))))

(test (read-tool-clamps-line-count-with-a-notice :fixture tool-authority)
  "A file longer than the line limit renders only the first limit lines, flags
:truncated, and surfaces a paging notice rather than erroring."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "read-clamp"))
         (tools-filesystem:*read-line-limit* 3))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "l1~%l2~%l3~%l4~%l5~%"))
                            context)
           (let* ((result (ext:invoke-tool protocol :read
                                           (list :path (namestring path)) context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (search "l3" text))
             (is (not (search "l4" text)) "lines past the limit are not rendered")
             (is (search "showing lines 1..3 of 5" text))
             (is (getf details :truncated))
             (is (= 3 (getf details :end)))
             (is (= 5 (getf details :lines)))))
      (ignore-errors (delete-file path)))))

(test (read-tool-clamps-an-oversized-explicit-range :fixture tool-authority)
  "An explicit range wider than the limit clamps from its own start."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "read-clamp-range"))
         (tools-filesystem:*read-line-limit* 3))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "l1~%l2~%l3~%l4~%l5~%l6~%"))
                            context)
           (let* ((result (ext:invoke-tool protocol :read
                                           (list :path (namestring path) :start 2)
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (search "showing lines 2..4 of 6" text))
             (is (not (search "l5" text)))
             (is (= 2 (getf details :start)))
             (is (= 4 (getf details :end)))
             (is (getf details :truncated))))
      (ignore-errors (delete-file path)))))

(test (read-tool-within-the-line-limit-has-no-notice :fixture tool-authority)
  "A file within the line limit renders whole, with no notice and no flag."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "read-no-clamp"))
         (tools-filesystem:*read-line-limit* 10))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*read-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "l1~%l2~%l3~%"))
                            context)
           (let* ((result (ext:invoke-tool protocol :read
                                           (list :path (namestring path)) context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (not (search "showing lines" text)))
             (is (not (getf details :truncated)))))
      (ignore-errors (delete-file path)))))

(test (search-windows-a-match-at-the-end-of-a-wide-line :fixture tool-authority)
  "A match near the end of an over-limit line is windowed around the match, not
front-truncated to the head."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-window-end"))
         (text:*render-line-limit* 10)
         (line (concatenate 'string (make-string 30 :initial-element #\.) "NEEDLE")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path) :content (format nil "~A~%" line))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :search
                                         (list :pattern "NEEDLE" :path (namestring path))
                                         context))))
             (is (search "[+26 chars]....NEEDLE" text)
                 "the window keeps the match and elides the head")
             (is (not (search ".........." text))
                 "the head run is not rendered whole")))
      (ignore-errors (delete-file path)))))

(test (search-renders-a-within-limit-match-line-whole :fixture tool-authority)
  "A match on a line within the limit renders the whole line, with no marker
and no multi-match suffix."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-window-short"))
         (text:*render-line-limit* 10))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path) :content (format nil "alpha beta~%"))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :search
                                         (list :pattern "beta" :path (namestring path))
                                         context))))
             (is (search (format nil "*1:~A|alpha beta"
                                 (tools-filesystem:line-hash "alpha beta"))
                         text))
             (is (not (search "[+" text)))
             (is (not (search "more match" text)))))
      (ignore-errors (delete-file path)))))

(test (search-counts-the-matches-a-window-hides :fixture tool-authority)
  "An over-limit line with several matches windows the first and reports how
many more the window cannot show."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-multi"))
         (text:*render-line-limit* 10)
         (line (format nil "X~AX~AX"
                       (make-string 12 :initial-element #\a)
                       (make-string 12 :initial-element #\a))))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path) :content (format nil "~A~%" line))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :search
                                         (list :pattern "X" :path (namestring path))
                                         context))))
             (is (search "(+2 more matches on this line)" text))))
      (ignore-errors (delete-file path)))))

(test (search-front-truncates-an-over-limit-context-line :fixture tool-authority)
  "A context line over the limit is front-truncated; only match lines window."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-context-trunc"))
         (text:*render-line-limit* 10)
         (wide "0123456789ABCDEFGHIJKLMNOPQRST"))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "~A~%MATCH~%" wide))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :search
                                         (list :pattern "MATCH" :path (namestring path)
                                               :context 1)
                                         context))))
             (is (search "0123456789[+20 chars]" text)
                 "the context line front-truncates")
             (is (not (search "ABCDEFGHIJ" text)))))
      (ignore-errors (delete-file path)))))

(test (search-windows-a-match-span-wider-than-the-limit :fixture tool-authority)
  "A single match wider than the limit shows its first window's worth with head
and tail markers, so the truncation of the match itself is visible."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-wide-span"))
         (text:*render-line-limit* 10)
         (line (concatenate 'string "xx" (make-string 25 :initial-element #\a) "yy")))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path) :content (format nil "~A~%" line))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :search
                                         (list :pattern "a+" :path (namestring path))
                                         context))))
             (is (search "[+2 chars]aaaaaaaaaa[+17 chars]" text))
             (is (not (search "more match" text))
                 "one wide match is not a multi-match line")))
      (ignore-errors (delete-file path)))))

(test (search-stops-at-the-output-character-budget :fixture tool-authority)
  "Output that crosses the character backstop stops the search mid-block: the
budget cuts a later match line within the same file, not only between files, so
one dense file cannot overrun the limit. The stop carries a notice and flags
:truncated."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-output-budget"))
         (tools-filesystem:*search-output-limit* 20))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path)
                                  :content (format nil "match-head~%match-tail~%"))
                            context)
           (let* ((result (ext:invoke-tool protocol :search
                                           (list :pattern "match" :path (namestring path))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (search "match-head" text) "the first match line still renders")
             (is (not (search "match-tail" text))
                 "the later match line is cut within the same file block")
             (is (search "output limit" text))
             (is (getf details :truncated))))
      (ignore-errors (delete-file path)))))

(test (search-does-not-count-zero-width-matches :fixture tool-authority)
  "An empty-capable pattern over an over-limit line counts only the matches that
consume characters, so it adds no inflated hidden-match tally."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "search-zero-width"))
         (text:*render-line-limit* 10)
         (line (make-string 20 :initial-element #\b)))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ext:invoke-tool protocol :write
                            (list :path (namestring path) :content (format nil "~A~%" line))
                            context)
           (let ((text (tool-result-text
                        (ext:invoke-tool protocol :search
                                         (list :pattern "a*" :path (namestring path))
                                         context))))
             (is (search "bbbbbbbbbb" text) "the over-limit line is windowed and shown")
             (is (not (search "more matches" text))
                 "zero-width matches add no inflated count")))
      (ignore-errors (delete-file path)))))

(test (search-skips-a-file-whose-scan-times-out :fixture tool-authority)
  "A catastrophically backtracking scan over one file is interrupted and the
file skipped, while other files still return; the skip is counted and flagged."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (dir (format nil "/tmp/kli-tool-test-~D-timeout-dir/" (sb-posix:getpid)))
         (bad (format nil "~Aa-bad.txt" dir))
         (good (format nil "~Ab-good.txt" dir))
         (tools-filesystem:*search-scan-timeout-seconds* 0.3))
    (unwind-protect
         (progn
           (install-extensions context
                               tools-filesystem:*write-tool-extension-manifest*
                               tools-filesystem:*search-tool-extension-manifest*)
           (ensure-directories-exist dir)
           (ext:invoke-tool protocol :write
                            (list :path bad
                                  :content (format nil "~A!~%"
                                                   (make-string 32 :initial-element #\a)))
                            context)
           (ext:invoke-tool protocol :write
                            (list :path good :content (format nil "aaaa~%"))
                            context)
           (let* ((result (ext:invoke-tool protocol :search
                                           (list :pattern "^(a+)+$"
                                                 :path (concatenate 'string dir "*"))
                                           context))
                  (text (tool-result-text result))
                  (details (ext:tool-result-details result)))
             (is (search "b-good.txt" text) "the well-behaved file still returns")
             (is (search "scan timed out on 1 file" text))
             (is (= 1 (getf details :timed-out)))
             (is (getf details :truncated))))
      (ignore-errors (delete-file bad))
      (ignore-errors (delete-file good))
      (ignore-errors (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir)
                                                 :validate (constantly t))))))

(test (edit-rejects-a-replace-over-a-truncated-line :fixture tool-authority)
  "Replacing a line shown truncated would silently discard its unseen tail, so
the edit is rejected before any write and the file is left untouched."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "edit-reject-replace"))
         (text:*render-line-limit* 10)
         (wide "0123456789ABCDEFGHIJ")
         (original (format nil "~A~%keeper~%" wide)))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path original)
           (let ((result (ext:invoke-tool protocol :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 1:~A..1:~A~%~~SHORT"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash wide)
                                                        (tools-filesystem:line-hash wide)))
                                          context)))
             (is (ext:tool-result-error-p result))
             (is (search "Rewrite the file with the write tool"
                         (tool-result-text result)))
             (is (string= original (uiop:read-file-string path))
                 "the file is untouched")))
      (ignore-errors (delete-file path)))))

(test (edit-allows-an-insert-next-to-a-truncated-line :fixture tool-authority)
  "Insert carries its own new payload, so it is not gated even adjacent to an
over-limit line."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "edit-insert-adjacent"))
         (text:*render-line-limit* 10)
         (wide "0123456789ABCDEFGHIJ"))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path (format nil "~A~%keeper~%" wide))
           (let ((result (ext:invoke-tool protocol :edit
                                          (list :input
                                                (format nil "@@ ~A~%< 1:~A~%~~new"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash wide)))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "new~%~A~%keeper~%" wide)
                          (uiop:read-file-string path)))))
      (ignore-errors (delete-file path)))))

(test (edit-allows-a-replace-on-a-within-limit-line :fixture tool-authority)
  "A replace whose range holds only within-limit lines is unaffected by the gate."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context))
         (path (tool-test-path "edit-replace-normal"))
         (text:*render-line-limit* 10))
    (unwind-protect
         (progn
           (install-file-tools context)
           (hashline-fixture context protocol path (format nil "alpha~%beta~%"))
           (let ((result (ext:invoke-tool protocol :edit
                                          (list :input
                                                (format nil "@@ ~A~%= 2:~A..2:~A~%~~BETA2"
                                                        (namestring path)
                                                        (tools-filesystem:line-hash "beta")
                                                        (tools-filesystem:line-hash "beta")))
                                          context)))
             (is (not (ext:tool-result-error-p result)))
             (is (string= (format nil "alpha~%BETA2~%") (uiop:read-file-string path)))))
      (ignore-errors (delete-file path)))))

;;;; Context-pollution bounds: the bash background poll windows an oversized
;;;; backlog tail-weighted and advances to live (heap-safe, never reading the
;;;; whole span), the foreground windowed reader front-truncates each line, and
;;;; an eval value is capped at the stream before its full print reaches the heap.

(test (bash-background-poll-windows-an-oversized-backlog :fixture tool-authority)
  "A poll backlog over the limit returns a tail-weighted window -- a small head,
an elision marker, then the recent tail -- and advances the offset to EOF, so the
next poll resumes at live rather than re-reading the dropped middle."
  (uiop:with-temporary-file (:pathname path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :external-format :utf-8)
      (dotimes (i 300000) (format stream "output line ~D~%" i)))
    (let ((eof (tools-bash::output-file-byte-length path)))
      (multiple-value-bind (text new-offset truncated)
          (tools-bash::read-job-output-window path 0 (* 1024 1024))
        (is (eq t truncated))
        (is (= eof new-offset) "the offset advances to live (EOF)")
        (is (search "output line 0" text) "a small head survives")
        (is (search "advanced to live" text) "the elision marker is present")
        (is (search "line 299999" text) "the recent tail survives")
        (is (<= (length text) (+ (* 1024 1024) 100))
            "the window is bounded by the limit plus the marker")))))

(test (bash-background-poll-passes-a-small-delta-whole :fixture tool-authority)
  "A backlog within the limit decodes whole with no truncation, and the offset
still advances to EOF."
  (uiop:with-temporary-file (:pathname path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :external-format :utf-8)
      (write-string "just a little output" stream))
    (let ((eof (tools-bash::output-file-byte-length path)))
      (multiple-value-bind (text new-offset truncated)
          (tools-bash::read-job-output-window path 0 (* 1024 1024))
        (is (not truncated))
        (is (= eof new-offset))
        (is (string= "just a little output" text))))))

(test (bash-foreground-front-truncates-a-long-line :fixture tool-authority)
  "A single structurally-long line in a within-aggregate capture is front-truncated
per line, so one line cannot flood the context even when the whole capture fits."
  (let ((text:*render-line-limit* 10))
    (uiop:with-temporary-file (:pathname path)
      (with-open-file (stream path :direction :output :if-exists :supersede
                                   :external-format :utf-8)
        (write-string "0123456789ABCDEFGHIJ" stream))
      (multiple-value-bind (text truncated)
          (tools-bash::read-windowed-output-file path (* 1024 1024))
        (is (not truncated) "the line fits the aggregate cap, so the window is untouched")
        (is (string= "0123456789[+10 chars]" text)
            "the long line is front-truncated to the per-line limit")))))

(test eval-value-render-stream-caps-a-huge-value
  "A value whose print exceeds the per-value limit is capped at the stream with a
[+N chars] marker and reported truncated; small values pass through verbatim and
report no truncation."
  (let ((tools-eval:*eval-value-character-limit* 10))
    (multiple-value-bind (lines truncated)
        (tools-eval::printable-values (list (make-string 100 :initial-element #\a)))
      (is (eq t truncated))
      (is (= 1 (length lines)))
      (is (search "[+92 chars]" (first lines))
          "the marker quantifies the dropped tail of the printed value"))
    (multiple-value-bind (lines truncated)
        (tools-eval::printable-values (list 1 2 "ok"))
      (is (not truncated))
      (is (equal '("1" "2" "\"ok\"") lines)))))

;;; Operator capability elevation. The interactive subject withholds the model
;;; actuators (process exec, in-image eval); a user-driven /bash or /eval
;;; elevates locally to confer the one atom it needs, still bounded by the
;;; declared "capabilities" policy. These drive the real runners against gated
;;; stand-in tools so no subprocess or live eval runs.

(defun run-faux-gated-tool (tool parameters context &key call-id on-update)
  (declare (ignore tool parameters context call-id on-update))
  (ext:make-tool-result
   :content (list (ext:make-tool-text-content "ran"))))

(ext:defextension faux-bash-tool
  (:provides
   (tool bash
     :label "Bash"
     :description "Gated stand-in for the bash tool."
     :parameters '(:object (:command :string))
     :runner #'run-faux-gated-tool
     :metadata '(:capabilities (:process/exec)))))

(ext:defextension faux-eval-tool
  (:provides
   (tool eval
     :label "Eval"
     :description "Gated stand-in for the eval tool."
     :parameters '(:object (:form :string))
     :runner #'run-faux-gated-tool
     :metadata '(:capabilities (:image/eval)))))

(defun operator-elevation-context (settings)
  "A kernel host with SETTINGS as global config and the gated stand-in bash and
eval tools installed."
  (let ((context (kli:make-kernel-host))
        (root (temp-config-root)))
    (switch-to-extension-protocol context)
    (install-config-from context root settings)
    (install-extension context *faux-bash-tool-extension-manifest*)
    (install-extension context *faux-eval-tool-extension-manifest*)
    context))

(test operator-command-elevates-under-absent-capabilities
  "With no capabilities policy, /bash and /eval confer their model actuator even
though the ambient interactive subject withholds it; the ambient subject is
untouched once the elevated extent ends."
  (let ((context (operator-elevation-context "{}"))
        (ext:*call-subject* ext:*ui-subject*))
    (is (not (commands:command-result-error-p
              (tools-bash:run-bash-command nil '(:tail "true") context))))
    (is (not (commands:command-result-error-p
              (tools-eval:run-eval-command nil '(:tail "(+ 1 2)") context))))
    (is (not (ext:check-capability ext:*call-subject* :process/exec)))
    (is (not (ext:check-capability ext:*call-subject* :image/eval)))))

(test operator-command-honors-capabilities-lockdown
  "A present capabilities policy that omits the actuator denies the operator
command, so a declared lockdown still attenuates /bash and /eval."
  (let ((context (operator-elevation-context "{\"capabilities\": [\"file/read\"]}"))
        (ext:*call-subject* ext:*ui-subject*))
    (signals ext:capability-denied
      (tools-bash:run-bash-command nil '(:tail "true") context))
    (signals ext:capability-denied
      (tools-eval:run-eval-command nil '(:tail "(+ 1 2)") context))))

(test operator-command-elevation-is-bounded-by-policy
  "When the policy itself names the actuator, the elevated subject confers it --
the elevation meets the policy rather than replacing it."
  (let ((context (operator-elevation-context
                  "{\"capabilities\": [\"process/exec\", \"image/eval\"]}"))
        (ext:*call-subject* ext:*ui-subject*))
    (is (not (commands:command-result-error-p
              (tools-bash:run-bash-command nil '(:tail "true") context))))
    (is (not (commands:command-result-error-p
              (tools-eval:run-eval-command nil '(:tail "(+ 1 2)") context))))))
