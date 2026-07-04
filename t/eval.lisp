(in-package #:kli/tests)

;;; Load semantics for the eval tool: :form is read and evaluated one top-level
;;; form at a time under the live package and readtable, the current package is
;;; sticky across calls, a directive nested inside a larger form is flagged, and
;;; unbalanced input is repaired loudly. Each case drives the real extension
;;; pipeline so the eval-session effect, worker thread, and result builders are
;;; all exercised together.

(test (eval-interleaves-in-package-across-forms :fixture tool-authority)
  "A defpackage / in-package / defun / call sequence in one :form defines into
the new package and the call ends there -- the form that follows an in-package is
read in the package the in-package selected, which read-all-first could not do."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool
                    protocol :eval
                    '(:form "(defpackage #:eval-interleave (:use #:cl))
(in-package #:eval-interleave)
(defun greet () :hi)
(greet)")
                    context))
           (details (ext:tool-result-details result)))
      (is (not (ext:tool-result-error-p result)))
      (is (equal '(":HI") (getf details :values)))
      (is (string= "EVAL-INTERLEAVE" (getf details :current-package))))))

(test (eval-read-depends-on-prior-evaluation :fixture tool-authority)
  "Interleaving is general: a later form whose READ depends on an earlier form's
evaluation -- a #. that references a variable the first form defines -- reads
correctly only because the first form already ran when the second is read."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((result (ext:invoke-tool
                   protocol :eval
                   '(:form "(defparameter cl-user::*eval-read-probe* 41)
#.(1+ cl-user::*eval-read-probe*)")
                   context)))
      (is (not (ext:tool-result-error-p result)))
      (is (equal '("42") (getf (ext:tool-result-details result) :values))))))

(test eval-nested-directive-detection
  "nested-directives flags a package/readtable directive nested at least one
level deep -- matched by symbol-name, package-insensitive -- while pruning quote
and function subtrees, and never flags a directive that is itself the top-level
form."
  (flet ((nd (form) (tools-eval::nested-directives form)))
    (is (equal '("IN-PACKAGE") (nd '(progn (in-package :p) (defstruct s)))))
    (is (equal '("DEFPACKAGE") (nd '(eval-when (:execute) (defpackage :z)))))
    ;; A nested reader-syntax directive misbehaves the same way in-package does.
    (is (equal '("SET-MACRO-CHARACTER")
               (nd '(progn (set-macro-character #\$ #'identity) 1))))
    ;; Any symbol named IN-READTABLE matches by name alone -- an uninterned one
    ;; stands in for named-readtables:in-readtable without that dependency.
    (is (equal '("IN-READTABLE")
               (nd (list 'progn (list (make-symbol "IN-READTABLE") :r) '(frob)))))
    (is (null (nd '(quote (in-package :p)))))
    (is (null (nd (list 'function '(in-package :p)))))
    ;; A backquoted list literal constructs a list and runs nothing, so the
    ;; quasiquote subtree is pruned like quote -- read the reader form as data.
    (is (null (nd (read-from-string "`(in-package :p)"))))
    (is (null (nd '(in-package :p))))
    ;; A binding-list head is a bound name, not a call -- not a directive.
    (is (null (nd '(let ((in-package 5)) in-package))))
    (is (null (nd '(flet ((in-package (x) x)) (list 1)))))
    ;; A real directive in a binding init or body still flags.
    (is (equal '("IN-PACKAGE") (nd '(let ((x 1)) (in-package :foo) x))))
    (is (equal '("IN-PACKAGE") (nd '(let ((x (in-package :foo))) x))))))

(test (eval-nested-directive-warning-rides-result :fixture tool-authority)
  "A nested directive's caution rides both the result text and :details
:warnings; a clean top-level in-package carries none."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((warned (ext:invoke-tool
                   protocol :eval
                   '(:form "(progn (in-package #:cl-user) (+ 1 1))") context))
          (clean (ext:invoke-tool
                  protocol :eval '(:form "(in-package #:cl-user)") context)))
      (is (= 1 (length (getf (ext:tool-result-details warned) :warnings))))
      (is (search "Caution"
                  (getf (first (ext:tool-result-content warned)) :text)))
      (is (null (getf (ext:tool-result-details clean) :warnings))))))

(test (eval-interactive-rides-nested-directive-warning :fixture tool-authority)
  "Interactive eval surfaces the same nested-directive caution the return path
does -- in the result text and :details :warnings."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((result (ext:invoke-tool
                   protocol :eval
                   '(:form "(progn (in-package #:cl-user) (+ 1 1))"
                     :on-error "interactive")
                   context)))
      (is (not (ext:tool-result-error-p result)))
      (is (= 1 (length (getf (ext:tool-result-details result) :warnings))))
      (is (search "Caution" (tool-result-text result))))))

(test (eval-paren-repair-is-loud :fixture tool-authority)
  "Unbalanced input is repaired before reading, the repair is announced in the
result text and recorded as :repaired-p/:repaired-source, and the repaired form
evaluates; balanced input is untouched and carries no repair marks."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((repaired (ext:invoke-tool protocol :eval '(:form "(+ 1 2") context))
           (details (ext:tool-result-details repaired))
           (clean (ext:invoke-tool protocol :eval '(:form "(+ 1 2)") context)))
      (is (not (ext:tool-result-error-p repaired)))
      (is (getf details :repaired-p))
      (is (string= "(+ 1 2)" (getf details :repaired-source)))
      (is (equal '("3") (getf details :values)))
      (is (search "repair"
                  (string-downcase
                   (getf (first (ext:tool-result-content repaired)) :text))))
      (is (null (getf (ext:tool-result-details clean) :repaired-p))))))

(test (eval-package-is-sticky-across-calls :fixture tool-authority)
  "An in-package in one call governs the next: a symbol defined under the new
package resolves unqualified in a later call; a captured eval error still
persists the package its forms reached; an explicit :package overrides the sticky
package for that call."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (ext:invoke-tool protocol :eval
                     '(:form "(defpackage #:eval-sticky (:use #:cl))
(in-package #:eval-sticky)
(defvar *probe* 7)")
                     context)
    (let ((same (ext:invoke-tool protocol :eval
                                 '(:form "(list (boundp '*probe*) *probe*)") context))
          (errored (ext:invoke-tool protocol :eval
                                    '(:form "(defpackage #:eval-sticky-2 (:use #:cl))
(in-package #:eval-sticky-2)
(error \"boom\")")
                                    context)))
      (is (equal '("(T 7)") (getf (ext:tool-result-details same) :values)))
      (is (string= "EVAL-STICKY" (getf (ext:tool-result-details same) :current-package)))
      (is (ext:tool-result-error-p errored))
      (is (string= "EVAL-STICKY-2"
                   (getf (ext:tool-result-details errored) :current-package)))
      (let ((after-error (ext:invoke-tool protocol :eval
                                          '(:form "(package-name *package*)") context))
            (override (ext:invoke-tool protocol :eval
                                       '(:form "(package-name *package*)"
                                         :package "CL-USER")
                                       context)))
        (is (equal '("\"EVAL-STICKY-2\"")
                   (getf (ext:tool-result-details after-error) :values)))
        (is (string= "COMMON-LISP-USER"
                     (getf (ext:tool-result-details override) :current-package)))))))

(test (eval-diagnostics-current-package-delta-and-forms :fixture tool-authority)
  "Every result carries :current-package; the text shows a Package: X -> Y line
only when the call moved the package; the error path reports :forms, the count of
forms that completed before the signal."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let ((unchanged (ext:invoke-tool protocol :eval '(:form "(+ 40 2)") context))
          (errored (ext:invoke-tool protocol :eval
                                    '(:form "(+ 1 1) (+ 2 2) (error \"boom\")") context)))
      (is (string= "COMMON-LISP-USER"
                   (getf (ext:tool-result-details unchanged) :current-package)))
      (is (not (search "Package:"
                       (getf (first (ext:tool-result-content unchanged)) :text))))
      (is (string= "COMMON-LISP-USER"
                   (getf (ext:tool-result-details errored) :current-package)))
      (is (= 2 (getf (ext:tool-result-details errored) :forms)))
      (is (search "earlier form"
                  (getf (first (ext:tool-result-content errored)) :text))))
    (let ((moved (ext:invoke-tool protocol :eval
                                  '(:form "(in-package #:keyword) 1") context)))
      (is (search "Package: COMMON-LISP-USER -> KEYWORD"
                  (getf (first (ext:tool-result-content moved)) :text))))))

(test eval-current-package-block-additive-is-idempotent
  "The current-package block is a pure :append layer. Recomposing the stack over an
immutable base yields exactly one block reflecting the live package -- idempotent,
never accreting blocks, and swapping the package without leaving a stale name."
  (let* ((package "FOO")
         (base "BASE SYSTEM PROMPT.")
         (stack (ext:install-layer
                 (ext:make-layer-stack) :eval
                 (lambda () (tools-eval::eval-package-block package))
                 :kind :append))
         (foo (ext:compose-layers stack base))
         (foo-again (ext:compose-layers stack base)))
    (is (search "Current eval package: FOO" foo))
    (is (search base foo))
    (is (string= foo foo-again)
        "recomposing from the immutable base is a fixpoint")
    (is (= 1 (count-substring (tools-eval::eval-package-block-open) foo))
        "exactly one current-package block")
    (setf package "BAR")
    (let ((bar (ext:compose-layers stack base)))
      (is (search "Current eval package: BAR" bar))
      (is (not (search "FOO" bar))
          "the block reflects the live package with no stale name")
      (is (= 1 (count-substring (tools-eval::eval-package-block-open) bar))
          "still one block after the package changed")
      (is (search base bar)))))

(test (eval-recompile-rerun-shares-sticky-and-repair :fixture tool-authority)
  "recompile-rerun shares the eval tool's sticky package and paren-repair: after
an eval enters a package, an unbalanced :definition with no :package is repaired
and redefines into that package, and the re-run sees the redefinition."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*
                        tools-eval:*recompile-rerun-tool-extension-manifest*)
    (ext:invoke-tool protocol :eval
                     '(:form "(defpackage #:eval-rr (:use #:cl))
(in-package #:eval-rr)")
                     context)
    (let* ((result (ext:invoke-tool
                    protocol :recompile-rerun
                    '(:definition "(defun probe (x) (* x x)"
                      :form "(probe 6)")
                    context))
           (details (ext:tool-result-details result)))
      (is (not (ext:tool-result-error-p result)))
      (is (getf details :repaired-p))
      (is (string= "(defun probe (x) (* x x))" (getf details :repaired-definition)))
      (is (search "PROBE" (getf details :redefined-symbol)))
      (is (string= "EVAL-RR" (getf details :current-package)))
      (is (eq :ok (getf (getf details :rerun) :status)))
      (is (equal '("36") (getf (getf details :rerun) :values)))
      (is (fboundp (find-symbol "PROBE" "EVAL-RR"))))))

(test (eval-recompile-rerun-skips-rerun-on-failed-definition :fixture tool-authority)
  "When :definition errors, the re-run is skipped rather than run against stale
state: the summary says so, :details :rerun is :skipped, and :form's side effect
never fires."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*
                        tools-eval:*recompile-rerun-tool-extension-manifest*)
    (makunbound 'cl-user::*e1-rerun-probe*)
    (let* ((result (ext:invoke-tool
                    protocol :recompile-rerun
                    '(:definition "(error \"bad definition\")"
                      :form "(defparameter cl-user::*e1-rerun-probe* :ran)")
                    context))
           (details (ext:tool-result-details result)))
      (is (ext:tool-result-error-p result))
      (is (eq :skipped (getf (getf details :rerun) :status)))
      (is (search "skipped" (string-downcase (tool-result-text result))))
      (is (not (boundp 'cl-user::*e1-rerun-probe*))
          "the re-run never ran, so :form's side effect did not fire"))))

(test (eval-timeout-interrupts-a-runaway-form :fixture tool-authority)
  "A form past the deadline is interrupted rather than wedging the caller: the
result is flagged timed-out and the sticky package is not advanced by the killed
worker."
  (let* ((context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (install-extensions context
                        event:*events-extension-manifest*
                        tools-eval:*eval-tool-extension-manifest*)
    (let* ((result (ext:invoke-tool protocol :eval
                                    '(:form "(loop)" :timeout "1") context))
           (details (ext:tool-result-details result)))
      (is (ext:tool-result-error-p result))
      (is (getf details :timed-out-p))
      (is (string= "COMMON-LISP-USER" (getf details :current-package))))))

(test eval-abort-persists-the-package-its-forms-reached
  "A parked interactive eval that an agent eval-aborts still leaves the session in
the package its forms reached -- the in-package already ran; aborting the erroring
form does not un-run it. (Symmetry with a captured eval-error.) eval-abort is debug
authority, so this test holds both :image/eval and :image/debug."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-abort-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(defpackage #:eval-abort-foo (:use #:cl))
(in-package #:eval-abort-foo)
(error \"park me\")"
                        :on-error "interactive")
                      context))
             (park-id (getf (ext:tool-result-details parked) :park)))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (ext:invoke-tool protocol :eval-abort (list :park park-id) context)
        (let ((after (ext:invoke-tool protocol :eval
                                      '(:form "(package-name *package*)") context)))
          (is (equal '("\"EVAL-ABORT-FOO\"")
                     (getf (ext:tool-result-details after) :values))))))))

(test eval-continue-unknown-restart-re-parks-with-feedback
  "Resuming a parked eval with a restart name no live restart matches re-parks and
tells the model the name was unknown -- distinct from the first park, which carries
no such note. eval-continue is debug authority."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-continue-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(error \"park me\")" :on-error "interactive")
                      context))
             (park-id (getf (ext:tool-result-details parked) :park)))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (is (null (getf (ext:tool-result-details parked) :restart-error)))
        (let ((reparked (ext:invoke-tool
                         protocol :eval-continue
                         (list :park park-id :restart "NO-SUCH-RESTART")
                         context)))
          (is (getf (ext:tool-result-details reparked) :parked-p))
          (is (string= park-id (getf (ext:tool-result-details reparked) :park)))
          (is (search "No active restart named NO-SUCH-RESTART"
                      (getf (ext:tool-result-details reparked) :restart-error)))
          (is (search "No active restart named NO-SUCH-RESTART"
                      (tool-result-text reparked))))
        (ext:invoke-tool protocol :eval-continue
                         (list :park park-id :restart "ABORT-EVAL") context)))))

(test eval-interactive-park-surfaces-pre-error-output
  "A form that prints before it errors parks with the printed output surfaced --
in :details :stdout and the result text -- so the model sees what ran before the
error, the same way the return path does."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-continue-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(progn (princ \"printed-before-error\") (error \"boom\"))"
                        :on-error "interactive")
                      context))
             (details (ext:tool-result-details parked)))
        (is (getf details :parked-p))
        (is (search "printed-before-error" (getf details :stdout)))
        (is (search "printed-before-error" (tool-result-text parked)))
        (ext:invoke-tool protocol :eval-continue
                         (list :park (getf details :park) :restart "ABORT-EVAL")
                         context)))))

(test eval-resume-reuses-original-timeout-budget
  "A parked eval resumed without an explicit :timeout reuses the budget the
original eval set, not the bare default; an explicit :timeout on the resume
overrides it for the resumed run."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-continue-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(progn (cerror \"Skip.\" \"a\") (cerror \"Skip.\" \"b\")
(error \"c\"))"
                        :on-error "interactive"
                        :timeout "120")
                      context))
             (park-id (getf (ext:tool-result-details parked) :park)))
        (is (= 120 (getf (ext:tool-result-details parked) :timeout-seconds)))
        (let ((reused (ext:invoke-tool
                       protocol :eval-continue
                       (list :park park-id :restart "CONTINUE") context)))
          (is (getf (ext:tool-result-details reused) :parked-p))
          (is (= 120 (getf (ext:tool-result-details reused) :timeout-seconds))
              "the resume inherited the original 120s budget, not the default"))
        (let ((overridden (ext:invoke-tool
                           protocol :eval-continue
                           (list :park park-id :restart "CONTINUE" :timeout "90")
                           context)))
          (is (getf (ext:tool-result-details overridden) :parked-p))
          (is (= 90 (getf (ext:tool-result-details overridden) :timeout-seconds))
              "an explicit resume :timeout overrides the inherited budget"))
        (ext:invoke-tool protocol :eval-continue
                         (list :park park-id :restart "ABORT-EVAL") context)))))

(test eval-continue-arg-rejects-trailing-tokens
  "An :arg carrying more than one form re-parks with a diagnostic rather than
silently using only the first form. eval-continue is debug authority."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-continue-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(cerror \"Skip.\" \"park me\")" :on-error "interactive")
                      context))
             (park-id (getf (ext:tool-result-details parked) :park)))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (let ((reparked (ext:invoke-tool
                         protocol :eval-continue
                         (list :park park-id :restart "CONTINUE" :arg "1 2")
                         context)))
          (is (getf (ext:tool-result-details reparked) :parked-p))
          (is (search "single form"
                      (getf (ext:tool-result-details reparked) :restart-error))
              "the trailing-token diagnostic rides :restart-error")
          (is (search "single form" (tool-result-text reparked))
              "and the result text"))
        (ext:invoke-tool protocol :eval-continue
                         (list :park park-id :restart "ABORT-EVAL") context)))))

(test eval-continue-unreadable-arg-re-parks
  "An :arg that cannot be read as a single form -- an incomplete \"(\" -- re-parks
with a diagnostic instead of destroying the park with an internal error, and a
valid resume on the same park still succeeds. eval-continue is debug authority."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-continue-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(cerror \"Skip.\" \"park me\")" :on-error "interactive")
                      context))
             (park-id (getf (ext:tool-result-details parked) :park)))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (let ((reparked (ext:invoke-tool
                         protocol :eval-continue
                         (list :park park-id :restart "CONTINUE" :arg "(")
                         context)))
          (is (getf (ext:tool-result-details reparked) :parked-p)
              "an unreadable :arg re-parks rather than destroying the park")
          (is (string= park-id (getf (ext:tool-result-details reparked) :park))
              "the same park survives")
          (is (search "could not be read"
                      (getf (ext:tool-result-details reparked) :restart-error))
              "the read diagnostic rides :restart-error")
          (is (not (search "ECASE" (tool-result-text reparked)))
              "no internal error leaks to the model"))
        (let ((resumed (ext:invoke-tool
                        protocol :eval-continue
                        (list :park park-id :restart "CONTINUE") context)))
          (is (not (ext:tool-result-error-p resumed))
              "a valid resume on the surviving park completes"))))))

(test eval-park-dead-thread-yields-interrupted-result
  "Defence in depth: a parked worker that dies without announcing completion
resolves to an interrupted (killed) result, not a nil outcome read through the
done path. eval-continue is debug authority."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-continue-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(cerror \"Skip.\" \"park me\")" :on-error "interactive")
                      context))
             (park (tools-eval::lookup-eval-park
                    context (getf (ext:tool-result-details parked) :park))))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (tools-eval::terminate-eval-thread (tools-eval::eval-park-thread park))
        (let ((result (tools-eval::resolve-park-event
                       park protocol context
                       (tools-eval::eval-park-package park) 30)))
          (is (ext:tool-result-error-p result))
          (is (getf (ext:tool-result-details result) :killed-p)
              "a dead worker yields a killed interrupted result")
          (is (not (search "ECASE" (tool-result-text result)))))))))

(test (eval-surfaces-value-level-truncation :fixture tool-authority)
  "A single returned value past the value cap is retained whole behind a handle: a
per-value marker names it, distinct from stdout truncation, read-result reaches the
full value, the :values-truncated flag still rides :details, and recompile-rerun's
:rerun wire object carries it too."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context
                               event:*events-extension-manifest*
                               tools-eval:*eval-tool-extension-manifest*
                               tools-eval:*recompile-rerun-tool-extension-manifest*
                               spill:*output-spill-extension-manifest*)
           (let ((tools-eval::*eval-value-character-limit* 8))
             (let* ((result (ext:invoke-tool
                             protocol :eval
                             '(:form "(make-string 50 :initial-element #\\a)") context))
                    (text (tool-result-text result))
                    (details (ext:tool-result-details result))
                    (handle (getf (first (getf details :value-handles)) :handle)))
               (is (not (ext:tool-result-error-p result)))
               (is (getf details :values-truncated) "the value-truncated flag rides :details")
               (is (search "value 0 truncated" text)
                   "a distinct per-value marker names the value truncation")
               (is (stringp handle) "the overflowing value is retained behind a handle")
               (is (search handle text) "the marker carries the handle token")
               (let ((page (ext:invoke-tool protocol :read-result
                                            (list :handle handle) context)))
                 (is (search "aaaaaaaaaaaa" (tool-result-text page))
                     "read-result reaches the full retained value, past the window")))
             (let* ((rr (ext:invoke-tool
                         protocol :recompile-rerun
                         '(:definition "(defun e7-big () (make-string 50 :initial-element #\\a))"
                           :form "(e7-big)")
                         context))
                    (rerun (getf (ext:tool-result-details rr) :rerun)))
               (is (eq :ok (getf rerun :status)))
               (is (getf rerun :values-truncated)
                   "the rerun wire object carries the value-truncated flag"))))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test (eval-recompile-rerun-summary-flags-value-truncation :fixture tool-authority)
  "recompile-rerun's human summary names a value-level truncation in its re-run line
via a per-value handle marker, not only the wire object."
  (let* ((spill:*output-spill-directory* (output-spill-temp-root))
         (context (kli:make-kernel-host))
         (protocol (switch-to-extension-protocol context)))
    (unwind-protect
         (progn
           (install-extensions context
                               event:*events-extension-manifest*
                               tools-eval:*eval-tool-extension-manifest*
                               tools-eval:*recompile-rerun-tool-extension-manifest*
                               spill:*output-spill-extension-manifest*)
           (let ((tools-eval::*eval-value-character-limit* 8))
             (let ((rr (ext:invoke-tool
                        protocol :recompile-rerun
                        '(:definition "(defun e2-big () (make-string 50 :initial-element #\\a))"
                          :form "(e2-big)")
                        context)))
               (is (not (ext:tool-result-error-p rr)))
               (is (search "value 0 truncated" (tool-result-text rr))
                   "the rerun summary surfaces the value-level truncation marker"))))
      (ignore-errors
       (uiop:delete-directory-tree (pathname spill:*output-spill-directory*)
                                   :validate t :if-does-not-exist :ignore)))))

(test eval-interactive-park-surfaces-each-warning-once
  "Across re-parks a nested-directive caution is surfaced once, when it first
accrues, not replayed at every later park. eval-continue is debug authority."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-continue-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(progn (in-package #:cl-user) 1)
(cerror \"Skip.\" \"first\")
(cerror \"Skip.\" \"second\")"
                        :on-error "interactive")
                      context))
             (park-id (getf (ext:tool-result-details parked) :park)))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (is (= 1 (length (getf (ext:tool-result-details parked) :warnings)))
            "the first park surfaces the nested-directive caution")
        (let ((second (ext:invoke-tool
                       protocol :eval-continue
                       (list :park park-id :restart "CONTINUE") context)))
          (is (getf (ext:tool-result-details second) :parked-p))
          (is (null (getf (ext:tool-result-details second) :warnings))
              "the second park does not replay the already-surfaced caution"))
        (ext:invoke-tool protocol :eval-continue
                         (list :park park-id :restart "ABORT-EVAL") context)))))

(test eval-abort-result-carries-accrued-warnings
  "An interactive eval aborted after a nested-directive caution accrued carries
that caution in the aborted result's :details :warnings, matching the value path.
eval-abort is debug authority."
  (with-granted-authority (:tools/standard :image/eval :image/debug)
    (let* ((context (kli:make-kernel-host))
           (protocol (switch-to-extension-protocol context)))
      (install-extensions context
                          event:*events-extension-manifest*
                          tools-eval:*eval-tool-extension-manifest*
                          tools-eval:*eval-abort-tool-extension-manifest*)
      (let* ((parked (ext:invoke-tool
                      protocol :eval
                      '(:form "(progn (in-package #:cl-user) 1)
(error \"park me\")"
                        :on-error "interactive")
                      context))
             (park-id (getf (ext:tool-result-details parked) :park))
             (aborted (ext:invoke-tool protocol :eval-abort
                                       (list :park park-id) context)))
        (is (getf (ext:tool-result-details parked) :parked-p))
        (is (getf (ext:tool-result-details aborted) :aborted-p))
        (is (= 1 (length (getf (ext:tool-result-details aborted) :warnings)))
            "the accrued caution rides the aborted result's :warnings")))))
