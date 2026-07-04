(in-package #:kli/tools/eval)

(defextension eval-tool
  (:provides
   (tool eval
     :label "Eval"
     :description "Evaluate Common Lisp forms in the live image. :form is loaded
like a file: each top-level form is read and evaluated before the next is read,
so a form that changes how later forms read -- in-package, a reader macro, #. --
takes effect immediately. The current package is sticky across calls (a REPL):
an in-package at top level governs this call and the ones that follow; pass
:package to start a call in a given package. Note a nested directive does not
get this treatment -- (progn (in-package :p) (defun ...)) reads the whole progn
in the old package before any of it runs, so put in-package at top level, as its
own form. Runs with full first-party image authority: a form can read, write,
and edit files and run processes. This is not a sandbox -- it grants no
confinement. With :on-error \"interactive\" an unhandled error parks the thread
on the live restarts instead of unwinding; resume it with eval-continue or
eval-abort (debug authority)."
     :parameters '(:object (:form :string)
                   (:package :string :optional t)
                   (:timeout :integer :optional t)
                   (:on-error :string :optional t))
     :runner #'run-eval-tool
     :metadata '(:capabilities (:image/eval)))
   (effect eval-session
     #'install-eval-session
     #'uninstall-eval-session)))

(defextension eval-continue-tool
  (:provides
   (tool eval-continue
     :label "Continue parked eval"
     :description "Resume a parked interactive eval by invoking one of its live
restarts by name. :park is the id the parked eval returned, :restart the restart
name (e.g. USE-VALUE), and :arg an optional value read in the eval package for
restarts that take one. The resumed computation reuses the original eval's timeout
unless :timeout overrides it. Returns the resumed outcome. Requires debug authority
over the live image."
     :parameters '(:object (:park :string)
                   (:restart :string)
                   (:arg :string :optional t)
                   (:timeout :integer :optional t))
     :runner #'run-eval-continue-tool
     :metadata '(:capabilities (:image/debug)))))

(defextension eval-abort-tool
  (:provides
   (tool eval-abort
     :label "Abort parked eval"
     :description "Unwind a parked interactive eval cleanly, leaving no parked
thread. :park is the id the parked eval returned; :timeout optionally overrides the
original eval's budget for the unwind. Requires debug authority over the live image."
     :parameters '(:object (:park :string)
                   (:timeout :integer :optional t))
     :runner #'run-eval-abort-tool
     :metadata '(:capabilities (:image/debug)))))

(defextension recompile-rerun-tool
  (:provides
   (tool recompile-rerun
     :label "Recompile and re-run"
     :description "Redefine a function then re-run a form in the live image in one
step. Evaluate :definition (a defun, capturing compiler notes), then evaluate
:form under structured error capture. The prior definition is snapshotted before
redefining and returned (:prior-fdefinition / :prior-fdefinition-restorable), but
the redefinition is applied immediately and is NOT automatically rolled back if
:form errors -- a failed re-run leaves the new definition installed; restore from
the snapshot manually if you need the old one. Both are loaded form-by-form and
share the sticky
current package (pass :package to override). Runs with full first-party image
authority -- not a sandbox."
     :parameters '(:object (:definition :string)
                   (:form :string)
                   (:package :string :optional t)
                   (:timeout :integer :optional t))
     :runner #'run-recompile-rerun-tool
     :metadata '(:capabilities (:image/eval)))))

(defextension eval-command
  (:requires
   (capability commands :contract commands/v1)
   (tool eval))
  (:provides
   (effect eval-command
     #'register-eval-command
     #'unregister-eval-command)))
