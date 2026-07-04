(in-package #:kli/tools/bash)

(defun make-bash-exec-contract ()
  (make-provider-contract
   :id :bash-exec/v1
   :capability :bash-exec
   :required-entries '(:run)))

(defun make-local-bash-exec-provider ()
  (make-provider
   :id +local-bash-exec-provider-id+
   :capability :bash-exec
   :contracts '(:bash-exec/v1)
   :entries (list :run #'local-bash-run)))

(defun make-persistent-shell-bash-exec-provider ()
  (make-provider
   :id +persistent-shell-bash-exec-provider-id+
   :capability :bash-exec
   :contracts '(:bash-exec/v1)
   :entries (list :run #'persistent-shell-bash-run)))

;; The bash tool honors a per-session policy -- working directory, environment
;; allow-list, and default timeout -- held in protocol storage and enforced by
;; the runner (see bash-policy), so it travels with the session rather than the
;; tool definition.
;;
;; Contract before capability: the capability validates its provider's
;; contracts at install, so the contract must already be registered.
(defextension bash-tool
  (:provides
   (contract bash-exec/v1
     (make-bash-exec-contract))
   (capability bash-exec
     (make-local-bash-exec-provider))
   (tool bash
     :label "Bash"
     :description "Run a shell command."
     :parameters '(:object (:command :string)
                   (:directory :string :optional t)
                   (:input :string :optional t)
                   (:shell :string :optional t)
                   (:timeout :integer :optional t)
                   (:run_in_background :boolean :optional t))
     :runner #'run-bash-tool
     :renderer (kli/ext:make-tool-presenter
                :present-call #'kli/ext:command-call-presenter)
     :metadata '(:capabilities (:process/exec)))))

(defextension bash-command
  (:requires
   (capability commands :contract commands/v1)
   (tool bash))
  (:provides
   (effect bash-command
     #'register-bash-command
     #'unregister-bash-command)))

;; A second bash-exec provider backed by one long-lived shell per session.
;; Ships installed but inert: the tool resolves it only once selected. The
;; teardown effect's retractor reaps the shell, so a protocol rollback leaves no
;; live process behind. Requires bash-tool so the bash-exec/v1 contract the
;; provider validates against is already registered.
(defextension persistent-shell
  (:requires
   (extension bash-tool))
  (:provides
   (capability bash-exec
     (make-persistent-shell-bash-exec-provider))
   (effect persistent-shell-teardown
     #'install-persistent-shell-teardown
     #'retract-persistent-shell-teardown)))

(defextension persistent-shell-command
  (:requires
   (capability commands :contract commands/v1)
   (extension persistent-shell))
  (:provides
   (effect persistent-shell-command
     #'register-bash-shell-command
     #'unregister-bash-shell-command)))

;; Background jobs for the Bash tool. Inert until a run_in_background launch
;; creates the first job. Three tools read, stop, and list jobs; the drain
;; effect's retractor SIGKILLs every live group on rollback.
(defextension bash-jobs
  (:requires
   (extension bash-tool))
  (:provides
   (tool bash-output
     :label "BashOutput"
     :description "Read new output from a background shell job started with the
Bash tool's run_in_background option. Returns the output captured since the last
read and whether the job is still running or has exited."
     :parameters '(:object (:job_id :string))
     :runner #'run-bash-output-tool
     :metadata '(:capabilities (:process/exec)))
   (tool bash-kill
     :label "BashKill"
     :description "Stop a running background shell job by its id. Terminates the
job's process group; its captured output stays readable until the session ends."
     :parameters '(:object (:job_id :string))
     :runner #'run-bash-kill-tool
     :metadata '(:capabilities (:process/exec)))
   (tool bash-jobs
     :label "BashJobs"
     :description "List the background shell jobs in this session: each job's id,
whether it is running or finished, its exit status when finished, and how many
bytes of output it has captured. Read-only."
     :parameters '(:object)
     :runner #'run-bash-jobs-tool
     :metadata '(:capabilities (:process/exec)))
   (effect bash-jobs-drain
     #'install-bash-jobs-drain
     #'retract-bash-jobs-drain)))
