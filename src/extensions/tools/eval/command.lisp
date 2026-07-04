(in-package #:kli/tools/eval)

(defun commands-provider (context)
  (require-capability-provider (active-protocol context)
                               :commands
                               :contract :commands/v1))

(defun eval-command-form (arguments)
  (or (getf arguments :tail)
      (getf arguments :form)
      (error "Eval command requires a form.")))

(defun command-result-from-tool-result (result)
  (make-command-result
   :content (kli/ext:tool-result-content result)
   :details (kli/ext:tool-result-details result)
   :error-p (kli/ext:tool-result-error-p result)))

(defun run-eval-command (command arguments context &key call-id on-update)
  ;; The interactive subject withholds :image/eval; a user-driven /eval elevates
  ;; locally to confer it, still bounded by the capabilities policy.
  (declare (ignore command))
  (let* ((form (eval-command-form arguments))
         (package (getf arguments :package))
         (parameters (if package
                         (list :form form :package package)
                         (list :form form)))
         (result (with-operator-capability (context :image/eval)
                   (invoke-tool (active-protocol context)
                                :eval
                                parameters
                                context
                                :call-id call-id
                                :on-update on-update))))
    (command-result-from-tool-result result)))

(defun make-eval-command ()
  (make-command :name :eval
                :label "Eval"
                :description "Evaluate Common Lisp forms in the live image."
                :arguments '(:tail :form)
                :runner #'run-eval-command
                :metadata '(:tool eval)))

(defun register-eval-command (protocol contribution context)
  (declare (ignore protocol))
  (provider-call (commands-provider context)
                 :register-command
                 context
                 :eval
                 (make-eval-command)
                 :source (contribution-extension contribution)
                 :tier :core))

(defun unregister-eval-command (protocol contribution context)
  (declare (ignore protocol))
  (provider-call (commands-provider context)
                 :unregister-command
                 context
                 (kli/ext:contribution-state contribution)))
