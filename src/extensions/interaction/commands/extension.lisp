(in-package #:kli/interaction/commands)

(defun make-commands-contract ()
  (make-provider-contract
   :id :commands/v1
   :capability :commands
   :required-entries
   '(:register-command
     :unregister-command
     :find-command
     :resolve-command
     :command-collisions
     :list-commands
     :invoke-command
     :recode-command)))

(defun make-commands-provider (&optional (service (make-command-service)))
  (make-provider
   :id :commands-provider
   :capability :commands
   :contracts '(:commands/v1)
   :entries
   (list :register-command
         (lambda (context name command &rest options)
           (declare (ignore context))
           (apply #'register-command service name command options))
         :unregister-command
         (lambda (context registration)
           (declare (ignore context))
           (unregister-command service registration))
         :find-command
         (lambda (name)
           (find-command service name))
         :resolve-command
         (lambda (name)
           (multiple-value-bind (command status matches)
               (resolve-command service name)
             (list :command command :status status :matches matches)))
         :command-collisions
         (lambda ()
           (command-collisions service))
         :list-commands
         (lambda ()
           (list-commands service))
         :invoke-command
         (lambda (name arguments context &rest options)
           (apply #'invoke-command service name arguments context options))
         :recode-command #'recode-command)))

(defextension commands
  (:provides
   (contract commands/v1
     (make-commands-contract))
   (capability commands
     (make-commands-provider))))
