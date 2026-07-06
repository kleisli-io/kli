(in-package #:kli/tui/completion)

(defun make-completion-contract ()
  (make-provider-contract
   :id :completion/v1
   :capability :completion
   :required-entries '(:candidates :argument-help)))

(defun make-completion-provider ()
  (make-provider
   :id :completion-provider
   :capability :completion
   :contracts '(:completion/v1)
   :entries
   (list :candidates
         (lambda (protocol kind query)
           (ecase kind
             (:command (command-candidates protocol))
             (:skill (skill-candidates protocol))
             (:file (file-candidates protocol))
             (:path (path-candidates protocol query))))
         :argument-help
         (lambda (protocol name tail &key (mode :passive))
           (argument-help protocol name tail :mode mode)))))

(defextension tui-completion
  (:provides
   (contract completion/v1
     (make-completion-contract))
   (capability completion
     (make-completion-provider))))
