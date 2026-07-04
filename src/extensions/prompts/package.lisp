(defpackage #:kli/prompts
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:contribution-state
                #:defextension
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content)
  (:import-from #:kli/config
                #:find-config-service
                #:config-service-resource-kinds
                #:installed-extension-resource-roots)
  (:import-from #:buildlisp/resources
                #:resource-root)
  (:export
   #:parse-command-arguments
   #:substitute-arguments
   #:parse-frontmatter

   #:prompt-template
   #:prompt-template-name
   #:prompt-template-description
   #:prompt-template-argument-hint
   #:prompt-template-body
   #:prompt-template-path
   #:load-prompt-template
   #:discover-prompt-templates
   #:expand-prompt-template

   #:register-prompt-template-commands
   #:unregister-prompt-template-commands
   #:*prompt-templates-extension-manifest*))

(in-package #:kli/prompts)
