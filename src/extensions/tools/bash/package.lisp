(defpackage #:kli/tools/bash
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol)
  (:import-from #:kli/ext
                #:contribution-extension
                #:contribution-state
                #:defextension
                #:invoke-tool
                #:make-provider
                #:make-provider-contract
                #:protocol-storage
                #:provider-call
                #:require-capability-provider
                #:tool-abort-requested-p
                #:tool-parameter
                #:required-tool-parameter
                #:tool-text-result)
  (:import-from #:kli/text
                #:whitespace-char-p
                #:string-prefix-p
                #:render-bounded-lines)
  (:import-from #:kli/output-spill
                #:adopt-file-spill
                #:register-file-handle
                #:spill-entry-token
                #:spill-entry-bytes
                #:format-spill-marker)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content
                #:*command-completion-mode*
                #:with-operator-capability)
  (:export
   #:run-bash-tool
   #:local-bash-run
   #:active-bash-exec-provider-id
   #:seed-bash-exec-provider-id
   #:resolve-bash-exec-provider-id
   #:bash-policy
   #:+local-bash-exec-provider-id+
   #:+persistent-shell-bash-exec-provider-id+
   #:*bash-output-character-limit*
   #:run-bash-command
   #:register-bash-command
   #:unregister-bash-command
   #:*bash-tool-extension-manifest*
   #:*bash-command-extension-manifest*
   #:*persistent-shell-extension-manifest*
   #:*persistent-shell-command-extension-manifest*
   #:*bash-jobs-extension-manifest*))
