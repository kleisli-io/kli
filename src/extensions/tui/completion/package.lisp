(defpackage #:kli/tui/completion
  (:use #:cl)
  (:import-from #:kli/ext
                #:defextension
                #:find-capability-provider
                #:make-provider
                #:make-provider-contract
                #:provider-call)
  (:import-from #:kli/interaction/commands
                #:command-name
                #:command-description
                #:command-arguments
                #:command-completer
                #:command-signature)
  (:import-from #:kli/tui/editor
                #:make-completion-candidate)
  (:import-from #:kli/skills
                #:make-ignore-matcher
                #:add-ignore-rules
                #:path-ignored-p)
  (:import-from #:kli/text
                #:blank-string-p
                #:string-prefix-p)
  (:export
   #:command-candidates
   #:skill-candidates
   #:file-candidates
   #:path-candidates
   #:directory-path-strings
   #:argument-help
   #:*tui-completion-extension-manifest*))
