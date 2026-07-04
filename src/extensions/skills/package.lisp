(defpackage #:kli/skills
  (:use #:cl)
  (:import-from #:kli
                #:active-protocol
                #:context-registry
                #:find-live-object)
  (:import-from #:kli/ext
                #:contribution-extension
                #:contribution-name
                #:contribution-state
                #:defextension
                #:find-capability-provider
                #:provider-call
                #:require-capability-provider)
  (:import-from #:kli/interaction/commands
                #:make-command
                #:make-command-result
                #:make-command-text-content)
  (:import-from #:kli/text
                #:blank-string-p)
  (:import-from #:kli/prompts
                #:parse-frontmatter)
  (:import-from #:buildlisp/resources
                #:resource-root)
  (:import-from #:kli/config
                #:project-ancestors
                #:find-config-service
                #:config-service-resource-kinds
                #:installed-extension-resource-root-keys)
  (:import-from #:kli/agent/session
                #:add-system-prompt-layer
                #:remove-system-prompt-layer
                #:recode-prompt-expansion-policy
                #:session-prompt-expansion-policy)
  (:export
   #:make-ignore-matcher
   #:add-ignore-lines
   #:add-ignore-rules
   #:path-ignored-p

   #:skill
   #:make-skill
   #:skill-name
   #:skill-description
   #:skill-path
   #:skill-base-dir
   #:skill-disable-model-invocation-p
   #:validate-skill-name
   #:validate-skill-description
   #:load-skill
   #:read-skill-body
   #:format-skills-for-prompt

   #:*user-agents-skills-directory*
   #:discover-skills-in-directory
   #:discover-skills

   #:find-sigil-skills
   #:expand-skill-sigils

   #:skill-invocation-text
   #:register-skill-commands
   #:unregister-skill-commands
   #:*skills-extension-manifest*))

(in-package #:kli/skills)
