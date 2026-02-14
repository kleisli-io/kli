;;;; claude-hooks - Package Definition
;;;;
;;;; Common Lisp DSL for authoring Claude Code hooks.
;;;; Wraps the stdin-JSON/stdout-JSON/exit-code protocol with
;;;; typed response builders and a defhook macro.

(defpackage :claude-hooks
  (:use :cl)
  (:export
   ;; JSON utilities
   #:jref
   #:make-ht
   #:encode-json
   #:parse-json

   ;; PreToolUse responses
   #:pre-tool-allow
   #:pre-tool-deny
   #:pre-tool-ask

   ;; PermissionRequest responses
   #:permission-allow
   #:permission-deny

   ;; Stop/SubagentStop responses
   #:stop-continue
   #:stop-allow

   ;; Generic responses
   #:context-response
   #:hook-context
   #:prompt-context
   #:session-start-context
   #:post-tool-context
   #:block-response
   #:post-tool-block
   #:empty-response
   #:suppress-output

   ;; Depot detection
   #:find-git-root
   #:find-depot-root
   #:find-world-root
   #:find-session-depot

   ;; Path normalization
   #:normalize-file-path

   ;; Session paths
   #:session-dir
   #:session-file
   #:ensure-session-dir

   ;; File I/O
   #:append-line
   #:append-line-unique
   #:read-lines
   #:read-json-file
   #:atomic-write-json

   ;; Prompt helpers
   #:*confirmation-words*
   #:trivial-prompt-p
   #:slash-command-p
   #:confirmation-p
   #:skip-prompt-p

   ;; Hook lifecycle
   #:hook-block-condition
   #:block!
   #:run-hook
   #:defhook

   ;; Testing
   #:test-run-hook))
