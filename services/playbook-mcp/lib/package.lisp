;;; Playbook MCP Library - Package Definition
;;; Core data layer for playbook pattern system.
;;; Used by both the MCP server and standalone CLI tools.

(defpackage #:playbook-mcp
  (:use #:cl #:alexandria)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  ;; Pattern data model
  (:export #:pattern
           #:make-pattern
           #:pattern-id
           #:pattern-domain
           #:pattern-content
           #:pattern-helpful
           #:pattern-harmful
           #:pattern-effectiveness
           #:pattern-evolution-history
           #:pattern-source-file
           #:pattern-line-number
           #:pattern-embedding
           #:pattern-to-line
           #:pattern-to-json
           #:generate-pattern-id)
  ;; Store operations
  (:export #:*pattern-store*
           #:store-pattern
           #:get-pattern
           #:remove-pattern
           #:list-patterns
           #:clear-patterns
           #:pattern-count
           #:patterns-by-domain
           #:update-pattern)
  ;; Parser
  (:export #:parse-pattern-line
           #:load-playbook-file
           #:load-playbook-files)
  ;; Search operations
  (:export #:search-patterns
           #:proven-patterns
           #:harmful-patterns
           #:semantic-search-patterns
           #:format-search-results)
  ;; File sync operations
  (:export #:update-pattern-in-file
           #:save-pattern-feedback
           #:save-pattern-evolution
           #:append-pattern-to-file)
  ;; Edge data model
  (:export #:edge
           #:make-edge
           #:edge-source
           #:edge-target
           #:edge-relation
           #:edge-weight
           #:edge-evidence)
  ;; Edge store operations
  (:export #:*edge-store*
           #:add-edge
           #:outgoing-edges
           #:remove-edges-for
           #:all-edges
           #:clear-edges
           #:edge-count
           #:node-count)
  ;; Weight functions
  (:export #:co-application-weight
           #:embedding-proximity-weight)
  ;; Edge serialization
  (:export #:edge-to-json-alist
           #:json-ht-to-edge
           #:load-edges-file
           #:save-edges-file)
  ;; Graph operations
  (:export #:*current-graph*
           #:*graph-stale*
           #:*last-rebuild-time*
           #:*manual-edges-path*
           #:rebuild-graph
           #:ensure-graph-fresh
           #:graph-outgoing
           #:graph-node-count
           #:graph-edge-count
           #:build-edge-store
           #:after-pattern-mutation)
  ;; Co-application ledger
  (:export #:*co-app-ledger*
           #:co-app-key
           #:parse-co-app-key
           #:load-co-app-ledger
           #:save-co-app-ledger
           #:generate-co-app-pairs
           #:update-co-app-ledger-from-session
           #:atomic-update-co-app-ledger
           #:prune-co-app-ledger
           #:ledger-to-co-app-edges
           #:compute-proximity-edges
           #:co-app-ledger-path
           #:flush-co-app-ledger
           #:record-activation-and-update-graph)
  ;; Activation
  (:export #:spread-activation
           #:activate-patterns)
  ;; Embedding operations
  (:export #:get-embedding
           #:ensure-pattern-embedding
           #:clear-embedding-cache
           #:embedding-cache-size
           #:save-embedding-cache
           #:load-embedding-cache
           #:prune-embedding-cache-file
           #:batch-embed-patterns
           #:compute-all-embeddings
           #:cosine-similarity
           #:*ollama-url*
           #:*embedding-model*
           #:initialize-ollama-config)
  ;; Root detection
  (:export #:mcp-find-depot-root
           #:mcp-find-world-root)
  ;; HTTP multi-session support
  (:export #:*http-mode*
           #:current-http-session-id
           #:ensure-playbook-session-context
           #:cleanup-inactive-playbook-sessions)
  ;; Session state
  (:export #:session-state
           #:make-session-state
           #:session-state-claude-session-id
           #:get-or-create-session
           #:get-session
           #:record-activation
           #:record-feedback
           #:record-domain
           #:session-summary
           #:cleanup-session
           #:session-to-json-alist
           #:load-task-pattern-state
           #:save-session-to-task
           #:aggregate-task-state
           #:format-session-resume-message)
  ;; PID → Claude session ID registry
  (:export #:register-claude-session-for-pid
           #:lookup-claude-session-by-pid
           #:resolve-claude-session-id)
  ;; MCP state file persistence (for handoff workflow)
  (:export #:write-activation-to-mcp-state
           #:read-mcp-activated-ids
           #:clear-mcp-activated-state)
  ;; Feedback state file (for Stop hook)
  (:export #:feedback-state-path
           #:write-feedback-state-file
           #:read-feedback-state-file)
  ;; File locking
  (:export #:with-file-lock)
  ;; Orphan cleanup
  (:export #:get-valid-pattern-ids
           #:cleanup-orphaned-data
           #:post-load-cleanup)
  ;; Signal handler cleanup
  (:export #:*mcp-lock-path*
           #:cleanup-mcp-lock
           #:graceful-shutdown
           #:install-signal-handlers)
  ;; Log verbosity
  (:export #:*log-verbose*))

(in-package #:playbook-mcp)

(defvar *log-verbose* nil
  "When true, emit informational log messages to *error-output*.
   Set from KLI_LOG environment variable at startup.")
