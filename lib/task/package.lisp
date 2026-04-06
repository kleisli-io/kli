(defpackage #:task
  (:use #:cl #:crdt)
  (:export
   ;; Event
   #:event #:make-event
   #:event-id #:event-timestamp #:event-session
   #:event-clock #:event-type #:event-data
   #:event-to-json-string #:json-string-to-event
   ;; Event Log
   #:event-log #:make-event-log
   #:event-log-events #:event-log-clock #:event-log-path
   #:elog-append #:elog-append-event #:elog-save #:elog-load #:elog-merge
   #:*elog-cache*
   ;; Edge Encoding
   #:encode-edge #:decode-edge #:edge-targets
   #:*child-bearing-edge-types*
   ;; Task State
   #:task-state #:make-task-state
   #:task-state-id #:task-state-description #:task-state-status
   #:task-state-edges #:task-state-claim #:task-state-sessions
   #:task-state-observations #:task-state-artifacts #:task-state-metadata
   #:task-state-files-touched
   #:apply-task-event #:compute-state
   #:session-has-left-p
   ;; Paths
   #:*tasks-root*
   #:detect-tasks-root
   #:task-directory #:task-events-path
   #:ensure-task-directory
   ;; Legacy Qualified ID Compat
   #:strip-depot-prefix
   ;; Task Info Extraction
   #:extract-date-prefix #:extract-topic #:humanize-task-name
   #:extract-goals-from-plan #:extract-tags #:file-mod-time
   ;; Graph Structure
   #:task-graph #:make-task-graph
   #:task-graph-nodes #:task-graph-forward #:task-graph-reverse
   #:graph-add-node #:graph-node-props #:graph-add-edge
   ;; Subgraph
   #:graph-subgraph #:graph-ego
   ;; Edge Extraction (four layers)
   #:extract-topic-edges #:extract-same-day-edges
   #:extract-state-edges #:try-parse-json-array
   ;; Enhanced topic edges (token-overlap)
   #:*token-stopwords* #:extract-task-tokens
   #:extract-token-overlap-edges #:extract-topic-edges-enhanced
   ;; Temporal Edges
   #:extract-timestamped-reference-edges #:temporal-chain-edges
   ;; Graph Construction
   #:build-task-graph
   ;; Graph Queries
   #:compute-frontier #:graph-upstream #:graph-downstream
   ;; Cycle Detection & SCC
   #:has-cycle-p #:find-cycles #:tarjan-scc #:condense #:topo-sort-condensation
   ;; Temporal Reachability
   #:reachable-at-time #:graph-at-time
   ;; Enriched Query (cross-graph naturality)
   #:compute-enriched-query
   ;; Formatting
   #:format-graph-context #:format-graph-stats #:format-task-neighbors
   #:format-task-list-grouped
   ;; Color Generation
   #:djb2-hash #:string-to-hue #:hsl-to-hex
   #:string-to-color #:string-to-oklch
   ;; Dashboard Rendering
   #:*event-type-display* #:event-display-info
   #:universal-time-to-iso #:relative-time
   #:format-event-rich #:enriched-task-view
   ;; Health Queries
   #:children-complete-p #:incomplete-descendants
   #:find-stale-tasks #:find-dead-ends #:find-declared-orphans
   #:find-stale-claims #:find-unexplored-frontier #:find-convergent-clusters
   #:find-premature-completions
   #:find-bidirectional-refs-with-graph #:find-unlinked-bidirectional-refs-with-graph
   #:find-unlinked-bidirectional-refs
   #:find-missing-markov-edges #:find-unorganized-tasks
   #:task-health-data #:format-health-report #:task-health-report
   ;; Graph Caching
   #:*graph-cache* #:*graph-cache-ttl* #:*graph-cache-lock*
   #:get-cached-task-graph #:clear-graph-cache #:invalidate-graph-cache
   ;; Infos Caching
   #:*infos-cache* #:get-cached-task-infos #:clear-infos-cache
   ;; Per-Task State Caching
   #:*task-state-cache* #:*task-state-cache-lock* #:*task-state-cache-ttl*
   #:get-cached-task-state #:invalidate-task-state-cache #:clear-task-state-cache
   ;; Plan-as-Graph (Theorem 10)
   #:task-children #:all-descendants
   #:load-child-states #:build-plan-graph
   #:plan-subgraph #:plan-frontier #:format-plan
   ;; Plan Tier 3: Generated Artifacts + Self-Attention
   #:stable-topo-sort #:generate-plan-markdown
   #:extract-parent #:plan-context #:format-plan-context
   #:auto-plan-context #:plan-to-json #:plan-to-rich-json
   #:extract-phase-details #:dashboard-emit-event
   ;; Dashboard JSON Projection
   #:graph-to-dashboard-json
   ;; Markov Category Enrichment
   #:action-functor
   #:event-type-entropy
   #:mutual-information-bigrams
   #:organization-indicator
   #:task-features
   #:affinity-score
   #:build-session-fingerprints
   #:fingerprint-to-vector
   #:classify-session-archetype
   #:find-missing-edges
   ;; Markov Batch Loading & Caching
   #:load-all-tasks
   #:*markov-cache* #:*markov-cache-ttl*
   #:get-cached-markov-data
   #:clear-markov-cache #:invalidate-markov-cache
   #:cached-all-tasks #:cached-task-metrics
   #:cached-session-fingerprints
   #:cached-missing-edges))
