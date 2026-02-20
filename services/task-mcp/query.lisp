(in-package #:task-mcp)

;;; TQ Query Tool
;;;
;;; Thin MCP wrapper over lib/task/query.lisp (the :tq package).
;;; Binds dynamic context and provides error handling.
;;; Provides mutation handler for ! operations.

;;; ============================================================
;;; MUTATION HANDLER
;;; ============================================================

(defun tq-mutation-handler (node-id op &rest args)
  "Execute a TQ mutation operation on NODE-ID.
   Temporarily switches *current-task-id* to node-id for emit-event.
   Restores both the global and HTTP session registry on exit,
   since emit-event calls finalize-session-context which would
   otherwise persist the temporary task ID to the registry.
   Returns :skipped for completed tasks, result string otherwise."
  (let ((prev-task *current-task-id*))
    (unwind-protect
         (progn
           (setf *current-task-id* node-id)
           ;; Persist to registry so emit-event's ensure-session-context
           ;; loads back this task, not the previous one
           (finalize-session-context)
           (case op
             ;; Status mutations
             (:complete
              (handler-case
                  (progn
                    ;; Check for incomplete descendants first
                    (let ((incomplete (task:incomplete-descendants node-id)))
                      (when incomplete
                        (return-from tq-mutation-handler
                          (format nil "blocked: ~D incomplete subtask~:P"
                                  (length incomplete)))))
                    (emit-event :task.update-status (list :status "completed"))
                    "completed")
                (error (c)
                  (if (search "completed" (format nil "~A" c))
                      :skipped
                      (error c)))))

             (:reopen
              (emit-event :task.update-status (list :status "active"))
              "reopened")

             ;; Observation
             (:observe
              (let ((text (first args)))
                (emit-event :observation (list :text text))
                (format nil "observed (~D chars)" (length text))))

             ;; Metadata
             (:set-meta
              (let ((key (first args))
                    (value (second args)))
                (emit-event :task.set-metadata
                            (list :key (if (keywordp key)
                                           (string-downcase (symbol-name key))
                                           key)
                                  :value value))
                (format nil "~A = ~A" key value)))

             ;; Edge mutations
             ;; Uses resolve-task-id to match task_link tool behavior
             (:link
              (let* ((target (first args))
                     (edge-type (second args))
                     (resolved-target (resolve-task-id target)))
                (emit-event :task.link
                            (list :target-id resolved-target
                                  :edge-type (if (keywordp edge-type)
                                                 (string-downcase (symbol-name edge-type))
                                                 edge-type)))
                (format nil "-> ~A (~A)" resolved-target edge-type)))

             ;; Uses resolve-task-id to match task_sever tool behavior
             (:sever
              (let* ((target (first args))
                     (edge-type (second args))
                     (resolved-target (resolve-task-id target)))
                (emit-event :task.sever
                            (list :target-id resolved-target
                                  :edge-type (if (keywordp edge-type)
                                                 (string-downcase (symbol-name edge-type))
                                                 edge-type)))
                (format nil "-/-> ~A (~A)" resolved-target edge-type)))

             ;; Inverse sever: sever from parent to this node
             ;; Usage: (-> (node "child1" "child2") (:sever-from-parent! :phase-of))
             ;; Inherits depot from parent (same pattern as :fork mutation)
             (:sever-from-parent
              (let* ((edge-type (or (first args) :phase-of))
                     (edge-type-kw (if (keywordp edge-type) edge-type
                                       (intern (string-upcase edge-type) :keyword)))
                     ;; Find parent that has this edge to node-id
                     (graph (tq:current-graph))
                     (reverse-edges (gethash node-id (task:task-graph-reverse graph)))
                     (parent-id (loop for edge in reverse-edges
                                      when (eq (second edge) edge-type-kw)
                                      return (first edge))))
                (if parent-id
                    (let* (;; Resolve parent to get qualified ID
                           (qualified-parent (resolve-task-id parent-id))
                           ;; Extract depot from parent (child inherits parent's depot)
                           (parent-depot (multiple-value-bind (depot bare)
                                             (task:parse-qualified-id qualified-parent)
                                           (declare (ignore bare))
                                           depot))
                           ;; Get bare node-id and its depot
                           (node-depot (multiple-value-bind (depot bare)
                                           (task:parse-qualified-id node-id)
                                         (declare (ignore bare))
                                         depot))
                           (bare-node-id (multiple-value-bind (depot bare)
                                             (task:parse-qualified-id node-id)
                                           (declare (ignore depot))
                                           bare))
                           ;; Qualify target with parent's depot to match what fork stored
                           (qualified-target (if (string= parent-depot "default")
                                                 bare-node-id
                                                 (task:qualify-task-id parent-depot bare-node-id))))
                      ;; Cross-depot check: reject if node and parent are in different depots
                      (when (and (not (string= node-depot "default"))
                                 (not (string= node-depot parent-depot)))
                        (return-from tq-mutation-handler
                          (format nil "cross-depot sever rejected: ~A (depot ~A) has parent ~A (depot ~A)"
                                  node-id node-depot qualified-parent parent-depot)))
                      ;; Emit sever on parent's event log
                      (setf *current-task-id* qualified-parent)
                      (finalize-session-context)
                      (emit-event :task.sever
                                  (list :target-id qualified-target
                                        :edge-type (string-downcase (symbol-name edge-type-kw))))
                      (format nil "~A -/-> ~A (~A)" qualified-parent qualified-target edge-type-kw))
                    (format nil "no parent with ~A edge to ~A" edge-type-kw node-id))))

             ;; Scaffolding: fork creates a new task as child
             ;; Uses qualified IDs to match task_fork tool behavior
             ;; Child inherits depot from parent (phase-of semantics)
             (:fork
              (let* ((local-name (first args))
                     (description (or (second args) ""))
                     (edge-type (or (third args) "phase-of"))
                     (full-name (ensure-date-prefix local-name))
                     ;; Resolve parent first to get its depot
                     (qualified-parent (resolve-task-id node-id))
                     ;; Extract depot from parent (child inherits parent's depot)
                     (parent-depot (multiple-value-bind (depot bare)
                                       (task:parse-qualified-id qualified-parent)
                                     (declare (ignore bare))
                                     depot))
                     ;; Qualify child with parent's depot
                     (qualified-id (if (string= parent-depot "default")
                                       full-name
                                       (task:qualify-task-id parent-depot full-name))))
                ;; Create the child task directory
                (task:ensure-task-directory qualified-id)
                ;; Emit create event in child context
                (setf *current-task-id* qualified-id)
                (finalize-session-context)
                (emit-event :task.create
                            (list :name qualified-id
                                  :bare-name full-name
                                  :depot parent-depot
                                  :description description
                                  :parent qualified-parent))
                ;; Emit fork event in parent context
                (setf *current-task-id* qualified-parent)
                (finalize-session-context)
                (emit-event :task.fork
                            (list :child-id qualified-id :edge-type edge-type))
                ;; Return the qualified task ID
                qualified-id))

             (otherwise
              (error "Unknown mutation operation: ~A" op))))
      ;; Restore both the in-process global AND the HTTP session registry.
      ;; emit-event calls finalize-session-context which persists *current-task-id*
      ;; to the registry — without this restore, the registry retains the temporary
      ;; task ID from the last emit-event call within the mutation.
      (setf *current-task-id* prev-task)
      (when *http-mode*
        (save-session-context *session-id*)))))

(define-session-tool task_query
    ((query string "TQ query as S-expression string")
     (safety_limit integer "Max tasks a mutation can affect (default 20, 0 to disable)" nil))
  "Execute a TQ (Task Query) expression against the task graph.

TQ is a pipeline-based query language for exploring tasks and their relationships.
Queries are S-expressions that start with a source and apply transformations.

## Starting Expressions
- :all - All tasks in the graph
- (node \"pattern\") - Tasks whose ID contains pattern
- (active) - Event-sourced tasks (have events.jsonl)
- (dormant) - Tasks without events
- (current) - Current task as node-set (requires task context)
- (query \"name\") - Execute a named query (see Named Queries below)

## Pipeline Steps
Use (-> source step1 step2 ...) to chain transformations:
- (:follow :edge-type) - Traverse forward edges (e.g., :phase-of, :depends-on)
- (:back :edge-type) - Traverse backward edges
- (:where predicate) - Filter nodes
- (:select :field1 :field2) - Project specific fields
- (:sort :field) - Sort by field (descending)
- (:take n) - Limit to first n results
- :ids - Extract just task IDs (list of strings)
- :count - Count nodes (integer)
- :enrich - Load full CRDT state (adds :crdt-status, :obs-count, :edge-count, :session-count, :display-name, plus any phase metadata keys like :objective, :acceptance, :steps, :context, :constraints)
- (:group-by :field) - Group by field value, returns alist of (value . nodes)

Note: :where, :sort, and :group-by auto-enrich when querying fields that require it. Explicit :enrich is only needed for :select on enriched fields.

## Named Queries
Pre-defined queries accessible via (query \"name\"):
- \"active-roots\" - Event-sourced tasks with no parent
- \"orphans\" - Tasks with no edges (neither incoming nor outgoing)
- \"leaf-tasks\" - Tasks with no children (phases)
- \"stale-phases\" - Active phases whose parent is completed
- \"plan\" - Phases of current task with enriched state
- \"plan-ready\" - Ready (non-completed) phases of current task
- \"recent\" - Tasks sorted by session count (most recently active)
- \"busy\" - Tasks sorted by observation count (most observations)
- \"hub-tasks\" - Tasks sorted by edge count (most connected)

## Predicates (for :where)
- (= :field value) - Field equals value
- (has :field) - Field exists and is non-nil
- (matches \"pattern\") - ID contains pattern
- (and pred1 pred2) - Both predicates true
- (or pred1 pred2) - Either predicate true
- (not pred) - Negate predicate

## Set Operations
- (union expr1 expr2) - Combine results
- (minus expr1 expr2) - Difference
- (intersect expr1 expr2) - Intersection

## Examples

Count all event-sourced tasks:
  (-> (active) :count)

Find phases of a specific task:
  (-> (node \"coalgebraic\") (:follow :phase-of) :ids)

Get active roots with most observations:
  (-> (query \"active-roots\") :enrich (:sort :obs-count) (:take 5) (:select :display-name :obs-count))

Find tasks with a specific topic:
  (-> :all (:where (= :topic \"mcp-tool\")) :ids)

Group tasks by topic:
  (-> (active) :enrich (:group-by :topic))

Find orphan tasks:
  (-> (query \"orphans\") :ids)

Enrich and sort by session count:
  (-> (active) :enrich (:sort :session-count) (:take 10))

## Mutation Steps (! suffix)
Apply mutations to all nodes in the pipeline result:
- (:complete!) - Mark nodes as completed
- (:reopen!) - Reopen completed nodes
- (:observe! \"text\") - Add observation to nodes
- (:set-meta! :key \"value\") - Set metadata key-value
- (:link! \"target-id\" :edge-type) - Add edge to target
- (:sever! \"target-id\" :edge-type) - Remove edge to target
- (:sever-from-parent! :edge-type) - Remove edge FROM parent TO each node (bulk sever)

SAFETY: Mutations are limited to 20 tasks per step by default.
Use safety_limit parameter to override (0 disables), or (:take N) / (:where ...) to narrow the set.
IMPORTANT: (node ...) uses substring matching — prefer (query \"plan\") for current task's phases.

Mutation examples:

Complete current task's phases (SAFE — scoped to current task):
  (-> (query \"plan\") (:complete!))

Complete specific tasks by full qualified ID:
  (-> (node \"kleisli:2026-02-15-phase-1\" \"kleisli:2026-02-15-phase-2\") (:complete!))

Mark filtered tasks complete:
  (-> (active) (:where (matches \"temp-\")) (:complete!))

Add observation to specific task:
  (-> (node \"my-task\") (:observe! \"Implementation complete\"))

Bulk sever phases from parent:
  (-> (query \"plan\") (:sever-from-parent! :phase-of))

## DAG Scaffolding
Create phased task structures in one expression:

Create phases with dependencies:
  (scaffold-plan! (p1 \"Core library\") (p2 \"Integration\" :after p1) (p3 \"Tests\" :after p2))

Create phases with metadata (self-contained specs):
  (scaffold-plan!
    (p1 \"Setup database\"
      :objective \"Create schema with migrations\"
      :acceptance \"Tables exist, migration passes\"
      :steps \"1. Define schema\\n2. Run migrations\\n3. Verify\"
      :context \"Read internal/storage/migrations/\")
    (p2 \"Implement API\" :after p1
      :objective \"REST endpoints for CRUD\"
      :acceptance \"All endpoints return 200\"))

Metadata keywords: :objective, :acceptance, :steps, :context, :constraints, :await, :ephemeral.
These are stored as task metadata and surfaced via :enrich, plan-markdown, and plan-context.

Create linear chain:
  (scaffold-chain! \"Phase 1\" \"Phase 2\" \"Phase 3\")"
  (handler-case
      (let* ((form (tq:safe-read-query query))
             (result (let ((tq:*graph* (get-or-build-graph))
                           ;; Graph now has qualified IDs, pass through directly
                           (tq:*current-task-id* *current-task-id*)
                           (tq:*mutation-handler* #'tq-mutation-handler)
                           (tq:*mutation-safety-limit*
                            (if safety_limit
                                (if (zerop safety_limit) nil safety_limit)
                                tq:*mutation-safety-limit*)))
                       (tq:interpret-query form))))
        ;; Invalidate graph cache after mutations that modify the graph
        ;; (scaffold creates new tasks/edges, mutations modify state)
        (when (or (tq:scaffold-result-p result)
                  (tq:mutation-log-p result))
          (task:clear-graph-cache))
        (make-text-content (tq:format-query-result result)))
    (tq:mutation-safety-exceeded (c)
      (make-text-content "Safety limit: ~A" c))
    (tq:tq-parse-error (c)
      (make-text-content "Parse error: ~A" (tq:tq-error-message c)))
    (tq:tq-error (c)
      (make-text-content "Query error: ~A" (tq:tq-error-message c)))
    (error (c)
      (make-text-content "Error: ~A" c))))
