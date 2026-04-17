(in-package #:task-mcp)

;;; TQ Query Tool
;;;
;;; Thin MCP wrapper over lib/task/query.lisp (the :tq package).
;;; Binds dynamic context and provides error handling.
;;; Provides mutation handler for ! operations.

;;; ============================================================
;;; MUTATION HANDLER
;;; ============================================================

(defun tq-require-non-nil (op field value)
  "Signal a structured error if VALUE is nil or an empty string.
   OP is the mutation keyword (e.g. :observe); FIELD names the argument
   being checked.  Produces an error that names the TQ op site so the
   caller can locate the bad mutation in their query."
  (when (or (null value)
            (and (stringp value) (zerop (length value))))
    (error "TQ ~A: required arg ~A is nil or empty" op field)))

(defun tq-mutation-handler (node-id op &rest args)
  "Execute a TQ mutation operation on NODE-ID.
   Temporarily switches *current-task-id* to node-id for emit-event.
   Binds *in-mutation* to suppress session registry reads/writes during
   the mutation — emit-event uses thread-local *current-task-id* directly
   instead of reloading from the shared registry.  This prevents transient
   task IDs (child, parent) from polluting the registry where other
   requests could read them.
   Restores *current-task-id* and persists to registry on exit via
   unwind-protect (outside *in-mutation* scope, so save is not suppressed).
   Returns :skipped for completed tasks, result string otherwise."
  (let ((prev-task *current-task-id*))
    (unwind-protect
         (let ((*in-mutation* t))
           (setf *current-task-id* node-id)
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
                (tq-require-non-nil :observe :text text)
                (emit-event :observation (list :text text))
                (format nil "observed (~D chars)" (length text))))

             ;; Metadata
             (:set-meta
              (let ((key (first args))
                    (value (second args)))
                (tq-require-non-nil :set-meta :key key)
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
                    (let* ((resolved-parent (resolve-task-id parent-id))
                           (bare-node-id (task:strip-depot-prefix node-id)))
                      ;; Emit sever on parent's event log
                      (setf *current-task-id* resolved-parent)
                      (emit-event :task.sever
                                  (list :target-id bare-node-id
                                        :edge-type (string-downcase (symbol-name edge-type-kw))))
                      (format nil "~A -/-> ~A (~A)" resolved-parent bare-node-id edge-type-kw))
                    (format nil "no parent with ~A edge to ~A" edge-type-kw node-id))))

             ;; Scaffolding: fork creates a new task as child
             (:fork
              (let ((local-name (first args))
                    (description (or (second args) ""))
                    (edge-type (or (third args) "phase-of")))
                (tq-require-non-nil :fork :local-name local-name)
                ;; Improve / validate the name at the mutation boundary so
                ;; every caller (scaffold-plan, scaffold-chain, direct
                ;; task_query) gets the same format validation task_create
                ;; enforces.  Signals task-validation:name-improvement-failed
                ;; if the name is invalid and cannot be derived from
                ;; DESCRIPTION — trust the name past this point.
                (let* ((improved-name (tq:maybe-improve-name local-name description))
                       (full-name (ensure-date-prefix improved-name))
                       (resolved-parent (resolve-task-id node-id)))
                  ;; Create the child task directory
                  (task:ensure-task-directory full-name)
                  ;; Emit create event in child context
                  (setf *current-task-id* full-name)
                  (emit-event :task.create
                              (list :name full-name
                                    :description description
                                    :parent resolved-parent))
                  ;; Emit fork event in parent context
                  (setf *current-task-id* resolved-parent)
                  (emit-event :task.fork
                              (list :child-id full-name :edge-type edge-type))
                  full-name)))

             (otherwise
              (error "Unknown mutation operation: ~A" op))))
      ;; Restore *current-task-id* and persist to registry.
      ;; *in-mutation* LET binding has ended, so save-session-context is
      ;; NOT suppressed — the registry receives the correct pre-mutation
      ;; task ID, not any transient intermediate value.
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
  (cond
    ((or (null query)
         (and (stringp query) (zerop (length query))))
     (make-text-content
      (format nil
              "task_query: required argument `query` is missing or empty.~%~
               Send it as {\"query\": \"<TQ expression>\"}, ~
               e.g. {\"query\": \"(-> :all :count)\"}.")))
    (t
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
         (make-text-content "Error: ~A" c))))))
