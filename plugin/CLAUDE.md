# kli — Task Management and Pattern Playbooks

## Playbook System

Pattern format: `[domain-NNNNNN] helpful=N harmful=M :: content`

**Workflow:** Search before implementing, give feedback after applying.

## Task Model

Tasks are event-sourced entities tracked via the task MCP server. Plans are task DAGs navigated via TQ (Task Query) expressions.

### Tasks Are Event-Sourced

Each task is a directory containing `events.jsonl` — an append-only log of typed events:

| Event Type | Purpose | Key Fields |
|------------|---------|------------|
| `task.create` | Task birth certificate | name, description, parent |
| `task.fork` | Create child task | child-id, edge-type |
| `task.link` | Add graph edge | target-id, edge-type |
| `task.sever` | Remove graph edge | target-id, edge-type |
| `task.update-status` | Status change | status (active/completed) |
| `task.set-metadata` | Set key-value | key, value |
| `observation` | Record finding | text |
| `artifact` | Register file | path, description |
| `session.join` | Session attached | session-id |
| `handoff.create` | Handoff document | path |

**State is computed by folding events** — no mutable state files.

### Task Identity

Tasks have qualified IDs: `depot:local-name` (e.g., `core:2026-02-02-fix-login`).

### Plans as Task DAGs

Plans are directed acyclic graphs of phase tasks connected by `phase-of` edges.

**Create plans with scaffold-plan!:**

```lisp
task_query("(scaffold-plan!
  (implement-core \"Core data structures\")
  (add-integration \"Integration layer\" :after implement-core)
  (write-tests \"Test coverage\" :after add-integration))")
```

**Navigate plans:**

```lisp
task_query("(query \"plan\")")        ; View plan with status
task_query("(query \"plan-ready\")")  ; Find ready phases
```

## TQ Quick Reference

TQ (Task Query) is a pipeline-based language for querying and mutating the task graph.

| Expression | Returns | Use Case |
|------------|---------|----------|
| `:all` | All tasks | Statistics |
| `(active)` | Event-sourced tasks | Working tasks |
| `(current)` | Current task | Context-aware queries |
| `(node "pattern")` | Tasks matching pattern | Find specific tasks |
| `(query "plan")` | Phases of current task | Plan navigation |

**Pipeline:** `(-> source step1 step2 ...)`

| Step | Purpose |
|------|---------|
| `(:follow :edge-type)` | Traverse forward edges |
| `(:back :edge-type)` | Traverse backward |
| `(:where pred)` | Filter |
| `:enrich` | Load full state |
| `:ids` | Extract IDs |
| `:count` | Count |
| `(:take n)` | Limit |

**Mutations (! suffix):**

| Mutation | Effect |
|----------|--------|
| `(:complete!)` | Mark completed |
| `(:observe! "text")` | Add observation |
| `(:set-meta! :key "val")` | Set metadata |
| `(:link! "id" :type)` | Add edge |
| `(:sever-from-parent! :type)` | Bulk sever |
| `(scaffold-plan! ...)` | Create plan phases |

## PQ Quick Reference

PQ (Playbook Query) queries the pattern graph:

| Operation | PQ Syntax |
|-----------|-----------|
| **Activate** | `(-> (activate "query" :boost (lisp nix)) (:take 5))` |
| **Search** | `(-> (search "query") (:take 5))` |
| **Feedback** | `(-> (pattern "id") (:feedback! :helpful "evidence"))` |
| **Add** | `(add! :domain :lisp :content "When X, do Y")` |
| **Proven** | `(-> (proven :min 3) (:take 10))` |

## Workflow Integration

| Command | Purpose |
|---------|---------|
| `/kli:research` | Document codebase through iterative research |
| `/kli:plan` | Create implementation plans with phase DAGs |
| `/kli:implement` | Execute plan phase-by-phase with TDD |
| `/kli:reflect` | Extract patterns from completed work |
| `/kli:handoff` | Create handoff for session transfer |
| `/kli:resume-handoff` | Resume from handoff document |
| `/kli:commit` | Context-aware git commits |
| `/kli:create-task` | Create event-sourced task |
| `/kli:resume-task` | Resume work on existing task |

## Observation Capture

During active work, record discoveries as observations — not patterns:

```
observe("Discovery: <what happened with evidence>")
observe("Constraint: <what failed and why>")
```

Patterns are extracted during reflection (`/kli:reflect`) through the Generator → Reflector → Curator pipeline.

## Edge Types

| Edge Type | Meaning |
|-----------|---------|
| `phase-of` | Child is phase of parent plan |
| `depends-on` | Must complete before starting |
| `related-to` | Conceptually related |
| `blocks` | Prevents progress |
