---
description: Create detailed implementation plans through iterative planning with artifact reuse
allowed-tools: mcp__task__*, Read, Write, Task, AskUserQuestion
---

# Implementation Plan

Create detailed, phased implementation plans as task DAGs following KLI methodology.

**The kli-planning skill provides comprehensive guidance on:**
- Research artifact reuse patterns
- Phase design principles (incremental, testable, clear boundaries, 3-7 phases optimal)
- Clarifying question templates (structured with context and options)
- Verification gate patterns (automated + manual, both required)
- Out-of-scope definition strategies
- Phase boundary specification
- Success criteria definition (automated + manual)

## Initial Setup

**When invoked without arguments:**
```
What would you like to create a plan for?

Examples:
- "Add WebSocket support to the API"
- "Refactor the CSS build pipeline"
- "Based on the research task, plan the migration"

What would you like to plan?
```

Wait for user input.

**When invoked with arguments:**
Goal is `$ARGUMENTS`. Proceed to planning.

### Task Setup

Call `task_get()` to check if there's already a current task. If so, use it. If not, create one:

```
task_create(name="plan-<goal>")
task_set_metadata(key="goals", value='["Create phased implementation plan for <goal>"]')
task_set_metadata(key="phase", value="planning")
```

Then call `task_get()` to retrieve the full task state — check for existing artifacts (research.md) and observations.

### Check Existing State

**If the task already has phase children** (check `task_query("(query \"plan\")")`):
```
Found existing plan with N phases (M complete, K pending).

Options:
1. Iterate on plan (modify phases)
2. Start fresh (create new task)

Which would you prefer?
```

**If `task_get()` shows a `research.md` artifact:**
- Read it fully — findings become foundations for the plan
- Token savings: Reusing research.md saves 40-50% tokens vs spawning duplicate sub-agents

## Planning Process

### Step 1: Handle Research Artifact (If Exists)

Read research.md FULLY (no limit/offset). Extract summary, findings, code references, playbook patterns, open questions. Present to user.

If no research: Gather current codebase state by spawning codebase-locator/analyzer as needed.

### Step 2: Activate Playbook Patterns (REQUIRED)

**Before planning**, activate relevant patterns:
```lisp
pq_query('(-> (activate "<planning task description>" :boost (<domain1> <domain2>)) (:take 5))')
```

This uses graph-based retrieval to find patterns for implementation approach, phasing strategies, and verification patterns. The activation is persisted for handoff continuity.

### Step 2.5: Discover Related Prior Work (Optional)

Spawn graph-analyst to find relevant prior tasks:

```
Task(
    subagent_type="graph-analyst",
    prompt='{"question": "What prior tasks relate to <planning goal>? Are there patterns or learnings I should consider?"}',
    description="Find related prior work"
)
```

This surfaces:
- Similar tasks that succeeded or failed
- Patterns that were helpful or harmful for similar work
- Potential dependencies or conflicts with existing work

**When to use:** If the planning goal involves work that may have been attempted before or relates to existing infrastructure.

**When to skip:** If this is clearly novel work with no prior history (e.g., integrating a brand new library).

### Step 3: Decompose into Phases

Break work into incremental phases with clear boundaries. Each phase independently testable.

**Phase design guidance:** See kli-planning skill for:
- Optimal phase count (3-7 phases)
- Phase boundary criteria
- Incremental delivery patterns
- Dependency management

### Step 4: Define Success Criteria

For each phase, specify automated verification (build, tests, TODO check) and manual verification (UI/UX, performance, acceptance).

### Step 5: Ask Clarifying Questions

List uncertainties requiring user input. For each, provide context and concrete options. Wait for responses. Update plan based on answers.

**When to iterate:** See kli-planning skill for exit criteria vs continue criteria.

### Step 6: Define Out-of-Scope

Explicitly list what's NOT being done to prevent scope creep.

### Step 7: Create Plan as Task DAG

Plans are task DAGs, not markdown files. Use TQ's `scaffold-plan!` for efficient creation.

**Present the plan outline to user for approval first.** Then create the DAG:

**Option 1: scaffold-plan! (Recommended for plans with dependencies)**
```
task_query("(scaffold-plan!
  (implement-core-library \"Core library with API surface\")
  (add-integration-layer \"Integration with existing system\" :after implement-core-library)
  (write-test-suite \"Comprehensive test coverage\" :after add-integration-layer))")
```
Creates all phases with dependencies in one expression. Names are validated for descriptiveness.

**Auto-improvement:** Short names like `p1` are auto-improved from descriptions:
- `(p1 "Research architecture")` → creates `research-architecture`

**Option 2: scaffold-chain! (For linear phase sequences)**
```
task_query("(scaffold-chain! \"Setup infrastructure\" \"Implement core logic\" \"Add test coverage\")")
```
Creates a linear dependency chain automatically.

**Option 3: task_fork (For complex custom structures)**
```
task_fork(name="implement-user-authentication", from=current_task_id, edge_type="phase-of",
          description="Implement OAuth2 flow\n\nChanges Required:\n- ...\n\nSuccess Criteria:\n- ...")
```
Use when you need more control over task naming or descriptions. Names are validated.

Verify the DAG: `task_query("(query \"plan\")")` — shows all phases with status, dependencies, and enriched fields (including `:alpha`, `:affinity` for Markov-aware ranking).

**Optionally write plan.md** as a human-readable artifact if the plan is complex enough to warrant it. The task DAG is the source of truth.

### Step 8: Record Planning Decisions

```
observe("Plan complete: N phases created. Key decisions: <decisions>. Open questions resolved: <questions>.")
```

### Step 9: Pattern Feedback

Give feedback on patterns that informed the plan:

```lisp
pq_query('(-> (pattern "<pattern-id>") (:feedback! :helpful "informed phase structure for X"))')
pq_query('(-> (pattern "<pattern-id>") (:feedback! :harmful "didnt apply to this planning context"))')
```

Record planning insights as observations (patterns are promoted during `/kli:reflect`):
```
observe("Planning insight: <description>. Evidence: plan phasing approach")
```

### Step 10: Present Plan to User

```
## Plan Complete: <Goal>

**Phases**: N phases created as task DAG

Plan DAG:
<output of task_query("(query \"plan\")")>

Next step: `/kli:implement` to execute the phases.
Use `task_query("(query \"plan-ready\")")` to see which phases are ready.
```

### Step 11: Iterate Plan (If Needed)

If user requests changes, modify the DAG and record via `observe()`:

**Add phases:**
```lisp
task_fork(name="new-phase", from=current_task_id, edge_type="phase-of", description="...")
```

**Remove phases (bulk sever):**
```lisp
;; Single phase
task_query("(-> (node \"obsolete-phase\") (:sever-from-parent! :phase-of))")

;; Multiple phases at once
task_query("(-> (node \"phase-1\" \"phase-2\") (:sever-from-parent! :phase-of))")
```

**Add dependencies:**
```lisp
task_query("(-> (node \"phase-2\") (:link! \"phase-1\" :depends-on))")
```

Record all changes: `observe("Plan iteration: <what changed and why>")`

## Resuming a Plan

1. `task_bootstrap(task_id)` — restores full context
2. `task_query("(query \"plan\")")` — see all phases with status
3. `task_query("(query \"plan-ready\")")` — see which phases are ready (non-completed)
4. Continue from appropriate step

## Remember

- **Plans are task DAGs** — use `scaffold-plan!` or `task_fork` to create phases
- **task_query("(query \"plan\")")** is the source of truth, not plan.md
- **Reuse research.md** if available (saves 40-50% tokens)
- Ask clarifying questions for ANY ambiguity
- Design phases following principles from kli-planning skill
- Both automated AND manual verification required for each phase
- Define out-of-scope explicitly
- **Give feedback** on patterns that informed the plan
- **Record decisions** via `observe()` — observations flow through the task event stream
- Get user confirmation before finalizing plan

## See Also

- CLAUDE.md - Task model, PQ/TQ reference, playbook workflow
