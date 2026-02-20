---
description: Resume work on a task by gathering context from event stream and graph state
argument-hint: "<task-id or empty for current>"
allowed-tools: mcp__task__*, Read, Glob, Task
---

# Resume Task

Resume work on a task by gathering context from the task MCP server's event stream, graph state, and artifacts. Unlike `/kli:resume_handoff` which requires a handoff document, this command works directly with the task's live state.

## When to Use

- **No handoff exists** — work was interrupted without creating a handoff
- **Picking up where you left off** — same session or new session
- **Checking task status** — understand what's been done and what's next
- **Exploring a task** — unfamiliar with a task and need context

Use `/kli:resume_handoff` instead when a handoff document exists and you want to follow its specific guidance.

## Initial Response

When this command is invoked:

### 1. If task ID provided

**Example**: `/core:resume-task 2026-01-31-coalgebraic-task-infrastructure`

- Bootstrap the task: `task_bootstrap(task_id="2026-01-31-coalgebraic-task-infrastructure")`
- This single call sets current task, emits session.join, and returns:
  - Full computed state (description, observations, artifacts, metadata)
  - Graph neighbors (related tasks, dependencies)
  - Playbook query (enriched semantic query)
  - Handoff document (if one exists)
- Proceed to context gathering

### 2. If no parameter provided

**Example**: `/core:resume-task`

**Step 1: Check Session Start Context**

The session start hook injects task context at conversation startup. Look for:

```
TASK[1]{dir,phase,last_artifact}:
 <task-dir>,Phase 2: Implementation,research.md
```

If `TASK[1]` exists:
- Extract task ID from the first field (e.g., `2025-12-12-task-name`)
- Bootstrap: `task_bootstrap(task_id="2025-12-12-task-name")`
- Proceed to context gathering

**Step 2: Check for Current Task**

If no session context, call `task_get()` to check if a current task is already set.

If a task is current:
- Announce: "Resuming current task: `<task-id>`"
- Proceed to context gathering

**Step 3: List Available Tasks**

If no current task, use TQ to find recent active tasks:

```
task_query('(-> (query "recent") (:take 10) (:select :display-name :crdt-status :obs-count :alpha :affinity))')
```

Present the list and ask which task to resume.

## Context Gathering

Once a task is identified, gather comprehensive context using task MCP tools.

### Step 1: Get Core State

The `task_bootstrap` call already provides:
- **State**: description, status, claim, sessions, observations, artifacts, metadata
- **Neighbors**: typed edges to related tasks
- **Playbook query**: enriched semantic query for pattern activation

If you used `task_set_current` + `task_get` separately, you have the same information.

### Step 2: Get Timeline

Retrieve recent events to understand activity:

```
timeline(limit=30)
```

This shows:
- Recent observations
- Session joins/leaves
- Artifact registrations
- Metadata changes
- Handoff creations (`:handoff.create` events)

### Step 3: Check Plan Structure (If Task Has Phases)

If the task has children (phases), query the plan:

```
task_graph(query="plan")           # Full plan structure
task_graph(query="plan-frontier")  # Which phases are ready
```

This reveals:
- Phase completion status (completed vs active)
- Dependency ordering
- Which phases are unblocked and ready to work on (ranked by affinity score)
- Any blocked phases waiting on dependencies

### Step 4: Check Graph Health

Query task health to identify issues:

```
task_health()
```

Or spawn graph-analyst for deeper analysis:

```
Task(
    subagent_type="graph-analyst",
    prompt='{"question": "What is the current state of task <task_id>? Are there stale phases, blocked work, or issues I should know about?"}',
    description="Analyze task graph state"
)
```

### Step 5: Activate Relevant Patterns

Use the enriched query from bootstrap to get relevant playbook patterns:

```
pq_query('(-> (activate "<enriched_query>" :boost (<domain1> <domain2>)) (:take 5))')
```

This surfaces patterns that are semantically relevant to this task's topic and its graph neighbors.

### Step 6: Read Critical Artifacts

If the task has registered artifacts, read them:
- `research.md` — prior research findings
- `plan.md` — detailed plan document (if exists alongside DAG)
- Recent handoffs — if `:handoff.create` events exist in timeline

Read artifacts FULLY without limit/offset to get complete context.

## Synthesis and Presentation

Present your analysis to the user:

```
## Task: [Task Name]

**Status**: [active/completed] | **Claim**: [held by session/unclaimed]
**Created**: [date] | **Sessions**: [count]

### Goals
[from metadata.goals]

### Current State

[Summary of what has been accomplished based on observations and artifacts]

### Plan Progress (if phases exist)

[X/Y] phases complete | [Z] ready to work on

**Completed:**
- ✓ Phase 1: [name]
- ✓ Phase 2: [name]

**Ready:**
- ○ Phase 3: [name] — [brief description]

**Blocked:**
- ○ Phase 4: [name] — waiting on Phase 3

### Recent Activity

[Last 3-5 significant events from timeline]

### Relevant Patterns

[Top 2-3 patterns from playbook activation]

### Artifacts

[List of registered artifacts with brief descriptions]

### Graph Context

[Related tasks, dependencies, what this enables]

### Recommended Next Steps

Based on the task state, I recommend:

1. **[Most logical next action]** — [why]
2. **[Second priority]** — [why]
3. **[Additional consideration]** — [why]

Shall I proceed with [recommended action 1]?
```

Get user confirmation before taking action.

## Special Cases

### Task Has No Observations

```
This task was created but has no recorded observations yet.

Goals: [from metadata]
Description: [from state]

Would you like me to:
1. Start researching this task (/kli:research)
2. Create a plan (/kli:plan)
3. Just observe the current state and proceed
```

### Task Is Completed

```
This task is marked as completed.

Completed at: [timestamp if available]
Sessions: [list]
Observations: [count]
Artifacts: [list]

To continue working on it, I would need to reopen it first.

Would you like me to:
1. Reopen the task and continue
2. Create a new related task for follow-up work
3. Just review the completed work
```

### Task Has Handoff Documents

If `:handoff.create` events exist in timeline:

```
This task has [N] handoff document(s):
- [path1] — [timestamp]
- [path2] — [timestamp]

Would you like me to:
1. Resume from the latest handoff (/kli:resume_handoff)
2. Ignore handoffs and work from live task state
```

### Task Is Stale

If task hasn't had activity in a long time (check session timestamps):

```
This task hasn't been worked on since [date].

The codebase may have changed significantly. Consider:
1. Re-validating any existing plan
2. Re-checking file references in artifacts
3. Running /kli:validate if implementation was in progress
```

## Guidelines

1. **Task Bootstrap is Canonical**
   - `task_bootstrap` is the primary entry point — it does everything in one call
   - Use it instead of multiple separate calls when starting fresh

2. **TQ for Complex Queries**
   - Use `task_query` for complex graph traversals
   - Examples:
     - `(-> (current) (:follow :phase-of) :ids)` — get phases of current task
     - `(-> (current) (:back :depends-on) :ids)` — what depends on this task
     - `(-> (query "plan-ready") :enrich (:sort :affinity) (:take 3))` — ready phases ranked by affinity

3. **Timeline Over Artifacts**
   - The timeline is the source of truth
   - Artifacts are useful but observations in the event stream are more current

4. **Don't Assume Handoff State**
   - Unlike resume_handoff, don't expect handoff document guidance
   - Work from the live task state

5. **Interactive Confirmation**
   - Always present analysis before taking action
   - Let user choose the next step
   - Don't auto-proceed with implementation

## Comparison with resume_handoff

| Aspect | resume-task | resume_handoff |
|--------|-------------|----------------|
| Input | Task ID or current task | Handoff document path |
| Source of truth | Event stream + graph | Handoff markdown |
| Guidance | Inferred from state | Explicit next steps |
| When to use | No handoff exists | Handoff was created |
| Context depth | Comprehensive (all tools) | Focused (handoff content) |

## Integration with Workflow

```
[Task Created]
     ↓
/kli:research → /kli:plan → /kli:implement → /kli:validate → /kli:reflect
     ↑                                              ↓
     └──────────── /core:resume-task ←─────────────┘
                  (re-enter anywhere)
```

`/core:resume-task` is the universal re-entry point for any task, at any stage.
