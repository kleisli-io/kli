---
description: Reflect on completed task and evolve playbooks
allowed-tools: mcp__task__*, Task, Read
---

# Reflect on Task

Extract learnings from completed tasks by orchestrating the reflector and curator agents. The event stream IS the observation source — no separate observation files needed.

> **Architecture Note**: This command orchestrates the reflection workflow. The kli-reflection skill provides the methodology (WHAT to evaluate). This command provides the orchestration (HOW to execute).

**The kli-reflection skill provides comprehensive guidance on:**
- Sequential execution pattern (reflector → curator)
- Observation analysis methodology
- Pattern effectiveness evaluation criteria
- Harm signal tier definitions (Tier 1/2/3 with responses)
- Evidence-based learning principles
- New pattern discovery process
- Reflection artifact structure

IMPORTANT: Before starting this workflow you ABSOLUTELY NEED to load the kli-reflection skill, NO EXCEPTIONS.

## Workflow Overview

```
┌──────────────────────────────────────────────────────────────┐
│                    /reflect WORKFLOW                          │
├──────────────────────────────────────────────────────────────┤
│ 1. GATHER STATE    (task_get + timeline for observations)    │
│ 2. REFLECTOR       (analyze observations from event stream)  │
│ 3. CURATOR         (update playbooks via MCP tools)          │
│ 4. REPORT          (combined summary)                        │
└──────────────────────────────────────────────────────────────┘

Task Isolation: Each task is reflected independently.
Cross-cutting knowledge emerges through playbook accumulation.

Two feedback pathways feed into playbooks:
- Curator: Analysis-based updates from reflection.md via PQ mutations
- Real-time: Feedback given via `(:feedback! ...)` during work
```

## Initial Response

When this command is invoked:

**1. Set up task context:**
- If task name provided: `task_bootstrap(task_id)`
- If no parameter: Call `task_get()` to check current task. If none, ask user.

**2. Verify task has sufficient evidence:**

```
task_get()           → Check observations, artifacts, metadata, phase
timeline(limit=50)   → Get full event history with observations
task_graph(query="plan") → Check plan completion status
```

**If task has observations** (from `observe()` calls during work):
- Proceed with reflection — the event stream contains the evidence

**If task has NO observations:**
```
This task has no recorded observations.

Observations are recorded via observe() during KLI commands (/research, /plan, /implement).
Without observations, there's insufficient evidence for pattern effectiveness analysis.

Options:
1. Reflect on what artifacts exist (reduced analysis)
2. Skip reflection for this task
```

**Note**: Not all tasks go through every phase. Simple tasks may skip research. Verify what evidence exists and proceed with available data.

## Step 0.5: Gather Graph Context (Optional)

For complex tasks with many phases, spawn graph-analyst first to get comprehensive graph state:

```
Task(
    subagent_type="graph-analyst",
    prompt='{"question": "What is the complete state of task <task_id>? Include all phases, their status, and any related tasks."}',
    description="Get comprehensive task graph state"
)
```

Pass this graph context to the reflector agent for more informed analysis.

**When to use:**
- Task has 5+ phases
- Task has cross-task dependencies
- Multiple patterns were activated during the task

**When to skip:**
- Simple tasks with 1-3 phases
- No cross-task relationships

## Step 1: Orchestrate Reflector Agent

**Spawn reflector agent as Task:**

Use Task tool with subagent_type: "reflector"

Pass parameters in prompt:
```
Analyze completed task and produce reflection artifact.

Task ID: <task_id>
Task directory: <task_dir from task_get>
Context: <brief task description from task_get>

To get the task's observations and evidence, use these MCP tools:
- task_set_current("<task_id>") to set context
- task_get() to get state with observations (last 3 shown)
- timeline(limit=50) to get ALL observations and events
- task_graph(query="plan") to see phase completion

Also read any artifacts listed in task_get output (research.md, plan.md, etc.)

Evaluate pattern effectiveness with evidence from observations.

Classify harm signals into tiers:
- Tier 1 (Auto-Action): outcome=FAILURE, explicit rejection → auto-increment harmful
- Tier 2 (Flag for Review): excessive iterations, implicit correction → increment with review note
- Tier 3 (Track Only): minor iterations, context mismatch → track but no counter change

Identify new patterns discovered during task.

Generate reflection.md artifact in the task directory with:
- Complete frontmatter
- Patterns applied and effectiveness
- Harm Signals section (tiered)
- Challenges and resolutions
- New patterns discovered
- Playbook update recommendations

Return summary when complete.
```

**Wait for reflector agent to complete.**

## Step 2: Orchestrate Curator Agent

**After reflector returns, spawn curator agent as Task:**

Use Task tool with subagent_type: "curator"

Pass parameters in prompt:
```
Update playbooks based on reflection artifact.

Task directory: <task_dir>
Reflection: <task_dir>/reflection.md

Read reflection.md recommendations.

Update playbook using PQ mutations:
- `(-> (pattern "id") (:feedback! :helpful "evidence"))` for effective patterns
- `(-> (pattern "id") (:feedback! :harmful "evidence"))` for misleading patterns
- `(add! :domain :X :content "...")` for new patterns discovered
- `(-> (pattern "id") (:evolve! "new content" :reason "why"))` for pattern description updates

Process harm signals by tier:
- Tier 1: `(:feedback! :harmful ...)`
- Tier 2: `(:feedback! :harmful ...)` with review note in evidence
- Tier 3: Track only (no feedback call)

Return summary of all changes made.
```

**Wait for curator agent to complete.**

## Step 3: Record and Present Results

```
task_set_current("<original_task_id>")
observe("Reflection complete: <N> patterns evaluated, <M> helpful, <K> harmful, <L> new patterns added")
```

Present to user:
```
Reflection complete!

**Task:** <task_id>

## Reflection Analysis
- Reflection: <task_dir>/reflection.md
- Patterns evaluated: <N> patterns
- Harm signals detected:
  - Tier 1 (auto-action): <N>
  - Tier 2 (flagged): <M>
  - Tier 3 (tracked): <K>

## Playbook Updates (Curator)
- Helpful incremented: <N> patterns
- Harmful incremented: <M> patterns
- New patterns added: <K> patterns

Review reflection.md for full analysis.
```

## Important Notes

- **Task isolation** — each task is reflected independently
- **Event stream is source of truth** — observations from `observe()` calls, surfaced by `task_get()` and `timeline()`
- **No observation files required** — observations flow through the task event stream via `observe()`
- **Sequential execution** — Reflector → Curator (dependencies)
- **Playbook updates via PQ** — `(:feedback! ...)`, `(add! ...)`, `(:evolve! ...)` (not file edits)
- **Cross-cutting knowledge** — emerges through playbook accumulation

## Error Handling

**If task has no observations:**
- Offer reduced analysis from artifacts only
- Or skip reflection

**If reflector agent fails:**
- Present error details
- Offer to retry

**If curator agent fails:**
- Note that reflection.md was created
- Offer to run curator manually or apply updates via playbook MCP tools directly

## Remember

You are an **orchestrator** for per-task reflection. Key responsibilities:
1. Gather task state and observations via `task_get()` + `timeline()`
2. Delegate analysis to reflector (reads event stream)
3. Delegate playbook updates to curator (uses playbook MCP tools)
4. Record results via `observe()`
5. Present combined results

Cross-cutting knowledge accumulates in playbooks over many reflections.
