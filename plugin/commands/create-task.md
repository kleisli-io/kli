---
description: Scaffold a task with event-sourced tracking via the task MCP server
argument-hint: "<task-name or description>"
allowed-tools: mcp__task__*, AskUserQuestion
---

# Create Task

Scaffold an event-sourced task with the task MCP server. This is for quick, focused work that doesn't need the full KLI research/plan/implement cycle.

## When to Use This

- Quick bug fixes, small features, one-off investigations
- Work you want tracked but don't need formal phases for
- Creating parent tasks to organize subtasks later
- Linking new work to existing tasks in the graph

## Process

### Step 1: Parse Arguments

Parse $ARGUMENTS to determine intent:

| Pattern | Mode | Behavior |
|---------|------|----------|
| No args | Interactive | Ask what the task is about |
| Short name (1-4 words) | Direct | Use as task name, infer description |
| Sentence/description | Infer | Extract a kebab-case name, use input as description |

**Name convention**: kebab-case, descriptive, no date prefix (auto-added by the server as `YYYY-MM-DD-<name>`).

Names are validated for descriptiveness. The server rejects meaningless names.

**Good names** (pass validation):
- `fix-login-redirect` - verb + object
- `add-retry-logic-to-api-client` - descriptive action
- `research-caching-strategies` - clear intent

**Bad names** (rejected):
- `P1`, `P2` - letter+number only
- `phase-1` - no semantic content after prefix
- `stuff`, `misc`, `wip` - vague words
- `foo`, `bar` - too short

If no arguments provided, ask:

```
What are you working on?

Describe the task briefly - I'll create a tracked task for it.
```

### Step 2: Check Context

Before creating, gather context:

1. **Check for active task**: Call `task_get()` (no args) to see if a current task is already set
2. **If active task exists**: Ask whether this new task should be:
   - A **subtask** (phase-of the current task)
   - A **related** task (linked but independent)
   - A **standalone** task (no connection)

This avoids orphaned tasks and keeps the graph connected.

### Step 3: Create the Task

Based on Step 2:

**Standalone or no parent context:**
```
Call task_create(name="<kebab-name>", description="<description>")
```

**Subtask of existing task:**
```
Call task_fork(name="<kebab-name>", from="<parent-id>", edge_type="phase-of", description="<description>")
Then call task_set_current(task_id="<new-task-id>") to switch context
```

**Related to existing task:**
```
Call task_create(name="<kebab-name>", description="<description>")
Then call task_link(target_id="<related-id>", edge_type="related-to")
```

### Step 4: Set Metadata

Set useful metadata on the new task:

```
Call task_set_metadata(key="tags", value="<comma-separated relevant tags>")
```

Infer tags from the description. Common tags: `bugfix`, `feature`, `refactor`, `investigation`, `infrastructure`, `nix`, `lisp`, `mcp`, `dashboard`, `shell`.

If the task has a clear scope, also set:
```
Call task_set_metadata(key="scope", value="<component or area>")
```

### Step 5: Record Initial Observation

Record context that will be useful when reviewing this task later:

```
Call observe(text="Created via /create-task. <any additional context from the conversation>")
```

Include relevant context like:
- What triggered this task (error message, user request, discovery during other work)
- Key files or components likely involved
- Any constraints or decisions already made

### Step 6: Report

Present the created task:

```
Task created: <full-name-with-date>

Description: <description>
Tags: <tags>
Parent: <parent if applicable>
Link: <relationship if applicable>

The task is now your active context. All observations, artifacts, and
metadata will be tracked in the event stream.

When done, mark complete with task_complete() or hand off with /kli:handoff.
```

## Error Handling

| Error | Response |
|-------|----------|
| Task MCP unavailable | "Task MCP server not responding. Have you run `kli init` in this project?" |
| Name too vague | Ask for a more descriptive name |
| Duplicate name | The date prefix usually prevents this, but if it happens, suggest appending a disambiguator |
| Parent task not found | List available tasks with `task_list()` and let user pick |

## Guidelines

- Task names should be self-documenting: someone reading the task list should understand what each task is about
- Don't over-tag: 2-4 tags is plenty
- Always check for an active parent before creating standalone tasks
- The initial observation is important: it's the first thing someone sees when bootstrapping the task later
- This command does NOT enter plan mode or create research documents. It creates a tracked task and sets context. The user decides what to do next.
