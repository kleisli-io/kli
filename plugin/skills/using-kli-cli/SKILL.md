---
name: using-kli-cli
description: Use the kli CLI to interact with task graphs via Bash when MCP tools are unavailable. Teaches kli <tool> --task <id> syntax for observations, queries, and task management. Use when teammates need task graph access or MCP is unavailable.
allowed-tools: Bash(kli:*)
---

# Using KLI CLI

The `kli` binary exposes every MCP tool as a CLI subcommand. This is the primary way for **teammates** (agents spawned via Task tool) to interact with the persistent task graph, since MCP tools are not available to teammates.

## Quick Start

```bash
# Record an observation
kli observe "Found the bug in parser.lisp:42" --task 2026-02-22-my-task

# Get task state
kli task_get --task 2026-02-22-my-task

# Search observations
kli obs_search "parser bug" --task 2026-02-22-my-task

# List all tasks (default: 50 most recent)
kli task_list
```

## Syntax

```
kli <tool_name> [positional_args...] [--named_arg value...] [--task <task_id>]
```

- **Positional args** fill required parameters in schema declaration order
- **Named args** use `--param_name value` syntax
- **`--task <id>`** sets task context before executing (equivalent to `task_set_current`)
- **Stdin pipe** for large text: `echo "long text" | kli observe --task <id>`

## Essential Commands for Teammates

### Recording Work

```bash
# Record observation (most common teammate action)
kli observe "Discovery: the auth module uses JWT not sessions" --task <id>

# Pipe large content
echo "Multi-line finding about
the codebase architecture" | kli observe --task <id>
```

### Reading Context

```bash
# Get full task state (description, observations, edges)
kli task_get --task <id>

# Search observations by meaning
kli obs_search "authentication flow" --task <id>

# View recent events
kli timeline --limit 20 --task <id>

# Get enriched retrieval (graph-aware observation search)
kli enriched_retrieve --task <id>
```

### Task Management

```bash
# List tasks (default 50, use --limit 0 for all)
kli task_list
kli task_list --grouped true

# Set metadata on task
kli task_set_metadata phase implementation --task <id>

# Mark task complete
kli task_complete --task <id>

# Create handoff document
kli handoff "Completed auth module implementation" --task <id>
```

### Querying

```bash
# Task query (TQ)
kli task_query '(-> (query "plan") :enrich (:select :display-name :crdt-status))'

# Pattern query (PQ)
kli pq_query '(-> (search "authentication") (:take 5))'
```

## Discovery

```bash
# List all available tools
kli help

# Get help for a specific tool
kli <tool_name> --help

# Example
kli observe --help
```

## For Team Leads

When spawning teammates that need task graph access, include the `--task` flag pattern in the task description:

```
Task(
  prompt="Research the auth module. Record findings with:
    kli observe 'your finding here' --task 2026-02-22-auth-research
  When done:
    kli task_complete --task 2026-02-22-auth-research",
  subagent_type="general-purpose"
)
```

Teammates have `Bash(kli:*)` permission by default — no extra configuration needed.

---

## Full Tool Reference

All 31 tools organized by category. Each tool shows its positional args in order, then named args with `--`.

### Recording & Feedback

#### `observe`
Record an observation for the current task.
```bash
kli observe <text> [--task <id>]
kli observe "Auth uses JWT tokens, not sessions" --task my-task

# Pipe large content from stdin
echo "detailed analysis..." | kli observe --task my-task
```

#### `obs_feedback`
Record whether an observation was helpful. Improves future search ranking.
```bash
kli obs_feedback <text> <outcome>
kli obs_feedback "Auth uses JWT tokens" success
kli obs_feedback "Misleading note about sessions" failure
```
- `outcome`: `success` or `failure`

#### `handoff`
Generate a handoff document for the current task.
```bash
kli handoff <summary> [--task <id>]
kli handoff "Completed auth module, tests passing" --task my-task
```

### Searching & Retrieval

#### `obs_search`
Search observations by meaning and keywords. Returns ranked results.
```bash
kli obs_search <query> [--k <int>] [--task_id <id>]
kli obs_search "authentication flow" --task my-task
kli obs_search "parser bug" --k 5
```

#### `enriched_retrieve`
Graph-aware observation search. Finds observations from this task and related tasks.
```bash
kli enriched_retrieve [--task_id <id>] [--k <int>]
kli enriched_retrieve --task_id my-task --k 10
```

#### `timeline`
Show recent events for a task.
```bash
kli timeline [--task_id <id>] [--limit <int>]
kli timeline --task_id my-task --limit 20
```

### Task Lifecycle

#### `task_create`
Create a new top-level task.
```bash
kli task_create <name> [--description <str>] [--depot <str>]
kli task_create 2026-02-22-auth-research --description "Research auth patterns"
```

#### `task_fork`
Create a subtask linked to a parent.
```bash
kli task_fork <name> [--from <parent_id>] [--edge_type <type>] [--description <str>] [--depot <str>]
kli task_fork phase-1-setup --from my-parent-task --description "Initial setup"
```
- `edge_type` defaults to `phase-of`. Use `related-to` for cross-depot references.

#### `spawn`
Spawn a child task from the current task (shorthand for fork from current).
```bash
kli spawn <name> [--reason <str>] [--task <id>]
kli spawn investigate-perf --reason "Unexpected slowdown in queries" --task my-task
```

#### `task_complete`
Mark a task as completed. Completed tasks reject further mutations.
```bash
kli task_complete [--task_id <id>]
kli task_complete --task_id my-task
```

#### `task_reopen`
Reopen a completed task, allowing mutations again.
```bash
kli task_reopen [--task_id <id>]
kli task_reopen --task_id my-task
```

### Task Context

#### `task_bootstrap`
Bootstrap full task context in one call. Sets current task, returns state, neighbors, playbook patterns, handoff, and swarm awareness.
```bash
kli task_bootstrap <task_id>
kli task_bootstrap 2026-02-22-auth-research
```

#### `task_set_current`
Lightweight context switch. Sets current task with minimal output.
```bash
kli task_set_current <task_id>
kli task_set_current my-task
```

#### `task_get`
Get computed state for a task (read-only, does not change current task).
```bash
kli task_get [--task_id <id>]
kli task_get --task_id my-task
```

#### `task_claim`
Claim exclusive ownership of a task for current session. Use for phases that modify shared files.
```bash
kli task_claim [--task_id <id>]
kli task_claim --task_id my-phase
```

#### `task_release`
Release claim on a task.
```bash
kli task_release [--task_id <id>]
kli task_release --task_id my-phase
```

#### `task_set_metadata`
Set a metadata key-value pair on the current task.
```bash
kli task_set_metadata <key> <value> [--task <id>]
kli task_set_metadata phase implementation --task my-task
kli task_set_metadata tags "auth,security" --task my-task
```
Convention keys: `display-name`, `goals` (JSON array), `scope`, `phase`, `tags` (comma-separated), `depends-on` (JSON array), `enables` (JSON array), `related-to` (JSON array).

### Graph Edges

#### `task_link`
Create a typed edge from current task to target.
```bash
kli task_link <target_id> <edge_type> [--task <id>]
kli task_link other-task depends-on --task my-task
```
Edge types: `depends-on`, `related-to`, `blocks`, `phase-of`.

#### `task_sever`
Remove an edge from current task to target.
```bash
kli task_sever <target_id> <edge_type> [--task <id>]
kli task_sever other-task depends-on --task my-task
```

#### `task_reclassify`
Change the type of an existing edge.
```bash
kli task_reclassify <target_id> <old_type> <new_type> [--task <id>]
kli task_reclassify other-task related-to depends-on --task my-task
```

### Querying

#### `task_list`
List all tasks with status.
```bash
kli task_list [--grouped <bool>] [--limit <int>]
kli task_list
kli task_list --grouped true --limit 20
```

#### `task_query`
Execute a TQ (Task Query) expression against the task graph. Pipeline-based S-expression language.
```bash
kli task_query '<tq_expr>' [--safety_limit <int>]
kli task_query '(-> (active) :count)'
kli task_query '(-> (query "plan") :enrich (:select :display-name :crdt-status))'
kli task_query '(-> (query "active-roots") :enrich (:sort :obs-count) (:take 5))'
```
Named queries: `active-roots`, `orphans`, `leaf-tasks`, `stale-phases`, `plan`, `plan-ready`, `recent`, `busy`, `hub-tasks`.

#### `task_graph`
Query the task relationship graph with preset query types.
```bash
kli task_graph [--query <type>] [--task_id <id>]
kli task_graph --query stats
kli task_graph --query plan --task_id my-task
kli task_graph --query plan-markdown --task_id my-task
```
Query types: `stats`, `frontier`, `temporal`, `knowledge`, `plan`, `plan-frontier`, `plan-markdown`, `plan-json`.

#### `pq_query`
Execute a PQ (Playbook Query) expression against the pattern graph.
```bash
kli pq_query '<pq_expr>'
kli pq_query '(-> :all :count)'
kli pq_query '(-> (search "authentication") (:take 5))'
kli pq_query '(-> (activate "debugging" :boost (lisp)) (:take 3))'
```

### Conflict Detection

#### `check_conflicts`
Batch check multiple files for concurrent edits by other sessions.
```bash
kli check_conflicts <file_paths> [--task_id <id>]
kli check_conflicts "src/auth.lisp,src/session.lisp,src/routes.lisp"
```

#### `file_activity`
Check who recently touched a single file.
```bash
kli file_activity <file_path> [--task_id <id>] [--hours <num>]
kli file_activity src/session.lisp --hours 2
```

### Health & Status

#### `task_health`
Task health report: stale forks, dead ends, unlinked roots.
```bash
kli task_health
```

#### `task_patterns`
Get pattern activation counts for the current task.
```bash
kli task_patterns [--task <id>]
```

#### `playbook_status`
Get playbook server status.
```bash
kli playbook_status
```

#### `playbook_graph_health`
Pattern graph health report: orphans, edge distribution, embedding coverage.
```bash
kli playbook_graph_health
```

### System

#### `session_register_pid`
Register Claude Code's PID for session file correlation. Called automatically by SessionStart hook — not typically used manually.
```bash
kli session_register_pid <pid>
```
