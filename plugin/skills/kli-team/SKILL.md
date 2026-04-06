---
name: kli-team
description: Team coordination domain knowledge for bridging Claude Code teams with the persistent task graph and observation system. Use when leading agent teams, spawning teammates, coordinating parallel work, or interpreting swarm awareness. Activates for team and multi-agent tasks. DO NOT use for single-agent work.
---

# KLI Team Coordination Skill

## Two Tool Systems

This skill uses tools from two layers that serve different purposes:

**Team tools** (PascalCase) — in-session coordination between teammates:
`TeamCreate`, `SendMessage`, `TaskCreate`, `TaskUpdate`, `TaskGet`, `TeamDelete`

**Task MCP tools** (snake_case) — persistent task graph that survives across sessions:
`task_bootstrap`, `observe`, `file_activity`, `check_conflicts`, `task_fork`, `task_claim`, `task_complete`, `handoff`

Both are needed. Team tools coordinate the current session's agents. Task MCP tools read and write the shared knowledge layer that future sessions inherit.

**Teammate access gap**: Teammates do NOT have MCP tool access (known limitation). Instead, teammates use the `kli` CLI via Bash: `kli <tool> --task <id>`. See the `using-kli-cli` skill for full reference. When spawning teammates, include kli CLI examples in their task descriptions so they know how to record observations and query the task graph.

## The Bridge Pattern (CRITICAL)

Claude Code teams and the persistent task graph are separate systems that don't automatically share context. **Your job as team lead is to bridge them.**

Without bridging:
- Teammates get generic task descriptions with no prior context
- Work overlaps because nobody checks file ownership
- Findings vanish when teammates shut down (no observations recorded)
- Future sessions can't learn from team outcomes

With bridging:
- Task descriptions carry observations, patterns, and conflict warnings from the task graph
- File activity checks prevent overlapping edits
- Teammate findings are aggregated as observations for future sessions
- Playbook patterns (reusable insights scored by helpfulness) evolve from team outcomes

## Three-Phase Lifecycle

### 1. Setup: Bootstrap Context, Then Spawn

**Before creating any team or tasks**, load context from the task graph:

```
task_bootstrap(task_id)
```

This returns:
- **Observations** from prior sessions on this task
- **Swarm awareness**: who else is working, similar tasks, departures
- **Graph neighbors**: related tasks, dependencies, blockers
- **Playbook patterns**: reusable insights with helpfulness scores, activated when relevant

**Use this context to write informed task descriptions:**

```
# BAD - context-free task description
TaskCreate(subject="Implement auth module")

# GOOD - informed by task graph context
TaskCreate(
  subject="Implement auth module",
  description="Prior research (obs: 'OAuth2 flow documented in research.md') "
              "identified 3 endpoints needed. session.lisp:449 already has "
              "fingerprint struct. Check file_activity('session.lisp') before "
              "editing - another session touched it 30min ago."
)
```

**Check for conflicts before assigning file-overlapping work:**

```
check_conflicts(file_paths="src/auth.lisp,src/session.lisp,src/routes.lisp")
```

If conflicts found, either: assign those files to one teammate only, or sequence the tasks with dependencies.

**Spawn the team after tasks are ready:**

```
TeamCreate(team_name="auth-impl")
```

Then spawn teammates via Task tool with `team_name` parameter and assign tasks via TaskUpdate with `owner`.

### 2. Monitor: Watch for Conflicts and Departures

During team execution, `task_bootstrap` output includes team-aware swarm awareness:

- **Team members**: `Team 'auth-impl': 3 members (researcher, implementer, tester)`
- **Cross-team similarity**: `[team 'other-team']` annotations on similar sessions
- **Departures**: `teammate in 'auth-impl'` when a member leaves
- **Orphaned phases**: Phases claimed by departed sessions that need pickup

**Respond to departures:**
1. Check if departed teammate's task is complete (TaskGet)
2. If incomplete: reassign to another teammate or handle yourself
3. If orphaned phase: claim it via `task_claim()`

**Detect file conflicts between teammates:**

```
file_activity(file_path="src/shared-module.lisp")
```

If two teammates are editing the same file, message one to pause:

```
SendMessage(type="message", recipient="implementer",
  content="Hold off on session.lisp - researcher is still editing it. "
          "Work on routes.lisp first.",
  summary="File conflict avoidance")
```

### 3. Synthesize: Aggregate Findings, Then Shut Down

When teammates complete their work:

1. **Aggregate findings as observations** (this bridges team work back into the persistent task graph):

```
observe("Team finding: Auth module needs 3 endpoints (login, refresh, logout). "
        "Researcher discovered existing session struct at session.lisp:449. "
        "Implementer confirmed OAuth2 flow works with existing token store.")
```

2. **Create handoff** for future sessions:

```
handoff(summary="Auth implementation complete via 3-person team")
```

3. **Shut down teammates gracefully:**

```
SendMessage(type="shutdown_request", recipient="researcher",
  content="Work complete, shutting down")
SendMessage(type="shutdown_request", recipient="implementer",
  content="Work complete, shutting down")
```

Wait for approval responses, then:

```
TeamDelete()
```

4. **Mark task complete:**

```
task_complete()
```

## Communication Principles

### Message Type Selection

| Type | Use When | Cost |
|------|----------|------|
| `message` (DM) | **Default.** Task updates, questions, coordination | 1 message |
| `broadcast` | Critical blockers affecting ALL teammates | N messages (1 per teammate) |
| `shutdown_request` | Graceful termination | 1 message + response |

**Default to DM.** Broadcast is almost never needed.

### Anti-Patterns

| Anti-Pattern | Problem | Better Approach |
|---|---|---|
| Broadcasting routine updates | Wastes resources, creates noise | DM the affected teammate |
| Spawning before bootstrapping | Teammates lack context, duplicate work | Always `task_bootstrap` first |
| No file conflict checks | Teammates overwrite each other's work | `check_conflicts` before assigning |
| Ignoring departures | Orphaned work, incomplete tasks | Reassign or claim orphaned phases |
| Not recording team findings | Knowledge lost when team shuts down | `observe()` before shutdown |
| Micromanaging via messages | Slows teammates, wastes tokens | Check in at milestones, not every step |
| Using UUIDs instead of names | Hard to read, error-prone | Always use teammate names |

### Plan Approval Workflow

When a teammate is spawned with `mode: "plan"`:
1. Teammate creates plan using read-only tools
2. Teammate calls ExitPlanMode → sends `plan_approval_request` to you
3. Review the plan, respond with `plan_approval_response` (approve or reject with feedback)

## Quick Reference: Tools for Team Leads

**Team tools** (in-session coordination):

| Tool | Phase | Purpose |
|------|-------|---------|
| `TeamCreate` | Setup | Create a team |
| `TaskCreate`/`TaskUpdate` | Setup | Create and assign work items to teammates |
| `SendMessage` | Monitor | Communicate with teammates |
| `TeamDelete` | Synthesize | Clean up team resources |

**Task MCP tools** (persistent task graph):

| Tool | Phase | Purpose |
|------|-------|---------|
| `task_bootstrap` | Setup | Load full task context (observations, swarm awareness, patterns) |
| `check_conflicts` | Setup | Batch file conflict check |
| `file_activity` | Setup/Monitor | Single file conflict check |
| `observe` | All | Record findings for future sessions |
| `task_fork` | Setup | Create phase subtasks in task graph |
| `task_claim` | Monitor | Claim orphaned phases |
| `handoff` | Synthesize | Create handoff document |
| `task_complete` | Synthesize | Mark task done |
