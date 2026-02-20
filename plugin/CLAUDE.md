## kli — Task Graphs and Pattern Playbooks

### Always Work in Task Context

If you're doing non-trivial work and not already in a task context, create one with `task_create`. Task context enables observations, graph relationships, and session coordination.

### Record Observations Liberally

During active work, capture discoveries, constraints, and decisions as observations via `observe()`. These persist across sessions and power retrieval. Don't wait — observe as you go.

### Patterns vs Observations

Observations are task-scoped raw findings. Patterns are cross-task reusable knowledge extracted during reflection. Record observations during work; patterns emerge from reflection.

### Verify Task Context After Sub-Agents

Sub-agents (graph-analyst, reflector, etc.) may call `task_bootstrap` or `task_set_current` on a different task, which switches YOUR current task context. After any Task tool call that spawns a sub-agent using task MCP tools, verify your task context with `task_set_current` before recording observations or creating handoffs. Failing to do this will write to the wrong task.

### Playbook Feedback

When you apply a pattern and it works, give `:helpful` feedback. When it misleads, give `:harmful` feedback. This improves future activation ranking.
