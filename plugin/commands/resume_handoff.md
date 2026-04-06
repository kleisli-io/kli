---
description: Resume work from handoff document with context analysis and validation
allowed-tools: mcp__task__*, Read, Glob
---

# Resume Work from Handoff Document

You are tasked with resuming work from a handoff document through an interactive process.

## Context

These handoffs contain critical context, learnings, and next steps from previous work sessions that need to be understood and continued.

## Initial Response

When this command is invoked:

### 1. If the path to a handoff document was provided

**Example**: `/kli:resume_handoff <task-dir>/handoffs/2025-10-26_14-30-00_phase-1.md`

- Skip the default message
- Immediately read the handoff document FULLY (no limit/offset)
- Immediately read any research or plan documents it links to under "Critical References"
- Do NOT use a sub-agent to read these critical files
- Derive the task ID from the path (the task name is the directory containing `handoffs/`)
- Set current task via `task_bootstrap` so subsequent MCP calls have context
- Begin analysis process by ingesting context
- Propose course of action to user and confirm

### 2. If a task name was provided

**Example**: `/kli:resume_handoff 2025-10-26-handoff-commands`

- Bootstrap task: `task_bootstrap(task_id="2025-10-26-handoff-commands")` — sets context + returns state with artifacts
- Check timeline: `timeline(limit=20)` — look for `:handoff.create` events which contain the handoff path
- Also glob for handoff files using the task directory from `task_get()`: `<task_dir>/handoffs/*.md`
- If no handoffs exist: "I can't find any handoff documents in this task. Would you like to create one with /kli:handoff?"
- If one handoff: Proceed with that handoff
- If multiple handoffs: Use the most recent (by timestamp in filename YYYY-MM-DD_HH-MM-SS)
- Read handoff FULLY and linked artifacts
- Begin analysis process

### 3. If no parameters provided

**Example**: `/kli:resume_handoff`

**Step 1: Use Session Start Context (Already Injected)**

The session start hook injects task context at the beginning of each session. Look for this in the conversation startup:

```
TASK[1]{dir,phase,last_artifact}:
 <task-dir>,Phase 2: Implementation,research.md
```

- If `TASK[1]` context exists: Extract the task directory from it (first field before comma)
- This context is **already in your conversation** - no file reading needed
- The session start hook has done the discovery work for you

**Step 2: Find Handoffs for Active Task**

If active task found:

1. Bootstrap task: `task_bootstrap(task_id)`
2. Check timeline for `:handoff.create` events: `timeline(limit=30)`
3. Also glob for handoff files: `<task_dir>/handoffs/*.md`
4. If handoffs exist:
   - Use most recent by filename timestamp (YYYY-MM-DD_HH-MM-SS)
   - Announce: "Found active task with handoff. Resuming from `{handoff_path}`..."
   - Proceed to read handoff and linked artifacts
5. If no handoffs but task exists:
   - Get task state with `task_get()` for observations and artifacts
   - Ask: "Found active task but no handoffs. Read observations instead? [Y/n]"

**Step 3: Fallback (No TASK Context)**

If no `TASK[1]` in session startup (rare - usually means no active task):

```
I'll help you resume work from a handoff document.

Please provide either:
- Full path: `/kli:resume_handoff <path-to-handoff-file>`
- Task name: `/kli:resume_handoff 2025-10-26-task-name`

Or I can help you find available handoffs. What would you like to do?
```

Then wait for user input.

## Process Steps

### Step 1: Read and Analyze Handoff

1. **Restore task context and activate relevant patterns**:
   ```
   task_bootstrap(task_id)
   pq_query('(-> (activate "<task topic>" :boost (<domain1> <domain2>)) (:take 5))')
   ```
   The activate query uses task topic + graph context to surface semantically relevant patterns.

2. **Read handoff document completely**:
   - Use Read tool WITHOUT limit/offset parameters
   - Extract all sections:
     - Task(s) and their statuses
     - Critical References
     - Recent changes
     - Learnings
     - Artifacts
     - Action items and next steps
     - Other notes

2. **Read referenced artifacts**:
   - Read all files mentioned in "Critical References" section
   - Read research.md if referenced
   - Read plan.md if referenced
   - Read any other critical files mentioned
   - Use Read tool FULLY for each file

3. **Verify current state** (read-only validation):
   - Check if mentioned files still exist at specified paths
   - Use `task_get()` to see current task state vs handoff state
   - Check `timeline(limit=20)` for recent activity since handoff
   - Check if recent changes are still present
   - Note any discrepancies between handoff and current state

4. **Verify graph state** (if task has phases):
   Spawn graph-analyst to check current graph state:

   ```
   Task(
       subagent_type="graph-analyst",
       prompt='{"question": "What is the current state of task <task_id>? Are there any stale phases, new blocking dependencies, or changes since the handoff?"}',
       description="Verify task graph state"
   )
   ```

   This catches:
   - Phases completed by another session since handoff
   - New blocking dependencies introduced
   - Related tasks that were created
   - Changes to task metadata or goals

### Step 2: Synthesize and Present Analysis

Present comprehensive analysis to user:

```
I've analyzed the handoff from [date] for task [name].

**Original Tasks:**
- [Task 1]: [Status from handoff] → [Current verification]
- [Task 2]: [Status from handoff] → [Current verification]

**Key Learnings from Handoff:**
- [Learning 1 with file:line reference]
- [Learning 2 with pattern discovered]

**Recent Changes Status:**
- [Change 1] - [Verified present/Missing/Modified]
- [Change 2] - [Verified present/Missing/Modified]

**Critical Artifacts Reviewed:**
- [research.md]: [Key findings summary]
- [plan.md]: [Phase status summary]

**Graph State** (if task has phases):
- [Graph-analyst findings about current state]
- [Phases completed since handoff]
- [New blocking dependencies]
- [Related tasks created]

**Recommended Next Actions:**
Based on the handoff's action items:
1. [Most logical next step]
2. [Second priority]
3. [Additional tasks]

**Discrepancies Found** (if any):
- [File mentioned but not found]
- [Change mentioned but different]
- [State mismatch]

Shall I proceed with [recommended action 1], or would you like to adjust the approach?
```

Get user confirmation before proceeding.

### Step 3: Create Action Plan

1. **Use TodoWrite to create task list**:
   - Convert action items from handoff into todos
   - Add any new tasks discovered during analysis
   - Prioritize based on dependencies

2. **Present the plan**:
   ```
   I've created a task list based on the handoff:

   [Show todo list]

   Ready to begin with the first task?
   ```

### Step 4: Begin Work

1. Start with first approved task
2. Reference learnings from handoff throughout work
3. Apply patterns discovered in handoff
4. Update progress as tasks complete

## Guidelines

1. **Be Thorough in Analysis**:
   - Read entire handoff document
   - Verify ALL mentioned changes exist
   - Check for regressions or conflicts
   - Read all referenced artifacts

2. **Be Interactive**:
   - Present findings before starting work
   - Get buy-in on approach
   - Allow course corrections
   - Adapt based on current vs handoff state

3. **Leverage Handoff Wisdom**:
   - Pay attention to "Learnings" section
   - Apply documented patterns
   - Avoid repeating mistakes mentioned
   - Build on discovered solutions

4. **Validate Before Acting**:
   - Never assume handoff state matches current
   - Verify file references still exist
   - Check for breaking changes since handoff
   - Confirm patterns still valid

5. **Avoid Unnecessary Sub-Agents**:
   - Read files directly in main context
   - Only spawn agents if complex verification needed
   - Most handoff resumption is straightforward reading

## Common Scenarios

**Clean Continuation**:
- All changes present, no conflicts
- Proceed with recommended actions

**Diverged Codebase**:
- Some changes missing or modified
- Reconcile differences and adapt plan

**Incomplete Work**:
- Tasks marked "in_progress"
- Complete unfinished work first

**Stale Handoff**:
- Significant time passed
- Re-evaluate strategy based on current state
