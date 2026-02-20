---
description: Implement approved plan phase-by-phase with TDD workflow and verification gates
allowed-tools: mcp__task__*, Read, Write, Edit, Glob, Grep
---

# Implement Plan

Execute phased implementation plans using TDD workflow (Red → Green → Refactor), running verification gates (automated + manual), and marking phases complete via `task_complete()` only after all verification passes.

**The kli-implementation skill provides comprehensive guidance on:**
- TDD methodology (Red → Green → Refactor cycle with discipline)
- Design principles (Extensibility, Composability, Parametricity with examples)
- Zero TODOs policy enforcement
- Verification gate requirements (automated before manual, both blocking)
- Deviation handling patterns

## Initial Setup

**Set up task context:**
- If task name provided: `task_bootstrap(task_id)`
- If no parameter: Call `task_get()` to check current task. If none, ask "Which plan would you like to implement?"

**Load plan structure:**
```
task_query("(query \"plan\")")       → Phase structure: phases, status, dependencies
task_query("(query \"plan-ready\")") → Which phases are ready to work on
```

**Check plan status:**
- If all phases completed: "Plan already complete"
- If phases remain: Identify next phase from plan-frontier (phases are ranked by affinity score — higher affinity = better next candidate)

**If plan.md artifact exists:** Read it for detailed success criteria and verification commands.

**Present initial response** with plan overview, phase count, resume point (first ready phase).

## Implementation Process

### Step 0: Load Context

**0a. Activate playbook patterns** (REQUIRED):
```lisp
pq_query('(-> (activate "<brief task description>" :boost (<domain1> <domain2>)) (:take 5))')
```

**0b. Load relevant skills**: Determine what kind of implementation this is. Load any domain-specific skills relevant to the task (e.g., design skills for UI work, language-specific skills for specialized domains).

### Step 1: Execute Phases (Loop Until All Complete)

Use `task_query("(query \"plan-ready\")")` to find the next ready phase. For each phase:

1. **Switch to phase task and announce start**:
   ```
   task_bootstrap("phase-N-<name>")  → Get phase description with changes required and success criteria
   ```
   Show phase name, overview, changes required, success criteria.

2. **Record phase start**: `observe("Starting phase N: <goal>")`

3. **Search Playbook Patterns**: ALWAYS search using `pq_query('(-> (search "<phase topic>") (:take 5))')` and `pq_query('(-> (proven :min 3) (:take 10))')`.

4. **Read Referenced Files FULLY**: Use Read without limit/offset for all mentioned files.

5. **TDD Red - Write Failing Tests**: Create tests that fail for correct reason.
   ```
   observe("TDD Red: Tests written for <what>. Failure reason: <reason>")
   ```

6. **TDD Green - Implement to Pass**: Implement minimum code to make tests pass.
   ```
   observe("TDD Green: Implementation complete. Tests passing.")
   ```

7. **TDD Refactor - Improve While Green**: Apply design principles (Extensibility, Composability, Parametricity). Run tests after EACH change.
   ```
   observe("TDD Refactor: Applied <principle>. Tests still green.")
   ```

8. **Run Automated Verification**: Execute ALL checks from phase description (build, tests, TODO check). If ANY fail, fix immediately and re-run.

9. **Request Manual Verification**: Present to user with automated status, manual checklist, files changed, testing instructions. ALWAYS give evidence for how you have tested that the implementation works. Wait for "approved" or feedback.

    If issues found: Fix, re-run automated verification, request again.

10. **Mark Phase Complete**:
    ```
    observe("Phase N complete. Verification passed. Key outcomes: <summary>")
    task_complete()  # Marks this phase task as completed
    ```

11. **Give Pattern Feedback** (per phase):
    ```lisp
    pq_query('(-> (pattern "<pattern-id>") (:feedback! :helpful "<evidence>"))')
    pq_query('(-> (pattern "<pattern-id>") (:feedback! :harmful "<what went wrong>"))')
    ```

12. **Return to parent and continue**:
    ```
    task_set_current(parent_task_id)
    task_query("(query \"plan-ready\")")  → Find next ready phase
    ```

13. **Handle Deviations**: If code differs from plan, PAUSE and inform user with options. Wait for decision. Record via `observe()`.

### Step 2: All Phases Complete

```
task_set_current(parent_task_id)
observe("Implementation complete. All N phases done. Key challenges: <summary>")
task_set_metadata(key="phase", value="complete")
```

### Step 3: Final Review

1. **Verify all applied patterns have feedback** — give `helpful` or `harmful` for every activated pattern
2. **Record novel insights as observations** — workarounds, anti-patterns, techniques used 2+ times:
   ```
   observe("Implementation insight: <description>. Evidence: <what happened>")
   ```
3. **Do NOT use `(add! ...)`** — pattern promotion goes through `/kli:reflect` (Reflector → Curator)

Present completion summary with next steps (`/kli:validate`, `/core:commit`, `/kli:reflect`).

## Resuming Implementation

1. `task_bootstrap(parent_task_id)` — restores context with plan progress
2. `task_query("(query \"plan\")")` — see all phases with completion status
3. `task_query("(query \"plan-ready\")")` — find next ready phase
4. Continue from Step 1 at the next incomplete phase

## Remember

- Follow **TDD discipline** from kli-implementation skill (Red → Green → Refactor)
- Apply **design principles** from kli-implementation skill
- Run ALL automated verification before requesting manual verification
- **`task_complete()`** marks phase done — replaces ✓ checkmarks in plan.md
- **`task_query("(query \"plan-ready\")")`** finds next phase — replaces ✓ parsing
- **`observe()`** records progress — observations flow through the task event stream
- Wait for manual approval before next phase
- NEVER introduce TODOs (zero TODOs policy)
- Read files FULLY before modifying
- Search playbook patterns for EACH phase
- **Give feedback per phase** on patterns applied
- One phase at a time - complete ALL verification before proceeding
- ALWAYS give proof that a phase was implemented successfully

## See Also

- CLAUDE.md - Task model, PQ/TQ reference, playbook workflow
