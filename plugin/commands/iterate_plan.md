---
description: Iterate on existing implementation plans with thorough research and updates
allowed-tools: mcp__task__*, Read, Write, Edit, Task, AskUserQuestion
---

# Iterate Implementation Plan

You are tasked with updating existing implementation plans based on user feedback. You should be skeptical, thorough, and ensure changes are grounded in actual codebase reality.

## Initial Response

When this command is invoked:

1. **Set up task context**:
   - If task name or path provided: `task_bootstrap(task_id)`
   - If no parameter: Call `task_get()` to check current task. If none, ask user.
   - Use `task_graph(query="plan")` to see the current plan structure (phases, status, dependencies)

2. **Handle different input scenarios**:

   **If NO task/plan identified**:
   ```
   I'll help you iterate on an existing plan.

   Which task's plan would you like to update? Provide the task name or use task_list() to find it.
   ```
   Wait for user input.

   **If task identified but NO feedback**:
   ```
   I've found the plan. Current structure:
   [output of task_graph(query="plan")]

   What changes would you like to make?

   For example:
   - "Add a phase for migration handling"
   - "Update the success criteria to include performance tests"
   - "Adjust the scope to exclude feature X"
   - "Split Phase 2 into two separate phases"
   ```
   Wait for user input.

   **If BOTH task AND feedback provided**:
   - Proceed immediately to Step 1
   - No preliminary questions needed

## Process Steps

### Step 1: Understand Current Plan

1. **Load plan structure from task DAG**:
   - `task_graph(query="plan")` — shows phases, status, dependencies
   - `task_get()` — shows description, goals, observations, metadata
   - If plan.md exists as artifact, read it for detailed criteria

2. **Understand the requested changes**:
   - Parse what the user wants to add/modify/remove
   - Identify if changes require codebase research
   - Determine scope of the update

### Step 2: Research If Needed

**Only spawn research tasks if the changes require new technical understanding.**

If the user's feedback requires understanding new code patterns or validating assumptions:

1. **Record iteration intent**: `observe("Plan iteration: <what user wants changed>")`

2. **Spawn parallel sub-tasks for research**:
   Use the right agent for each type of research:

   **For code investigation:**
   - **codebase-locator** - To find relevant files
   - **codebase-analyzer** - To understand implementation details
   - **pattern-finder** - To find similar patterns

   **For historical context (use PQ queries):**
   - `pq_query('(-> (search "<topic>") (:take 5))')` - Find patterns
   - `pq_query('(-> (proven :min 3) (:take 10))')` - Get proven patterns (helpful >= 3)

   **Be EXTREMELY specific about directories**:
   - Include full path context in prompts
   - Specify exact directories to search

3. **Read any new files identified by research**:
   - Read them FULLY into the main context
   - Cross-reference with the plan requirements

4. **Wait for ALL sub-tasks to complete** before proceeding

### Step 3: Present Understanding and Approach

Before making changes, confirm your understanding:

```
Based on your feedback, I understand you want to:
- [Change 1 with specific detail]
- [Change 2 with specific detail]

My research found:
- [Relevant code pattern or constraint]
- [Important discovery that affects the change]

I plan to update the plan by:
1. [Specific modification to make]
2. [Another modification]

Does this align with your intent?
```

Get user confirmation before proceeding.

### Step 4: Update the Plan

Plans are task DAGs. Update the plan structure using task MCP tools:

1. **Modify the DAG as needed**:
   - **Add phases** (preferred): Use `scaffold-plan!` for multiple phases with dependencies:
     ```
     task_query("(scaffold-plan!
       (new-phase \"Implement new feature\" :after existing-phase)
       (follow-up \"Integration tests\" :after new-phase))")
     ```
   - **Add single phase**: `task_fork(name="implement-new-feature", from=parent_task_id, edge_type="phase-of", description="...")` + add dependency edges with `task_link`. Names are validated for descriptiveness (avoid `P1`, `phase-1`, etc.)
   - **Update phase description**: Switch to phase task with `task_set_current`, then `observe("Updated scope: <changes>")`, switch back
   - **Reorder phases**: Adjust `depends-on` edges with `task_link` / `task_sever`
   - **Remove phase(s)**: Use TQ bulk sever for efficiency, then record the decision:
     ```lisp
     ;; Single phase removal
     task_query("(-> (node \"obsolete-phase\") (:sever-from-parent! :phase-of))")

     ;; Multiple phases at once (replaces multiple task_sever calls)
     task_query("(-> (node \"phase-1\" \"phase-2\" \"phase-3\") (:sever-from-parent! :phase-of))")
     ```
     Then: `observe("Phases removed: <names>. Reason: <why>")`

2. **If plan.md artifact exists**, update it to match the DAG changes:
   - Use the Edit tool for surgical changes
   - Keep all file:line references accurate
   - Update success criteria if needed

3. **Ensure consistency**:
   - Verify with `task_graph(query="plan")` after changes
   - Maintain the distinction between automated vs manual success criteria
   - Include specific file paths for new content

4. **Record the iteration**: `observe("Plan iteration complete: <summary of changes>")`

### Step 5: Review and Complete

1. **Present the changes made**:
   ```
   I've updated the plan for task [task-name].

   Changes made:
   - [Specific change 1]
   - [Specific change 2]

   The updated plan now:
   - [Key improvement]
   - [Another improvement]

   Would you like any further adjustments?
   ```

2. **Be ready to iterate further** based on feedback

## Important Guidelines

1. **Be Skeptical**:
   - Don't blindly accept change requests that seem problematic
   - Question vague feedback - ask for clarification
   - Verify technical feasibility with code research
   - Point out potential conflicts with existing plan phases

2. **Be Surgical**:
   - Make precise edits, not wholesale rewrites
   - Preserve good content that doesn't need changing
   - Only research what's necessary for the specific changes
   - Don't over-engineer the updates

3. **Be Thorough**:
   - Read the entire existing plan before making changes
   - Research code patterns if changes require new technical understanding
   - Ensure updated sections maintain quality standards
   - Verify success criteria are still measurable

4. **Be Interactive**:
   - Confirm understanding before making changes
   - Show what you plan to change before doing it
   - Allow course corrections
   - Don't disappear into research without communicating

5. **Track Progress**:
   - Use `observe()` to record iteration decisions and progress
   - Verify plan DAG with `task_graph(query="plan")` after changes

6. **No Open Questions**:
   - If the requested change raises questions, ASK
   - Research or get clarification immediately
   - Do NOT update the plan with unresolved questions
   - Every change must be complete and actionable

## Success Criteria Guidelines

When updating success criteria, always maintain the two-category structure:

1. **Automated Verification** (can be run by execution agents):
   - Commands that can be run: `make test`, `npm run lint`, `pytest`, `cargo test`, etc.
   - Use your project's existing build/test commands
   - Specific files that should exist
   - Code compilation/type checking

2. **Manual Verification** (requires human testing):
   - UI/UX functionality
   - Performance under real conditions
   - Edge cases that are hard to automate
   - User acceptance criteria

## Sub-task Spawning Best Practices

When spawning research sub-tasks:

1. **Only spawn if truly needed** - don't research for simple changes
2. **Spawn multiple tasks in parallel** for efficiency
3. **Each task should be focused** on a specific area
4. **Provide detailed instructions** including:
   - Exactly what to search for
   - Which directories to focus on
   - What information to extract
   - Expected output format
5. **Request specific file:line references** in responses
6. **Wait for all tasks to complete** before synthesizing
7. **Verify sub-task results** - if something seems off, spawn follow-up tasks

## Example Interaction Flows

**Scenario 1: User provides everything upfront**
```
User: /iterate_plan 2025-10-16-feature - add phase for error handling
Assistant: [Reads plan, researches error handling patterns if needed, updates plan]
```

**Scenario 2: User provides just task name**
```
User: /iterate_plan 2025-10-16-feature
Assistant: I've found the plan. What changes would you like to make?
User: Split Phase 2 into two phases - one for backend, one for frontend
Assistant: [Proceeds with update]
```

**Scenario 3: User provides no arguments**
```
User: /iterate_plan
Assistant: Which task's plan would you like to update? Provide the task name or use task_list() to find it.
User: 2025-10-16-feature
Assistant: I've found the plan. What changes would you like to make?
User: Add more specific success criteria
Assistant: [Proceeds with update]
```
