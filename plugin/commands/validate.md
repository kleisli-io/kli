---
description: Validate implementation against plan, verify success criteria, identify issues
allowed-tools: mcp__task__*, Read, Glob, Grep, Task
---

# Validate Plan

You are tasked with validating that an implementation plan was correctly executed, verifying all success criteria and identifying any deviations or issues.

## Context

Task state gathered via MCP:

Call `task_get()` to retrieve current task state including description, phase, observations, artifacts, and graph context. If no current task, use `task_list()` to find available tasks.

## Initial Setup

When invoked:

1. **Set up task context**:
   - If task name provided: `task_bootstrap(task_id)`
   - If no parameter: Call `task_get()` to check current task, or `task_list()` to find available tasks
   - Ask if no task can be determined: "Which task should I validate?"

2. **After getting task context**:
```
I'll validate task: [Task Name]

Loading task state via task_get() and task_graph(query="plan").

I'll verify:
1. All phases are complete
2. Automated checks pass
3. Code matches plan specifications
4. Manual verification is clear

Starting validation...
```

## Validation Process

### Step 1: Load Task State and Plan

**Retrieve task state and plan structure:**
```
task_get()                      → Full state: description, observations, artifacts, metadata
task_graph(query="plan")        → Phase structure: phases, status, dependencies
task_graph(query="plan-frontier") → Which phases are ready/completed
timeline(limit=20)              → Recent activity and observations
```

**Read task artifacts** (from artifacts list in task_get output):
- Read plan.md if registered as artifact (for verification commands and criteria)
- Read research.md if registered (for additional context)

**Extract validation context:**

From task state (`task_get`):
- Task description, goals, and phase metadata
- Observations from all phases (implementation decisions, challenges)
- Registered artifacts (what files were produced)

From plan DAG (`task_graph(query="plan")`):
- Phase completion status (completed vs active)
- Phase dependencies and ordering
- Success criteria from phase descriptions

**Identify scope:**
- Which files should have been modified? (from phase descriptions and artifacts)
- What functionality should exist? (from task goals)
- What tests should pass? (from phase success criteria)
- What patterns should be followed? (from observations)

**Verify Plan DAG Health:**

Spawn graph-analyst to verify plan integrity:

```
Task(
    subagent_type="graph-analyst",
    prompt='{"question": "Is the task plan DAG healthy? Are there stale phases, orphan tasks, or broken dependencies?"}',
    description="Verify plan DAG health"
)
```

This catches:
- Phases marked complete that still have incomplete dependencies
- Orphan phases not connected to the main task
- Stale phases that should be addressed
- Blocked tasks that might be unblocked now
- Missing Markov transition edges between related tasks
- Unorganized tasks (below observation threshold)

Include DAG health findings in the validation report.

### Step 2: Verify Automated Criteria

For each phase's "Automated Verification" section:

1. **Extract commands** from plan
   - Example: `npm run build`, `cargo build`, `go build ./...`
   - Example: `make test`, `pytest`, `cargo test`
   - Example: `npm run lint`, `eslint src/`

2. **Run each command**:
   - Execute exactly as specified in plan
   - Capture output (success/failure)
   - Note any warnings or errors

3. **Document results**:
   ```
   ✓ Phase 1 Automated Checks:
     ✓ Build succeeded
     ✓ tests pass (24 passed, 0 failed)

   ⚠️ Phase 2 Automated Checks:
     ✓ Build succeeded
     ✗ Linting failed (3 warnings in src/handler.py:42)
   ```

### Step 3: Code Review Against Plan

**Compare implementation to plan specifications:**

1. **Read mentioned files** from plan "Changes Required" sections
2. **Cross-reference with research.md** (if available):
   - Compare implementation against patterns documented in research
   - Verify code references from research were followed
   - Check if open questions from research were addressed

3. **Verify changes match plan**:
   - Were specified functions added/modified?
   - Does structure match plan?
   - Are there unexpected changes?
   - Do changes follow patterns from research.md?

4. **Spawn analyzer agents ONLY if needed**:
   Execute this step ONLY if:
   - Artifacts (plan.md + research.md) don't provide sufficient context, OR
   - Complex verification is needed beyond what artifacts document, OR
   - Inconsistencies found that require deeper analysis

   If spawning agents:
   - Use **codebase-analyzer** to verify complex changes
   - Use **pattern-finder** to check consistency
   - Provide context from artifacts to focus agent analysis

5. **Document findings**:
   ```
   Matches Plan:
   - Database migration added table as specified
   - API endpoints implement correct methods
   - Error handling follows plan pattern

   Deviations:
   - Used different variable name (minor)
   - Added extra validation (improvement)

   Potential Issues:
   - Missing index could impact performance
   - No rollback handling mentioned
   ```

### Step 4: Assess Manual Verification

**Review manual criteria from plan:**

1. **List what needs manual testing**:
   - UI functionality checks
   - Performance testing
   - Edge case verification
   - Integration testing

2. **Ensure criteria are clear and actionable**:
   - Can a developer follow these steps?
   - Are expected results specified?
   - Are edge cases covered?

3. **If criteria are vague**, suggest improvements

### Step 5: Generate Validation Report

**Present comprehensive findings:**

```markdown
## Validation Report: [Task Name]

**Task**: [task_id from task_get()]
**Date**: [Current date]
**Commits**: [git commit range if identifiable]

### Phase Completion Status

✓ Phase 1: [Name] - Complete
✓ Phase 2: [Name] - Complete
⚠️ Phase 3: [Name] - Issues found (see below)

### Plan DAG Health

**Graph-analyst findings:**
- ✓ No stale phases detected
- ✓ No orphan tasks
- ⚠️ 1 blocked task waiting on external dependency

(Include specific findings from graph-analyst output)

### Automated Verification Results

**Phase 1:**
✓ Build succeeds
✓ Tests pass (24 passed, 0 failed)

**Phase 2:**
✓ Build succeeds
✗ Linting: 3 warnings in src/handler.py:42-45
  - Warning: unused variable 'x'
  - Warning: missing type annotation

**Phase 3:**
✓ Integration tests pass

### Code Review Findings

#### Verified Against Research:
(If research.md exists)
- Follows pattern documented in research.md section X
- Code references from research (file:line) were followed
- Open questions from research addressed appropriately
- Implementation consistent with research findings

#### Matches Plan Specifications:
- Database migration correctly adds `users` table
- API endpoints implement specified REST methods
- Error handling follows documented pattern
- Test coverage added as planned

#### Deviations from Plan:
- **src/handler.py:42**: Used different approach than planned (minor, arguably better)
- **src/validator.py:89**: Added extra input validation (improvement, not in plan)
- **Naming**: Used `processRequest` instead of `handleRequest` (inconsistent)

#### Potential Issues:
- **Performance**: Missing index on foreign key `user_id` could impact queries
- **Error handling**: Migration has no rollback procedure
- **Documentation**: New API endpoints not documented
- **Edge case**: No handling for empty input in `processRequest`

### Manual Verification Assessment

**From Plan - Clear and Actionable:**
- [ ] Verify feature appears correctly in UI dashboard
- [ ] Test with >1000 users to check performance
- [ ] Confirm error messages are user-friendly

**From Plan - Needs Clarification:**
- [ ] "Test edge cases" - Which edge cases specifically?
  - Suggestion: Empty input, max length input, special characters

**Additional Manual Testing Recommended:**
- [ ] Verify integration with existing auth system
- [ ] Test rollback procedure for migration
- [ ] Check API documentation is updated

### Summary

**Overall Status**: ⚠️ **Implementation mostly complete, minor issues found**

**Blockers**: None

**Warnings**:
- 3 linting warnings should be addressed
- Missing index could impact production performance
- Documentation gaps exist

**Recommendations**:
1. Fix linting warnings before merge
2. Add index on `user_id` or document performance trade-off
3. Add API documentation for new endpoints
4. Clarify manual test cases for edge cases

**Ready for Reflection?** ✓ Yes, but fix linting warnings first

**Ready for PR?** ⚠️ After addressing warnings and documentation
```

## Special Cases

### Plan Not Found

```
Could not find a plan for this task. No phases found via task_query("(query \"plan\")").

Did you mean one of these recent tasks?
[List from task_list() or task_query("(query \"recent\")")]

Please provide the correct task name.
```

### No Checkmarks in Plan

```
The plan has no phase checkmarks yet.

Options:
1. Run /implement to execute the plan
2. If implementation is done but plan not updated, I can validate anyway
3. If validation shows implementation is complete, I can update the plan

How should I proceed?
```

### Validation Failures

```
⚠️ VALIDATION FAILURES DETECTED

Critical Issues:
- Build fails: build command returned error code 1
- Tests failing: 5/24 tests fail
- Missing implementation: Phase 3 not started

Recommendations:
1. Fix build errors before proceeding
2. Debug failing tests
3. Complete Phase 3 implementation

Cannot proceed to reflection until these are resolved.

Would you like me to help debug these issues?
```

## Important Guidelines

1. **Be thorough but practical**:
   - Focus on what matters for correctness
   - Don't nitpick trivial style differences
   - Highlight real issues that affect functionality

2. **Run all automated checks**:
   - Never skip verification commands
   - If a command fails, investigate why
   - Report failures clearly with error messages

3. **Think critically**:
   - Does implementation actually solve the problem?
   - Are there edge cases not handled?
   - Could this break existing functionality?

4. **Be constructive**:
   - Frame issues as opportunities to improve
   - Suggest solutions, not just problems
   - Acknowledge what was done well

5. **Consider maintainability**:
   - Is code readable and well-structured?
   - Are patterns consistent with codebase?
   - Will future developers understand this?

## Validation Checklist

Always verify:
- [ ] All phases marked complete in plan
- [ ] Plan DAG is healthy (no stale/orphan phases)
- [ ] All automated tests from plan executed
- [ ] Test results documented (pass/fail)
- [ ] Code changes match plan specifications
- [ ] No regressions introduced (existing tests still pass)
- [ ] Manual verification steps are clear
- [ ] Error handling is robust
- [ ] Documentation updated if needed

## Integration with Workflow

**Position in KLI cycle:**
```
/research → /plan → /implement → /validate → /reflect
                                      ↑
                                   You are here
```

**Relationship to other commands:**
- After `/implement` completes all phases
- Before `/reflect` updates playbooks
- Can help prepare for PR/commit

**When to use:**
- After implementation, before reflection
- Before creating PR
- When resuming work to verify state
- To catch issues early

## What NOT to Do

- Don't skip automated verification commands
- Don't validate without reading the plan
- Don't accept "looks good" without running checks
- Don't nitpick trivial style choices
- Don't proceed to reflection if critical issues exist
- Don't create validation artifact file (just report)

## Remember

Validation is your last chance to catch issues before reflecting and updating playbooks. Be thorough, be honest, and be constructive. The goal is to ensure quality before marking the task complete and learning from it.
