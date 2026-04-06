---
name: kli-planning
description: Planning phase domain knowledge including phase design principles, artifact reuse patterns, and verification strategies. Use when creating implementation plans, structuring phased work, or designing incremental delivery. Activates for planning and decomposition tasks. DO NOT use for research or implementation.
---

# KLI Planning Phase Skill

## Requirements & Standards

### Error Amplification in Planning

Planning errors amplify **100x** through implementation:

```
1 unclear phase → 10+ hours debugging scope
1 missing verification → 50+ test failures discovered late
1 technical detail missed → 100+ lines of rework
```

**Exit Criteria Enforcement:**
Because of 100x amplification, you must meet ALL exit criteria:
- ✅ All phases clearly defined with boundaries
- ✅ Success criteria specified (automated + manual)
- ✅ No open questions or uncertainties
- ✅ All technical decisions made with evidence

### Phase Design Principles (CRITICAL)

Every phase must be:

1. **Incremental**: Single logical unit of work
   - NOT: "Add feature X, Y, and Z"
   - YES: "Phase 1: Add X, Phase 2: Add Y, Phase 3: Add Z"

2. **Testable**: Clear verification points
   - NOT: "Update code"
   - YES: "Update code (verified by: tests pass)"

3. **Clear Boundaries**: No overlap or ambiguity
   - Each file/function belongs to exactly one phase
   - No "finish anything from Phase 1" in Phase 2

4. **Build on Previous**: Sequential dependency chain
   - Phase N+1 assumes Phase N complete
   - Cannot skip phases

5. **3-7 Phases Typical**: Right granularity
   - < 3 phases: Too coarse, verification gaps
   - > 7 phases: Too fine, overhead dominates
   - Adjust based on complexity

### Verification Requirements

**Both Required** (never proceed without verification):

**Automated Verification:**
- Build success: `<your build command>`
- Tests pass: `<your test command>`
- Syntax/lint checks: `<your linter>` (e.g., eslint, mypy, clippy, go vet)
- TODO detection: Zero TODOs policy
- Pattern: Run automatically, blocking, before manual

**Manual Verification:**
- User judgment required (UI/UX, performance, acceptance)
- Pattern: Present clear checklist to user
- Pattern: Wait for explicit approval before next phase

**Both are BLOCKING** - implementation cannot proceed without both passing.

## Overview

The KLI planning phase transforms research findings into detailed, phased implementation plans. It reuses research artifacts (saves 40-50% tokens), defines clear phase boundaries with verification gates, and incorporates playbook patterns to ensure implementation success.

**Role in KLI Workflow:**
1. **Generator Role**: Plan command implements Generator directly
2. **Artifact Production**: Creates plan as task DAG (via `scaffold-plan!` or `task_fork`), optionally writes plan.md as artifact
3. **Artifact Consumption**: Detects and reuses `research.md`
4. **Foundation for Implementation**: Provides detailed roadmap via `task_query("(query \"plan\")")`

**Key Characteristics:**
- Iterative planning with user feedback loops
- Clear phase boundaries and verification gates
- Research artifact reuse for efficiency
- Playbook pattern application
- Out-of-scope explicit definition

## Quick Start

**Before starting any planning:** Activate playbook patterns (REQUIRED):
```
pq_query('(-> (activate "<planning task>" :boost (<relevant domains>)) (:take 5))')
```
This retrieves proven patterns via graph-based search and persists for handoff continuity.

**Basic Planning Workflow:**

1. **Check for research.md** (artifact reuse)
   - If exists → Read FULLY, present to user, ask how to proceed
   - If not exists → Gather context via subagents (locator, analyzer, pattern-finder)

2. **Reference activated patterns**
   - Apply planning/implementation patterns from `(activate ...)` output
   - Use phasing strategies from patterns
   - Include verification approaches from patterns

3. **Draft initial plan structure**
   - Break into 3-7 phases
   - Each phase: incremental, testable, clear boundaries
   - Define success criteria (automated + manual)

4. **Iterate on plan structure**
   - Present structure to user for feedback
   - Ask clarifying questions for uncertainties
   - Spawn additional agents if technical details unclear
   - Refine until no open questions remain

5. **Write plan.md** artifact
   - Frontmatter with metadata (from task-metadata)
   - Overview, current state, phases with verification
   - References to research.md and playbook patterns
   - Explicit out-of-scope section

## Phase Design Methodology

### Incremental Phasing Strategy

**Good Phase Breakdown Example:**
```
Phase 1: Infrastructure Setup
- Create directory structure
- Add configuration files
- Verify: directories exist, configs load

Phase 2: Core Functionality
- Implement main logic
- Add unit tests
- Verify: tests pass, no TODOs

Phase 3: Integration
- Connect to existing system
- Add integration tests
- Verify: system tests pass, manual smoke test
```

**Bad Phase Breakdown Example:**
```
Phase 1: Implement Everything
- Add files
- Write tests
- Make it work
- Verify: somehow works?

Phase 2: Fix Issues
- Fix whatever is broken from Phase 1
- Verify: no more issues?
```

### Testable Boundaries

**Each phase needs:**

1. **Automated Verification**:
   ```markdown
   **Automated Verification:**
   - [ ] Build passes: `<your build command>`
   - [ ] Tests pass: `<your test command>`
   - [ ] Lint/type check passes: `<your linter>`
   - [ ] No TODOs: `! grep -r TODO phase-files`
   ```

2. **Manual Verification**:
   ```markdown
   **Manual Verification:**
   - [ ] Feature X works as expected (test: try scenario Y)
   - [ ] No regressions in feature Z (test: verify Z still works)
   - [ ] Performance acceptable (test: operation completes in < 5s)
   ```

3. **Exit Criteria**:
   ```markdown
   **Phase N complete when:**
   - All automated checks pass
   - All manual checks approved by user
   - No open issues or questions
   - Ready to proceed to Phase N+1
   ```

### Success Criteria Patterns

**Automated Criteria Examples:**
- Build/compilation success
- Test suite passes
- Syntax validation
- Linting passes
- TODO detection (zero TODOs)
- Performance benchmarks met
- API contract tests pass

**Manual Criteria Examples:**
- UI/UX appearance correct
- User acceptance criteria met
- No regressions in related features
- Documentation updated
- Edge cases handled appropriately
- Error messages clear and helpful

## Workflows

### Research Artifact Reuse

**When research.md exists:**

```
1. Read research.md FULLY (no limit/offset)
   ↓
2. Extract sections:
   - Summary
   - Detailed Findings
   - Code References (file:line)
   - Playbook Patterns
   - Open Questions
   ↓
3. Present to user:
   "I found existing research. Here's what was discovered:
    [Summary of findings]

    How would you like to proceed?
    1. Proceed with planning (use research as-is)
    2. Supplement research (focused investigation)
    3. Re-research (fresh start)"
   ↓
4. Handle user choice:
   - Option 1: Continue to planning with research context
   - Option 2: Spawn targeted agents, update research.md
   - Option 3: Offer to run /kli:research instead
```

**Why this matters:**
- Saves 40-50% tokens in planning phase (measured impact)
- Avoids duplicate research via subagents
- Maintains single source of truth
- Research quality already vetted

**When research.md does NOT exist:**
- Spawn context-gathering agents (locator, analyzer, pattern-finder)
- Document findings in observations
- Consider suggesting user run /kli:research first if complex

### Standard Planning Workflow

```
1. Check for research.md
   ↓
2. If exists: Read and present
   If not: Gather context via subagents
   ↓
3. Record planning observations via observe()
   ↓
4. Apply patterns from PQ queries (`(-> (search "...") ...)`, `(-> (proven) ...)`)
   - Reference planning patterns
   - Reference implementation patterns
   - Include verification approaches
   ↓
5. Draft initial plan structure
   - Break into 3-7 phases
   - Each phase: incremental, testable, clear
   - Define success criteria
   ↓
6. Present structure to user
   ↓
7. User provides feedback
   ↓
8. Gap analysis:
   - Are there open questions?
   - Are technical details clear?
   - Is verification approach defined?
   ↓
9. If gaps exist:
   - Ask clarifying questions
   - Spawn additional agents if needed
   - Iterate (return to step 5)
   ↓
10. If no gaps:
   - Create plan as task DAG (task_fork + phase-of edges)
   - Optionally write plan.md as artifact
   - Record final status via observe()
```

### Clarifying Questions Pattern

**When to ask:**
- Technical detail unclear (which approach?)
- User preference unknown (UI/UX decisions)
- Multiple valid options (need user choice)
- Scope ambiguity (what's in/out of scope?)

**Question Structure:**
```markdown
**Question N:** <Specific question>
- Context: <Why this matters for implementation>
- Current state: <What research/analysis showed>
- Options: <If applicable>
  1. <Option A>: <Implications>
  2. <Option B>: <Implications>
```

**Examples:**

**Good Clarifying Question:**
```
**Question 1:** Should error messages be user-facing or developer-facing?
- Context: Affects error handling strategy and message content
- Current state: Research shows existing errors are developer-facing
- Options:
  1. User-facing: Add translation layer, simpler messages
  2. Developer-facing: Keep technical, include stack traces
```

**Bad Clarifying Question:**
```
**Question 1:** How should we do it?
- Context: Need to know
```

### Iteration Decision Patterns

**Continue iterating if:**
- Open questions remain unanswered
- User feedback requires structural changes
- Technical approach unclear or unverified
- Verification strategy undefined
- Phase boundaries ambiguous
- Success criteria incomplete

**Exit iteration loop only when:**
- ✅ All phases have clear scope and boundaries
- ✅ Success criteria defined (automated + manual) for each phase
- ✅ No open questions remain
- ✅ Technical approach is clear and validated
- ✅ Playbook patterns referenced
- ✅ Research.md referenced (if exists)
- ✅ Out-of-scope explicitly defined

### Out-of-Scope Definition Strategy

**Why explicit out-of-scope matters:**
- Prevents scope creep during implementation
- Sets user expectations clearly
- Provides reference for deviation decisions
- Documents intentional limitations

**Pattern:**
```markdown
## What We're NOT Doing

**Explicit out-of-scope items to prevent scope creep:**

- NOT implementing feature Y (reason: complexity/timeline/dependencies)
- NOT modifying component Z (reason: separate concern)
- NOT supporting use case W (reason: edge case, defer to future)
- NOT optimizing for X (reason: premature optimization)
```

**Common out-of-scope items:**
- Edge cases (document for future)
- Performance optimizations (unless critical)
- UI polish (unless UX-critical)
- Additional features (keep focused)
- Refactoring unrelated code (separate task)

## Verification & Completion

### Plan Completeness Checks

**Before marking status: draft complete:**

✅ **Phases Defined:**
- Each phase has clear name and description
- Boundaries explicit (no overlap)
- Sequential dependencies clear
- Typically 3-7 phases

✅ **Verification Specified:**
- Every phase has automated verification list
- Every phase has manual verification list
- Both automated AND manual required
- Verification criteria specific and testable

✅ **No Open Questions:**
- All technical decisions made
- All ambiguities resolved
- User preferences captured
- Approach validated

✅ **Evidence-Based:**
- References research.md findings (if exists)
- References playbook patterns with IDs
- References code locations (file:line)
- All claims backed by evidence

✅ **Out-of-Scope Defined:**
- Explicit list of what's NOT being done
- Rationale for each exclusion
- Clear boundaries established

### Validation Requirements

**Self-Check Questions:**
1. Could someone implement from this plan without guessing?
2. Are phase boundaries clear and non-overlapping?
3. Are verification criteria specific and testable?
4. Have all open questions been resolved?
5. Is out-of-scope explicitly defined?
6. Does plan reference research.md if it exists?
7. Are playbook patterns applied and referenced?

**Common Failure Modes:**
- Phases too large (not incremental)
- Verification criteria vague ("make it work")
- Open questions swept under rug
- Phase overlap/ambiguity
- Missing out-of-scope definition
- Not reusing research.md
- Not applying playbook patterns

## Reference

### Core Principles

- Create detailed plan before implementation (not rough outline)
- Implement phase-by-phase, run automated verification after each
- Never proceed to next phase without passing automated verification
- Don't batch multiple phases - implement incrementally with verification
- Detect and reuse research.md (saves 40-50% tokens)
- Phase scope reduction is valid mid-implementation when earlier phases deliver sufficient user value - strategic prioritization, not scope creep

### Phase Templates

**Infrastructure Phase Template:**
```markdown
## Phase N: Infrastructure/Setup

**Overview:** <What infrastructure is being created>

**Changes Required:**
- Create directory X
- Add configuration Y
- Initialize component Z

**Success Criteria:**

Automated Verification:
- [ ] Directories exist: `test -d path`
- [ ] Configs load: `<your validation command>`
- [ ] No syntax errors

Manual Verification:
- [ ] Structure matches design
- [ ] Conventions followed
```

**Feature Implementation Phase Template:**
```markdown
## Phase N: Feature Implementation

**Overview:** <What feature is being added>

**Changes Required:**
- Implement logic in file.ext
- Add tests in test-file.ext
- Update documentation

**Success Criteria:**

Automated Verification:
- [ ] Build passes: `<your build command>`
- [ ] Tests pass: `<your test command>`
- [ ] No TODOs: `! grep TODO phase-files`

Manual Verification:
- [ ] Feature works as expected (test: specific scenario)
- [ ] Edge cases handled
- [ ] No regressions
```

### Playbook Feedback Workflow

**After completing a plan, give feedback on patterns used:**

1. **Review patterns from `(activate ...)` query** that informed the plan structure.

2. **Give feedback:**
   ```lisp
   pq_query('(-> (pattern "<pattern-id>") (:feedback! :helpful "shaped phase structure for X"))')
   pq_query('(-> (pattern "<pattern-id>") (:feedback! :harmful "didnt apply to this planning context"))')
   ```

3. **Planning-specific patterns to capture:**
   - Phasing strategies that worked well
   - Decomposition approaches
   - Verification patterns that would catch issues early
   - Out-of-scope definition strategies

4. **Record planning insights as observations** (patterns promoted during `/kli:reflect`):
   ```
   observe("Planning insight: <description>. Evidence: plan approach")
   ```

### Next Steps

After completing planning:
1. **Give feedback** on patterns that informed the plan
2. User reviews plan for completeness
3. If approved → Run `/kli:implement` to execute plan phase-by-phase
4. If changes needed → Iterate on plan
5. Implementation phase will load plan and execute phases sequentially

## TQ Cheatsheet (Task Query Language)

Plans are task DAGs. Use TQ to inspect and manipulate them:

**Inspecting Plans:**
```
task_query("(query \"plan\")")                    # All phases with enriched state
task_query("(query \"plan-ready\")")              # Non-completed phases only
task_query("(-> (current) (:follow :phase-of) :ids)")  # Just phase IDs
```

**Creating Plans (scaffold-plan!):**
```
task_query("(scaffold-plan!
  (implement-core-library \"Core library with validation\")
  (add-integration-layer \"Integration\" :after implement-core-library)
  (write-test-suite \"Tests\" :after add-integration-layer))")
```
Creates phases with dependencies in one expression. Phase names are validated for descriptiveness.

**Auto-improvement:** If you use short names like `p1` with descriptions, they're auto-improved:
- `(p1 "Research architecture")` → creates phase named `research-architecture`

**Creating Linear Chains:**
```
task_query("(scaffold-chain! \"Setup infrastructure\" \"Implement core\" \"Add tests\")")
```
Creates a linear dependency chain automatically.

**Restructuring Plans (bulk sever):**
```
;; Remove single phase from plan
task_query("(-> (node \"obsolete-phase\") (:sever-from-parent! :phase-of))")

;; Remove multiple phases at once (replaces multiple task_sever calls)
task_query("(-> (node \"phase-1\" \"phase-2\" \"phase-3\") (:sever-from-parent! :phase-of))")

;; Add dependency between phases
task_query("(-> (node \"phase-2\") (:link! \"phase-1\" :depends-on))")
```
`:sever-from-parent!` finds each node's parent via reverse graph lookup and severs the edge. This is the inverse of severing TO a target — it severs FROM parent.

### Phase Naming Guidelines

Phase names are validated to ensure the task graph remains navigable.

**Good names** (pass validation):
- `implement-user-auth` - verb + object
- `phase-1-fix-login-redirect` - prefix + semantic content
- `research-caching-strategies` - action + topic
- `CREATE-VALIDATION-MODULE` - descriptive (case insensitive)

**Bad names** (rejected or auto-improved from description):
- `P1`, `P2`, `P3` - letter+number only, no meaning
- `phase-1` - no semantic content after prefix
- `stuff`, `misc`, `wip` - vague words
- `foo`, `bar` - too short

**Named Queries:**
- `"plan"` - Phases of current task with enriched state
- `"plan-ready"` - Ready (non-completed) phases
- `"recent"` - Tasks by session count (most active)
- `"busy"` - Tasks by observation count
- `"hub-tasks"` - Tasks by edge count (most connected)
