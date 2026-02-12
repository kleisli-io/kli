---
name: kli-implementation
description: Implementation phase domain knowledge including TDD methodology, design principles (Extensibility/Composability/Parametricity), and quality standards. Use when implementing features using TDD workflow, writing code with tests, or applying design principles. Activates for coding and testing tasks. DO NOT use for research or planning.
---

<!-- ACE Metadata:
  source: ace/playbooks/ace.md, .claude/commands/implement.md
  command-complement: .claude/commands/implement.md
  sync-threshold: 3
  last-synced: 2026-01-02
  version: 1.1
  pattern-ids: [ace-017], [ace-052], [ace-060], [ace-089], [ace-094], [ace-113]
-->

# ACE Implementation Phase Skill

## Requirements & Standards

### TDD Discipline (CRITICAL)

**Test-Driven Development is NON-NEGOTIABLE in ACE implementation.**

The cycle MUST be followed:
1. **Red**: Write failing tests FIRST
2. **Green**: Implement minimum code to pass tests
3. **Refactor**: Improve design while keeping tests green

**Why This Matters:**
- Tests define behavior before implementation (design thinking)
- Failing tests confirm tests actually test something
- Passing tests confirm implementation meets requirements
- Green tests during refactor confirm no regressions
- Pattern [ace-052]: Never proceed without passing automated verification

**Common TDD Violations:**
- ❌ Writing implementation before tests
- ❌ Skipping Red phase (tests never fail)
- ❌ Skipping Refactor phase (technical debt accumulates)
- ❌ Batching multiple features before testing
- ✅ One feature → Write test → Implement → Refactor → Verify

### Design Principles (The Three Pillars)

**Every implementation decision must consider:**

**1. Extensibility** - Can new variants be added without modifying existing code?
- Use: Directory-based registration (not hardcoded lists)
- Use: Plugin architectures (load from directories)
- Avoid: Enum/switch statements (require modification)
- Example: `projects/nixos-module-engine/src/domains/` - new domains added by creating files

**2. Composability** - Can components be combined in new ways?
- Use: Algebraic effects (free monads, handlers)
- Use: Pure data structures (no hidden state)
- Use: Function composition (small, focused functions)
- Example: `projects/nixos-module-engine/` - effects compose to build complex operations

**3. Parametricity** - Are values parameterized instead of hardcoded?
- Use: Configuration via arguments/environment
- Avoid: Magic strings/numbers embedded in code
- Avoid: Assumptions about deployment context
- Example: `nix/buildRust/` - all paths parameterized, no assumptions

**Applying These Principles:**
- Ask during refactor: "How would I add a new variant?"
- Ask during refactor: "Can I compose this with something else?"
- Ask during refactor: "Are there any hardcoded assumptions?"

### Zero TODOs Policy

**NO TODOs ARE ALLOWED IN COMMITTED CODE**

**Why:**
- TODOs in committed code become technical debt
- Incomplete work blocks phase completion
- Verification should catch TODOs automatically

**Enforcement:**
```bash
# Automated check (in every phase)
git diff --name-only | grep -E "^(ace/|projects/|nix/|ops/)" | \
  xargs rg "TODO|FIXME|HACK" && \
  echo "TODOs found - must fix" || \
  echo "No TODOs ✓"
```

**Handling TODOs:**
- Complete the work before phase ends
- If truly future work: Create GitHub issue, remove TODO
- If out of scope: Document in plan.md out-of-scope, remove TODO
- Never commit code with TODOs

## Overview

The implementation phase executes plans using strict TDD methodology, applies design principles (Extensibility/Composability/Parametricity), and enforces verification gates (automated + manual) before proceeding to next phases. It produces observe() calls for reflection.

**Role in Workflow:**
1. **Plan Navigation**: Uses `task_query("(query \"plan-ready\")")` to find next phase, `task_get()` on phase tasks for details
2. **Observation Recording**: Records progress via `observe()` into the event stream
3. **Phase-by-Phase**: One phase at a time, verification gates between, `task_complete()` marks phase done
4. **Pattern Application**: Apply patterns from PQ queries (`(-> (search "...") ...)`, `(-> (proven) ...)`)

**Key Characteristics:**
- TDD cycle for every feature (Red→Green→Refactor)
- Design principles applied during Refactor
- Automated + manual verification (both required, both blocking)
- Resume capability via ✓ checkmarks in plan.md
- Deviation handling (pause and ask user)

## Quick Start

**Before starting any implementation:** Activate playbook patterns (REQUIRED):
```
pq_query('(-> (activate "<implementation task>" :boost (<relevant domains>)) (:take 5))')
```
This retrieves proven patterns via graph-based search and persists for handoff continuity.

**Basic Implementation Workflow (Per Phase):**

1. **Announce phase** start
   - Extract from plan.md: Overview, Changes Required, Success Criteria
   - Update observations file

2. **Reference activated patterns** for this phase
   - Apply patterns from `(activate ...)` output
   - Document pattern applications in observations

3. **Read referenced files FULLY**
   - All files mentioned in "Changes Required"
   - No limit/offset parameters
   - Full context before changes

4. **TDD Red**: Write failing tests
   - Tests that express desired behavior
   - Run to confirm they fail for right reason
   - Update observations

5. **TDD Green**: Implement to pass tests
   - Minimum code to make tests pass
   - Run tests frequently
   - Update observations

6. **TDD Refactor**: Improve design
   - Apply Extensibility/Composability/Parametricity
   - One change at a time
   - Keep tests green throughout
   - Update observations

7. **Run automated verification**
   - Build, tests, TODO check, etc.
   - ALL must pass before manual verification
   - Document results in observations

8. **Add ✓ to plan.md** phase header
   - ONLY after automated verification passes

9. **Request manual verification**
   - Present checklist from plan.md
   - Wait for user approval
   - Document approval in observations

10. **Proceed to next phase** or complete

## TDD Methodology

### Phase 1: Red (Write Failing Tests)

**Goal:** Create tests that fail for the RIGHT reason

**Steps:**

1. **Identify what to test:**
   - From phase "Changes Required" in plan.md
   - What behavior must the code exhibit?
   - What edge cases must be handled?

2. **Write tests expressing desired behavior:**
   ```lisp
   ;; Example: FiveAM test
   (test my-feature-works
     "Tests that my-feature produces correct output"
     (is (= expected-value (my-feature input))))
   ```

3. **Run tests to confirm failure:**
   ```bash
   nix build -f . project.tests
   # Expected: FAILED - "my-feature not defined" or similar
   ```

4. **Verify failure reason:**
   - ✅ Good: "function not defined", "feature not implemented"
   - ❌ Bad: syntax error, import failure, test logic error
   - If bad failure: Fix test, try again

**Document in observations:**
```markdown
### TDD Red: Write Failing Tests

**Tests written:**
- `test/my-feature.lisp:15` - Tests correct output for valid input
- `test/my-feature.lisp:23` - Tests error handling for invalid input

**Test execution:**
$ nix build -f . project.tests
Result: FAILED (as expected)
Reason: my-feature function not defined

**Effectiveness:** Tests fail for correct reason ✓
```

**Critical Success Factors:**
- Tests MUST fail before implementing
- Failure reason MUST be "not implemented" (not syntax/import errors)
- Tests MUST be specific and focused
- Tests MUST express desired behavior clearly

### Phase 2: Green (Implement to Pass Tests)

**Goal:** Implement MINIMUM code to make tests pass

**Steps:**

1. **Implement changes from plan.md:**
   - Follow "Changes Required" specification
   - Focus on making tests pass (not perfection yet)
   - Reference playbook patterns from `(-> (search "...") ...)` output

2. **Run tests FREQUENTLY:**
   ```bash
   # After each logical unit of code:
   nix build -f . project.tests
   ```

3. **Iterate until tests pass:**
   - Add missing functionality
   - Handle edge cases caught by tests
   - Fix test failures

4. **Verify ALL tests pass:**
   ```bash
   nix build -f . project.tests
   # Expected: SUCCESS - all tests pass
   ```

**Document in observations:**
```markdown
### TDD Green: Implement to Pass Tests

**Changes made:**
- `src/my-feature.lisp:42` - Added my-feature function
- `src/my-feature.lisp:58` - Added input validation
- `src/errors.lisp:15` - Added custom error for invalid input

**Challenges encountered:**
- Input validation required additional error type
- Resolution: Added custom error condition

**Test execution:**
$ nix build -f . project.tests
Result: PASSED ✓

**Effectiveness:** All tests passing after implementation
```

**Critical Success Factors:**
- Implementation focused on passing tests (perfection comes in Refactor)
- Tests run frequently (catch issues early)
- All tests pass before proceeding to Refactor
- Changes align with plan.md specification

### Phase 3: Refactor (Improve Design While Tests Green)

**Goal:** Improve code quality WITHOUT changing behavior

**The Three Design Principles (Apply in Order):**

**1. Extensibility Check:**

Question: "How would I add a new variant without modifying this code?"

Patterns:
- **Directory-based registration**: New variants added by creating files
  ```
  domains/
    http.lisp      # HTTP domain
    file.lisp      # File domain
    network.lisp   # Network domain (added later without modifying existing)
  ```

- **Plugin architectures**: Load modules from directories at runtime
  ```lisp
  (defun load-plugins (directory)
    (mapcar #'load (uiop:directory-files directory "*.lisp")))
  ```

- **Avoid hardcoded lists/enums**: Use discovery instead
  ```lisp
  ;; ❌ Bad: Hardcoded list (requires modification)
  (defun available-domains ()
    '(http file network))  ; Need to edit when adding new domain

  ;; ✅ Good: Discovery-based (no modification needed)
  (defun available-domains ()
    (mapcar #'domain-name
            (load-domains "src/domains/")))
  ```

**2. Composability Check:**

Question: "Can I combine this with other components in new ways?"

Patterns:
- **Algebraic effects**: Build complex operations from simple effects
  ```lisp
  ;; Effects compose naturally
  (run-effects
    (with-http-client
      (with-database
        (fetch-user-data user-id))))
  ```

- **Pure data structures**: No hidden state, easy to reason about
  ```lisp
  ;; ✅ Good: Pure function, composes easily
  (defun process-data (data config)
    (transform data (config-transform-fn config)))
  ```

- **Function composition**: Small, focused functions that combine
  ```lisp
  (defun process-pipeline (input)
    (-> input
        validate
        transform
        persist))
  ```

**3. Parametricity Check:**

Question: "Are there any hardcoded values that should be parameters?"

Patterns:
- **No magic strings/numbers**:
  ```lisp
  ;; ❌ Bad: Magic number
  (defun retry-operation (op)
    (loop repeat 3 do (attempt op)))  ; Why 3?

  ;; ✅ Good: Parameterized
  (defun retry-operation (op &key (max-retries 3))
    (loop repeat max-retries do (attempt op)))
  ```

- **Configuration via arguments**:
  ```lisp
  ;; ❌ Bad: Assumes environment
  (defun connect-db ()
    (connect "localhost" 5432))  ; Hardcoded!

  ;; ✅ Good: Configurable
  (defun connect-db (host port)
    (connect host port))
  ```

- **No deployment assumptions**:
  ```lisp
  ;; ❌ Bad: Assumes specific path
  (defun load-config ()
    (read-file "/etc/myapp/config.toml"))

  ;; ✅ Good: Path provided by caller
  (defun load-config (config-path)
    (read-file config-path))
  ```

**Refactoring Process:**

1. Identify refactoring opportunity (apply one principle)
2. Make ONE change at a time
3. Run tests after EACH change
4. Confirm tests still pass (GREEN)
5. Repeat for next refactoring

**Document in observations:**
```markdown
### TDD Refactor: Improve Design While Tests Green

**Refactorings applied:**

1. **Extensibility improvement:**
   - Changed: domain-loader from hardcoded list to directory-based discovery
   - Why: New domains can be added by creating files (no code changes)
   - Pattern: [org-018] directory-based registration
   - Tests: Still passing ✓

2. **Composability improvement:**
   - Changed: extracted validate-input as separate pure function
   - Why: Can now compose with other validators, reuse in tests
   - Pattern: [lisp-036] separate business logic from side effects
   - Tests: Still passing ✓

3. **Parametricity improvement:**
   - Changed: retry-count from hardcoded 3 to parameter
   - Why: Different use cases need different retry counts
   - Tests: Still passing ✓

**Final test execution:**
$ nix build -f . project.tests
Result: PASSED ✓ (tests green throughout refactoring)
```

**Critical Success Factors:**
- Tests REMAIN GREEN throughout (run after every change)
- One refactoring at a time (don't batch)
- Apply all three principles systematically
- Reference playbook patterns for guidance

## Workflows

### Standard Phase Implementation Workflow

```
1. Find ready phase: task_query("(query \"plan-ready\")") → pick first
   Switch to phase task: task_bootstrap("phase-N")
   - Extract from phase description: overview, changes, success criteria
   - observe("Starting phase N: <goal>")
   ↓
2. Apply patterns from PQ queries (`(-> (search "...") ...)`, `(-> (proven) ...)`)
   - Reference relevant patterns for this phase's domain
   - Record pattern applications via observe()
   ↓
3. Read referenced files FULLY
   - All files in "Changes Required"
   - Use Read without limit/offset
   - Understand full context
   ↓
4. TDD Red: Write failing tests
   - Express desired behavior in tests
   - Run tests, confirm failure for right reason
   - observe("TDD Red: <tests and failure reason>")
   ↓
5. TDD Green: Implement to pass tests
   - Write minimum code to pass
   - Run tests frequently
   - observe("TDD Green: <implementation summary>")
   ↓
6. TDD Refactor: Improve design
   - Apply Extensibility check
   - Apply Composability check
   - Apply Parametricity check
   - Keep tests green throughout
   - observe("TDD Refactor: <improvements applied>")
   ↓
7. Run automated verification
   - Build, tests, TODO check, etc.
   - Record results via observe()
   - Fix failures immediately (don't proceed)
   ↓
8. Request manual verification
   - Present checklist from phase description
   - Wait for user approval
   ↓
9. Mark phase complete:
   - observe("Phase N complete. Key outcomes: <summary>")
   - task_complete()  # Marks phase task as completed
   ↓
10. Return to parent: task_set_current(parent_id)
    Continue to next phase via task_query("(query \"plan-ready\")")
    If issues found: Fix, re-verify, request approval again
```

### Resume Capability Pattern

**Phase completion tracked via task DAG:**

```
task_query("(query \"plan\")")       → Shows all phases with completion status
task_query("(query \"plan-ready\")") → Shows phases ready to work on (non-completed)
```

**Resume behavior:**
- `task_bootstrap(parent_task_id)` then `task_query("(query \"plan-ready\")")` finds resume point
- Completed phases are immutable (task_complete guard)
- Record resume via `observe("Resuming from phase N")`

### Deviation Handling Pattern

**When code/reality differs from plan:**

1. **PAUSE immediately**
2. **Inform user of discrepancy:**
   ```
   PAUSE: Code differs from plan

   Issue: <Describe discrepancy>
   Plan expected: <What plan.md said>
   Reality found: <What actually exists>

   Proposed adaptation: <How to adjust>

   Options:
   1. Proceed with adapted approach (document in observations)
   2. Update plan.md to reflect reality
   3. Different approach (please specify)
   ```
3. **Wait for user decision**
4. **Document in observations:**
   ```markdown
   ### Deviation from Plan

   Issue: <discrepancy>
   Plan vs Reality: <comparison>
   User decision: <option chosen>
   Adaptation: <how implementation adjusted>
   ```

**Common Deviations:**
- File structure differs from plan
- API signatures changed since planning
- Dependencies added/removed
- Assumptions invalid (discovered during implementation)

## Verification & Quality Gates

### Automated Verification Checklist

**MUST pass before requesting manual verification:**

```bash
# 1. Build passes
nix build -f . <target>
# Result: SUCCESS

# 2. Tests pass
nix build -f . <tests-target>
# Result: SUCCESS

# 3. TODO check (Zero TODOs policy)
git diff --name-only | grep -E "^(ace/|projects/|nix/|ops/)" | \
  xargs rg "TODO|FIXME|HACK"
# Result: No matches (exit code 1 from rg)

# 4. Syntax validation (language-specific)
nix-instantiate --parse file.nix  # For Nix
sbcl --noinform --non-interactive --load file.lisp  # For Lisp

# 5. Additional checks (from plan.md Success Criteria)
<Custom checks as specified>
```

**If ANY check fails:**
- Fix immediately
- Re-run verification
- Update observations with fix
- Do NOT proceed until all pass

### Manual Verification Checklist

**Present to user for judgment:**

```
Phase N - READY FOR MANUAL VERIFICATION

Automated Verification: ✅ PASSED
- Build: ✓
- Tests: ✓
- TODO check: ✓
- <Other checks>: ✓

Manual Verification Required:

Please verify (from plan.md):
□ <Manual check 1 from plan>
□ <Manual check 2 from plan>
□ <Manual check 3 from plan>

Files changed:
- <List files modified in this phase>

How to test:
<Testing instructions if applicable>

Type "approved" to proceed or describe issues found.
```

**Wait for user response** - do NOT proceed without approval

### TodoWrite Integration Pattern

**Use TodoWrite for phase tracking:**

```
At phase start:
- Create subtasks for phase components
- Mark first subtask "in_progress"

During implementation:
- Mark subtasks "completed" as work finishes
- Update progress incrementally

Before phase ✓:
- Verify all phase subtasks "completed"
- Verify only ONE task "in_progress" (next phase or none)
```

**Pattern [ace-089]:** Add explicit tracing for debugging integration issues

### Pattern Tracking with Source Attribution

**IMPORTANT: Track pattern applications for reflection pipeline**

When you apply a pattern during implementation, document it in observations with:
- **Pattern ID** - The `[id]` from the pattern bullet
- **Source** - Either "skill" (from loaded skill) or "playbook" (from playbook tools)
- **Context** - Why/where you applied it
- **Outcome** - SUCCESS or FAILURE

**Document patterns in observations:**

```markdown
## Pattern Applications

### Applied Patterns

| Pattern | Source | Context | Outcome |
|---------|--------|---------|---------|
| [ace-052] | skill:kli-implementation | Verification gate before phase 2 | SUCCESS |
| [ace-017] | skill:kli-implementation | Phase-by-phase execution | SUCCESS |
| [lisp-044] | playbook:lisp.md | Subprocess capture pattern | SUCCESS |
| [nix-new] | playbook:nix.md | Experimental Nix pattern | FAILURE |

### Patterns That Helped
- [ace-052] - Verification gate caught 2 issues early
- [lisp-044] - Subprocess pattern worked perfectly

### Patterns That Caused Issues
- [nix-new] - Led to 5 extra iterations, approach didn't fit context
```

**When to record:**
1. **On application** - When you consciously apply a pattern from playbook tools or loaded skill
2. **On outcome** - When you can determine if the pattern helped or caused issues
3. **At phase end** - Summarize patterns used and their effectiveness

**Source attribution matters because:**
- Patterns from skills need effectiveness tracking
- Patterns from playbooks need promotion/demotion signals
- Reflection pipeline uses this for curator decisions

## Reference

### Pattern IDs (from ace/playbooks/ace.md)

- **[ace-017]** helpful=32: Implement phase-by-phase, run automated verification after each
- **[ace-052]** helpful=38: Never proceed without passing automated verification
- **[ace-060]** helpful=22: Don't batch phases - implement incrementally
- **[ace-089]** helpful=2: Add explicit tracing for debugging
- **[ace-094]** helpful=5: Integration validation - test real interaction, not mocks
- **[ace-113]** helpful=15: TDD workflow - Red (failing tests), Green (implement to pass), Refactor (design principles). Especially critical for infrastructure code with nested data structures

### File References

**Command Source:**
- `.claude/commands/implement.md:201-235` - TDD Red phase
- `.claude/commands/implement.md:237-271` - TDD Green phase
- `.claude/commands/implement.md:273-328` - TDD Refactor phase (design principles)
- `.claude/commands/implement.md:330-359` - Automated verification

**Playbook Source:**
- `ace/playbooks/ace.md` - ACE workflow patterns (helpful >= 3)
- `ace/playbooks/depot-organization.md` - Extensibility patterns (directory-based registration)
- `ace/playbooks/lisp.md` - Composability patterns (pure functions, effects)

### Next Steps

After completing all phases:

1. **Give feedback on all applied patterns:**
   Review the Pattern Applications table in observations. For each row with SUCCESS/FAILURE:
   ```lisp
   pq_query('(-> (pattern "<pattern-id>") (:feedback! :helpful "<evidence from Outcome column>"))')
   pq_query('(-> (pattern "<pattern-id>") (:feedback! :harmful "<evidence from Outcome column>"))')
   ```

2. **Record discoveries as observations** (patterns promoted during `/ace:reflect`):
   ```
   observe("Implementation discovery: <description>. Evidence: <what happened>")
   ```

3. Set parent task metadata: `task_set_metadata(key="phase", value="complete")`
4. Record final status via `observe()`: completion summary, challenges, patterns used
5. Run `/ace:validate` to verify implementation against plan
6. Run `/core:commit` to create conventional commit message
7. Run `/ace:reflect` to extract learnings and update playbooks

## TQ Cheatsheet (Task Query Language)

**Plan Inspection:**
```
task_query("(query \"plan\")")       # All phases with enriched state
task_query("(query \"plan-ready\")") # Non-completed phases (ready to work on)
task_query("(current)")              # Current task as node-set
```

**Phase Navigation:**
```
task_query("(-> (current) (:follow :phase-of) :enrich)")  # Phases with full state
task_query("(-> (current) (:follow :phase-of) :ids)")     # Just phase IDs
```

**Batch Mutations:**
```
task_query("(-> (node \"temp-\") (:complete!))")           # Complete matching tasks
task_query("(-> (current) (:observe! \"Implementation complete\"))")  # Add observation
```

## See Also

- `ace/lib/playbook-integration.md` - Full playbook workflow reference
