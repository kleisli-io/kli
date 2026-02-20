---
name: kli-implementation
description: Implementation phase domain knowledge including TDD methodology, design principles (Extensibility/Composability/Parametricity), and quality standards. Use when implementing features using TDD workflow, writing code with tests, or applying design principles. Activates for coding and testing tasks. DO NOT use for research or planning.
---

# KLI Implementation Phase Skill

## Requirements & Standards

### TDD Discipline (CRITICAL)

**Test-Driven Development is NON-NEGOTIABLE in KLI implementation.**

The cycle MUST be followed:
1. **Red**: Write failing tests FIRST
2. **Green**: Implement minimum code to pass tests
3. **Refactor**: Improve design while keeping tests green

**Why This Matters:**
- Tests define behavior before implementation (design thinking)
- Failing tests confirm tests actually test something
- Passing tests confirm implementation meets requirements
- Green tests during refactor confirm no regressions
- Never proceed without passing automated verification

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
- Example: A `plugins/` directory where new plugins are added by creating files — no registration code to modify

**2. Composability** - Can components be combined in new ways?
- Use: Middleware/pipeline patterns (compose handlers)
- Use: Pure data structures (no hidden state)
- Use: Function composition (small, focused functions)
- Example: A middleware pipeline where handlers compose — each handler is independent, ordering is configuration

**3. Parametricity** - Are values parameterized instead of hardcoded?
- Use: Configuration via arguments/environment
- Avoid: Magic strings/numbers embedded in code
- Avoid: Assumptions about deployment context
- Example: A CLI tool where all paths, ports, and URLs come from config or arguments — no hardcoded values

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
git diff --name-only | xargs rg "TODO|FIXME|HACK" && \
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
- Resume capability via task DAG completion status
- Deviation handling (pause and ask user)

## Quick Start

**Before starting any implementation:** Activate playbook patterns (REQUIRED):
```
pq_query('(-> (activate "<implementation task>" :boost (<relevant domains>)) (:take 5))')
```
This retrieves proven patterns via graph-based search and persists for handoff continuity.

**Basic Implementation Workflow (Per Phase):**

1. **Announce phase** start
   - Extract from phase task: Overview, Changes Required, Success Criteria
   - Record via observe()

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
   - Record via observe()

5. **TDD Green**: Implement to pass tests
   - Minimum code to make tests pass
   - Run tests frequently
   - Record via observe()

6. **TDD Refactor**: Improve design
   - Apply Extensibility/Composability/Parametricity
   - One change at a time
   - Keep tests green throughout
   - Record via observe()

7. **Run automated verification**
   - Build, tests, TODO check, etc.
   - ALL must pass before manual verification
   - Document results in observations

8. **Mark phase complete in task DAG**
   - ONLY after automated verification passes

9. **Request manual verification**
   - Present checklist from phase task description
   - Wait for user approval
   - Document approval in observations

10. **Proceed to next phase** or complete

## TDD Methodology

### Phase 1: Red (Write Failing Tests)

**Goal:** Create tests that fail for the RIGHT reason

**Steps:**

1. **Identify what to test:**
   - From phase "Changes Required" in task description
   - What behavior must the code exhibit?
   - What edge cases must be handled?

2. **Write tests expressing desired behavior:**
   ```python
   # Example test (any testing framework)
   def test_my_feature_works():
       """Tests that my_feature produces correct output"""
       assert my_feature(input) == expected_value
   ```

3. **Run tests to confirm failure:**
   ```bash
   # Run your project's test suite:
   <your test command>   # e.g., pytest, npm test, cargo test, go test ./...
   # Expected: FAILED - "my_feature not defined" or similar
   ```

4. **Verify failure reason:**
   - ✅ Good: "function not defined", "feature not implemented"
   - ❌ Bad: syntax error, import failure, test logic error
   - If bad failure: Fix test, try again

**Document in observations:**
```markdown
### TDD Red: Write Failing Tests

**Tests written:**
- `test/test_my_feature.py:15` - Tests correct output for valid input
- `test/test_my_feature.py:23` - Tests error handling for invalid input

**Test execution:**
$ <your test command>
Result: FAILED (as expected)
Reason: my_feature function not defined

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

1. **Implement changes from plan:**
   - Follow "Changes Required" specification
   - Focus on making tests pass (not perfection yet)
   - Reference playbook patterns from `(-> (search "...") ...)` output

2. **Run tests FREQUENTLY:**
   ```bash
   # After each logical unit of code:
   <your test command>
   ```

3. **Iterate until tests pass:**
   - Add missing functionality
   - Handle edge cases caught by tests
   - Fix test failures

4. **Verify ALL tests pass:**
   ```bash
   <your test command>
   # Expected: SUCCESS - all tests pass
   ```

**Document in observations:**
```markdown
### TDD Green: Implement to Pass Tests

**Changes made:**
- `src/my_feature.py:42` - Added my_feature function
- `src/my_feature.py:58` - Added input validation
- `src/errors.py:15` - Added custom error for invalid input

**Challenges encountered:**
- Input validation required additional error type
- Resolution: Added custom exception class

**Test execution:**
$ <your test command>
Result: PASSED ✓

**Effectiveness:** All tests passing after implementation
```

**Critical Success Factors:**
- Implementation focused on passing tests (perfection comes in Refactor)
- Tests run frequently (catch issues early)
- All tests pass before proceeding to Refactor
- Changes align with plan specification

### Phase 3: Refactor (Improve Design While Tests Green)

**Goal:** Improve code quality WITHOUT changing behavior

**The Three Design Principles (Apply in Order):**

**1. Extensibility Check:**

Question: "How would I add a new variant without modifying this code?"

Patterns:
- **Directory-based registration**: New variants added by creating files
  ```
  plugins/
    http.py        # HTTP plugin
    file.py        # File plugin
    network.py     # Network plugin (added later without modifying existing)
  ```

- **Plugin architectures**: Load modules from directories at runtime
  ```python
  def load_plugins(directory):
      return [import_module(f) for f in glob(f"{directory}/*.py")]
  ```

- **Avoid hardcoded lists/enums**: Use discovery instead
  ```python
  # ❌ Bad: Hardcoded list (requires modification)
  def available_backends():
      return ["http", "file", "network"]  # Need to edit when adding new backend

  # ✅ Good: Discovery-based (no modification needed)
  def available_backends():
      return [b.name for b in load_backends("src/backends/")]
  ```

**2. Composability Check:**

Question: "Can I combine this with other components in new ways?"

Patterns:
- **Middleware/pipeline patterns**: Build complex operations from simple handlers
  ```python
  # Handlers compose naturally
  app = Pipeline(
      authenticate,
      authorize,
      handle_request,
  )
  ```

- **Pure data structures**: No hidden state, easy to reason about
  ```python
  # ✅ Good: Pure function, composes easily
  def process_data(data, config):
      return transform(data, config.transform_fn)
  ```

- **Function composition**: Small, focused functions that combine
  ```python
  def process_pipeline(input):
      return persist(transform(validate(input)))
  ```

**3. Parametricity Check:**

Question: "Are there any hardcoded values that should be parameters?"

Patterns:
- **No magic strings/numbers**:
  ```python
  # ❌ Bad: Magic number
  def retry_operation(op):
      for _ in range(3):  # Why 3?
          attempt(op)

  # ✅ Good: Parameterized
  def retry_operation(op, max_retries=3):
      for _ in range(max_retries):
          attempt(op)
  ```

- **Configuration via arguments**:
  ```python
  # ❌ Bad: Assumes environment
  def connect_db():
      return connect("localhost", 5432)  # Hardcoded!

  # ✅ Good: Configurable
  def connect_db(host, port):
      return connect(host, port)
  ```

- **No deployment assumptions**:
  ```python
  # ❌ Bad: Assumes specific path
  def load_config():
      return read_file("/etc/myapp/config.toml")

  # ✅ Good: Path provided by caller
  def load_config(config_path):
      return read_file(config_path)
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
   - Changed: backend loader from hardcoded list to directory-based discovery
   - Why: New backends can be added by creating files (no code changes)
   - Tests: Still passing ✓

2. **Composability improvement:**
   - Changed: extracted validate_input as separate pure function
   - Why: Can now compose with other validators, reuse in tests
   - Tests: Still passing ✓

3. **Parametricity improvement:**
   - Changed: retry count from hardcoded 3 to parameter
   - Why: Different use cases need different retry counts
   - Tests: Still passing ✓

**Final test execution:**
$ <your test command>
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
   Plan expected: <What plan said>
   Reality found: <What actually exists>

   Proposed adaptation: <How to adjust>

   Options:
   1. Proceed with adapted approach (document in observations)
   2. Update plan to reflect reality
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
<your build command>   # e.g., npm run build, cargo build, go build ./...
# Result: SUCCESS

# 2. Tests pass
<your test command>    # e.g., pytest, npm test, cargo test, go test ./...
# Result: SUCCESS

# 3. TODO check (Zero TODOs policy)
git diff --name-only | xargs rg "TODO|FIXME|HACK"
# Result: No matches (exit code 1 from rg)

# 4. Syntax/lint validation (language-specific)
# Use your ecosystem's linter or type checker, e.g.:
#   eslint src/           # JavaScript/TypeScript
#   mypy src/             # Python
#   cargo clippy          # Rust
#   go vet ./...          # Go

# 5. Additional checks (from plan Success Criteria)
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

Please verify (from plan):
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

Before phase completion:
- Verify all phase subtasks "completed"
- Verify only ONE task "in_progress" (next phase or none)
```

**Debugging tip:** Add explicit tracing for debugging integration issues

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
| Verification gate | skill:kli-implementation | Verification gate before phase 2 | SUCCESS |
| Phase-by-phase | skill:kli-implementation | Phase-by-phase execution | SUCCESS |
| Directory registration | playbook | Plugin loader uses discovery | SUCCESS |

### Patterns That Helped
- Verification gate caught 2 issues early
- Directory registration avoided hardcoded list

### Patterns That Caused Issues
- <pattern-id> - Led to extra iterations, approach didn't fit context
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

### Core Principles

- Implement phase-by-phase, run automated verification after each
- Never proceed without passing automated verification
- Don't batch phases - implement incrementally
- Add explicit tracing for debugging
- Integration validation - test real interaction, not mocks
- TDD workflow - Red (failing tests), Green (implement to pass), Refactor (design principles). Especially critical for infrastructure code with nested data structures

### Playbook Source

Patterns are managed via PQ queries:
- Proven patterns: `(-> (proven :min 3) (:take 10))`
- Domain-specific: `(-> (activate "query" :boost (lisp nix)) (:take 5))`

### Next Steps

After completing all phases:

1. **Give feedback on all applied patterns:**
   Review the Pattern Applications table in observations. For each row with SUCCESS/FAILURE:
   ```lisp
   pq_query('(-> (pattern "<pattern-id>") (:feedback! :helpful "<evidence from Outcome column>"))')
   pq_query('(-> (pattern "<pattern-id>") (:feedback! :harmful "<evidence from Outcome column>"))')
   ```

2. **Record discoveries as observations** (patterns promoted during `/kli:reflect`):
   ```
   observe("Implementation discovery: <description>. Evidence: <what happened>")
   ```

3. Set parent task metadata: `task_set_metadata(key="phase", value="complete")`
4. Record final status via `observe()`: completion summary, challenges, patterns used
5. Run `/kli:validate` to verify implementation against plan
6. Run `/core:commit` to create conventional commit message
7. Run `/kli:reflect` to extract learnings and update playbooks

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
