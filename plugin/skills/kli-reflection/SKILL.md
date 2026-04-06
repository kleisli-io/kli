---
name: kli-reflection
description: Reflection phase domain knowledge including pattern evaluation, observation analysis, and evidence-based learning. Use when analyzing task outcomes, evaluating pattern effectiveness, or updating knowledge bases with learnings. Activates for retrospective and learning tasks. DO NOT use for research, planning, or implementation.
---

# KLI Reflection Phase Skill

This skill provides **declarative knowledge** for the reflection phase: methodology, criteria, schemas, and principles. For the **procedural workflow** (orchestration steps), see the `/kli:reflect` command.

## Evidence-Based Learning (CRITICAL)

**All pattern evaluations MUST be backed by observation evidence.**

Reflection is NOT about opinions or intuitions - it's about extracting learnings from captured task trajectories:

- "Pattern X seems useful" - NOT VALID (no evidence)
- "Pattern X applied in implementation observations (via task_get/timeline):142, resulted in 40% token savings (measured)" - VALID (evidence-based)

### Why Observations Matter

| Purpose | How Observations Help |
|---------|----------------------|
| Concrete evidence | Pattern effectiveness backed by real application |
| Challenge documentation | Shows problems encountered and how resolved |
| Pattern tracking | Shows which patterns were actually applied |
| Impact measurement | Time saved, errors avoided, quality improved |
| Playbook evolution | Data-driven counter updates |

### Observation Requirements

Observations are recorded via `observe()` during KLI commands and surfaced via:
- `task_get()` — shows the last 3 observations
- `timeline(limit=50)` — shows all events including observations

Evidence categories:
- **Research observations** — research iteration trajectory, findings, agent effectiveness
- **Planning observations** — planning decisions, phase design rationale
- **Implementation observations** — TDD cycles, challenges, verification results

**No Observations = No Reflection**: Cannot evaluate patterns without evidence, cannot update counters without justification.

---

## Sequential Execution Pattern

**reflector → curator (MUST be sequential, NOT parallel)**

The agents have dependencies:
1. **Reflector** reads event stream (via `task_get` + `timeline`), produces reflection.md
2. **Curator** reads reflection.md, updates playbooks via PQ mutations (`(:feedback! ...)`, `(add! ...)`, `(:evolve! ...)`)

**Why Sequential**: Each agent depends on the previous agent's output. Spawning in parallel means later agents have no input to work with.

---

## Pattern Evaluation Methodology

### What to Extract from Observations

Use `task_get()` + `timeline(limit=50)` to surface observations. Look for:

**From research phase observations:**
- Which agents were effective?
- Iteration count (fewer is better)
- Gap analysis and resolution
- Exit criteria achievement
- **Evidence**: Agent effectiveness notes, iteration counts

**From planning phase observations:**
- Research artifact reuse (token savings from reusing research.md)
- Plan iteration count, phase design decisions
- Clarifying questions asked
- **Evidence**: Research reuse notes, iteration counts

**From implementation phase observations:**
- TDD discipline (Red → Green → Refactor documented)
- Design principles applied (Extensibility, Composability, Parametricity)
- Verification attempts per phase
- Challenges encountered and resolved
- **Evidence**: TDD iterations, refactoring notes, verification results

### Pattern Effectiveness Criteria

**Helpful Indicators:**
```
✅ Pattern was applied (documented in observations)
✅ Led to positive outcome (faster, fewer errors, cleaner code)
✅ Had measurable impact (X% faster, Y fewer iterations)
✅ Matched intended use case
✅ Would recommend using again
```

**Harmful Indicators:**
```
❌ Pattern was applied (documented in observations)
❌ Led to negative outcome (slower, more errors, confusion)
❌ Had measurable negative impact
❌ Mismatched use case or misleading
❌ Would NOT recommend using again
```

**Neutral (No Counter Change):**
```
⚪ Pattern mentioned but not actually applied
⚪ Applied but no observable impact
⚪ Insufficient evidence to evaluate
```

### Evaluation Process

1. Find pattern reference in observations (e.g., "artifact reuse saved tokens")
2. Extract context: What was done? What was the outcome?
3. Look for measurable evidence: time saved, errors avoided, quality improved
4. Classify: Helpful, Harmful, or Neutral
5. Document evidence in reflection.md

---

## Harm Signal Tier Definitions

Pattern harm is classified into tiers for appropriate response:

### Tier 1: Auto-Action (Definitive Harm)

**Signals:**
- outcome=FAILURE recorded
- Git reverts of pattern application
- Explicit user rejection ("that didn't work")
- Test failures directly caused by pattern

**Response:** Auto-increment harmful counter. Clear evidence of damage.

### Tier 2: Flag for Review (Probable Harm)

**Signals:**
- Excessive iterations (>5 for simple task)
- Implicit correction (user redoes work differently)
- Confusion requiring clarification
- Time wasted on wrong approach

**Response:** Increment harmful counter with review note. Needs human judgment.

### Tier 3: Track Only (Uncertain)

**Signals:**
- Minor iterations (normal debugging)
- Context mismatch (pattern applied to wrong domain)
- Partial success (worked but not optimal)

**Response:** Track in reflection.md but no counter change. Insufficient evidence.

---

## New Pattern Discovery

### When to Identify New Pattern

- Novel approach used that isn't in playbooks
- Recurring solution that proved effective (seen 2+ times)
- Workaround for common issue
- Integration technique that worked well

### New Pattern Documentation Template

```markdown
### New Pattern Discovered

**Pattern**: [temp-id] <Short description>
**Context**: <When this pattern applies>
**Approach**: <What to do>
**Outcome**: <Result with evidence from this task>
**Recommendation**: <Add to playbook>
**Domain**: <your domain>  (e.g., python, typescript, rust, infrastructure, api, frontend)
```

### Quality Criteria for New Patterns

| Criterion | Requirement |
|-----------|-------------|
| Specific | Actionable advice, not vague guidance |
| Evidence | Effectiveness proven in observations |
| Reusable | Applicable in similar future contexts |
| Novel | Not covered by existing patterns |

---

## Agent Responsibilities

### Reflector Agent

| Aspect | Details |
|--------|---------|
| **Input** | Task ID — uses `task_get()` + `timeline()` for observations, reads artifacts |
| **Process** | Analyze observations from event stream, evaluate patterns, classify harm signals, discover new patterns |
| **Output** | reflection.md artifact with recommendations |
| **Tools** | mcp__task__*, Read, Grep, Search |

> **Note**: The event stream (observations from `observe()`) is the source of truth for pattern evidence.

### Curator Agent

| Aspect | Details |
|--------|---------|
| **Input** | reflection.md artifact |
| **Process** | Update playbook via MCP tools, process harm signals by tier, add new patterns |
| **Output** | Playbook updates via PQ mutations (`(:feedback! ...)`, `(add! ...)`, `(:evolve! ...)`) |
| **Tools** | pq_query, Read, Grep, Search |

---

## Reflection Artifact Structure

### reflection.md Template

```markdown
---
date: <ISO timestamp>
task: <task directory>
status: complete
---

# Reflection: <Task Name>

## Patterns Applied

### [pattern-id]: <Pattern Name>
- **Applied in**: timeline event N
- **Context**: <What was done>
- **Outcome**: <Result with evidence>
- **Effectiveness**: Helpful | Harmful | Neutral
- **Recommendation**: increment helpful | increment harmful | no change

## Harm Signals

### Tier 1 (Auto-Action)
- [pattern-id]: <evidence of definitive harm>

### Tier 2 (Flagged for Review)
- [pattern-id]: <evidence of probable harm>

### Tier 3 (Tracked Only)
- [pattern-id]: <uncertain signal>

## New Patterns Discovered

### [temp-id]: <Pattern Name>
- **Context**: <When this applies>
- **Approach**: <What to do>
- **Outcome**: <Result with evidence>
- **Recommendation**: Add to <playbook-name>

## Challenges & Resolutions

### Challenge: <Description>
- **Context**: <When encountered>
- **Resolution**: <How resolved>
- **Pattern**: <Existing or new>

## Playbook Update Recommendations

**Playbook Updates:**
- [pattern-NNN]: increment helpful (evidence: <ref>)
- [pattern-MMM]: increment harmful (evidence: <ref>)
- Add new pattern: [temp-id] via `(add! :domain :ace :content "...")`

## Summary

**Patterns Evaluated:** <N>
**Helpful:** <count> | **Harmful:** <count> | **Neutral:** <count>
**New Patterns:** <count>
**Harm Signals:** Tier 1: <N>, Tier 2: <M>, Tier 3: <K>
**Key Learnings:** <3-5 bullet points>
```

---

## TQ and Observation Tools for Reflection

Use these to gather evidence for pattern evaluation:

```
obs_search(query="pattern X applied")       # Find observations mentioning a pattern
enriched_retrieve(k=10)                      # Context-aware retrieval for current task
obs_feedback(text="...", outcome="success")  # Record observation quality feedback
task_query("(query \"plan\")")               # Review plan phases and their status
task_query("(query \"busy\")")               # Tasks with most observations (richest evidence)
```

---

## Reference

### Core Principles

- Document what IS, not what SHOULD BE
- Reflector agent analyzes observations
- Add explicit tracing for debugging

### Playbook Access

Patterns are managed via PQ queries, not file paths:
- All patterns: `(-> :all (:group-by :domain))`
- Proven patterns: `(-> (proven :min 3) (:take 10))`
- Domain-specific: `(-> :all (:where (domain= :lisp)) :ids)`
