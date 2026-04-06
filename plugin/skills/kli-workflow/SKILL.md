---
name: kli-workflow
description: Meta-orchestration skill for KLI framework. Provides high-level overview of research/planning/implementation/reflection phases and coordinates when to use phase-specific skills (kli-research, kli-planning, kli-implementation, kli-reflection). Auto-invoked for general KLI workflow questions. For phase-specific guidance, the appropriate phase skill will be loaded.
---

# KLI Workflow Meta-Orchestration Skill

## Overview

Coordinate KLI workflow using three roles: Generator (produce artifacts), Reflector (analyze outcomes), Curator (maintain knowledge). Use phase-specific skills for detailed execution guidance.

### Three-Role Architecture

**Generator Role** - Execute via slash commands:
- Run `/kli:research` to document codebase → Load **kli-research skill** for methodology
- Run `/kli:plan` to design phased implementation as task DAG → Load **kli-planning skill** for phase design
- Run `/kli:implement` to execute with TDD workflow → Load **kli-implementation skill** for TDD discipline
- Record progress via `observe()` (files tracked automatically by PostToolUse hook)

**Reflector Role** - Analyze via reflector agent:
- Read observations from event stream (`task_get()` + `timeline()`)
- Evaluate pattern effectiveness (helpful/harmful counters)
- Discover new patterns from task execution
- Generate reflection.md with playbook update recommendations

**Curator Role** - Maintain via curator agent:
- Update playbooks via PQ mutations (`(:feedback! ...)`, `(add! ...)`, `(:evolve! ...)`)
- Increment helpful/harmful counters based on evidence
- Add new patterns discovered by Reflector

### Observation-Driven Learning

Record observations via `observe()` in all Generator commands:
- Implementation trajectory and decisions
- Challenges encountered and resolutions
- Patterns applied and their effectiveness

Observations flow through the event stream. Reflector reads them via `task_get()` + `timeline()` → Curator updates playbooks via MCP tools → evidence-based knowledge evolution.

## Phase Coordination

### Phase Skill Selection

**Load kli-research for:**
- Starting new tasks requiring codebase understanding
- Investigating technical questions or root causes
- Understanding HOW code works or WHERE components live
- Documenting current state before planning changes

**Load kli-planning for:**
- Creating implementation plans for features or refactorings
- Breaking work into incremental phases with verification gates
- Defining success criteria (automated + manual)
- Designing phased approaches with clear boundaries

**Load kli-implementation for:**
- Executing implementation plans phase-by-phase
- Applying TDD workflow (Red → Green → Refactor)
- Following design principles (Extensibility/Composability/Parametricity)
- Managing verification gates and phase completion

**Load kli-reflection for:**
- Extracting learnings from completed tasks
- Analyzing patterns that helped or hindered
- Updating playbooks with evidence-based insights
- Discovering new patterns from implementation experience

### Phase Selection Decision Tree

```
Task type determines phase:
├─ Understand existing code → Load kli-research
├─ Design implementation approach → Load kli-planning
├─ Write code and tests → Load kli-implementation
└─ Extract learnings from results → Load kli-reflection
```

## Phase Transitions

### Execute Standard Workflow: Research → Plan → Implement → Reflect

**1. Research Phase**
- Run: `/kli:research`
- Produce: research.md, findings via `observe()`
- **→ Transition to Planning:** Research questions answered, codebase understood

**2. Planning Phase**
- Run: `/kli:plan`
- Produce: Plan as task DAG (`task_fork` + `phase-of` edges), optionally plan.md as artifact
- Record: Planning decisions via `observe()`
- **Reuse: research.md** (40-50% token savings)
- **→ Transition to Implementation:** Plan approved, phases clearly defined

**3. Implementation Phase**
- Run: `/kli:implement`
- Produce: Code changes, test files
- Record: TDD cycles, challenges via `observe()` on phase tasks
- Navigate: `task_query("(query \"plan-ready\")")` for next phase, `task_complete()` marks done
- **→ Transition to Reflection:** All phases complete, all verification passed

**4. Reflection Phase**
- Run: `/kli:reflect`
- Produce: reflection.md (pattern evaluation)
- Update: Playbooks via PQ mutations (`(:feedback! ...)`, `(add! ...)`, `(:evolve! ...)`)
- **→ Complete:** Knowledge extracted, playbooks evolved

### Skip Research or Planning (When Appropriate)

**Skip research for:**
- Tasks not requiring codebase understanding (pure documentation, small fixes)
- Code already deeply understood
- Research completed in previous related task

**Skip planning for:**
- Trivial tasks (<50 lines, single file, obvious approach)
- Exploratory work where plan would be premature
- Emergency hotfixes with well-understood single change

**Never skip:**
- Implementation verification gates (verify before proceeding)
- Reflection for complex tasks (capture knowledge required)

## Artifact Flow

### How Artifacts Connect Phases

```
research.md ─────────────> plan (task DAG)
  ↓                            ↓
  └─> observe() events    task_fork(phase-of) creates phases
                               ↓
                          /kli:implement navigates via plan-frontier
                               ↓
                          observe() per phase, task_complete() marks done
                               ↓
                          All observations (event stream) ──> /kli:reflect
                                                                ↓
                                                             reflection.md
                                                                ↓
                                                             playbook MCP tools
```

**Apply Artifact Reuse Pattern**:
- Detect existing research.md in `/kli:plan`
- Avoid re-spawning research subagents
- Reference findings directly in plan
- Save 40-50% tokens in planning phase

### Use Event Stream for Reflection

Record via `observe()` in all commands to provide:
- Complete trajectory documentation for Reflector analysis
- Evidence trail for pattern effectiveness evaluation
- Input for playbook helpful/harmful counter updates
- Historical record surfaced by `task_get()` + `timeline()`

## Load Phase Skills for Detailed Guidance

### Access Execution Methodology

Load phase-specific skills for detailed methodology, workflows, and checklists:

- **kli-research** - Documentarian philosophy, agent selection, decomposition patterns, exit criteria
- **kli-planning** - Phase design principles, verification strategies, artifact reuse, clarifying questions
- **kli-implementation** - TDD methodology, design principles (Extensibility/Composability/Parametricity), verification gates
- **kli-reflection** - Sequential execution, pattern evaluation, observation analysis, playbook evolution

Phase skills auto-invoke when using corresponding commands. This meta-skill coordinates WHEN to use each, while phase skills provide HOW to execute.

## Quick Reference

### Phase Comparison Table

| Phase | Command | Input | Output | Key Skill |
|-------|---------|-------|--------|-----------|
| Research | /kli:research | Task description | research.md | kli-research |
| Planning | /kli:plan | research.md (optional) | Task DAG (+ optional plan.md) | kli-planning |
| Implementation | /kli:implement | Task DAG | Code + tests | kli-implementation |
| Reflection | /kli:reflect | Event stream (timeline) | reflection.md | kli-reflection |

### Verification Gate Pattern

Critical principle across all phases:

```
Complete phase work
    ↓
Run automated verification (must pass)
    ↓
Mark phase complete in task DAG
    ↓
Request manual verification (must approve)
    ↓
ONLY THEN → Proceed to next phase
```

**Never proceed without passing BOTH automated and manual verification.**

### Core Principles

- Never proceed to next phase without passing automated verification
- Implement phase-by-phase, run automated verification after each
- Don't batch multiple phases without verification
- Reuse research.md artifacts (saves 40-50% tokens in planning)

### Next Steps

1. **For research tasks**: Load kli-research skill for detailed methodology
2. **For planning tasks**: Load kli-planning skill for phase design patterns
3. **For implementation**: Load kli-implementation skill for TDD discipline
4. **For reflection**: Load kli-reflection skill for pattern evaluation

## TQ Quick Reference (Task Query Language)

Plans are task DAGs. Use TQ (`task_query`) to inspect and navigate:

```
task_query("(query \"plan\")")           # All phases with status
task_query("(query \"plan-ready\")")     # Next phases to work on
task_query("(query \"health\")")         # Stale phases + orphans + leaf tasks
task_query("(query \"active-roots\")")   # Top-level active tasks
task_query("(-> (active) :enrich (:sort :session-count) (:take 5))")  # Most active tasks
```

For plan creation and restructuring, see the kli-planning skill's TQ Cheatsheet.

### Documentation

- CLAUDE.md - Task model, PQ/TQ reference, playbook workflow
- Patterns are managed via PQ queries (see CLAUDE.md for syntax)
