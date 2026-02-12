---
name: kli-workflow
description: Meta-orchestration skill for ACE (Agentic Context Engineering) framework. Provides high-level overview of research/planning/implementation/reflection phases and coordinates when to use phase-specific skills (kli-research, kli-planning, kli-implementation, kli-reflection). Auto-invoked for general ACE workflow questions. For phase-specific guidance, the appropriate phase skill will be loaded.
---

<!-- ACE Metadata:
  role: meta-orchestration
  coordinates: kli-research, kli-planning, kli-implementation, kli-reflection
  source: ace/playbooks/ace.md
  sync-threshold: 3
  last-synced: 2026-01-02
  version: 2.1
  pattern-ids: [ace-017], [ace-052], [ace-060], [ace-078]
-->

# ACE Workflow Meta-Orchestration Skill

## Overview

Coordinate ACE (Agentic Context Engineering) workflow using three roles: Generator (produce artifacts), Reflector (analyze outcomes), Curator (maintain knowledge). Use phase-specific skills for detailed execution guidance.

### Three-Role Architecture

**Generator Role** - Execute via slash commands:
- Run `/research` to document codebase → Load **kli-research skill** for methodology
- Run `/plan` to design phased implementation as task DAG → Load **kli-planning skill** for phase design
- Run `/implement` to execute with TDD workflow → Load **kli-implementation skill** for TDD discipline
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
- Run: `/research`
- Produce: research.md, findings via `observe()`
- **→ Transition to Planning:** Research questions answered, codebase understood

**2. Planning Phase**
- Run: `/plan`
- Produce: Plan as task DAG (`task_fork` + `phase-of` edges), optionally plan.md as artifact
- Record: Planning decisions via `observe()`
- **Reuse: research.md** (40-50% token savings via [ace-078])
- **→ Transition to Implementation:** Plan approved, phases clearly defined

**3. Implementation Phase**
- Run: `/implement`
- Produce: Code changes, test files
- Record: TDD cycles, challenges via `observe()` on phase tasks
- Navigate: `task_query("(query \"plan-ready\")")` for next phase, `task_complete()` marks done
- **→ Transition to Reflection:** All phases complete, all verification passed

**4. Reflection Phase**
- Run: `/reflect`
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
- Implementation verification gates ([ace-052]: verify before proceeding)
- Reflection for complex tasks (capture knowledge required)

## Artifact Flow

### How Artifacts Connect Phases

```
research.md ─────────────> plan (task DAG)
  ↓                            ↓
  └─> observe() events    task_fork(phase-of) creates phases
                               ↓
                          /implement navigates via plan-frontier
                               ↓
                          observe() per phase, task_complete() marks done
                               ↓
                          All observations (event stream) ──> /reflect
                                                                ↓
                                                             reflection.md
                                                                ↓
                                                             playbook MCP tools
```

**Apply Artifact Reuse Pattern** ([ace-078]):
- Detect existing research.md in `/plan`
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

| Phase | Command | Input | Output | Key Skill | Duration |
|-------|---------|-------|--------|-----------|----------|
| Research | /research | Task description | research.md | kli-research | 1-3 hours |
| Planning | /plan | research.md (optional) | plan.md | kli-planning | 30min-2hr |
| Implementation | /implement | plan.md | Code + tests | kli-implementation | Hours-days |
| Reflection | /reflect | Event stream (timeline) | reflection.md | kli-reflection | 30min-1hr |

### Verification Gate Pattern

Critical principle across all phases ([ace-052], [ace-017]):

```
Complete phase work
    ↓
Run automated verification (must pass)
    ↓
Add ✓ to plan.md phase header
    ↓
Request manual verification (must approve)
    ↓
ONLY THEN → Proceed to next phase
```

**Never proceed without passing BOTH automated and manual verification.**

### Core Patterns

- **[ace-052]** helpful=38: Never proceed to next phase without passing automated verification
- **[ace-017]** helpful=32: Implement phase-by-phase, run automated verification after each
- **[ace-060]** helpful=22: Don't batch multiple phases without verification
- **[ace-078]** helpful=8: Reuse research.md artifacts (saves 40-50% tokens in planning)

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

### Related Skills

- **nix-development** - Nix/NixOS domain patterns, auto-invoked for .nix files
- **lisp-development** - Common Lisp domain patterns, auto-invoked for .lisp files
- **depot-organization** - Repository structure and file placement conventions

### Documentation

- Source playbook: `ace/playbooks/ace.md` (73 patterns, helpful/harmful tracked)
- ACE framework: `ace/WORKFLOW.md` (architecture documentation)
- Task model: `ace/lib/task-model.md` (plans as DAGs, graph queries, conventions)
- Commands: `shells/modules/ai/claude/profiles/ace/commands/{research,plan,implement,reflect}/command.md`
