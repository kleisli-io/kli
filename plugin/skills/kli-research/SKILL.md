---
name: kli-research
description: Research phase domain knowledge including documentarian philosophy, decomposition patterns, agent selection, and exit criteria. Use when conducting codebase research, investigating technical questions, or documenting what exists. Activates for exploration and discovery tasks. DO NOT use for planning or implementation.
---

# KLI Research Phase Skill

## Requirements & Standards

### Apply Documentarian Philosophy (CRITICAL)

**Primary goal**: Document and explain codebase AS IT EXISTS TODAY

**NEVER:**
- Suggest improvements or changes (unless explicitly asked)
- Perform root cause analysis (unless explicitly asked)
- Propose future enhancements or refactoring
- Critique implementation or identify problems
- Recommend optimizations or architectural changes
- Evaluate code quality or suggest best practices

**ALWAYS:**
- Describe what exists, where it exists, how it works, how components interact
- Document current patterns, conventions, and design implementations
- Provide concrete evidence with `file:line` references for ALL claims
- State hypotheses as hypotheses when investigating bugs (NOT facts)
- Use tentative language: "may be", "appears to", "suggests"
- Mark status as `incomplete` if root causes unverified
- Create technical map/documentation of existing system

**Core Principle**: Document what IS, not what SHOULD BE

### Error Amplification Awareness

Research errors amplify **1000x** through downstream phases:

```
1 incorrect assumption → 100+ lines wrong planning → 1000+ lines wrong implementation
1 missed component → cascades through plan → wasted implementation
1 misunderstood pattern → applied incorrectly throughout codebase
```

**Impact on Thoroughness:**
- Iterate until ALL gaps resolved (no premature exit)
- Verify ALL claims with `file:line` evidence (never assume)
- Spawn additional agents when ANY uncertainty exists
- NEVER make assumptions - always verify in codebase
- Research phase has highest leverage: extra iteration here prevents 1000x waste downstream

## Overview

Establish accurate codebase understanding before planning or implementation. Use observation-driven discovery through parallel specialized agents, iterative gap analysis, and artifact reuse to create comprehensive `research.md` documents with full `file:line` evidence.

**Execute Generator Role in KLI Workflow**:
1. **Implement Generator**: Run research command directly (cannot spawn other commands)
2. **Produce Artifacts**: Create `research.md`, record findings via `observe()`
3. **Build Evidence Base**: Provide foundation for planning phase
4. **Apply Patterns**: Reference playbooks for established patterns

**Follow Key Principles**:
- Apply documentarian approach (describe, don't prescribe)
- Spawn agents in parallel for efficiency
- Iterate in feedback loops until complete
- Read files fully before spawning agents

## Quick Start

**Before starting any research:** Activate playbook patterns (REQUIRED):
```
pq_query('(-> (activate "<research question>" :boost (<relevant domains>)) (:take 5))')
```
This retrieves prior learnings via graph-based search and persists for handoff continuity.

**Basic Research Workflow:**

1. **Read mentioned files FULLY** (if user provides paths)
   - Use Read tool WITHOUT limit/offset parameters
   - Extract full context before decomposing research

2. **Analyze and decompose** research question
   - Reference activated patterns for prior learnings
   - Identify components, patterns, concepts
   - Determine relevant directories/files/architectures
   - Consider if online research needed (rare)

3. **Spawn parallel agents** in single message
   - USUALLY: codebase-locator (find WHERE)
   - OFTEN: codebase-analyzer (understand HOW)
   - SOMETIMES: pattern-finder (find examples)
   - RARELY: sub-agent for external research (docs, articles)

4. **Compile results** and analyze gaps
   - Extract findings from each agent
   - Identify missing information
   - Decide if additional iteration needed

5. **Write research.md** artifact
   - Comprehensive findings with `file:line` references
   - Clear structure with sections
   - Status: complete or incomplete
   - Document playbook patterns used

## Playbook Workflow

### Pattern Discovery During Research

Research is a prime source of new patterns. Actively capture reusable insights:

**Observation triggers:**
- "This approach worked well" → `observe()` with evidence
- "I've seen this before" → `observe()` referencing existing pattern
- "This failed, worth noting" → `observe()` documenting the failure

**Discovery workflow:**
1. Record findings as observations: `observe("Research finding: <description with evidence>")`
2. All findings are observations by nature — research follows the documentarian philosophy (document what IS)
3. Patterns are promoted during `/kli:reflect` via the litmus test (transferable? actionable? prescriptive?)

**Observation quality bar:**
- GOOD: "When debugging Nix build failures, check derivation inputs first (not runtime). Evidence: 3/3 issues were input-related in this research."
- BAD: "Nix debugging is hard" (not actionable, no evidence)

**What to record as observations:**
- Architectural discoveries (how the system works)
- Anti-patterns found (things that don't work)
- Workarounds that succeeded
- Cross-cutting concerns observed
- File organization conventions

**Do NOT use `(add! ...)` during research.** Research findings are observations, not patterns.

## Research Methodology

### Decomposition Patterns

**Three-Level Decomposition:**

1. **Components**: What are the major pieces?
   - Identify modules, classes, functions
   - Map dependencies and relationships
   - Document interfaces and APIs

2. **Patterns**: What design patterns are used?
   - Registry patterns, builders, factories
   - Data structures and algorithms
   - Architectural patterns (MVC, effects systems, etc.)

3. **Concepts**: What domain concepts exist?
   - Business logic abstractions
   - Domain-specific terminology
   - Conceptual models and their implementations

**Decomposition Strategy:**
- Start with high-level structure (directories, modules)
- Drill down into specific components as needed
- Follow dependencies to understand interactions
- Use breadth-first initially, then depth-first for details

### Agent Selection Criteria

**Playbook MCP tools** (use before spawning agents):
- **When**: At start of every task before spawning other agents
- **Why**: Find established patterns that inform approach
- **PQ queries**: `(-> (search "query") (:take 5))`, `(-> (proven :min 3) ...)`
- **How to use**: Search for relevant patterns, reference them in your research approach

**codebase-locator** (WHERE questions):
- **When**: Need to find WHERE code lives
- **Why**: Fast file/directory discovery without deep reading
- **What to tell it**: Feature/component to locate
- **What it knows**: Glob/Grep patterns, directory structures

**codebase-analyzer** (HOW questions):
- **When**: Need to understand HOW specific code works
- **Why**: Deep reading and flow documentation
- **What to tell it**: Which files to analyze, what to document
- **What it knows**: Code reading, flow documentation, pattern explanation

**pattern-finder** (EXAMPLE questions):
- **When**: Need similar examples of patterns
- **Why**: Find existing implementations to understand conventions
- **What to tell it**: Pattern to find (e.g., "builder pattern", "registry")
- **What it knows**: Code similarity search, example documentation

### Research Capabilities (tool-adaptive)

For research beyond the local codebase, ALWAYS spawn a sub-agent describing
the goal. Select the most specialized available agent type for the capability;
fall back to general-purpose if no specialized agent exists. Never do external
research directly in the main session.

**Web research** (EXTERNAL TEXT questions):
- **When**: Need external documentation, technical articles, best practices
- **Goal**: Find and summarize relevant external information with source URLs
- **Prompt the sub-agent with**: What to find, what sources matter, what format to return
- **Default**: Prefer codebase-only research unless clear external need
- **Scope**: Documentation, articles, library/framework docs, concepts

**Repository analysis** (GITHUB questions):
- **When**: Need to analyze external GitHub repositories
- **Goal**: Map repo structure, understand architecture, identify key patterns
- **Prompt the sub-agent with**: Repo owner/name or URL, what to investigate (structure, stack, specific files)

**Visual/design research** (VISUAL questions):
- **When**: Need UI patterns, component examples, design inspiration, branding analysis
- **Goal**: Gather visual design evidence and document patterns found
- **Prompt the sub-agent with**: What patterns to find, which sites/URLs to analyze, what to document
- **Use cases**: Component examples, design system analysis, layout patterns, branding research

**Code extraction** (EXTRACT questions):
- **When**: Need to extract UI component code from a live site
- **Goal**: Produce usable code in a target framework from an existing page
- **Prompt the sub-agent with**: URL, target element/selector, output framework

**Research Type Detection**:

**Visual/Design Research Indicators** → Visual research capability:
- Component examples, UI patterns, design systems
- Screenshots, images, visual inspiration
- Branding, color palettes, typography analysis
- Layout patterns, responsive design examples
- Keywords: "modern", "beautiful", "design", "UI", "component", "screenshot", "visual"

**Text/Documentation Research** → Web research capability:
- Technical concepts, best practices, documentation
- Code examples (non-visual), architecture patterns
- Library/framework usage (non-UI documentation)
- Keywords: "how to", "best practices", "documentation", "guide", "tutorial"

**Mixed Research** → Multiple capabilities in parallel:
- UI framework research (Tailwind, React, Vue components)
- Design + implementation patterns
- Spawn one sub-agent for visual evidence, another for technical docs
- Synthesize both findings in research.md

**Spawning Strategy** (parallel spawning):
```
Start broad → Then targeted → Multiple parallel when independent
(use PQ queries first: `(-> (search "...") (:take 5))`, `(-> (proven) ...)`)

Example (codebase research):
Message 1: codebase-locator (find files)
Message 2: codebase-analyzer on promising files found by locator

Example (external research):
Message 1: sub-agent with web research goal (parallel with playbook patterns)
→ sub-agent finds docs, articles, examples; returns summary with URLs

Example (mixed research):
Message 1: sub-agent(visual goal) + sub-agent(docs goal) in parallel
→ Visual sub-agent: design patterns, component examples
→ Docs sub-agent: framework documentation, best practices
```

**Agent Prompting Principles:**
- Don't write detailed HOW instructions - agents know their jobs
- Focus on WHAT to search/analyze
- Remind them: "document, don't evaluate"
- Keep prompts concise but clear on scope

### Root Cause Verification

**NEVER claim root cause without verification**

This is one of the most damaging research errors:
- Downstream phases build on false assumptions
- Planning locks in wrong solution
- Entire implementation wasted
- User loses trust in research quality

**For Bug Investigation:**

❌ **DON'T**: "The issue is caused by X" (without verification)
✅ **DO**: "X appears to be a likely cause based on [evidence], but requires verification"

❌ **DON'T**: "This fixes the problem" (without testing)
✅ **DO**: "Testing hypothesis: does changing X resolve the issue?"

**Verification Requirements:**
1. Actually reproduce the issue in test environment
2. Apply proposed fix and confirm issue resolves
3. Verify fix doesn't introduce new issues
4. Document test methodology and results

**Status Marking:**
- Mark status: `incomplete` if root cause not verified through testing
- Include in Open Questions: "Root cause hypothesis requires verification: [specific test needed]"
- Only mark `complete` after successful verification

**Tentative Language for Unverified Hypotheses:**
- "may be caused by"
- "appears to suggest"
- "evidence points to"
- "hypothesis: X is responsible for Y"

## Workflows

### Standard Research Workflow

```
1. User provides research question
   ↓
2. Read any directly mentioned files FULLY (main context)
   ↓
3. Record initial context via observe()
   ↓
4. Analyze and decompose question
   - Break into components/patterns/concepts
   - Identify relevant areas to explore
   - Create TodoWrite task list
   ↓
5. Spawn parallel agents (Iteration 1)
   - Single message, multiple Task calls
   - Use PQ queries first (`(-> (search "...") ...)`, `(-> (proven) ...)`)
   - Usually: codebase-locator
   - Often: codebase-analyzer, pattern-finder
   ↓
6. Wait for ALL agents to complete
   ↓
7. Compile results and analyze gaps
   - Extract findings from each agent
   - Identify missing information
   - Note which agents were effective
   ↓
8. Gap analysis decision:
   - If gaps exist → Spawn additional agents (Iteration N+1)
   - If complete → Write research.md artifact
   ↓
9. Write research.md with full evidence
   - Comprehensive findings
   - All file:line references
   - Status: complete or incomplete
   - Playbook patterns referenced
   ↓
10. Record final status via observe()
```

### Iteration Decision Patterns

**Spawn Additional Iteration If:**
- Key questions unanswered
- File:line references missing for claims
- Component interactions unclear
- Patterns insufficiently documented
- Gaps identified in agent results
- ANY uncertainty exists

**Exit Iteration Loop Only When:**
- ✅ Research question fully answered with concrete evidence
- ✅ All code references documented with `file:line`
- ✅ No gaps in understanding current implementation
- ✅ Playbook patterns identified and referenced
- ✅ Status marked: `complete`

**Common Gap Patterns:**
- Locator found files, but need analyzer to understand HOW
- Analyzer explained one component, but dependencies unclear
- Pattern found, but need more examples to confirm convention
- External library used, but need sub-agent for external docs

### Artifact Reuse Pattern

When creating `research.md`:

**Structure for Reuse:**
```markdown
# Research: <Topic>

## Summary
<High-level 2-3 sentence summary>

## Detailed Findings

### Component 1
<Findings with file:line references>

### Component 2
<Findings with file:line references>

## Architecture Diagram
<If helpful>

## Playbook Patterns
- [pattern-id-1]: description
- [pattern-id-2]: description

## Open Questions
<List any remaining uncertainties>

## References
<All file:line references in one place>
```

**Why This Matters:**
- Planning phase detects and reuses research.md
- Comprehensive research = better plans = correct implementation
- Invest extra iteration here to prevent waste downstream

## Verification & Exit Criteria

### Research Completeness Checks

**Before marking status: complete:**

✅ **Question Answered:**
- Original research question has concrete answer
- No "I think" or "probably" - only verified facts
- Hypotheses clearly marked as hypotheses

✅ **Evidence Complete:**
- All claims backed by `file:line` references
- Code snippets included where helpful
- References section lists all files examined

✅ **No Gaps:**
- All components mentioned are documented
- All dependencies traced and explained
- No "TODO: investigate further" items remaining

✅ **Patterns Referenced:**
- Playbook patterns identified and cited with IDs
- Pattern effectiveness noted in observations
- Relevant patterns from playbook tools included

✅ **Root Causes Verified** (if investigating bugs):
- Hypotheses tested and confirmed
- Verification methodology documented
- Test results included

### Validation Requirements

**Self-Check Questions:**
1. Could someone implement from this research without guessing?
2. Are all file:line references accurate and sufficient?
3. Have I documented what IS (not what SHOULD BE)?
4. Are unverified hypotheses clearly marked?
5. Did I iterate enough to resolve all gaps?

**Common Failure Modes:**
- Premature exit from iteration loop
- Assumptions without verification
- Missing file:line references
- Mixing description with prescription
- Root cause claims without testing

## TQ for Research

Use TQ to discover prior work and navigate task context:

```
task_query("(query \"active-roots\")")   # Find top-level tasks
task_query("(-> (active) (:where (matches \"auth\")) :ids)")  # Find tasks by keyword
obs_search(query="OAuth2 implementation")  # Search observations across tasks
enriched_retrieve(k=5)                     # Context-aware observation retrieval
```

## Next Steps

After completing research:
1. User reviews research.md for completeness
2. If approved → Run `/kli:plan` to create implementation plan (reuses research.md)
3. If gaps → Additional research iteration
4. Planning phase will detect and load research.md automatically
