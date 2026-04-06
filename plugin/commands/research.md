---
description: Document codebase as-is through iterative research with observation capture 
allowed-tools: mcp__task__*, Task, Read, Glob, Grep
---

# Research Codebase

Research the codebase or external topics by delegating to sub-agents.

**The kli-research skill provides comprehensive guidance on:**
- Documentarian philosophy (document what IS, not what SHOULD BE)
- Error amplification principles (research errors amplify 1000x downstream)
- Research decomposition patterns
- Exit criteria evaluation (when research is complete)

## Research Strategies

Five research strategies are available. Bundled agents are always present; other capabilities use the most specialized available agent type, falling back to general-purpose.

| Strategy | Keywords | Approach |
|----------|----------|----------|
| **codebase** | "how", "where", "implementation", "code", "architecture" | Bundled: codebase-locator, codebase-analyzer, pattern-finder |
| **visual** | "design", "UI", "visual", "component", "inspiration", "peer" | Sub-agent: describe visual research goal |
| **github** | "repo", "repository", "github.com", "open source", "package source" | Sub-agent: describe repo analysis goal |
| **external** | "library docs", "framework", "documentation", "how to use X" | Sub-agent: describe web research goal |
| **graph** | "prior tasks", "patterns for", "project health", "task history", "related tasks", "what has been done" | Bundled: graph-analyst |

## Initial Setup

When invoked without arguments, respond:
```
I'm ready to research. Please provide your research question:

- **Codebase research**: "How does authentication work?", "Where are API endpoints?"
- **Visual research**: "Find modern card component examples", "Analyze nordic design trends"
- **GitHub research**: "Analyze the tokio-rs/tokio repository", "Map the Next.js repo structure"
- **External docs**: "How does React Server Components work?", "Redis caching best practices"
- **Graph research**: "What prior tasks relate to MCP?", "What patterns have been effective for Lisp?", "Project health?"
- **Hybrid**: "How should we improve our navigation based on best practices?"
```

Wait for user's research query.

## Research Process

### Step 0: Set Up Task Context

Call `task_get()` to check if there's already a current task. If so, use it. If not, create one:

```
task_create(name="research-<description>")
task_set_metadata(key="goals", value='["Research <question>", "Document findings with file:line evidence"]')
task_set_metadata(key="phase", value="research")
```

Then call `task_get()` to retrieve the full task state including any existing observations and artifacts.

### Step 0.5: Activate Playbook Patterns (REQUIRED)

**Before researching**, activate relevant patterns:
```lisp
pq_query('(-> (activate "<research question>" :boost (<domain1> <domain2>)) (:take 5))')
```

This retrieves prior learnings and patterns that may inform the research. The activation is persisted for handoff continuity.

### Step 1: Classify Research Type

Analyze the query to determine strategy:

**Codebase keywords**: "how", "where", "implementation", "code", "architecture", "what calls", "imports"
**Visual keywords**: "design", "UI", "visual", "component", "inspiration", "peer", "navigation examples", "modern"
**GitHub keywords**: "repo", "repository", "github.com/", "open source", "package source", "analyze X repo"
**External keywords**: "docs", "documentation", "library", "framework", "how to use", "best practices for X library"
**Graph keywords**: "prior tasks", "prior work", "task history", "patterns for", "pattern effectiveness", "what patterns", "project health", "task health", "graph health", "related tasks", "what relates to", "dependencies", "stale", "blocked", "orphan", "what has been done for", "similar tasks"

### Step 3: Spawn Research Agents

**For codebase research**, spawn appropriate agents based on the question:

```
# For locating files/components
Task(
    subagent_type="codebase-locator",
    prompt="<research question>. Task dir: <task_dir>",
    description="Locate relevant files"
)

# For deep analysis of specific components
Task(
    subagent_type="codebase-analyzer",
    prompt="<research question>. Task dir: <task_dir>",
    description="Analyze implementation"
)

# For finding similar patterns
Task(
    subagent_type="pattern-finder",
    prompt="<research question>. Task dir: <task_dir>",
    description="Find related patterns"
)
```

**For web/external research** (documentation, articles, best practices):

Select the most specialized available agent type for web research;
fall back to general-purpose.

```
Task(
    subagent_type=<most specialized available for web research>,
    prompt="Research goal: <what to find and why>.
            Topics: <specific libraries, concepts, or questions>.
            Return: summary of findings with source URLs.",
    description="Web research: <topic>"
)
```

**For GitHub repository research:**

Select the most specialized available agent type for repository analysis;
fall back to general-purpose.

```
Task(
    subagent_type=<most specialized available for repo analysis>,
    prompt="Analyze the <owner>/<repo> repository.
            Focus: <structure | architecture | specific component>.
            Return: key files, architecture summary, patterns found.",
    description="Analyze <owner>/<repo>"
)
```

**For visual/design research** (UI patterns, inspiration, branding):

Select the most specialized available agent type for visual/design research;
fall back to general-purpose.

```
Task(
    subagent_type=<most specialized available for visual research>,
    prompt="Research visual design patterns for <goal>.
            Analyze: <URLs, site types, or design domains to examine>.
            Return: patterns found, layout/color/typography analysis.",
    description="Visual research: <topic>"
)
```

**For graph-based research (task/pattern graphs):**

```
Task(
    subagent_type="graph-analyst",
    prompt='{"question": "<research question about tasks, patterns, or project health>"}',
    description="Query task/pattern graphs"
)
```

Use for questions about prior work, pattern effectiveness, task relationships, or project health. The graph-analyst queries TQ (task graph) and PQ (pattern graph) to answer from the graph perspective.

**Capability selection guidance:**
- `codebase-locator` for "where is X?" questions (bundled, always available)
- `codebase-analyzer` for "how does X work?" questions (bundled, always available)
- `pattern-finder` for "how is X done elsewhere?" questions (bundled, always available)
- `graph-analyst` for prior tasks, patterns, project health (bundled, always available)
- Sub-agent for web research: external docs, articles, best practices
- Sub-agent for repo analysis: GitHub repository structure and architecture
- Sub-agent for visual research: design patterns, UI inspiration, branding

The agents will:
1. Research the question using their specialized tools
2. Generate findings in the task directory
3. Return with status, findings, and evidence

### Step 4: Handle Hybrid Research

If the query needs BOTH strategies (e.g., "How should we redesign our navigation?"):

1. First spawn codebase agents (bundled) to understand current implementation
2. Then spawn a sub-agent for visual/external research describing what patterns to find
3. Combine results in a summary

### Step 5: Present Results

After agents return:

1. Record key findings: `observe("Research findings: <summary of key discoveries>")`
2. Write research.md to the task directory (path from `task_get()`)
3. Present to user:
   - Status (success/partial/failure)
   - Key findings summary
   - Evidence references (file:line for codebase, screenshots for visual)
   - Suggested next steps

### Step 6: Record Research Findings as Observations

Research produces observations (system-specific findings), not patterns (reusable techniques). Record all discoveries via `observe()`:

```
observe("Research finding: <discovery with file:line evidence>")
observe("Architecture insight: <how X works based on code analysis>")
observe("Constraint found: <limitation discovered with evidence>")
```

**Do NOT use `(add! ...)` during research.** Research follows the documentarian philosophy — document what IS, not prescribe what to DO. Findings are observations by nature. Reusable patterns emerge later during reflection (`/kli:reflect`), which applies the litmus test:
- **Transferable**: Would help on a *different* project?
- **Actionable**: Says "when X, do Y" (not "X exists")?
- **Prescriptive**: Gives advice, not description?

**What to record as observations:**
- Architectural discoveries (how the system works)
- Anti-patterns found (things that don't work)
- Workarounds that succeeded
- Cross-cutting concerns observed

### Step 7: Handle Follow-Up Questions

**Simple clarification:** Re-spawn relevant agent with refined question
**Component extraction:** For visual research, ask if user wants code extracted
**Full iteration:** Spawn agents again with follow-up context

## CRITICAL: Delegation Required

**DO NOT research the codebase yourself.** You MUST delegate to specialized agents.

❌ **WRONG**: Using Read, Grep, Glob, or Search to investigate the question directly
❌ **WRONG**: Answering the research question from your own knowledge

✅ **CORRECT**: Spawn specialized agents via Task tool and let them do the work

The agents:
- Have access to appropriate tools for their specialty
- Return structured results
- Track findings in the task directory

**Your job**: Set up the task directory, spawn appropriate agents, synthesize and present results.

## Remember

- Follow **documentarian philosophy** from kli-research skill
- **ALWAYS delegate to specialized agents** - never research directly
- Auto-detect strategy from query keywords
- Use hybrid mode for questions needing both perspectives
- **Record findings** as observations via `observe()` — patterns are promoted during `/kli:reflect`
- **Record findings** via `observe()` — observations flow through the event stream
- Get user validation before marking complete
- No placeholder values in artifacts

## See Also

- CLAUDE.md - Task model, PQ/TQ reference, playbook workflow
