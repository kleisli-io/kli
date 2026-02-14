# kli

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
<!-- [![CI](https://github.com/kleisli-io/kli/actions/workflows/ci.yml/badge.svg)](https://github.com/kleisli-io/kli/actions) -->

Persistent context and structured workflows for [Claude Code](https://docs.anthropic.com/en/docs/claude-code). kli runs alongside your normal Claude Code usage — including Agent teams — giving both Claude and you always-available context so you never spend time reconstructing what happened last session or coordinating parallel work. Use it passively (hooks and MCP servers record and surface context automatically) or actively (`/kli:research` → `/kli:plan` → `/kli:implement` → `/kli:reflect`).

<!-- TODO: Replace with actual screenshot of the composition graph view -->
![kli task graph](docs/images/composition-graph.png)

## Quick Start

```bash
curl -fsSL https://kli.kleisli.io/install | sh
```

Then, in any project:

```bash
kli init
```

This configures Claude Code with kli's MCP servers, hooks, skills, agents, and commands. Claude Code now has persistent, structured memory and workflows:

```
You: Research the auth module, then plan and implement rate limiting

Claude Code automatically:
  creates a tracked task                    # persisted as an event-sourced graph
  loads context + graph neighbors           # full project awareness in one call
  records findings as observations          # "Auth uses JWT with RS256..."
  retrieves relevant patterns               # prior learnings surface automatically
  ... picks up exactly where it left off next session
```

## What kli Does

kli is a single binary that runs alongside Claude Code. Its hooks and MCP servers work passively — observing, recording, coordinating, and surfacing context automatically. Its skills give you structured workflows when you want them. It provides five layers:

### MCP Servers

Two [MCP](https://modelcontextprotocol.io/) servers expose 31 tools that Claude Code calls automatically as it works.

**Task graph** (28 tools) -- Tasks are event-sourced: every mutation is an immutable event in an append-only log. Concurrent agents merge via CRDTs, so no writes are ever lost even when multiple sessions edit the same task simultaneously. Claude Code queries the graph through a purpose-built query language rather than dozens of separate tools — keeping context clean while enabling advanced searches like "find the most active tasks," "get ready phases for my plan," or "scaffold a dependency chain."

**Pattern learning** (3 tools) -- Patterns carry `helpful` and `harmful` counters scored as `max(0, helpful - harmful)`. Patterns that work get promoted; patterns that cause problems get demoted. Claude Code retrieves relevant patterns automatically based on what you're working on — surfacing prior learnings without you having to search for them.

### Hooks

Lifecycle hooks that run automatically during Claude Code sessions:

- **Session start/leave** -- initializes task context, detects parallel sessions, registers with MCP servers
- **Tool call tracking** -- records file touches, builds session fingerprints, feeds the co-application graph
- **File conflict detection** -- warns before concurrent edits to the same file

### Skills, Agents, and Commands

`kli init` installs a complete set of Claude Code extensions:

- **Skills** -- structured workflows that guide Claude Code through a complete development cycle:
  - `/kli:research` -- codebase exploration with observation capture
  - `/kli:plan` -- iterative planning with phase decomposition
  - `/kli:implement` -- TDD with verification gates
  - `/kli:reflect` -- pattern extraction from experience
- **Agents** -- specialized subagents for parallel work (codebase analysis, web research, graph queries, design research)
- **Commands** -- slash commands like `/commit` (auto-generated conventional commits with task integration), `/create-task`, `/handoff`

### Swarm Coordination

When multiple Claude Code sessions work on the same codebase, kli coordinates them:

- **File conflict detection** -- warns before concurrent edits to the same file
- **Session fingerprinting** -- tracks which files and tools each session touches
- **Orphan phase pickup** -- detects when a session exits mid-task, allows another to continue

### Dashboard

Web UI for visualizing task graphs, health diagnostics, activity timelines, plan DAGs, and topic clusters:

```bash
kli dashboard    # starts on http://localhost:8080
```

12 views including a WebGL composition graph, frontier board, forest hierarchy, and Markov cluster analysis.

## Why kli?

kli doesn't replace how you use Claude Code — it makes it better. Use Claude Code exactly as you normally would, run Agent teams, work across multiple sessions. kli sits underneath and ensures that context is always available, patterns are always surfacing, and parallel work never collides.

Without kli, Claude Code sessions are stateless. Every session starts blank — no memory of prior work, no awareness of patterns that helped before, no coordination between sessions or agents.

| Without kli | With kli |
|---|---|
| Context lost between sessions | Event-sourced task graph persists observations, handoffs, plans |
| Same mistakes repeated, same patterns rediscovered | Helpful/harmful scoring learns what works across sessions |
| Parallel sessions overwrite each other's files | File conflict detection + session fingerprints |
| Flat task lists, no structure | Typed edges: `phase-of`, `depends-on`, `blocks`, `related-to` |
| No record of what was tried and why | Full timeline with observations, tool calls, session joins |
| Manual context loading every session | Full graph context loaded automatically when resuming work |

## Installation

### One-line install

```bash
curl -fsSL https://kli.kleisli.io/install | sh
kli init   # in any project where you want to use it
```

Supports Linux (x86_64, aarch64) and macOS (x86_64, Apple Silicon). The installer downloads a pre-built binary from GitHub releases. `kli init` configures MCP servers, hooks, skills, agents, and commands for the current project.

### Nix

```bash
nix build          # produces result/bin/kli
nix run            # run directly
nix flake check    # run test suites (CRDT, task, playbook, hooks)
```

### From source

Requires SBCL and Quicklisp:

```lisp
(asdf:load-system :kli)
(kli:main)
```

## Architecture

kli is a single Common Lisp binary (SBCL). One binary handles everything: MCP servers, hook dispatch, dashboard, and project initialization.

```
Claude Code <--stdio--> kli serve --task       (28 tools)
Claude Code <--stdio--> kli serve --playbook   (3 tools)
Claude Code ---calls--> kli hook <event>       (session, tool, conflict hooks)
Browser     <--http-->  kli dashboard          (web UI on :8080)
Developer   ---runs---> kli init               (sets up project)
```

### Event sourcing

Each task is an append-only event log (`events.jsonl`). State is computed by replaying events through CRDT merge functions. This gives you:

- **Consistency** -- vector clocks establish causal order; concurrent events merge deterministically
- **Auditability** -- every observation, edge change, and metadata update is preserved with timestamp and session ID
- **Multi-agent safety** -- OR-Sets and LWW-Registers resolve concurrent writes without coordination locks

### Query languages

Rather than exposing dozens of narrow MCP tools (one per query type), kli provides two purpose-built query languages — TQ for tasks and PQ for patterns. Claude Code uses these automatically through skills and workflows. The languages support filtering, sorting, grouping, set operations, and mutations in composable pipelines, giving Claude Code expressive power without polluting tool context.

### Libraries

| Library | Description |
|---------|-------------|
| `lib/crdt` | G-Set, OR-Set, PN-Counter, LWW-Register, LWW-Map, Vector Clock |
| `lib/task` | Event log, CRDT state, DAG algorithms, TQ engine, Markov clustering |
| `lib/playbook` | PQ engine, pattern store, helpful/harmful scoring, activation graph |
| `lib/mcp-framework` | MCP server framework: JSON-RPC 2.0, tool registry, schema generation |
| `lib/mcp-http` | HTTP+SSE transport with session management |
| `lib/lol-reactive` | Reactive web framework for the dashboard (HTMX, signals, Tailwind) |
| `lib/claude-hooks` | Claude Code hook handlers (session lifecycle, tool tracking, conflict detection) |

## What kli Is Not

kli is not a general-purpose project management tool and does not replace GitHub Issues, Linear, or Jira. It is not a fork or replacement for Claude Code — it's a layer on top. kli is most useful when you work on multi-session tasks where context persistence, structured workflows, and pattern learning matter. It works with vanilla Claude Code, Agent teams, and any MCP-compatible client.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

[MIT](LICENSE)

---

Built by [Kleisli](https://kleisli.io) in Tromso, Norway.
