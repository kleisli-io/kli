# kli

Event-sourced task graphs, pattern learning, and swarm coordination for [Claude Code](https://docs.anthropic.com/en/docs/claude-code).

Built on Kleisli categories: CRDTs as join-semilattices, coalgebraic task semantics, Markov clustering, spreading-activation retrieval, and Bayesian pattern scoring.

## What This Is

kli is an MCP server that gives Claude Code structured task management:

- **Event-sourced tasks** — every mutation is an immutable event, enabling replay, audit, and concurrent multi-agent merging via CRDTs
- **TQ query language** — pipeline-based S-expression queries over the task graph (`(-> (active) :enrich (:sort :obs-count) (:take 5))`)
- **PQ query language** — same pipeline model for the playbook pattern graph, with spreading-activation retrieval
- **Swarm coordination** — stigmergic signals between parallel Claude Code sessions (file conflict detection, session fingerprinting, orphan phase pickup)
- **Pattern learning** — Bayesian quality scoring on patterns extracted from implementation experience; patterns that help get promoted, patterns that harm get demoted

## Libraries

| Library | Description |
|---------|-------------|
| `lib/crdt` | Join-semilattice primitives: G-Set, OR-Set, PN-Counter, LWW-Register, LWW-Map, Vector Clock |
| `lib/task` | Event-sourced task graph: events, CRDT state, DAG algorithms, TQ query engine, Markov clustering |
| `lib/playbook` | PQ query engine for pattern graphs |
| `lib/mcp-framework` | MCP server framework: tool/resource/prompt registration, schema generation, hooks, evolution |
| `lib/mcp-http` | HTTP+SSE transport for MCP (multi-session, CORS, session lifecycle) |

## Services

| Service | Description |
|---------|-------------|
| `services/task-mcp` | MCP server exposing task graph operations, TQ queries, embedding-based retrieval, swarm coordination |
| `services/playbook-mcp` | MCP server exposing pattern management, PQ queries, activation, graph edges |

## Build

Requires [Nix](https://nixos.org/) with flakes enabled.

```bash
nix build          # produces result/bin/kli
nix run            # run kli directly
nix flake check    # run test suites
```

For development (REPL loading via ASDF), see `kli.asd`.

## Usage

```bash
# Start as MCP server (stdio transport, called by Claude Code)
kli serve --task

# Start playbook MCP server
kli serve --playbook

# Show version
kli version

# Show help
kli help
```

## License

MIT
