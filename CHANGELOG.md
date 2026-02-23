# Changelog

All notable changes to kli are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.2] - 2026-02-23

### Added

- Provably correct MCP depot resolution via `/proc/net/tcp` socket inode tracing (Linux)
- macOS MCP depot resolution via `lsof` TCP connection ownership query
- Embedding persistence: save/load cache to disk across daemon restarts
- Elog-load memoization: per-request caching prevents redundant disk reads

### Fixed

- MCP sessions resolve to correct depot with parallel Claude sessions (was heuristic, now kernel-authoritative on Linux, lsof-authoritative on macOS)
- `task_fork` and `spawn` inherit depot from parent task ID (defense-in-depth)
- Forward declaration for `*log-verbose*` in embeddings.lisp (build order fix)

### Removed

- Level 2b heuristic depot bridge (replaced by `/proc`-based PID resolution)
- Bridge-push in `register-claude-pid` (no longer needed)

## [0.2.1] - 2026-02-23

### Added

- TQ comparison predicates: `>`, `<`, `>=` in `:where` clauses (e.g., `(:where (> :obs-count 10))`)
- TQ `:skip N` step for pagination
- TQ `:edges` step for graph edge inspection per node
- Batch embedding via `ollama-embed-batch` for observation indexing (single API call)
- `vec-normalize` and pre-normalized embedding storage (dot product = cosine similarity)

### Fixed

- Embedding cache collision risk: keyed by full text string instead of sxhash fixnum
- TQ `format-query-result` handles `:edges` output format

### Removed

- PN-Counter CRDT (zero production references)
- Dead Markov functions: `compute-event-markov-kernel`, `bisimulation-quotient`, `free-energy-reduction`, `cosine-similarity`, `cached-free-energy`
- Dead vector clock functions: `vc-happened-before-p`, `vc-concurrent-p`

## [0.2.0] - 2026-02-22

### Added

- CLI tool dispatch: `kli <tool> [args] --task <id>` exposes all 31 MCP tools as subcommands
- Schema-driven argument parsing with positional/named args and per-tool `--help`
- HTTP session management: `--task` flag sets task context before tool execution
- `using-kli-cli` skill for teammate CLI access (MCP workaround)
- `kli-team` skill updated with teammate access gap documentation

### Fixed

- `task_list` performance: ~3750x speedup (3.75s → 1-2ms) via cached formatting

## [0.1.0] - 2026-02-20

Initial release.

### Added

- Event-sourced task graph with CRDT-based concurrent editing
- TQ (Task Query) pipeline language for filtering, sorting, grouping, and mutating tasks
- PQ (Playbook Query) pipeline language for pattern retrieval with Bayesian scoring
- Pattern learning system with helpful/harmful feedback and embedding-based retrieval
- Task MCP server (HTTP transport) with scaffold-plan, task lifecycle, and graph queries
- Playbook MCP server (HTTP transport) for pattern activation and feedback
- Claude Code hooks: session tracking, tool call recording, file conflict detection, playbook activation
- Skills: research, plan, implement, reflect, validate, handoff, resume
- Dashboard web UI with task graph visualization, health metrics, and activity timeline
- `kli init` command for project setup (MCP servers, hooks, skills, agents, commands)
- `kli update` self-update command with atomic replacement and rollback
- `kli serve` for running MCP servers (task and playbook)
- Ollama integration for local embeddings (nomic-embed-text)
- Cross-platform support: x86_64-linux, aarch64-darwin
- Install script at `https://kli.kleisli.io/install`
