# Changelog

All notable changes to kli are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.0] - 2026-04-26

This release fixes the long-standing `task_complete` persistence bug where the daemon acknowledged completions in-memory but the durable `events.jsonl` tail never reflected the status change, leaving parents impossible to close. Alongside the fix, the HTTP transport's session-identity story is rebuilt around the `Claude-Session-Id` header, the daemon is hardened against heap exhaustion and corrupt event logs, request deadlines are now per-route, and cold bootstrap is ~33× faster.

### Added

- **Per-route request deadline overrides** — `*route-deadline-overrides*` alist lets each endpoint declare its own timeout. Built-ins: `GET /mcp` 24h (SSE long-poll), `POST /mcp` 30s (tool dispatch), `POST /admin/unwedge` 60s, `GET /health` 5s. Active config exposed via `/admin/diag` under `route_deadlines`.
- **`Claude-Session-Id` HTTP header** authoritative for session resolution — replaces peer-PID coalescing, which conflated parallel Claude shells sharing a parent PID into one session.
- **`/admin/unwedge` endpoint** — runtime cleanup of dead-Claude session contexts; supports `?max_age_hours=N` override.
- **`/admin/diag` endpoint** — daemon health snapshot (sessions, threads, close-wait, GC status, lock-trace tail, route deadlines).
- **Session GC thread** — periodic eviction of inactive session contexts, bounded by `*session-gc-interval-seconds*` and `*session-gc-max-age-hours*`.
- **`reconcile-miskeyed-feedback-state-files`** — daemon-startup scan that archives MCP-sid-keyed `feedback-state.json` files (left over from before the header-path migration) under `.claude/sessions/.miskeyed/`.
- **Playbook sidecar consistency audit** (`verify-playbook-consistency`) — read-only inspection of orphans across `playbook-evidence.json`, `playbook-relevance-feedback.json`, and `playbook-co-applications.json`. Repair-in-place during `post-load-cleanup`; non-zero totals emit a structured warning at startup.
- **Direction-aware edge encoding** — `task.link` writes `:phase-of-parent` / `:forked-from-parent` for upward links so `task-children` no longer over-counts the parent as a pseudo-child or inflates the incomplete-descendant count.
- **`kli admin verify-events` + one-shot repair** — surfaces unparseable `events.jsonl` lines and rebuilds them out of `.quarantine` sidecars.
- **Three-layer nil/empty validation** at MCP tool boundaries — explicit error path replacing silent-NIL handling.
- **Scaffold rejection of non-descriptive phase names** — phase-of links refuse "Phase 1" / "P2" placeholders at the boundary.
- **Session locks with deadlines and diagnostics** — long lock holds surface in `/admin/diag` lock-trace ring buffer.
- **Pattern regression test suite** — `pattern.activate` event emission, scaffold-plan registry isolation, sub-agent fork race coverage.

### Fixed

- **`task_complete` persistence** — events now land in the target task's `events.jsonl`, not the focus task's. `emit-event` takes an explicit `:task-id` parameter; `task_complete` / `task_reopen` / `task_release` no longer rely on a `let`-shadow of `*current-task-id*` that was being clobbered mid-request by `load-session-context`.
- **Asymmetric edge encoding** — parents no longer counted as their own pseudo-children, so `task-children` returns NIL for leaves and parents close cleanly once their children complete.
- **Unknown session contexts no longer auto-resurrect** — HTTP returns `404 code=session-not-found`; the CLI drops its persisted sid and re-initialises rather than silently inheriting a virgin context.
- **SSE long-poll no longer 503's every 30 seconds** — `GET /mcp` uses a 24h SSE deadline (one log slice previously recorded 1352 deadline warnings on a single session).
- **Heap exhaustion under concurrent load** — `--dynamic-space-size 4096` now threads from the kli wrapper through `buildLisp.program`; `*task-state-cache*` bounded; single-pass `extract-all-state-edges` + `task-health-data`.
- **Corrupt events.jsonl tolerance** — unparseable lines quarantined to `events.jsonl.quarantine` sidecars rather than aborting reads.
- **Playbook session contract** — `record-activation` / `record-feedback` signal `unknown-session-error` instead of silently creating a virgin session; `write-feedback-state-file` reads existing on-disk content under `with-file-lock` and unions activated/feedback_given before writing, killing last-writer-wins across parallel Claude shells.
- **Playbook patterns load into memory at daemon startup** — `pq_query` previously returned 0 patterns despite `playbook.md` existing on disk.
- **Session registry race in concurrent sub-agent forks** — eliminated.
- **Orphaned threads on remote-eval timeout** — swank-mcp / task-mcp boundary fixed.
- **Stale `kli-meta-path`, session-resolution leaks, legacy `playbook-mcp` references** — cleared.
- **Session-context save discipline** — eliminates state leaks between request boundaries.
- **Concurrency fixes + parser gate** in task-mcp / hooks integration.

### Performance

- **Cold bootstrap**: 91s → 2.7s (~33×). Edge-read deduplication, inlined orphan check, frontier-affinity cap, elog-load per-request caching, single-pass graph build.
- **`task_health`**: 37s → 7s (~5×).

### Changed

- **`*pid-claude-session-registry*`** reshaped from `PID → (sid . ts)` to `PID → list of (sid . ts)` newest-first; multiple sids per PID now coexist instead of overwriting each other. Resolver tolerates the legacy single-cell shape for safe hot-loads.
- **`emit-event`** is now a pure persistence-layer function — does not call `ensure-session-context`. Session context loading lives exclusively at the request-entry layer (HTTP `:around` method on acceptor-dispatch-request, plus `define-task-tool` macro injection).
- **`playbook-mcp` package renamed to `playbook`** and the standalone service directory deleted — playbook is now a library inside the unified daemon.

### Removed

- **`(or claude-session-id session-id)` fallback** in `write-feedback-state-file` — replaced with a single mode-agnostic invariant: claude-sid must be set or the write refuses.
- **Peer-PID coalescing** as the primary session-resolution mechanism — `Claude-Session-Id` header takes precedence; peer-PID remains as a defensive lookup only.
- **In-body `require-current-task` re-runs** inside `emit-event` — request-entry guards (HTTP `:around` + `define-task-tool` macro) are the single source of truth.
- **Sandbox-incompatible playbook tests** removed from `PLAYBOOK.TESTS` aggregate.

## [0.3.1] - 2026-02-28

### Fixed

- Heap exhaustion during cold graph build — `*elog-cache*` retained all 860 tasks' event-logs simultaneously; now disabled during `build-multi-depot-task-graph` so each task's data is GC-eligible after processing (peak memory: 161MB vs previous OOM at 1GB)
- SBCL heap default too small — kli wrapper now sets `--dynamic-space-size 4096` (was relying on compiled-in 1GB default)
- Multi-session attribution bugs — file-conflict hook rewritten as thin HTTP client delegating to daemon's `/file-conflict` endpoint, enforcing PID-liveness checks and session-aware scanning
- Stale in-memory session contexts — `cleanup-inactive-sessions` wired into `task_create` and `task_bootstrap` for opportunistic GC of departed sessions

### Added

- `GET /file-conflict` HTTP endpoint on task-mcp daemon for hook integration with proper session attribution
- `scan-task-enrichment` and `file-conflict-for-caller` server-side functions for PID-checked conflict detection

## [0.3.0] - 2026-02-23

### Added

- Query-scoped `:not-relevant` feedback — mark patterns as "fine but wrong for this query" without distorting global quality scores (`:helpful`/`:harmful`)
- Relevance penalty in pattern activation — accumulated `:not-relevant` signals reduce similarity scores for matching domain contexts, with 7-day exponential decay
- Relevance feedback sidecar storage (`playbook-relevance-feedback.json`) with atomic I/O, additive merge across depots, and pruning
- `normalize-feedback-type` helper handling 8+ input variants including `:irrelevant` alias
- Server startup merges relevance feedback from all `PLAYBOOK_PATHS` depots
- Feedback nudge hook shows all 3 feedback options (`:helpful`, `:harmful`, `:not-relevant`)

## [0.2.3] - 2026-02-23

### Added

- `kli status` command — one-shot task overview (state, plan progress, recent observations)

### Fixed

- Critical sxhash collision in observation system — obs-graph and retrieval hash tables keyed by fixnum sxhash caused silent data aliasing between different texts with the same hash; re-keyed to full text strings with `:test 'equal`
- Race conditions in graph cache (`*graph-cache-lock*` upgraded to recursive lock, all access sites wrapped) and embedding dirty counter (read under lock)
- Event-id 1-second collision — added monotonic `*event-counter*` for unique event IDs within a session
- Scaffold-plan registry pollution — `tq-mutation-handler` no longer writes transient task IDs to the shared session registry during `:fork` operations; uses `*in-mutation*` flag to suppress registry reads/writes, preventing scaffold-plan from creating phases under the wrong parent task

### Removed

- `*latest-registered-depot*` process-wide fallback (replaced by per-session depot resolution)

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
