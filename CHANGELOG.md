# Changelog

All notable changes to kli are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.2] - 2026-07-10

### Changed

- OpenAI and Codex model catalogues now include GPT-5.6 (`gpt-5.6`, `gpt-5.6-sol`, `gpt-5.6-terra`, and `gpt-5.6-luna`) and remove deprecated GPT-5.4 entries. OpenAI-family reasoning effort now accepts `max`.

### Fixed

- Live `eval` and `recompile-rerun` now unwind SBCL control-stack exhaustion into a structured resource error. Interactive evals complete and retract their park, so runaway recursion no longer leaves the agent hung or terminates the KLI session, and later eval calls remain usable.
- Codex Responses WebSocket requests that close after `response.created` but before emitting any model delta now discard the failed connection and fall back to SSE. Once output has been emitted, kli still refuses to replay the request, preventing duplicated text or tool calls.
- Thinking streams now preserve separate reasoning content blocks, so final provider updates replace only their own block instead of removing earlier thinking entries from the same turn.

## [0.2.1] - 2026-07-09

### Fixed

- Codex Responses WebSocket continuations now recover from `previous_response_not_found` error frames by clearing the cached continuation and retrying the full conversation context. This fixes follow-up prompts surfacing "Previous response ... not found" or taking a slower retry path when a cached `previous_response_id` is no longer available on the server.

## [0.2.0] - 2026-07-09

### Headline changes

#### Context views make model requests explicit

- **Provider replay is built from explicit context views.** KLI now uses explicit context views for durable session logs, editable context, summarizer input, provider replay, request audit, and provider-visible accounting. Model requests are built from deliberate provider-shaped replay instead of the generic transcript projection.

### Added

- Development shells now expose a `kli-debug` image when debug Swank support is available.

### Fixed

- `/reload` now prepares user extensions before touching the running set, runs through the same extension-load authority as boot, and leaves existing extensions active when a broken edit fails to load. `/profile`, `/enable`, `/disable`, and `/uninstall` now share the same lifecycle lock, and failed live profile switches roll back to the previous profile, settings overlay, and extension set. This fixes extensions disappearing after `/reload` ([#3](https://github.com/kleisli-io/kli/issues/3)).
- Codex Responses WebSocket aborts now shut down the underlying stream instead of closing the WebSocket/TLS object from another thread. Cached WebSocket sessions are retired on abort/discard so the reader thread owns final close, avoiding SBCL corruption warnings during TUI aborts under poor network timing.
- Provider context replay and compaction now use explicit context views, provider-shaped replay, and provider-visible accounting so sessions can recover after large tool outputs or oversized tool-call arguments without repeatedly overflowing the model context. This fixes context-window failures after large reads or writes ([#4](https://github.com/kleisli-io/kli/issues/4)).

### Changed

- The `bash` tool no longer rejects a fixed list of interactive-looking command names such as `nvim`, `less`, `ssh`, `tmux`, or `screen`. A `process/exec` grant now admits every command string consistently; terminal-oriented programs still run without a PTY and may hang, fail, or produce poor captured output. When steering the model, ask it for non-interactive command-line equivalents, explicit timeouts for uncertain commands, or a background launch plus later output polling for long-running work.

## [0.1.6] - 2026-07-07

### Fixed

- Relocatable bundles now consume cl-deps native runtime contracts for OpenSSL, shipping provider modules, setting `OPENSSL_MODULES`, and discovering the host CA certificate bundle before launch. This fixes HTTPS release/update paths such as `kli update` on hosts without a Nix store.
- Boot snapshot reuse now refreshes runtime-sensitive install-time state in full install order after rebinding the user's config roots and settings. This fixes startup sessions that reported `Unknown model ... in settings` or failed to select a runtime `providers.json` default model ([#1](https://github.com/kleisli-io/kli/issues/1)), and completes the runtime-refresh fix for bundled skill commands such as `/skill:creating-extensions` being unavailable until a manual refresh ([#2](https://github.com/kleisli-io/kli/issues/2)).

## [0.1.5] - 2026-07-06

### Added

- The `openai-codex` provider can stream through the Responses WebSocket transport. The default `auto` transport uses WebSocket when the selected provider/model supports it, keeps SSE available as a fallback, and reuses cached context across same-session turns with `previous_response_id` deltas.
- `kli -p` accepts repeated `--model-option KEY=VALUE` overrides, with `--option` as a shorter alias. Generated shell completions now include known option values such as `transport=sse`, `transport=websocket`, `transport=websocket-cached`, and reasoning-effort values.
- `kli -p --timings` emits model request timing markers in JSON and stream-JSON output, including payload size, HTTP or WebSocket milestones, first provider event, first visible delta, tool/thinking events, and completion timing.
- The `/bash` slash command accepts timeout/background flags and manual Tab completion now uses Bash programmable completion, help-derived options/subcommands, and path fallback for shell command arguments.

### Fixed

- `kli -p --profile ...` now boots the requested profile instead of always using the print profile.
- JSON timing output now serializes details as JSON objects, arrays, booleans, and nulls instead of Lisp plist and symbol strings.
- Built-in file mutation tools now keep model-visible result details compact. The TUI still renders bounded private diffs, and model transports reject accidental full-file `old`, `new`, preview, patched, or repaired payloads from those tools.
- Anchored file rows shown to models now use `LINE:HH|content` instead of `LINE:HH content` across read, search, edit results, repair accepts, and `edit-sexp` diffs. The pipe delimiter is accepted when copied back into edit anchors and keeps Lisp indentation from being confused with row syntax.
- Boot snapshot reuse now refreshes runtime-derived providers, prompt commands, and skills against the current config roots. If a runtime refresh fails, kli abandons the reused snapshot for that boot and falls back to a full profile install with a visible diagnostic.

## [0.1.4] - 2026-07-05

### Fixed

- Prompt templates bundled by a runtime-installed extension now surface as slash commands. Installing or retracting a user extension re-runs the prompt-template scan, so `kli install <ext>` — and `/install`, `/uninstall`, `/enable`, `/disable`, `/reload` mid-session — immediately adds or withdraws the extension's prompt commands. Previously the scan ran once at profile install, so an extension loaded afterwards exposed its tools but none of its prompt commands.
- `kli mcp-serve` no longer prints "session bind skipped (Subject lacks capability :AGENT/SESSION/SWITCH.)" on every serve. The serve loop's session bind runs under a subject carrying exactly the capability it needs, so the bind succeeds and tools that record a session id get a real one.
- Cold-cache extension loads no longer print macro redefinition warnings. A source-distributed unit installs its cross-file macros twice by design (compile-time evaluation, then the fasl load); the resulting redefinition warnings are muffled while everything else still reports.

## [0.1.3] - 2026-07-05

### Fixed

- Extensions installed from a source bundle now serve their bundled prompts and resources. A directory unit may carry a generated `resources.sexp` manifest mapping resource-root keys to unit-relative directories; the loader registers each key against the placed tree, so `kli mcp-serve <ext>` exposes the extension's prompt templates over `prompts/list` and `resources/list`, and the interactive session mounts them as slash commands. Previously only extensions compiled into the image resolved their bundled resources.

## [0.1.2] - 2026-07-04

### Fixed

- Linux bundles now ship the full shared-library closure of their bundled libraries. `libz.so.1`, needed by the bundled `libsqlite3`, was missing, so extensions using SQLite full-text search failed on hosts without a Nix store.
- The process now shows as `kli` in `ps`, `top`, and `tmux` instead of `ld-linux-x86-64.so.2`.

## [0.1.1] - 2026-07-04

### Fixed

- Profile group members now activate by requirement rather than declared order: a member whose requirements a later entry provides is deferred and retried instead of erroring, and when nothing can progress the error names every blocked extension and its missing requirements.
- An error escaping the configured-image dump is now reported on stderr with a backtrace and a nonzero exit, instead of vanishing silently with the process.

## [0.1.0] - 2026-07-04

First public release of kli — a radically extensible coding agent for the modern Lisp hacker. kli runs inside its own live SBCL image over a small boot kernel that knows only how to install, switch, and roll back protocols; everything else is an extension on that kernel.

### Added

- **Protocol kernel.** A minimal boot kernel with three verbs — install, switch, and roll back — that knows nothing about what an extension is. The extension system itself, its manifests, contribution kinds, capabilities, and tools, is a protocol installed on the kernel through those same verbs.
- **Live specialization and hot patching.** A `:method` contribution adds a method to any generic in the running image, and retraction is its inverse. Behavior lives in CLOS cells over pandoric closures, so a hot patch swaps the code while keeping the closed-over state — the editor keeps its buffer and the transcript its scrollback — behind a condition-system fault barrier that degrades a throwing patch instead of killing the session. Every contribution carries a retractor, so retraction drains exactly what it added and nothing else.
- **Safe protocol switching.** A protocol switch validates, smoke-tests, swaps, and rolls back on any error.
- **Agent loop as an extension.** The message handler, skill expander, retry count, and compaction threshold are pandoric closures you can open and hot patch while a turn is in flight.
- **Model providers.** OpenAI, Codex, and OpenAI-compatible endpoints ride a provider registry, wired with OAuth and credential auth.
- **Tools.** bash, eval, and a file surface — read, write, edit, find, and search — over hashline anchors. Reads emit line and content-hash anchors; edits are anchor-validated patches that reject stale anchors instead of clobbering drifted files; search results arm the anchor cache so a match is editable without a separate read.
- **Terminal UI.** Sessions with compacting logs, slash commands, themes, keybindings, and status widgets, all themselves extensions.
- **Command-line entry points.** `kli` opens the terminal UI; `kli version`, `kli help`, and `kli docs` cover version, help, and documentation; `kli -p`/`--print` runs headless with `--output-format text|json|stream-json`; and `kli --print-authority` prints the exact capabilities a session will hold.
- **Extension install and serve.** `kli install` installs an extension, verifying signed extensions against their trust roots; `kli mcp-serve` serves an extension over MCP.
- **Self-update.** `kli update` fetches and installs a release over a certificate-verified TLS connection, with `--version` to pin a target.
- **Authority-based security.** kli enforces authority — which capabilities an agent may exercise — rather than containment, and the configured-image producer takes a `sandbox` option that wraps the entrypoint in bwrap.
- **Install paths.** A one-line install (`curl -fsSL https://kli.kleisli.io | sh`) drops a relocatable binary under `~/.local`, overridable with `KLI_INSTALL_DIR` and pinnable with `KLI_VERSION`; a Nix flake ships `overlays.default`, `packages.<system>.default`, and NixOS and Home Manager modules exposing `programs.kli`; or build from source with SBCL and qlot. Prebuilt bundles cover Linux x86_64 and aarch64 and macOS aarch64.
- **Documentation** at [docs.kleisli.io](https://docs.kleisli.io).
