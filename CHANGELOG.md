# Changelog

All notable changes to kli are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
