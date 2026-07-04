# Changelog

All notable changes to kli are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
