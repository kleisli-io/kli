# Changelog

All notable changes to kli are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
