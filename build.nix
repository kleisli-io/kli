{ pkgs, lib, buildLisp, lisp, replOrn ? null, src ? ./., versionSexp ? ./version.sexp }:

let
  # ──────────────────────────────────────────────────────────────────
  # Group 1: Kernel — minimal substrate (live objects, registry,
  # context, protocol class, boot protocol).
  # ──────────────────────────────────────────────────────────────────
  kernelSrcs = [
    ./src/package.lisp
    ./src/kernel.lisp
    ./src/protocols/boot.lisp
  ];

  # ──────────────────────────────────────────────────────────────────
  # Group 2: Extension protocol — framework for defining, loading,
  # installing, and retracting extensions on top of the kernel.
  # ──────────────────────────────────────────────────────────────────
  extensionProtocolSrcs = [
    ./src/extensions/extension-protocol/package.lisp
    ./src/extensions/extension-protocol/subject.lisp
    ./src/extensions/extension-protocol/fault-barrier.lisp
    ./src/extensions/extension-protocol/protocol.lisp
    ./src/extensions/extension-protocol/presentation.lisp
    ./src/extensions/extension-protocol/facade.lisp
    ./src/extensions/extension-protocol/syntax.lisp
    ./src/extensions/extension-protocol/resource.lisp
    ./src/extensions/extension-protocol/trace.lisp
  ];

  # ──────────────────────────────────────────────────────────────────
  # Group 3: Extensions — per-extension package files, defextension
  # bodies, and kli/app boot orchestration.
  # ──────────────────────────────────────────────────────────────────

  # Low-level spill store: caps tool output to a context budget and writes the
  # full output to /tmp so the model retrieves the rest via Read/search. Needs
  # only the extension protocol (storage + run-identity root), so it loads right
  # after it and before every consuming tool.
  outputSpillSrcs = [
    ./src/extensions/output-spill/package.lisp
    ./src/extensions/output-spill/store.lisp
  ];

  objectSrcs = [
    ./src/extensions/object/package.lisp
    ./src/extensions/object/model.lisp
  ];

  eventSrcs = [
    ./src/extensions/event/package.lisp
    ./src/extensions/event/protocol.lisp
    ./src/extensions/event/kinds.lisp
    ./src/extensions/event/clause.lisp
  ];

  profileSrcs = [
    ./src/extensions/profiles/package.lisp
    ./src/extensions/profiles/kind.lisp
  ];

  # Concrete profiles compose the ordered manifest groups, so they load after
  # every extension manifest is defined (just before app boot orchestration).
  profileBundleSrcs = [
    ./src/extensions/profiles/groups.lisp
    ./src/extensions/profiles/headless.lisp
    ./src/extensions/profiles/print.lisp
    ./src/extensions/profiles/interactive-terminal.lisp
    ./src/extensions/profiles/human-in-loop.lisp
    ./src/extensions/profiles/autonomous.lisp
    ./src/extensions/profiles/selection.lisp
    ./src/extensions/profiles/spec.lisp
  ];

  textSrcs = [
    ./src/extensions/text/package.lisp
    ./src/extensions/text/text.lisp
    ./src/extensions/text/json.lisp
  ];

  sessionLogSrcs = [
    ./src/extensions/session/log/package.lisp
    ./src/extensions/session/log/model.lisp
    ./src/extensions/session/log/protocol.lisp
    ./src/extensions/session/log/context.lisp
    ./src/extensions/session/log/compaction.lisp
    ./src/extensions/session/log/serialize.lisp
    ./src/extensions/session/log/file-store.lisp
  ];

  contextLensSrcs = [
    ./src/extensions/context/lens/package.lisp
    ./src/extensions/context/lens/model.lisp
    ./src/extensions/context/lens/capsule.lisp
    ./src/extensions/context/lens/protocol.lisp
    ./src/extensions/context/lens/serialize.lisp
  ];

  authSrcs = [
    ./src/extensions/auth/core/package.lisp
    ./src/extensions/auth/core/model.lisp
    ./src/extensions/auth/core/env.lisp
    ./src/extensions/auth/core/protocol.lisp
    ./src/extensions/auth/core/value.lisp
    ./src/extensions/auth/core/persistence.lisp
    ./src/extensions/auth/core/operations.lisp
  ];

  oauthSrcs = [
    ./src/extensions/auth/oauth/package.lisp
    ./src/extensions/auth/oauth/pkce.lisp
    ./src/extensions/auth/oauth/openai-codex.lisp
  ];

  modelRegistrySrcs = [
    ./src/extensions/model/registry/package.lisp
    ./src/extensions/model/registry/model.lisp
    ./src/extensions/model/registry/protocol.lisp
  ];

  modelRuntimeSrcs = [
    ./src/extensions/model/runtime/package.lisp
    ./src/extensions/model/runtime/model.lisp
    ./src/extensions/model/runtime/protocol.lisp
  ];

  transportSrcs = [
    ./src/extensions/model/transports/package.lisp
    ./src/extensions/model/transports/sse.lisp
    ./src/extensions/model/transports/shared.lisp
    ./src/extensions/model/transports/openai-responses.lisp
    ./src/extensions/model/transports/anthropic-messages.lisp
    ./src/extensions/model/transports/openai-completions.lisp
  ];

  providerCommonSrcs = [
    ./src/extensions/model/providers/common/package.lisp
    ./src/extensions/model/providers/common/runtime.lisp
  ];

  codexProviderSrcs = [
    ./src/extensions/model/providers/codex/package.lisp
    ./src/extensions/model/providers/codex/runtime.lisp
  ];

  openaiProviderSrcs = [
    ./src/extensions/model/providers/openai/package.lisp
    ./src/extensions/model/providers/openai/runtime.lisp
  ];

  anthropicProviderSrcs = [
    ./src/extensions/model/providers/anthropic/package.lisp
    ./src/extensions/model/providers/anthropic/runtime.lisp
  ];

  compatibleProviderSrcs = [
    ./src/extensions/model/providers/compatible/package.lisp
    ./src/extensions/model/providers/compatible/runtime.lisp
  ];

  agentLoopSrcs = [
    ./src/extensions/agent/loop/package.lisp
    ./src/extensions/agent/loop/model.lisp
    ./src/extensions/agent/loop/runtime.lisp
    ./src/extensions/agent/loop/protocol.lisp
  ];

  agentSessionSrcs = [
    ./src/extensions/agent/session/package.lisp
    ./src/extensions/agent/session/model.lisp
    ./src/extensions/agent/session/runtime.lisp
    ./src/extensions/agent/session/protocol.lisp
  ];

  tuiStyleSrcs = [
    ./src/extensions/tui/style/package.lisp
    ./src/extensions/tui/style/color.lisp
    ./src/extensions/tui/style/emit.lisp
    ./src/extensions/tui/style/theme.lisp
  ];

  tuiCoreSrcs = [
    ./src/extensions/tui/core/package.lisp
    ./src/extensions/tui/core/model.lisp
    ./src/extensions/tui/core/protocol.lisp
    ./src/extensions/tui/core/behavior.lisp
  ];

  tuiKeymapSrcs = [
    ./src/extensions/tui/keymap/package.lisp
    ./src/extensions/tui/keymap/keymap.lisp
    ./src/extensions/tui/keymap/kinds.lisp
  ];

  tuiStatusSrcs = [
    ./src/extensions/tui/status/package.lisp
    ./src/extensions/tui/status/registry.lisp
    ./src/extensions/tui/status/kinds.lisp
    ./src/extensions/tui/status/facade.lisp
    ./src/extensions/tui/status/usage.lisp
    ./src/extensions/tui/status/spinner.lisp
  ];

  tuiViewSrcs = [
    ./src/extensions/tui/views/package.lisp
    ./src/extensions/tui/views/model.lisp
    ./src/extensions/tui/views/render.lisp
  ];

  tuiInputSrcs = [
    ./src/extensions/tui/input/package.lisp
    ./src/extensions/tui/input/model.lisp
    ./src/extensions/tui/input/classifiers.lisp
    ./src/extensions/tui/input/decoder.lisp
    ./src/extensions/tui/input/routing.lisp
  ];

  tuiEditorSrcs = [
    ./src/extensions/tui/editor/package.lisp
    ./src/extensions/tui/editor/model.lisp
    ./src/extensions/tui/editor/paste.lisp
    ./src/extensions/tui/editor/completion.lisp
    ./src/extensions/tui/editor/behavior.lisp
    ./src/extensions/tui/editor/render.lisp
  ];

  tuiTerminalSrcs = [
    ./src/extensions/tui/terminal/package.lisp
    ./src/extensions/tui/terminal/background.lisp
    ./src/extensions/tui/terminal/model.lisp
    ./src/extensions/tui/terminal/memory.lisp
    ./src/extensions/tui/terminal/process.lisp
    ./src/extensions/tui/terminal/diff.lisp
  ];

  tuiMarkdownSrcs = [
    ./src/extensions/tui/markdown/package.lisp
    ./src/extensions/tui/markdown/inline.lisp
    ./src/extensions/tui/markdown/syntax.lisp
    ./src/extensions/tui/markdown/markdown.lisp
    ./src/extensions/tui/markdown/diff.lisp
  ];

  tuiAnsiSrcs = [
    ./src/extensions/tui/ansi/package.lisp
    ./src/extensions/tui/ansi/ansi.lisp
  ];

  tuiTranscriptSrcs = [
    ./src/extensions/tui/transcript/package.lisp
    ./src/extensions/tui/transcript/model.lisp
    ./src/extensions/tui/transcript/render.lisp
    ./src/extensions/tui/transcript/scrollback.lisp
    ./src/extensions/tui/transcript/projection.lisp
  ];

  tuiCommandSrcs = [
    ./src/extensions/tui/commands/package.lisp
    ./src/extensions/tui/commands/parser.lisp
    ./src/extensions/tui/commands/render.lisp
  ];

  tuiAppSrcs = [
    ./src/extensions/tui/app/package.lisp
    ./src/extensions/tui/app/model.lisp
    ./src/extensions/tui/app/runtime.lisp
  ];

  runtimePackageSrcs = [
    ./src/extensions/runtime/control/package.lisp
    ./src/extensions/runtime/introspection/package.lisp
    ./src/extensions/runtime/journal/package.lisp
    ./src/extensions/runtime/snapshot/package.lisp
    ./src/extensions/runtime/snapshot/protocol.lisp
    ./src/extensions/runtime/history/package.lisp
    ./src/extensions/runtime/isolated/package.lisp
  ];

  # Persistent subprocess JSON-RPC/stdio transport plus the MCP client surface
  # riding it (membrane + client). A library: package above, implementation
  # here, no contributions of its own.
  isolatedSrcs = [
    ./src/extensions/runtime/isolated/jsonrpc.lisp
    ./src/extensions/runtime/isolated/process.lisp
    ./src/extensions/runtime/isolated/membrane.lisp
    ./src/extensions/runtime/isolated/mcp.lisp
    ./src/extensions/runtime/isolated/lift.lisp
  ];

  # The outbound MCP-server membrane: re-exposes any installed extension's tool
  # surface to an external client over the shared isolated framing. Generic over
  # the extension protocol, so it loads after the protocol, the transports schema
  # mapper, and the framing.
  mcpServerSrcs = [
    ./src/extensions/runtime/mcp-server/package.lisp
    ./src/extensions/runtime/mcp-server/membrane.lisp
    ./src/extensions/runtime/mcp-server/server.lisp
  ];

  # The relocation seam. blessed-libs.lisp depends only on CL plus
  # sb-alien/sb-ext/uiop, so it loads before any app source and its FASL is in
  # selfLib by the time the preDump form references reopen-blessed-libs.
  relocationSrcs = [
    ./src/extensions/runtime/relocation/package.lisp
    ./src/extensions/runtime/relocation/blessed-libs.lisp
  ];

  interactionCommandSrcs = [
    ./src/extensions/interaction/commands/package.lisp
    ./src/extensions/interaction/commands/model.lisp
    ./src/extensions/interaction/commands/clause.lisp
  ];

  interactionBasicCommandSrcs = [
    ./src/extensions/interaction/basic-commands/package.lisp
  ];

  interactionSessionCommandSrcs = [
    ./src/extensions/interaction/session-commands/package.lisp
  ];

  configSrcs = [
    ./src/extensions/config/package.lisp
    ./src/extensions/config/paths.lisp
    ./src/extensions/config/settings.lisp
    ./src/extensions/config/service.lisp
    ./src/extensions/config/settings-kind.lisp
    ./src/extensions/config/command.lisp
  ];

  promptsSrcs = [
    ./src/extensions/prompts/package.lisp
    ./src/extensions/prompts/template.lisp
    ./src/extensions/prompts/discovery.lisp
  ];

  skillsSrcs = [
    ./src/extensions/skills/package.lisp
    ./src/extensions/skills/ignore.lisp
    ./src/extensions/skills/skill.lisp
    ./src/extensions/skills/discovery.lisp
    ./src/extensions/skills/sigil.lisp
  ];

  contextFilesSrcs = [
    ./src/extensions/context/files/package.lisp
    ./src/extensions/context/files/context-files.lisp
  ];

  tuiCompletionSrcs = [
    ./src/extensions/tui/completion/package.lisp
    ./src/extensions/tui/completion/sources.lisp
  ];

  authorSrcs = [
    ./src/extensions/author/package.lisp
    ./src/extensions/author/author.lisp
  ];

  debugSrcs = [
    ./src/extensions/debug/package.lisp
  ];

  modelCommandSrcs = [
    ./src/extensions/model/commands/package.lisp
    ./src/extensions/model/commands/runtime.lisp
  ];

  configWiringSrcs = [
    ./src/extensions/config/wiring/package.lisp
    ./src/extensions/config/wiring/wiring.lisp
  ];

  observabilitySrcs = [
    ./src/extensions/observability/package.lisp
  ];

  contextCommandSrcs = [
    ./src/extensions/context/commands/package.lisp
    ./src/extensions/context/commands/runtime.lisp
  ];

  toolEvalSrcs = [
    ./src/extensions/tools/eval/package.lisp
    ./src/extensions/tools/eval/runtime.lisp
    ./src/extensions/tools/eval/model.lisp
    ./src/extensions/tools/eval/command.lisp
  ];

  toolFilesystemSrcs = [
    ./src/extensions/tools/filesystem/package.lisp
    ./src/extensions/tools/filesystem/anchors.lisp
    ./src/extensions/tools/filesystem/hashline.lisp
    ./src/extensions/tools/filesystem/runtime.lisp
  ];

  toolBashSrcs = [
    ./src/extensions/tools/bash/package.lisp
    ./src/extensions/tools/bash/runtime.lisp
    ./src/extensions/tools/bash/command.lisp
  ];

  toolLispSrcs = [
    ./src/extensions/tools/lisp/package.lisp
    ./src/extensions/tools/lisp/runtime.lisp
    ./src/extensions/tools/lisp/edit-sexp.lisp
  ];

  toolIntrospectSrcs = [
    ./src/extensions/tools/introspect/package.lisp
    ./src/extensions/tools/introspect/runtime.lisp
  ];

  toolTraceSrcs = [
    ./src/extensions/tools/trace/package.lisp
    ./src/extensions/tools/trace/runtime.lisp
  ];

  appSrcs = [
    ./src/extensions/app/package.lisp
    ./src/extensions/app/util.lisp
    ./src/extensions/app/cli-grammar.lisp
    ./src/extensions/app/headless-io.lisp
    # +kli-version+ compiled in (no resource lookup) so it survives relocation.
    ./src/extensions/app/version-const.lisp
    ./src/extensions/app/version.lisp
    ./src/extensions/app/user-extensions.lisp
    ./src/extensions/app/remote-install.lisp
    ./src/extensions/app/release-signer.lisp
    ./src/extensions/app/install-command.lisp
    ./src/extensions/app/profile-command.lisp
    ./src/extensions/app/main.lisp
    ./src/extensions/app/mcp-serve.lisp
    ./src/extensions/app/install-cli.lisp
    ./src/extensions/app/update.lisp
    ./src/extensions/app/docs-command.lisp
    ./src/extensions/app/output-format.lisp
    ./src/extensions/app/print.lisp
    ./src/extensions/app/completion.lisp
    ./src/extensions/app/dispatch.lisp
  ];

  extensionSrcs = [
    ./src/extensions/object/extension.lisp
    ./src/extensions/event/extension.lisp
    ./src/extensions/interaction/commands/extension.lisp
    ./src/extensions/interaction/basic-commands/extension.lisp
    ./src/extensions/interaction/session-commands/extension.lisp
    ./src/extensions/config/extension.lisp
    ./src/extensions/config/wiring/extension.lisp
    ./src/extensions/observability/extension.lisp
    ./src/extensions/prompts/extension.lisp
    ./src/extensions/skills/extension.lisp
    ./src/extensions/context/files/extension.lisp
    ./src/extensions/session/log/extension.lisp
    ./src/extensions/context/lens/extension.lisp
    ./src/extensions/auth/core/extension.lisp
    ./src/extensions/auth/oauth/extension.lisp
    ./src/extensions/model/registry/extension.lisp
    ./src/extensions/model/runtime/extension.lisp
    ./src/extensions/model/providers/codex/extension.lisp
    ./src/extensions/model/providers/openai/extension.lisp
    ./src/extensions/model/providers/anthropic/extension.lisp
    ./src/extensions/model/providers/compatible/extension.lisp
    ./src/extensions/model/commands/extension.lisp
    ./src/extensions/context/commands/extension.lisp
    ./src/extensions/agent/loop/extension.lisp
    ./src/extensions/agent/session/extension.lisp
    ./src/extensions/tools/eval/extension.lisp
    ./src/extensions/tools/filesystem/extension.lisp
    ./src/extensions/tools/lisp/extension.lisp
    ./src/extensions/tools/bash/extension.lisp
    ./src/extensions/tools/introspect/extension.lisp
    ./src/extensions/tools/trace/extension.lisp
    ./src/extensions/output-spill/extension.lisp
    ./src/extensions/tui/style/extension.lisp
    ./src/extensions/tui/keymap/extension.lisp
    ./src/extensions/tui/status/extension.lisp
    ./src/extensions/tui/views/extension.lisp
    ./src/extensions/tui/input/extension.lisp
    ./src/extensions/tui/editor/extension.lisp
    ./src/extensions/tui/terminal/extension.lisp
    ./src/extensions/tui/markdown/extension.lisp
    ./src/extensions/tui/transcript/extension.lisp
    ./src/extensions/tui/completion/extension.lisp
    ./src/extensions/tui/app/extension.lisp
    ./src/extensions/runtime/control/extension.lisp
    ./src/extensions/runtime/introspection/extension.lisp
    ./src/extensions/runtime/journal/extension.lisp
    ./src/extensions/runtime/snapshot/extension.lisp
    ./src/extensions/runtime/history/extension.lisp
    ./src/extensions/debug/extension.lisp
  ];

  testSrcs = [
    ./t/package.lisp
    ./t/authority-helpers.lisp
    ./t/kernel.lisp
    ./t/remote-install-pins.lisp
    ./t/remote-install-trust.lisp
    ./t/trust-root-config.lisp
    ./t/release-signer.lisp
    ./t/install-command.lisp
    ./t/web-install.lisp
    ./t/update-verify.lisp
    ./t/update.lisp
    ./t/two-ledger-coherence.lisp
    ./t/cross-image-snapshot.lisp
    ./t/session-identity.lisp
    ./t/output-spill.lisp
    ./t/subject.lisp
    ./t/authority-lattice.lisp
    ./t/authority-surface.lisp
    ./t/grant-morphism.lisp
    ./t/isolated-transport.lisp
    ./t/mcp-client.lisp
    ./t/inbound-lift.lisp
    ./t/spawn-gate.lisp
    ./t/isolated-pin.lisp
    ./t/delegation.lisp
    ./t/grant-seed-and-drain.lisp
    ./t/removable-layer.lisp
    ./t/mcp-server.lisp
    ./t/capability-gates.lisp
    ./t/commands.lisp
    ./t/basic-commands.lisp
    ./t/session-commands.lisp
    ./t/config.lisp
    ./t/session-log.lisp
    ./t/context-lens.lisp
    ./t/session-persistence.lisp
    ./t/file-session-store.lisp
    ./t/auth-model-registry.lisp
    ./t/model-options.lisp
    ./t/auth-credentials.lisp
    ./t/capability-gate-symmetry.lisp
    ./t/auth-oauth.lisp
    ./t/fake-model-provider.lisp
    ./t/model-runtime.lisp
    ./t/event.lisp
    ./t/model-responses-transport.lisp
    ./t/model-completions-transport.lisp
    ./t/model-anthropic-transport.lisp
    ./t/agent-loop.lisp
    ./t/model-tool-calling.lisp
    ./t/codex-provider.lisp
    ./t/openai-provider.lisp
    ./t/anthropic-provider.lisp
    ./t/compatible-provider.lisp
    ./t/model-substrate-retraction.lisp
    ./t/profiles.lisp
    ./t/profiles-spec.lisp
    ./t/agent-session.lisp
    ./t/settings-wiring.lisp
    ./t/load-on-open.lisp
    ./t/compaction.lisp
    ./t/integration-session.lisp
    ./t/profiles-headless.lisp
    ./t/profiles-interactive-terminal.lisp
    ./t/profiles-stubs.lisp
    ./t/profiles-boot.lisp
    ./t/profile-command.lisp
    ./t/model-commands.lisp
    ./t/tools.lisp
    ./t/eval.lisp
    ./t/lisp.lisp
    ./t/introspect-tools.lisp
    ./t/trace-tool.lisp
    ./t/text.lisp
    ./t/tui-core.lisp
    ./t/status-usage.lisp
    ./t/status-spinner.lisp
    ./t/tui-views.lisp
    ./t/tui-input.lisp
    ./t/tui-editor.lisp
    ./t/tui-terminal.lisp
    ./t/tui-terminal-background.lisp
    ./t/tui-transcript.lisp
    ./t/tui-projection.lisp
    ./t/tui-commands.lisp
    ./t/tui-app.lisp
    ./t/tui-surfaces.lisp
    ./t/eliminate-image-global-protocol-state.lisp
    ./t/manifest-completeness-and-method-wrapper.lisp
    ./t/prompts.lisp
    ./t/skills.lisp
    ./t/context-files.lisp
    ./t/context-lens-commands.lisp
    ./t/app-fatal.lisp
    ./t/debug.lisp
    ./t/relocation.lisp
    ./t/resource-root.lisp
    ./t/load-warnings.lisp
    ./t/version.lisp
    ./t/dispatch.lisp
    ./t/cli-grammar.lisp
    ./t/output-format.lisp
    ./t/print-mode.lisp
    ./t/docs-command.lisp
    ./t/sqlite-blessed.lisp
    ./t/recode-integration.lisp
    ./t/author.lisp
    ./t/tui-style.lisp
    ./t/tui-rendering.lisp
    ./t/tui-markdown.lisp
    ./t/tui-ansi.lisp
    ./t/tui-keybindings.lisp
    ./t/tui-completion.lisp
    ./t/fault-barrier.lisp
    ./t/observability.lisp
  ];

  # The library's serial load order, also the single source of truth the
  # generated kli.asd `:components` mirror.
  allSrcs =
    kernelSrcs
    ++ textSrcs
    ++ extensionProtocolSrcs
    ++ outputSpillSrcs
    ++ objectSrcs
      ++ eventSrcs
      ++ profileSrcs
      ++ sessionLogSrcs
      ++ contextLensSrcs
      ++ authSrcs
      ++ oauthSrcs
      ++ modelRegistrySrcs
      ++ modelRuntimeSrcs
      ++ transportSrcs
      ++ providerCommonSrcs
      ++ codexProviderSrcs
      ++ openaiProviderSrcs
      ++ anthropicProviderSrcs
      ++ compatibleProviderSrcs
      ++ agentLoopSrcs
      ++ agentSessionSrcs
      ++ tuiStyleSrcs
      ++ tuiCoreSrcs
      ++ tuiKeymapSrcs
      ++ tuiStatusSrcs
      ++ tuiViewSrcs
      ++ tuiInputSrcs
      ++ tuiEditorSrcs
      ++ tuiTerminalSrcs
      ++ tuiMarkdownSrcs
      ++ tuiAnsiSrcs
      ++ tuiTranscriptSrcs
      ++ interactionCommandSrcs
      ++ interactionBasicCommandSrcs
      ++ interactionSessionCommandSrcs
      ++ configSrcs
      ++ promptsSrcs
      ++ skillsSrcs
      ++ contextFilesSrcs
      ++ tuiCompletionSrcs
      ++ authorSrcs
      ++ tuiCommandSrcs
      ++ tuiAppSrcs
      ++ runtimePackageSrcs
      ++ isolatedSrcs
      ++ mcpServerSrcs
      ++ relocationSrcs
      ++ modelCommandSrcs
      ++ observabilitySrcs
      ++ contextCommandSrcs
      ++ toolEvalSrcs
      ++ toolFilesystemSrcs
      ++ toolLispSrcs
      ++ toolBashSrcs
      ++ configWiringSrcs
      ++ toolIntrospectSrcs
      ++ toolTraceSrcs
      ++ debugSrcs
      ++ extensionSrcs
      ++ profileBundleSrcs
      ++ appSrcs;

  # +kli-version+ generated from versionSexp at build time (no IFD).
  versionConst = pkgs.runCommand "kli-version-const.lisp" { } ''
    printf '(in-package #:kli/app)\n(defparameter +kli-version+ %s)\n' "$(cat ${versionSexp})" > $out
  '';

  # Library compiles the generated version-const; allSrcs stays the asd source of truth.
  librarySrcs =
    map (s: if s == ./src/extensions/app/version-const.lisp then versionConst else s) allSrcs;

  # Generated kli.asd whose `:components` mirror the serial allSrcs (one source
  # of truth, drift-checked), so a plain sbcl/qlot build works on its own.
  # External systems are resolved by the checked-in qlfile; build.lisp dumps
  # the image.
  asd =
    let
      rootStr = toString ./.;

      # `./x.lisp` path literal -> "x" component string (relative to the asd dir).
      relativize = s:
        lib.removeSuffix ".lisp" (lib.removePrefix (rootStr + "/") (toString s));

      # The asd's first component is the vendored resource-support file
      # (vendor/buildlisp-resources.lisp). Resources resolve at runtime via the
      # KLI_DATA_DIR env override, so the store-path registration forms are omitted.
      componentPaths = [ "vendor/buildlisp-resources" ] ++ map relativize allSrcs;
      componentForms =
        lib.concatMapStringsSep "\n               " (p: ''(:file "${p}")'') componentPaths;

      # Upstream ASDF system names for `:depends-on` -- these are NOT the
      # buildLisp attr names (jzon's system is com.inuoe.jzon). Order mirrors
      # the library deps.
      dependsOn = [
        "let-over-lambda"
        "com.inuoe.jzon"
        "drakma"
        "ironclad"
        "cl-base64"
        "3bmd"
        "3bmd-ext-code-blocks"
        "3bmd-ext-tables"
        "colorize"
        "cl-difflib"
        "cl-ppcre"
        "sqlite"
        "paren-repair"
        "websocket-driver-client"
        "sb-posix"
      ];
      dependsForms = lib.concatMapStringsSep " " (s: ''"${s}"'') dependsOn;

      asdText = ''
        ;;;; kli system definition -- GENERATED; do not hand-edit.
        ;;;; External systems are pinned by the checked-in qlfile.
        (defsystem "kli"
          :description "Relocatable Common Lisp CLI/TUI agent framework"
          :version (:read-file-form "version.sexp")
          :author "Kleisli.IO"
          :license "MIT"
          :depends-on (${dependsForms})
          :serial t
          :components (${componentForms}))
      '';
    in
    pkgs.writeText "kli.asd" asdText;

  # Resource roots, keyed by the literal string the runtime resolves with
  # (buildlisp/resources:resource-path). The shipped tarball lays each key out
  # verbatim under share/kli, so KLI_DATA_DIR resolves them on a relocated host.
  resourcesAttr = {
    "kli/tui/style" = ./src/extensions/tui/style/themes;
    "kli/skills" = ./src/extensions/skills/builtin;
  };

  library = (buildLisp.library {
    name = "kli";

    resources = resourcesAttr;

    deps = [
      lisp.let-over-lambda
      lisp.jzon
      lisp.drakma
      lisp.ironclad
      lisp.cl-base64
      lisp."3bmd"
      lisp."3bmd-ext-code-blocks"
      lisp."3bmd-ext-tables"
      lisp.colorize
      lisp.cl-difflib
      lisp.cl-ppcre
      lisp.sqlite
      lisp.paren-repair
      lisp.websocket-driver-client
      (buildLisp.bundled "sb-posix")
      (buildLisp.bundled "asdf")
    ];

    srcs = librarySrcs;

    tests = {
      deps = [
        lisp.fiveam
      ];
      srcs = testSrcs;
      expression = "(fiveam:run! 'kli/tests::all)";
    };

    passthru =
      let
        # Build-wiring for the relocatable-boot seam, emitted in CL-USER at
        # preDump (after every dep FASL has loaded and before save-lisp-and-die).
        # Generates the blessed SONAME set from the live image, flips :dont-save
        # on those shared-object entries, registers the availability hook, and
        # asserts the crash-prevention invariant. Shared verbatim by the shipped
        # binary and the relocation probe so the two never drift.
        blessedLibsSeam = ''
          ;; Runs in CL-USER at preDump, after every dep FASL has loaded (so each blessed
          ;; lib's top-level use-foreign-library has already recorded a *shared-objects*
          ;; entry) and before save-lisp-and-die.
          ;;
          ;; Generate the blessed set from the live image: the directory-less namestrings
          ;; are the SONAME-resolved entries reopen will retry. Never hand-list, never
          ;; derive from DT_SONAME.
          (setf kli/runtime/relocation:*blessed-sonames*
                (remove-duplicates
                 (remove-if-not
                  #'kli/runtime/relocation:directoryless-namestring-p
                  (mapcar #'sb-alien::shared-object-namestring sb-alien::*shared-objects*))
                 :test #'string=))

          ;; Flip :dont-save on each blessed entry using the struct's OWN pathname.
          ;; load-shared-object keys EQUAL on the pathname and flips the flag in place, so
          ;; re-passing the struct's pathname touches the existing entry rather than
          ;; creating a second, still-saved one. A soname string would mismatch and
          ;; re-introduce the reopen crash.
          (dolist (obj sb-alien::*shared-objects*)
            (when (kli/runtime/relocation:directoryless-namestring-p
                   (sb-alien::shared-object-namestring obj))
              (sb-alien:load-shared-object (sb-alien::shared-object-pathname obj)
                                           :dont-save t)))

          ;; Register the availability hook. push (not append) so it runs before any other
          ;; init hook. The function is already compiled into selfLib, referenced
          ;; fully-qualified because preDump runs in CL-USER.
          (push #'kli/runtime/relocation:reopen-blessed-libs sb-ext:*init-hooks*)

          ;; Crash-prevention assertion: every blessed entry must now be dont-save-flagged,
          ;; else the saved image would crash reopening it on a relocated host. dont-save
          ;; filtering happens before purify, so the two do not interact.
          (let ((unflagged
                  (loop for obj in sb-alien::*shared-objects*
                        for namestring = (sb-alien::shared-object-namestring obj)
                        when (and (kli/runtime/relocation:directoryless-namestring-p namestring)
                                  (not (sb-alien::shared-object-dont-save obj)))
                          collect namestring)))
            (when unflagged
              (format *error-output*
                      "~&ERROR: blessed SONAMEs not dont-save-flagged: ~{~A~^, ~}~%"
                      unflagged)
              (finish-output *error-output*)
              (sb-posix:exit 1))
            (when (null kli/runtime/relocation:*blessed-sonames*)
              (format *error-output*
                      "~&ERROR: *blessed-sonames* empty -- no directory-less shared object found at preDump~%")
              (finish-output *error-output*)
              (sb-posix:exit 1))
            (format t "~&Relocatable-boot seam: blessed ~{~A~^, ~} (dont-save set)~%"
                    kli/runtime/relocation:*blessed-sonames*))
        '';
        # Pre-install the default profile into the image at preDump so boot reuses
        # the installed object graph instead of re-running the deterministic
        # manifest install on every start. Runs before blessedLibsSeam: should the
        # install touch a foreign library, the seam's blessed-set computation and
        # crash-prevention assertion still cover the new entry. The snapshot holds
        # only the pure installed graph -- no fds, threads, sockets, or db handles,
        # all of which are acquired at session bind or first use -- so it survives
        # save-lisp-and-die. Build-environment settings (empty) are captured; boot
        # rebinds the config dirs and reloads before reuse.
        bootSnapshotSeam = ''
          (kli/app:build-boot-snapshot)
        '';
        program = buildLisp.program {
          name = "kli";
          deps = [
            library
          ];
          main = "kli/app:dispatch-main";
          preDump = bootSnapshotSeam + blessedLibsSeam;
        };
        # Non-interactive entrypoint that reports the relocation state and exits.
        # Shares the shipped binary's preDump seam so the relocation check exercises
        # the same image wiring as the shipped binary.
        relocationProbe = buildLisp.program {
          name = "kli-reloc-probe";
          deps = [
            library
          ];
          main = "kli/app:relocation-probe-main";
          preDump = blessedLibsSeam;
        };
        # Release-signing tool, separate from the shipped kli: signs over the same
        # primitives the verifier uses, so verify accepts its output by construction.
        releaseSigner = buildLisp.program {
          name = "release-signer";
          deps = [
            library
          ];
          main = "kli/app:release-signer-main";
        };
        # Axis B: the qlot build's external pins vs the canonical cl-deps set.
        # GitHub systems must pin the exact rev cl-deps builds (exposed as
        # passthru.rev); quicklisp-dist systems must be present. let-over-lambda
        # is vendored in cl-deps, so its rev is the recorded provenance commit.
        qlfileGithubChecks =
          let
            pins = [
              { system = "3bmd"; rev = lisp."3bmd".passthru.rev; }
              { system = "colorize"; rev = lisp.colorize.passthru.rev; }
              { system = "cl-difflib"; rev = lisp.cl-difflib.passthru.rev; }
              { system = "let-over-lambda"; rev = lisp.let-over-lambda.passthru.rev; }
              { system = "sqlite"; rev = lisp.sqlite.passthru.rev; }
            ];
          in
          lib.concatMapStringsSep "\n" (p: ''
            got=$(awk -v sys="${p.system}" '$1=="github" && $2==sys { for (i=1;i<=NF;i++) if ($i==":ref") print $(i+1) }' ${src}/qlfile | tr -d '"')
            if [ "$got" != "${p.rev}" ]; then
              echo >&2
              echo "qlfile github pin for ${p.system} (:ref \"$got\") drifts from the canonical cl-deps rev (${p.rev})." >&2
              echo "Reconcile the qlfile :ref with cl-deps." >&2
              exit 1
            fi
          '') pins;

        qlfileQlChecks =
          let
            systems = [ "com.inuoe.jzon" "drakma" "ironclad" "cl-base64" "cl-ppcre" ];
          in
          lib.concatMapStringsSep "\n" (s: ''
            if [ "$(awk -v sys="${s}" '$1=="ql" && $2==sys {f=1} END {print f+0}' ${src}/qlfile)" != "1" ]; then
              echo >&2
              echo "qlfile is missing the quicklisp-dist pin for ${s}." >&2
              exit 1
            fi
          '') systems;

        # Fails the build when the checked-in qlot artifacts drift from their
        # single sources of truth: kli.asd's component list vs the serial
        # allSrcs, and the qlfile external pins vs the canonical cl-deps revs.
        driftGate = pkgs.runCommand "kli-drift-gate" { } ''
          if ! diff -u ${src}/kli.asd ${asd}; then
            echo >&2
            echo "kli.asd is out of sync with the source list in build.nix." >&2
            echo "Regenerate it from the build and commit the result." >&2
            exit 1
          fi

          if ! diff -u ${buildLisp.resourceSupportLisp} ${src}/vendor/buildlisp-resources.lisp; then
            echo >&2
            echo "vendor/buildlisp-resources.lisp drifts from the build system's resource support." >&2
            echo "Re-vendor it from the canonical and commit the result." >&2
            exit 1
          fi

          ${qlfileGithubChecks}
          ${qlfileQlChecks}

          touch $out
        '';

        # Source files allowed to take lattice-top authority (legitimate only
        # at the irreducible boot). Any other use site is an escalation surface
        # and fails the gate; the subject definitions and package forms are
        # excluded.
        topAuthorityAllowlist = [
          "src/extensions/app/main.lisp"
        ];
        topAuthorityGate = pkgs.runCommand "kli-top-authority-gate" { } ''
          cd ${src}
          found=$(grep -rlE 'with-system-authority|make-system-subject' src \
                    --include='*.lisp' \
                  | grep -vE 'extension-protocol/subject\.lisp$|/package\.lisp$' \
                  | LC_ALL=C sort -u)
          expected=$(printf '%s\n' ${lib.escapeShellArgs topAuthorityAllowlist} \
                     | LC_ALL=C sort -u)
          if [ "$found" != "$expected" ]; then
            echo >&2
            echo "Top-authority allowlist drift." >&2
            echo "Full system authority is permitted only at the irreducible" >&2
            echo "boot; every other principal must run under a bounded subject." >&2
            echo "Reconcile the source use sites with the allowlist." >&2
            echo >&2
            echo "Allowed top-bearing files:" >&2
            printf '  %s\n' $expected >&2
            echo "Found top-bearing files:" >&2
            printf '  %s\n' $found >&2
            exit 1
          fi
          touch $out
        '';

        # File-write, file-edit, and process-exec authority is granted only in
        # the mediation seam, before any runner. This gate fails the build if
        # any authorization call names those atoms outside that module -- the
        # sole authorizer -- so no other site can grant them out of band.
        soleAuthorizerGate = pkgs.runCommand "kli-sole-authorizer-gate" { } ''
          cd ${src}
          found=$(grep -rnE \
                    '(require-capability|check-capability)[^)]*:(file/write|file/edit|process/exec)' \
                    src --include='*.lisp' \
                  | grep -vE 'extension-protocol/' \
                  | LC_ALL=C sort -u) || true
          if [ -n "$found" ]; then
            echo >&2
            echo "Out-of-band tool-atom authorization detected." >&2
            echo "File-write, file-edit, and process-exec authority is granted" >&2
            echo "only in the mediation seam, as an argument-aware coordinate;" >&2
            echo "move the demand there or remove it." >&2
            echo >&2
            echo "Offending sites:" >&2
            printf '%s\n' "$found" >&2
            exit 1
          fi
          touch $out
        '';

        # Relocatable launcher with a literal /bin/sh shebang (no store path) that
        # stops SBCL runtime-option parsing with a `--`, so `--version`/`--help`
        # reach kli rather than the SBCL runtime, then execs the image beside it.
        kliLauncher = pkgs.writeText "kli.sh" ''
          #!/bin/sh
          _dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
          exec "$_dir/.kli-wrapped" -- "$@"
        '';

        # The released binary: the raw relocatable image plus the launcher.
        # install.sh sets KLI_DATA_DIR/LD_LIBRARY_PATH around this and execs
        # bin/kli directly.
        kliBinary = pkgs.runCommand "kli-binary" { } ''
          mkdir -p $out/bin
          cp ${program}/bin/.kli-wrapped $out/bin/.kli-wrapped
          cp ${kliLauncher} $out/bin/kli
          chmod +x $out/bin/kli
        '';

        # Two-process witness: `kli install` then a fresh `kli mcp-serve` that
        # rediscovers the placed extension and serves it. Loopback origin and
        # git pins are built in-derivation; no VM, no external network.
        installE2E = pkgs.runCommand "kli-install-e2e"
          { nativeBuildInputs = [ pkgs.python3 pkgs.git ]; } ''
          export HOME="$TMPDIR/home"
          mkdir -p "$HOME"
          python3 ${src}/t/e2e/drive.py \
            --kli ${kliBinary}/bin/kli \
            --fixture ${src}/t/e2e/fixture \
            --single ${src}/t/e2e/fixture-single/solo.lisp
          touch $out
        '';

        # The runtime resource roots under share/kli, each key copied verbatim
        # (so "kli/skills" lands at share/kli/kli/skills). KLI_DATA_DIR points
        # here; the doubled leading "kli/" is the resolver's literal key.
        shareResources = pkgs.runCommand "kli-share" { } (
          lib.concatStrings (lib.mapAttrsToList (key: dir: ''
            mkdir -p "$out/share/kli/${key}"
            cp -rT ${dir} "$out/share/kli/${key}"
          '') resourcesAttr));

        # Self-contained relocatable bundle: the image plus its full runtime
        # closure (dynamic loader, DT_NEEDED libs, and the dlopen'd blessed libs
        # the relocation probe reports) plus the resource roots. Boots on any
        # host at the build glibc or newer with /nix/store absent, launching the
        # image through the bundled loader. Supersedes the hand-listed soname
        # assembly that the release tarball job does today.
        kliRelocatable = buildLisp.mkRelocatableBundle {
          name = "kli";
          inherit program;
          share = shareResources;
          dataDir = "share/kli";
          dlopenProbe = { drv = relocationProbe; bin = "kli-reloc-probe"; };
        };

        swankAttrs = lib.optionalAttrs (replOrn != null) (
          let
            debugSwank = replOrn.define {
              protocol = replOrn.protocols.swank;
              mode = replOrn.Mode.Background;
              port = 14191;
              portEnvVar = "KLI_SWANK_PORT";
            };
            debug = buildLisp.program {
              name = "kli-debug";
              deps = [
                library
              ];
              main = "kli/app:dispatch-main";
              swank = debugSwank;
            };
          in
          {
            inherit debug debugSwank;
            programWithSwank = debug;
          });
      in
      {
        inherit src program library relocationProbe releaseSigner bootSnapshotSeam blessedLibsSeam asd driftGate
          topAuthorityGate soleAuthorizerGate kliBinary installE2E shareResources kliRelocatable;
        modelProviderSources = ./src/extensions/model/providers;
        # The installer, served verbatim at the kli site root (single source of truth).
        installScript = ./install.sh;
        # Kernel substrate (package + kernel, no boot protocol), pulled by path so
        # a consumer can compile the real kernel into its image and build on it.
        kernelSources = [ ./src/package.lisp ./src/kernel.lisp ];
        # Kernel + extension protocol + the tui/core render and behavior seams,
        # in the canonical load order, so a consumer can dispatch through the real
        # render-transcript-event generic and hot-patch a real pandoric behavior
        # cell. Stops at tui/core: no style, object, or event extensions.
        tuiCoreSources =
          kernelSrcs ++ textSrcs ++ extensionProtocolSrcs ++ tuiCoreSrcs;
      } // swankAttrs;
  }).overrideAttrs (old: {
    # The image refuses to build unless the source-level invariants hold: the
    # qlot/resource drift gate, the lattice-top allowlist, and the
    # sole-authorizer invariant. Failing any one fails every downstream build
    # (binary, relocatable, tests).
    nativeBuildInputs =
      old.nativeBuildInputs
      ++ [ old.passthru.driftGate old.passthru.topAuthorityGate old.passthru.soleAuthorizerGate ];
  });
in
library
