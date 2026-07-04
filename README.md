<div align="center">
<img src="assets/kli-logo.png" alt="kli" width="200" />
<br />
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/kli-wordmark-dark.svg" />
  <img src="assets/kli-wordmark-light.svg" alt="kli" height="60" />
</picture>

<br />

<strong>A radically extensible coding agent for the modern Lisp hacker.</strong>

<br />

</div>

```sh
curl -fsSL https://kli.kleisli.io | sh
```

kli is a coding agent that runs inside its own live SBCL image. The boot kernel is
small. It knows only how to install, switch, and roll back protocols, and nothing
about what an extension is. The extension system itself, the manifests, the
contribution kinds, the capabilities and the tools, is a protocol installed on that
kernel through those same verbs. Every model provider, every tool, the agent loop,
and the whole TUI is an extension on a protocol. Every contribution carries a
retractor, so retraction drains exactly what it added and nothing else, and you can
install, retract, swap, or hot patch any of it from the REPL without dropping the
session. Extensions all the way down. If that makes you want to open
`src/kernel.lisp`, kli is for you.

## How it works

- **Specialize the running program.** A `:method` contribution adds a method to any
  generic in the live image, and retraction is `remove-method`. You change the
  program's own behavior and roll it back.
- **Rewrite without restarting, keep the state.** Behavior lives in CLOS cells over
  pandoric closures. A hot patch swaps the code and keeps the closed-over state, so
  the editor keeps its buffer and the transcript its scrollback. It runs behind a
  condition-system fault barrier, so a hot-patched function that throws degrades
  instead of killing the session.
- **Switch the whole world, safely.** A protocol switch validates, smoke-tests,
  swaps, and rolls back on any error.

Two dispatch axes are split on purpose. CLOS specialization handles image-global,
per-class behavior, and capability providers, which are closure tables, handle
per-protocol-instance variation. The agent loop itself is built this way, with the
message handler, the skill expander, the retry count, and the compaction threshold
all pandoric closures you can open and hot patch while a turn is in flight.

## Installing

Install the prebuilt binary, use Nix, or build from source.

```sh
curl -fsSL https://kli.kleisli.io | sh
```

It installs under `~/.local`, overridable with `KLI_INSTALL_DIR` and pinnable with
`KLI_VERSION`, and puts a `kli` wrapper on your `PATH`. Then run `kli`.

### With Nix

Run it without installing, or add it to your profile.

```sh
nix run github:kleisli-io/kli
nix profile install github:kleisli-io/kli
```

The flake ships `overlays.default` and `packages.<system>.default`, plus
`homeManagerModules.default` and `nixosModules.default`, both exposing
`programs.kli`. Add the NixOS module to a flake.

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.kli.url = "github:kleisli-io/kli";

  outputs = { nixpkgs, kli, ... }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        kli.nixosModules.default
        ({ pkgs, ... }: {
          programs.kli = {
            enable = true;
            extensions = [ ];
            blessedNativeLibs = [ pkgs.sqlite ];
            settings.theme = "dark";
          };
        })
      ];
    };
  };
}
```

Home Manager is identical through `kli.homeManagerModules.default`, configuring the
same `programs.kli`.

For a project-local image without touching your system, configure one in `shell.nix`.

```nix
let
  kli = builtins.getFlake "github:kleisli-io/kli";
  system = builtins.currentSystem;
  pkgs = import <nixpkgs> { inherit system; };
in
pkgs.mkShell {
  packages = [
    (kli.lib.${system}.mkConfiguredKli {
      extensions = [ ];
      blessedNativeLibs = [ pkgs.sqlite ];
      dataDir = "$PWD/.kli-data";   # null resolves the data dir from the environment
      settings.theme = "dark";
    })
  ];
}
```

Each extension is a derivation exposing `passthru.name` and
`passthru.manifestSymbol`, and baked settings sit below your own as defaults.

### From source

SBCL and [qlot](https://github.com/fukamachi/qlot), with external systems pinned in
`qlfile`.

```sh
qlot install
qlot exec sbcl --script build.lisp
./bin/kli
```

## What's in the box

Model providers (OpenAI, Codex, OpenAI-compatible endpoints) are themselves
extensions on a provider registry, wired with OAuth and credential auth. Tools cover
bash, eval, and a file surface of read, write, edit, find, and search over hashline
anchors. Reads emit line and content-hash anchors, edits are anchor-validated patches
that reject stale anchors instead of clobbering drifted files, and search results arm
the same anchor cache so a match is editable without a separate read. The TUI,
sessions with compacting logs, slash commands, themes, keybindings, and status
widgets are all extensions too.

## Security

kli enforces authority, which capabilities an agent may exercise, not containment. By
default nothing kli runs is isolated from the host, so a granted shell command, file
write, or `eval` acts with the full privileges of the kli process. For autonomous or
untrusted use, run kli inside your own confinement, bwrap, a container, or a VM.

Run `kli --print-authority` to print the exact capabilities a session will hold, so
you can size that confinement. The configured-image producer also takes a `sandbox`
option that wraps the entrypoint in bwrap. See [Security model and
sandboxing](https://docs.kleisli.io/kli/concepts/security-model-and-sandboxing) for
the boundary in full and bwrap, container, and systemd recipes.

## Tests

```sh
nix flake check
```

FiveAM suites under `t/`.
