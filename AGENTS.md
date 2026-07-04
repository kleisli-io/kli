# kli

Kernel of live objects + protocol primitive (`install` / `switch` / `rollback`). Lives in `src/kernel.lisp` + `src/protocols/boot.lisp`. **Everything else is an extension.**

## Cross-extension imports

Every package: `:import-from #:kli` (kernel), `:import-from #:kli/ext` (extension vocabulary), third-party libs. From a sibling extension, import only **interface** — exported classes and exported generic functions written for the purpose. Methods on an imported generic are recorded in the importing extension's manifest as a `:method` contribution (see `defcontribution-kind :method` in `extension-protocol/syntax.lisp`); retraction calls `remove-method`. Importing a sibling's **implementation** (internal functions, internal state, unexported symbols) remains forbidden — the package system enforces the interface/implementation split. Foreign symbols outside this discipline (e.g. manifest factories in a baseline list) appear quoted, never imported.

Capability/provider routing (`find-capability-provider` + `provider-call`) is reserved for **per-protocol-instance variation** — auth backends, model runtimes, registries, app singletons, and other behavior whose provider is chosen by which protocol is calling. CLOS specialization is for image-global, per-class dispatch. Accessor-shaped calls on values (e.g. `:message-role msg`) belong as exported generics, not as provider entries.

## Layout

`src/extensions/<group>/<extension>/` — one extension per leaf. Group directories never also contain `extension.lisp`. A leaf typically holds `package.lisp`, then any of `model.lisp` / `runtime.lisp` / `command.lisp`, then `extension.lisp` (the `defextension` form).

## Extensions as values

`defextension foo (:requires ...) (:provides ...)` produces `*foo-extension-manifest*` — a `defparameter` holding a factory thunk. The single protocol-mutating step is `install-manifest manifest protocol context`; undo with `deactivate-extension`. Same shape everywhere: REPL, production boot, agent, test, snapshot.

## Per-protocol storage

`extension-protocol` carries one typed slot per closed contribution kind (`extensions`, `installed-contributions`, `capabilities`, `provider-contracts`, `tools`, plus the method-installations refcount table) — these are the protocol's own schema for the manifest taxonomy, not extension-claimed state. Extension-claimed per-protocol state goes through `protocol-storage` / `(setf protocol-storage)` / `ensure-protocol-storage`, keyed by extension-owned symbols. **Image-global `defvar` for protocol state is a bug** — a second protocol overwrites it. Set-once REPL-inspection defvars (e.g. `kli/app:*current-context*`) are permitted.

## Reversibility

Every contribution kind has a retractor. `:effect (name installer retractor)` is the general case: the installer's return value is stored in `contribution-state`; the retractor reads it back. Deactivating an extension drains every contribution it installed. The provider extensions (`src/extensions/model/providers/*/extension.lisp`) are the canonical `:effect` consumers; extension bundling lives in `src/extensions/profiles/groups.lisp`.

## Capability gates

`*call-subject*` (default `system-subject`, passes everything) carries the caller's capability set. Dangerous ops call `(require-capability :cap)`; implications declared in `*capability-implications*` (e.g. `:image/recode` ⇒ `:manifest/install :manifest/retract`). Tests rebind: `(let ((ext:*call-subject* (ext:make-subject :capabilities '(...)))) ...)`.

Gated sites: `invoke-tool`, `activate`/`deactivate`/`recode-extension`, `control-*`, agent tool dispatch, auth `register`/`resolve`/`inspect`.

## Bootstrap

`make-kernel-host` → `install-protocol (make-extension-protocol)` + `switch-protocol` → `install-manifest *app-baseline-boot-extension-manifest*`. TUI adds `install-manifest *tui-app-boot-extension-manifest*`. Entry points `kli/app:main` / `:run-tui-main` / `:dispatch-main` (the program toplevel: argv dispatch over version/update/help, default to the TUI) are bound by string from `default.nix:program`.

## Verification

`nix flake check` runs the FiveAM suites under `t/`. REPL-driven: compile, load, eval.
