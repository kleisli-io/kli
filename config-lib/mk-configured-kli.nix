# Producer for a configured kli image. Extensions are composed as build
# dependencies, so their native libraries and resource roots flow through
# buildLisp; settings and registries are baked into a resource; and a fresh
# image is dumped whose boot shim declares the compiled-in extension set.
{ pkgs, lib, buildLisp, kliLibrary }:

{ extensions ? [ ]
, settings ? { }
, registries ? [ ]
, blessedNativeLibs ? [ ]
, dataDir ? null
, sandbox ? null
}:

let
  inherit (lib) concatMapStringsSep concatStringsSep optionalString;
  inherit (import ./lib.nix { inherit lib; }) toSexp settingsToSexp bwrapArgs;

  extNames = map (e: e.passthru.name) extensions;
  manifestSymbols = map (e: e.passthru.manifestSymbol) extensions;
  extensionSettings =
    lib.foldl' lib.recursiveUpdate { }
      (map (e: e.passthru.kliSettings or { }) extensions);
  mergedSettings = lib.recursiveUpdate extensionSettings settings;

  # Every composed root must own a distinct resolver key; a clash would
  # silently shadow one root when the image self-registers them at load.
  allResourceKeys =
    builtins.attrNames (kliLibrary.passthru.lispResources or { })
    ++ lib.concatMap (e: builtins.attrNames (e.passthru.lispResources or { })) extensions
    ++ [ "kli/nix-config" ];
  keyCounts = lib.foldl' (acc: k: acc // { ${k} = (acc.${k} or 0) + 1; }) { } allResourceKeys;
  duplicateKeys = lib.filter (k: keyCounts.${k} > 1) (lib.attrNames keyCounts);

  # The boot shim compiles last, after the core and every extension FASL, so
  # its setf overrides the core's nil default. Manifest references are literal
  # package-qualified symbols: the reader hard-errors at compile time if an
  # extension fails to export one.
  manifestList =
    if manifestSymbols == [ ] then "nil"
    else "(list " + concatMapStringsSep " " (s: "'" + s) manifestSymbols + ")";
  baselineList =
    if extNames == [ ] then "nil"
    else "(list " + concatMapStringsSep " " (s: "\"" + s + "\"") extNames + ")";
  shim = pkgs.writeText "kli-nix-config.lisp" ''
    (in-package #:cl-user)
    (setf kli/profiles:*nix-declared-extension-manifests* ${manifestList})
    (setf kli/profiles:*nix-declared-baseline-ids* ${baselineList})
  '';

  nixConfigDir = pkgs.runCommand "kli-nix-config" { } ''
    mkdir -p "$out"
    cp ${pkgs.writeText "config.sexp" ''
      (:settings ${settingsToSexp mergedSettings}
       :registries ${toSexp registries}
       :declared-ids ${toSexp extNames}
       :manifest-symbols ${toSexp manifestSymbols}
       :baseline-ids ${toSexp extNames}
       :data-dir ${if dataDir == null then "nil" else toSexp dataDir})
    ''} "$out/config.sexp"
  '';

  configuredLibrary = buildLisp.library {
    name = "kli-configured";
    deps = [ kliLibrary ] ++ extensions;
    srcs = [ shim ];
    resources = { "kli/nix-config" = nixConfigDir; };
    cLibraries = blessedNativeLibs;
  };

  # Baked into the dumped image: fail the build if the shim did not land the
  # declared manifest in the live variable.
  manifestAssertion = concatMapStringsSep "\n" (sym: ''
    (unless (member '${sym} kli/profiles:*nix-declared-extension-manifests*)
      (format *error-output* "~&ERROR: nix-declared manifest ${sym} missing from the dumped image~%")
      (finish-output *error-output*)
      (sb-posix:exit 1))
  '') manifestSymbols;

  program = buildLisp.program {
    name = "kli";
    deps = [ configuredLibrary ];
    cLibraries = blessedNativeLibs;
    main = "kli/app:dispatch-main";
    preDump = optionalString (manifestSymbols != [ ]) (manifestAssertion + "\n")
      + kliLibrary.passthru.bootSnapshotSeam
      + kliLibrary.passthru.blessedLibsSeam;
  };

  # Symbol-name component, uppercased: SBCL stores it verbatim in the dumped
  # image, so finding it proves the extension was actually compiled in. A
  # stale image built against the pre-merge core lacks the symbol entirely.
  symName = sym: lib.toUpper (lib.last (lib.splitString ":" sym));
  mkManifestGate = imgDrv: pkgs.runCommand "kli-manifest-gate" { } (''
    img="${imgDrv}/bin/.kli-wrapped"
    if [ ! -e "$img" ]; then
      echo "no dumped image at $img" >&2
      exit 1
    fi
  '' + concatMapStringsSep "\n" (sym: ''
    if ! grep -aqF '${symName sym}' "$img"; then
      echo "manifest symbol ${symName sym} absent from dumped image" >&2
      exit 1
    fi
  '') manifestSymbols + ''

    touch "$out"
  '');

  # External bwrap wrapper: confinement the binary cannot switch off. denyRead
  # masks are appended last (after --ro-bind / /) so they win, and probed at
  # run time since a path's file-vs-directory kind is not known at build time.
  sandboxed = spec: inner:
    let
      denyReadLines = concatMapStringsSep "\n" (p: ''
        if [ -d "${p}" ]; then
          args+=(--tmpfs "${p}")
        elif [ -e "${p}" ]; then
          args+=(--bind /dev/null "${p}")
        fi
      '') (spec.denyRead or [ ]);
    in
    pkgs.writeShellScriptBin "kli" ''
      set -eu
      args=(
        ${lib.escapeShellArgs (bwrapArgs spec)}
        --bind "$PWD" "$PWD"
        --chdir "$PWD"
      )
      ${denyReadLines}
      exec ${pkgs.bubblewrap}/bin/bwrap "''${args[@]}" -- ${inner}/bin/kli "$@"
    '';

  annotated = program.overrideAttrs (old: {
    passthru = old.passthru // {
      inherit configuredLibrary mkManifestGate;
      manifestGate = mkManifestGate program;
    };
  });

  # Completion the image emits itself, from the plain (non-bwrapped) image;
  # `completion bash` is boot-free. The smoke gate fails the build on a broken script.
  completion = pkgs.runCommand "kli-completion" { } ''
    dir="$out/share/bash-completion/completions"
    mkdir -p "$dir"
    HOME="$TMPDIR" ${program}/bin/kli completion bash > "$dir/kli"
    if ! test -s "$dir/kli"; then
      echo "kli completion bash produced no output" >&2
      exit 1
    fi
    if ! grep -q 'complete -F _kli kli' "$dir/kli"; then
      echo "kli completion script missing the _kli registration" >&2
      exit 1
    fi
  '';

  # Add completion to an image arm, preserving its wrapper (bin/kli resolves
  # .kli-wrapped via dirname-$0, symlinkJoin-safe) and its passthru.
  withCompletion = drv:
    (pkgs.symlinkJoin {
      name = "kli";
      paths = [ drv completion ];
    }).overrideAttrs (_: { passthru = drv.passthru; });

  result =
    if sandbox == null then withCompletion annotated
    else withCompletion ((sandboxed sandbox program).overrideAttrs (old: {
      passthru = (old.passthru or { }) // annotated.passthru // { unsandboxed = annotated; };
    }));
in
if duplicateKeys == [ ]
then result
else throw "kli config: resource key collision across composed roots: ${concatStringsSep ", " duplicateKeys}"
