# Pure rendering helpers for the configured-image producer: the config
# s-expression serialiser and the bwrap argument vector. Kept apart from the
# producer so the same definitions back both the build and its unit tests.
{ lib }:

let
  inherit (lib) concatMapStringsSep concatStringsSep mapAttrsToList;

  # `|...|` so the reader keeps the key's case; bare :defaultProvider upcases.
  barKey = k: "|" + lib.escape [ "\\" "|" ] k + "|";

  mkSexp = renderKey: v:
    if builtins.isString v then "\"" + lib.escape [ "\\" "\"" ] v + "\""
    else if builtins.isBool v then (if v then "t" else "nil")
    else if builtins.isInt v then toString v
    # #(...) so baked arrays read back as vectors; the parsers reject lists.
    else if builtins.isList v then "#(" + concatMapStringsSep " " (mkSexp renderKey) v + ")"
    else if builtins.isAttrs v then
      "(" + concatStringsSep " " (mapAttrsToList (k: val: ":${renderKey k} ${mkSexp renderKey val}") v) + ")"
    else throw "kli config: cannot serialise value of type ${builtins.typeOf v}";

  # Bare keys (registries etc. read via upcasing keyword literals).
  toSexp = mkSexp (k: k);

  # Case-preserved keys for the settings sub-form (camelCase lookups).
  settingsToSexp = mkSexp barKey;

  # bwrapArgs : sandbox-spec -> [String]
  #
  # The build-static portion of the bwrap argv: fixed mounts and namespaces,
  # the network toggle, the extra writable binds, and the denyEnv unsets.
  # Runtime-dynamic flags (the working-directory bind/chdir and denyRead's
  # file-vs-directory probe) are appended by the wrapper script, where $PWD and
  # the on-disk path kind are known.
  #
  # nestedUserns switches the /proc strategy: inside an outer user namespace
  # with a masked /proc, the kernel refuses a fresh procfs mount, so bind the
  # existing masked /proc and defer PID isolation to the outer container (drop
  # --unshare-pid). The default path is unchanged: fresh /proc + new PID ns.
  bwrapArgs = spec:
    let
      nested = spec.nestedUserns or false;
      procMount = if nested then [ "--bind" "/proc" "/proc" ] else [ "--proc" "/proc" ];
    in
    [
      "--ro-bind"
      "/"
      "/"
      "--dev"
      "/dev"
    ]
    ++ procMount
    ++ [ "--tmpfs" "/tmp" ]
    ++ lib.optional (!nested) "--unshare-pid"
    ++ [ "--unshare-ipc" ]
    ++ lib.optional (!(spec.network or true)) "--unshare-net"
    ++ lib.concatMap (p: [ "--bind" p p ]) (spec.writablePaths or [ ])
    ++ lib.concatMap (n: [ "--unsetenv" n ]) (spec.denyEnv or [ ]);
in
{
  inherit toSexp settingsToSexp bwrapArgs;
}
