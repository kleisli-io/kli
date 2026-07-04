# Pure expr/expected assertions over the producer's rendering helpers and
# option declarations. Wired into the flake's nix-unit check; no derivations
# are forced, so the attrset evaluates directly under nix-unit.
{ lib }:

let
  inherit (import ./lib.nix { inherit lib; }) toSexp settingsToSexp bwrapArgs;
  options = import ./options.nix { inherit lib; };

  staticBwrapBase = [
    "--ro-bind"
    "/"
    "/"
    "--dev"
    "/dev"
    "--proc"
    "/proc"
    "--tmpfs"
    "/tmp"
    "--unshare-pid"
    "--unshare-ipc"
  ];

  # nestedUserns swaps the fresh-proc + pid-unshare for a bind of the outer
  # container's masked /proc, with no --unshare-pid.
  nestedBwrapBase = [
    "--ro-bind"
    "/"
    "/"
    "--dev"
    "/dev"
    "--bind"
    "/proc"
    "/proc"
    "--tmpfs"
    "/tmp"
    "--unshare-ipc"
  ];
in
{
  testSexpString = { expr = toSexp "dark"; expected = "\"dark\""; };
  testSexpStringQuoteEscaped = { expr = toSexp "a\"b"; expected = "\"a\\\"b\""; };
  testSexpStringBackslashEscaped = { expr = toSexp "a\\b"; expected = "\"a\\\\b\""; };
  testSexpBoolTrue = { expr = toSexp true; expected = "t"; };
  testSexpBoolFalse = { expr = toSexp false; expected = "nil"; };
  testSexpInt = { expr = toSexp 4096; expected = "4096"; };

  # The #(...) invariant: arrays must render as vectors, not lists — the
  # parsers (parse-id-list, capabilities-subject) reject lists.
  testSexpListIsVector = { expr = toSexp [ "a" "b" ]; expected = "#(\"a\" \"b\")"; };
  testSexpEmptyListIsVector = { expr = toSexp [ ]; expected = "#()"; };
  testSexpCapabilitiesAreVector = {
    expr = toSexp { capabilities = [ "file/read" "process/exec" ]; };
    expected = "(:capabilities #(\"file/read\" \"process/exec\"))";
  };
  testSexpAttrs = { expr = toSexp { theme = "dark"; }; expected = "(:theme \"dark\")"; };
  testSexpAttrsSortedKeys = {
    expr = toSexp { b = 2; a = 1; };
    expected = "(:a 1 :b 2)";
  };

  # Bar-escaped keys so the reader preserves camelCase.
  testSettingsSexpKeyBarEscaped = {
    expr = settingsToSexp { defaultProvider = "synthetic"; };
    expected = "(:|defaultProvider| \"synthetic\")";
  };
  testSettingsSexpNestedKeysBarEscaped = {
    expr = settingsToSexp { keyBindings = { acceptEdit = "ctrl-y"; }; };
    expected = "(:|keyBindings| (:|acceptEdit| \"ctrl-y\"))";
  };
  testSettingsSexpListValuesEscapeThrough = {
    expr = settingsToSexp { allowedTools = [ "bash" "edit" ]; };
    expected = "(:|allowedTools| #(\"bash\" \"edit\"))";
  };

  testBwrapEmptySpecDefaults = {
    expr = bwrapArgs { };
    expected = staticBwrapBase;
  };
  testBwrapNetOnOmitsUnshareNet = {
    expr = builtins.elem "--unshare-net" (bwrapArgs { network = true; });
    expected = false;
  };
  testBwrapNetOffUnsharesNet = {
    expr = bwrapArgs { network = false; };
    expected = staticBwrapBase ++ [ "--unshare-net" ];
  };
  testBwrapWritablePaths = {
    expr = bwrapArgs { writablePaths = [ "/var/tmp/kli" "/srv/data" ]; };
    expected = staticBwrapBase ++ [
      "--bind"
      "/var/tmp/kli"
      "/var/tmp/kli"
      "--bind"
      "/srv/data"
      "/srv/data"
    ];
  };
  testBwrapDenyEnvUnsetsNames = {
    expr = bwrapArgs { denyEnv = [ "GH_TOKEN" "AWS_SECRET_ACCESS_KEY" ]; };
    expected = staticBwrapBase ++ [
      "--unsetenv"
      "GH_TOKEN"
      "--unsetenv"
      "AWS_SECRET_ACCESS_KEY"
    ];
  };
  testBwrapDenyEnvEmptyOmitsUnsetenv = {
    expr = builtins.elem "--unsetenv" (bwrapArgs { denyEnv = [ ]; });
    expected = false;
  };
  testBwrapDenyEnvAbsentOmitsUnsetenv = {
    expr = builtins.elem "--unsetenv" (bwrapArgs { });
    expected = false;
  };

  testBwrapNestedUsernsBindsProc = {
    expr = bwrapArgs { nestedUserns = true; };
    expected = nestedBwrapBase;
  };
  testBwrapNestedUsernsDropsUnsharePid = {
    expr = builtins.elem "--unshare-pid" (bwrapArgs { nestedUserns = true; });
    expected = false;
  };
  testBwrapNestedUsernsMountsNoFreshProc = {
    expr = builtins.elem "--proc" (bwrapArgs { nestedUserns = true; });
    expected = false;
  };
  testBwrapDefaultKeepsUnsharePid = {
    expr = builtins.elem "--unshare-pid" (bwrapArgs { });
    expected = true;
  };
  testBwrapNestedUsernsComposesWithNetOff = {
    expr = bwrapArgs { nestedUserns = true; network = false; };
    expected = nestedBwrapBase ++ [ "--unshare-net" ];
  };

  testOptionSandboxDefaultNull = { expr = options.sandbox.default; expected = null; };
  testOptionSettingsDefaultEmpty = { expr = options.settings.default; expected = { }; };
  testOptionRegistriesDefaultEmpty = { expr = options.registries.default; expected = [ ]; };
  testOptionExtensionsDefaultEmpty = { expr = options.extensions.default; expected = [ ]; };
  testOptionDataDirDefaultNull = { expr = options.dataDir.default; expected = null; };
}
