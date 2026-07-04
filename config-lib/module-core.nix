# Shared programs.kli module: declares the options, builds a configured image
# from them, and installs it through the supplied target fragment.
{ mkConfiguredKliFor, install }:

{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkEnableOption mkIf mkMerge types;
  cfg = config.programs.kli;
  sharedOptions = import ./options.nix { inherit lib; };
  mkConfiguredKli = mkConfiguredKliFor pkgs.stdenv.hostPlatform.system;
in
{
  options.programs.kli = sharedOptions // {
    enable = mkEnableOption "the kli agent framework";

    package = mkOption {
      type = types.package;
      default = mkConfiguredKli {
        inherit (cfg) extensions settings registries blessedNativeLibs dataDir sandbox;
      };
      defaultText = lib.literalExpression "an image configured from the options above";
      description = "Package to install. Defaults to an image built from the options above; set to override.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [{
        assertion = !(lib.any (e: (e.passthru.name or null) == "kli") cfg.extensions);
        message = "programs.kli.extensions must not include the core kli library.";
      }];
    }
    (install cfg.package)
  ]);
}
