# NixOS surface for programs.kli.
{ mkConfiguredKliFor }:

import ./module-core.nix {
  inherit mkConfiguredKliFor;
  install = pkg: { environment.systemPackages = [ pkg ]; };
}
