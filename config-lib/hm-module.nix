# Home Manager surface for programs.kli.
{ mkConfiguredKliFor }:

import ./module-core.nix {
  inherit mkConfiguredKliFor;
  install = pkg: { home.packages = [ pkg ]; };
}
