{ pkgs, lib, buildLisp, lisp, replOrn, ... }:

import ./build.nix {
  inherit pkgs lib buildLisp lisp replOrn;
  src = ./.;
}
