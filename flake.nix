{
  description = "kli - a radically extensible coding agent";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/88d3861acdd3d2f0e361767018218e51810df8a1";
    cl-deps.url = "github:kleisli-io/cl-deps";
    cl-deps.inputs.nixpkgs.follows = "nixpkgs";
    # Pinned to a release built against this nixpkgs; newer nix-unit requires a
    # nixComponents version absent from the pinned nixpkgs.
    nix-unit.url = "github:nix-community/nix-unit/1c9ab50554eed0b768f9e5b6f646d63c9673f0f7";
    nix-unit.inputs = {
      nixpkgs.follows = "nixpkgs";
      nix-github-actions.follows = "";
      treefmt-nix.follows = "";
    };
  };

  outputs = { self, nixpkgs, cl-deps, nix-unit, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      buildFor = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (cl-deps.lib.${system}) buildLisp lisp;
        in
        import ./build.nix {
          inherit pkgs buildLisp lisp;
          lib = pkgs.lib;
          src = ./.;
        };
    in
    {
      overlays.default = final: prev: {
        kli = (buildFor prev.system).program;
      };

      lib = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (cl-deps.lib.${system}) buildLisp;
        in
        {
          mkConfiguredKli = import ./config-lib/mk-configured-kli.nix {
            inherit pkgs buildLisp;
            lib = pkgs.lib;
            kliLibrary = buildFor system;
          };
          knownExtensions = { };
        });

      homeManagerModules.default = import ./config-lib/hm-module.nix {
        mkConfiguredKliFor = system: self.lib.${system}.mkConfiguredKli;
      };

      nixosModules.default = import ./config-lib/nixos-module.nix {
        mkConfiguredKliFor = system: self.lib.${system}.mkConfiguredKli;
      };

      # Pure { expr; expected; } suite over the configured-image producer's
      # rendering helpers and option declarations; consumed by `nix-unit
      # --flake .#tests` in the per-system check below.
      tests = import ./config-lib/tests.nix { lib = nixpkgs.lib; };

      packages = forAllSystems (system:
        let library = buildFor system; in {
          default = library.program;
          kli = library.program;
          kli-binary = library.kliBinary;
          kli-share = library.shareResources;
          kli-relocatable = library.kliRelocatable;
          reloc-probe = library.relocationProbe;
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/kli";
        };
      });

      checks = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          library = buildFor system;
        in
        {
          inherit library;
          tests = library.tests;
          drift = library.driftGate;
          top-authority = library.topAuthorityGate;
          sole-authorizer = library.soleAuthorizerGate;
          nix-unit = pkgs.runCommand "kli-config-lib-nix-unit"
            { nativeBuildInputs = [ nix-unit.packages.${system}.default ]; } ''
            export HOME="$(realpath .)"
            nix-unit --eval-store "$HOME" \
              --extra-experimental-features flakes \
              --override-input nixpkgs ${nixpkgs} \
              --override-input cl-deps ${cl-deps} \
              --override-input nix-unit ${nix-unit} \
              --flake ${self}#tests
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in {
          default = pkgs.mkShell {
            packages = [ pkgs.sbcl ];
          };
        });
    };
}
