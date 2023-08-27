{
  description = "A servant template";

  inputs = {
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "servant-template";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: rec {
            openapi3 =
              pkgs.lib.pipe super.openapi3
                [
                  pkgs.haskell.lib.unmarkBroken
                  pkgs.haskell.lib.dontCheck
                ];
          };
        };
      in
      {
        defaultPackage = self.packages.${system}.${packageName};
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            servant-auth-server =
              pkgs.lib.pipe haskellPackages.servant-auth-server
                [
                  pkgs.haskell.lib.unmarkBroken
                  pkgs.haskell.lib.dontCheck
                ];

            tomland =
              pkgs.lib.pipe haskellPackages.tomland
                [
                  pkgs.haskell.lib.doJailbreak
                  pkgs.haskell.lib.dontCheck
                ];
          };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };
        };

        devShells.default = pkgs.mkShell rec {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          inputsFrom = builtins.attrValues self.packages.${system};
          nativeBuildInputs = with pkgs; [
          ];
          buildInputs = with pkgs; [
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-live
            elmPackages.elm-language-server
            haskellPackages.haskell-language-server
            haskellPackages.ormolu
            haskellPackages.cabal-install
            haskellPackages.ghcid
            hpack
            jq
            nodejs
            postgresql
            toml2json
            watchexec
            zlib
          ];

          # Ensure that libz.so and other libraries are available to TH
          # splices, cabal repl, etc.
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
        };
      });
}
