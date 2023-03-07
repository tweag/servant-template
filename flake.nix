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
        pkgs = import nixpkgs {
          inherit system;

          config.allowBroken = true;
        };
        stack-nix = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --nix \
                --no-nix-pure \
                --nix-shell-file=nix/stack-integration.nix \
              "
          '';
        };
      in
      {
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
        devShells.default = pkgs.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          buildInputs = with pkgs; [
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-live
            elmPackages.elm-language-server
            haskellPackages.haskell-language-server
            haskellPackages.hspec-discover
            haskellPackages.ormolu
            jq
            nodejs
            postgresql
            stack-nix
            toml2json
            watchexec
            zlib
          ];
        };
      });
}
