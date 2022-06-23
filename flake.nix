{
  description = "A servant template";

  nixConfig = {
    extra-substituters = [
      "https://tweag-servant-template.cachix.org"
    ];
    extra-trusted-public-keys = [
      "tweag-servant-template.cachix.org-1:FXFYXlGuectsxqO48eP+uSqPHw2lJDeSPY63JWU713U="
    ];
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
        devShell =
          pkgs.mkShell {
            buildInputs = with pkgs; [
              elmPackages.elm
              elmPackages.elm-live
              elmPackages.elm-language-server
              haskellPackages.haskell-language-server
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
