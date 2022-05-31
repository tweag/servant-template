let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

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
pkgs.mkShell {
  name = "tagger-dev";
  NIX_PATH = "nixpkgs=" + pkgs.path;
  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-language-server
    haskellPackages.haskell-language-server
    haskellPackages.ormolu
    jq
    nodejs
    postgresql
    stack-nix
    toml2json
    watchexec
    zlib
  ];
}
