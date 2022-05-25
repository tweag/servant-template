with (import <nixpkgs> {});
let
  stack-wrapped = pkgs.symlinkJoin {
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
mkShell {
  name = "tagger-dev";
  NIX_PATH = "nixpkgs=" + pkgs.path;
  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-language-server
    haskellPackages.haskell-language-server
    haskellPackages.ormolu
    nodePackages.npm
    postgresql
    stack-wrapped
    watchexec
    zlib
  ];
  shellHook = builtins.readFile ./bin/db/setup;
}
