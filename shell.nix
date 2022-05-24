with (import <nixpkgs> {});
mkShell {
  name = "tagger-dev";
  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-language-server
    haskellPackages.haskell-language-server
    haskellPackages.ormolu
    nodePackages.npm
    postgresql
    stack
    watchexec
    zlib
  ];
  shellHook = builtins.readFile ./bin/db/setup;
}
