with (import <nixpkgs> {});
mkShell {
  name = "tagger-dev";
  buildInputs = with pkgs; [
    haskellPackages.haskell-language-server
    haskellPackages.ormolu
    elmPackages.elm
    elmPackages.elm-language-server
    nodePackages.npm
    stack
    postgresql
    zlib
  ];
  shellHook = builtins.readFile ./bin/setupdb;
}
