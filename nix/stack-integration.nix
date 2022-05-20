{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "tagger-api";
  buildInputs = [ glpk pcre ];
}
