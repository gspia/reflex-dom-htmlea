{ stdenv
, makeWrapper
, haskell
, haskellPackages
, which
}:

stdenv.mkDerivation {

    name = "nix-tags-haskell";

    src = ./.;

    buildInputs = [ makeWrapper ];

    hasktags = haskell.packages.ghc802.hasktags;
    haskdogs = haskellPackages.haskdogs;
    inherit which;

    builder = ./builder.sh;

}
