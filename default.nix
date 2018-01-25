{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    /* common = ./common; */
    /* backend = ./backend; */
    /* frontend = ./frontend; */
    reflex-dom-htmlea  = ./liblea;
    example1 = ./example1;
    exampleTbl = ./exampleTbl;
  };

  /* android.exampleTbl = { */
  /*   executableName = "exampleTbl"; */
  /*   applicationId = "org.example.exampleTbl"; */
  /*   displayName = "Example Tables App"; */
  /* }; */
  /* ios.keyboard = { */
  /*   executableName = "keyboard"; */
  /*   bundleIdentifier = "org.example.keyboard"; */
  /*   bundleName = "Example iOS App (keyboard ex)"; */
  /* }; */

  shells = {
    /* ghc   = [ "liblea" ]; */
    /* ghcjs = [ "liblea" ]; */
    ghc   = [ "reflex-dom-htmlea" "example1" "exampleTbl" ];
    ghcjs = [ "reflex-dom-htmlea" "example1" "exampleTbl" ];
    /* ghcjs = [ "reflex-dom-htmlea" "example2" ]; */
    /* ghcjs = [ "reflex-dom-htmlea" "example1" ]; */
    /* ghcjs = [ "reflex-dom-htmlea" ]; */
  };
  tools = ghc: with ghc; [
    pkgs.haskellPackages.ghc-mod
    pkgs.haskellPackages.hasktags
    pkgs.haskellPackages.haskdogs
    pkgs.haskellPackages.hdevtools
    pkgs.haskellPackages.hindent
    pkgs.haskellPackages.hsimport
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.pointfree
    pkgs.haskellPackages.pointful
    pkgs.haskellPackages.stylish-haskell
  ];
})
