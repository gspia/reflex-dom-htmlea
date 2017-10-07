{ reflex-platform ? import ./reflex-platform.nix
, compiler   ? "ghcjs"
} :
# Note that default has ghcjs as default compiler while 
# shell.nix has ghc. This way we can use ghcid and some other tools
# while developing (e.g. using the work-on ghc -script).
# nix-build uses default.nix and thus ghcjs.
let
  initialNixpkgs = import <nixpkgs> {};

  /* sources = { */
  /*   reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub { */
  /*     owner  = "reflex-frp"; */
  /*     repo   = "reflex-platform"; */
  /*     rev    = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3"; */
  /*     sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad"; */
  /*   }; */
  /* }; */
  /* reflex-platform = import sources.reflex-platform {}; */

  pkgs  = reflex-platform.nixpkgs.pkgs;
  hpkgs = initialNixpkgs.pkgs.haskellPackages;


  indexHtml = ''
    <!DOCTYPE html>
    <html>
      <head>
         <title>htmlea example</title>
      </head>
      <body>
      </body>
      <script language="javascript" src="js/htmlLeaEx.min.js"></script>
    </html>
  '';

  adjust-for-ghcjs = drv: {
    executableSystemDepends = [
      hpkgs.cabal-install
    ];
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out
      mkdir -p $out/css
      mkdir -p $out/figs
    '';
  };
#       cp ./figs/* $out/figs/
#       mkdir -p $out/js
#       cp $out/bin/htmlLeaEx.jsexe/all.js $out/js/htmlLeaEx.js
#       cd $out/bin/htmlLeaEx.jsexe
#       closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/htmlLeaEx.min.js
#       cat <<EOF > $out/index.html
#         ${indexHtml}
#       EOF
# Note that if editor makes comments with the /* */, then inside the above shell 
# script (the above '' ''-block) those /**/-blocks are not recognized as comments.
# In nix-lang /* */ work as comments.
      /* rm -Rf $out/lib */
      /* rm -Rf $out/nix-support */
      /* rm -Rf $out/share */
  #    rm -Rf $out/bin/short.jsexe
  #    rm -Rf $out/bin
  #    cd $out/js
  #    gzip short.min.js
  # zopfli is gzip on steroids.
  #    zopfli -i1000 short.min.js
  #installPhase = ''
  #  mkdir $out
  #  cp -r ./* $out/
  #'';
  # phase = ["unpackPhase" "buildPhase" "installPhase"];
  adjust-for-ghc = drv: {
    executableSystemDepends = [
      reflex-platform.${compiler}.ghcid
      # The next one is a nice example on how to wrap tools to nix env.
      (pkgs.callPackage (import ./tools/nix-tags-haskell) {})
      hpkgs.ghc-mod
      hpkgs.hasktags
      hpkgs.haskdogs  # stack config set system-ghc --global true
      hpkgs.hdevtools
      hpkgs.hlint
      hpkgs.pointfree
      hpkgs.pointful
      /* hpkgs.stack */
    ];
    /* executableHaskellDepends = [ */
    /* ]; */
  };

  adjust =
    if compiler == "ghcjs"
    then adjust-for-ghcjs
    else adjust-for-ghc;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
      wai-middleware-static = pkgs.haskell.lib.dontCheck (super.wai-middleware-static);
    });
  };
  htmlea-code-base = 
    haskellPackages.callPackage ./reflex-dom-htmlea.nix { inherit compiler; };
  htmlea-code = 
    pkgs.haskell.lib.overrideCabal htmlea-code-base adjust;
in 
  htmlea-code 


