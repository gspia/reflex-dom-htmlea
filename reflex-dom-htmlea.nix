{ mkDerivation, stdenv, base, containers, data-default, ghcjs-dom
      , jsaddle, reflex, reflex-dom-core, text
      , compiler ? "ghc"
      , ghcjs-base ? ""
      , jsaddle-webkit2gtk ? ""
      , jsaddle-warp ? ""
      , warp ? ""
      , wai-app-static ? ""
      , wai-middleware-static ? ""
      , websockets ? ""
}:
mkDerivation {
    pname = "reflex-dom-htmlea";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
      base containers data-default jsaddle reflex reflex-dom-core text
    ] 
    ++ (if compiler == "ghc"
          then [jsaddle-warp warp wai-app-static wai-middleware-static websockets]
          else []
       );

    executableHaskellDepends = [
      base containers ghcjs-dom jsaddle reflex
      reflex-dom-core text
    ]
    ++ (if compiler == "ghc"
            then [jsaddle-webkit2gtk]
            else [ghcjs-base]);
    description = "A reflex-dom API for HTML elements and attributes";
    license = stdenv.lib.licenses.bsd3;
}

