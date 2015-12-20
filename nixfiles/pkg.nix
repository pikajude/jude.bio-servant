{ mkDerivation, aeson, base, bytestring, either, exceptions
, filepath, http-media, http-types, lens, monad-control
, monad-logger, mtl, path-pieces, persistent, persistent-postgresql
, persistent-sqlite, persistent-template, servant, servant-server
, stdenv, template-haskell, text, time, wai, warp
}:
mkDerivation {
  pname = "jude-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring either exceptions filepath http-media
    http-types lens monad-control monad-logger mtl path-pieces
    persistent persistent-postgresql persistent-sqlite
    persistent-template servant servant-server template-haskell text
    time wai warp
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
