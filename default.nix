{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, aeson, attoparsec, base
      , blaze-builder, blaze-html, blaze-markup, bytestring
      , bytestring-conversion, cereal, clientsession, containers, cookie
      , either, exceptions, file-embed, filepath, highlighter, http-media
      , http-types, ixset, lens, markdown, monad-control, monad-logger
      , mtl, network-uri, path-pieces, pcre-light, random, safecopy
      , servant, servant-server, shakespeare, stdenv, template-haskell
      , text, time, transformers, uuid, vault, wai, wai-app-static
      , wai-extra, wai-session, wai-session-clientsession, warp
      }:
      mkDerivation {
        pname = "jude-web";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          acid-state aeson attoparsec base blaze-builder blaze-html
          blaze-markup bytestring bytestring-conversion cereal clientsession
          containers cookie either exceptions file-embed filepath highlighter
          http-media http-types ixset lens markdown monad-control
          monad-logger mtl network-uri path-pieces pcre-light random safecopy
          servant servant-server shakespeare template-haskell text time
          transformers uuid vault wai wai-app-static wai-session
          wai-session-clientsession warp
        ];
        executableHaskellDepends = [ base wai-extra warp ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
