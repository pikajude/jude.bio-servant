{ mkDerivation, acid-state, aeson, base, blaze-builder, blaze-html
, blaze-markup, bytestring, clientsession, containers, cookie
, either, exceptions, file-embed, filepath, highlighter, http-media
, http-types, ixset, lens, markdown, monad-control, monad-logger
, mtl, network-uri, path-pieces, pcre-light, random, safecopy
, servant, servant-server, shakespeare, stdenv, template-haskell
, text, time, transformers, uuid, vault, wai, wai-app-static
, wai-extra, wai-session, warp
}:
mkDerivation {
  pname = "jude-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state aeson base blaze-builder blaze-html blaze-markup
    bytestring clientsession containers cookie either exceptions
    file-embed filepath highlighter http-media http-types ixset lens
    markdown monad-control monad-logger mtl network-uri path-pieces
    pcre-light random safecopy servant servant-server shakespeare
    template-haskell text time transformers uuid vault wai
    wai-app-static wai-session warp
  ];
  executableHaskellDepends = [ base wai-extra warp ];
  license = stdenv.lib.licenses.mit;
}
