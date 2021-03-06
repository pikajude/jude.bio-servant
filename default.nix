{ mkDerivation, acid-state, aeson, base, blaze-html, blaze-markup
, bytestring, cereal, clientsession, cookie, digestive-functors
, digestive-functors-blaze, file-embed, filepath, highlighter
, hspec, hspec-wai, hspec-wai-json, http-media, http-types, ixset
, markdown, mtl, network-uri, pcre-light, pwstore-fast, safecopy
, servant, servant-docs, servant-server, shakespeare, stdenv
, template-haskell, text, time, transformers, utf8-string, vault
, wai, wai-app-static, wai-extra, wai-session
, wai-session-clientsession, warp
}:
mkDerivation {
  pname = "jude-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state aeson base blaze-html blaze-markup bytestring cereal
    clientsession cookie digestive-functors digestive-functors-blaze
    file-embed filepath highlighter http-media http-types ixset
    markdown mtl network-uri pcre-light pwstore-fast safecopy servant
    servant-docs servant-server shakespeare template-haskell text time
    transformers utf8-string vault wai wai-app-static wai-extra
    wai-session wai-session-clientsession
  ];
  executableHaskellDepends = [ base wai-extra warp ];
  testHaskellDepends = [
    acid-state aeson base blaze-html blaze-markup bytestring cereal
    clientsession cookie digestive-functors digestive-functors-blaze
    file-embed filepath highlighter hspec hspec-wai hspec-wai-json
    http-media http-types ixset markdown mtl network-uri pcre-light
    pwstore-fast safecopy servant servant-docs servant-server
    shakespeare template-haskell text time transformers utf8-string
    vault wai wai-app-static wai-extra wai-session
    wai-session-clientsession
  ];
  license = stdenv.lib.licenses.mit;
}
