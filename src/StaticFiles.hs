{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module StaticFiles where

import Data.ByteString
import Data.FileEmbed
import Language.Haskell.TH.Syntax     (qRunIO)
import Network.Wai
import Network.Wai.Application.Static
import StaticFiles.Delegated          (allCss)

staticFiles :: [(FilePath, ByteString)]
staticFiles =
    [ ("css/all.css",         $(qRunIO allCss >>= bsToExp))
    , ("favicon.ico",         $(embedFile "static/img/favicon.ico"))
    , ("img/github.png",      $(embedFile "static/img/github.png"))
    , ("img/otter.png",       $(embedFile "static/img/otter.png"))
    , ("img/linkedin.png",    $(embedFile "static/img/linkedin.png"))
    , ("img/github@2x.png",   $(embedFile "static/img/github@2x.png"))
    , ("img/otter@2x.png",    $(embedFile "static/img/otter@2x.png"))
    , ("img/linkedin@2x.png", $(embedFile "static/img/linkedin@2x.png"))
    , ("js/all.js",           $(embedFile "static/js/all.js"))

    , ("fonts/fontawesome-webfont.woff2",
        $(embedFile "bower_components/font-awesome/fonts/fontawesome-webfont.woff2"))
    , ("fonts/fontawesome-webfont.woff",
        $(embedFile "bower_components/font-awesome/fonts/fontawesome-webfont.woff"))
    , ("fonts/fontawesome-webfont.ttf",
        $(embedFile "bower_components/font-awesome/fonts/fontawesome-webfont.ttf"))
    ]

serveFile :: Application
serveFile = staticApp set where
    set = (defaultWebAppSettings $ error "unused") { ssLookupFile = ssLookupFile embedded }
    embedded = embeddedSettings staticFiles
