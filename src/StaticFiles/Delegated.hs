{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module StaticFiles.Delegated where

import           API
import qualified Data.ByteString         as B
import           Data.ByteString.Lazy
import           Data.Monoid
import           Data.Text.Lazy.Encoding
import           HTMLRendering           (myUrlRenderer)
import           Prelude                 hiding (readFile)
import           Text.Lucius

allCss :: IO B.ByteString
allCss = do
    normalizeCss <- readFile "bower_components/foundation/css/normalize.min.css"
    foundationCss <- readFile "bower_components/foundation/css/foundation.min.css"
    fontAwesomeCss <- readFile "bower_components/font-awesome/css/font-awesome.min.css"
    return $ toStrict $ mconcat
        [ normalizeCss, foundationCss, fontAwesomeCss
        , cssToBs $(luciusFile "static/css/all.lucius")
        , cssToBs $(luciusFile "static/css/form.lucius")
        , cssToBs $(luciusFile "static/css/home.lucius")
        , cssToBs $(luciusFile "static/css/single.lucius")
        ]
    where
        cssToBs c = encodeUtf8 (renderCss $ c myUrlRenderer)
