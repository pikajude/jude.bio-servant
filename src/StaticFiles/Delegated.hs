{-# LANGUAGE TemplateHaskell #-}

module StaticFiles.Delegated where

import qualified Data.ByteString         as B
import           Data.ByteString.Lazy
import           Data.Monoid
import           Data.Text.Lazy.Encoding
import           Prelude                 hiding (readFile)
import           Text.Lucius

allCss :: IO B.ByteString
allCss = do
    normalizeCss <- readFile "bower_components/foundation/css/normalize.min.css"
    foundationCss <- readFile "bower_components/foundation/css/foundation.min.css"
    return $ toStrict $ mconcat
        [ normalizeCss, foundationCss
        , cssToBs $(luciusFile "static/css/all.lucius")
        , cssToBs $(luciusFile "static/css/home.lucius")
        , cssToBs $(luciusFile "static/css/read.lucius")
        ]
    where
        cssToBs c = encodeUtf8 (renderCss $ c undefined)
