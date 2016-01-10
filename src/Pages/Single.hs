{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pages.Single where

import           API
import qualified Data.ByteString             as BS
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text, pack, unpack)
import           Data.Text.Encoding
import           Data.Text.Lazy              (fromStrict)
import           HTMLRendering
import           Models
import           Servant.API
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Hamlet
import           Text.Highlighter            hiding (Single)
import           Text.Markdown
import           Text.Regex.PCRE.Light

instance MimeRender HTML Single where
    mimeRender _ (Single e loggedInUser) = defaultLayout $ do
        setTitle $ unTitle (essayTitle e)
        render $(hamletFile "static/html/single.hamlet")

renderMd :: EssayContent -> Html
renderMd (EssayContent m) = markdown defWithHighlight (fromStrict m) where
    defWithHighlight = def { msBlockCodeRenderer = rendered }
    pickLexer cod = fromMaybe textLexer $ lookup (maybe "text" unpack cod) fixedLexers
    fixedLexers = map (\ (_,x) -> (head (lAliases x), x)) lexers
    textLexer = Lexer "text" [] [] [] [Match ".*" Text Continue] [dotall]
    renderer l (tx,_) = case runLexer (pickLexer l) (encodeUtf8 tx) of
        Left es -> [Token Text $ encodeUtf8 $ "parse error: " <> pack (show es)]
        Right ts -> ts
    rendered ah bh = H.figure . decentFormat $ renderer ah bh
    decentFormat ts = H.table H.! A.class_ "highlight-table" $ H.tr $ do
        H.td H.! A.class_ "linenos" $
            H.div H.! A.class_ "lineno" $
                H.pre (lineNos (countLines ts))

        H.td H.! A.class_ "code" $
            H.div H.! A.class_ "highlight" $
                H.pre $ highlight ts
    countLines [] = 0
    countLines (Token _ s:ts) =
        -- elemIndices 10 counts the number of newlines (char 10)
        -- in the string
        length (BS.elemIndices 0x0A s) + countLines ts
    highlight [] = return ()
    highlight (Token t s:ts) = do
        H.span H.! A.class_ (H.toValue $ shortName t) $ toHtml (decodeUtf8 s)
        highlight ts
    lineNos n = lineNos' 1 where
        lineNos' c
            | c - 1 <= n = do
                toHtml (show c)
                toHtml (asText "\n")
                lineNos' (c + 1)
            | otherwise = return ()

asText :: Text -> Text
asText = id
