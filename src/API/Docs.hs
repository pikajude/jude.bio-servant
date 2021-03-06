{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module API.Docs where

import API
import Combinators
import Data.Proxy
import Data.Void     (Void)
import HTMLRendering (HTML)
import Models
import Servant
import Servant.Docs

instance ToSample EssayUpdate where
    toSamples Proxy = [("I don't know what this is", sample)] where
        sample = EssayUpdate (Just "This is my new essay title")
                             (Just "This is my new essay content")

instance ToSample EssayNew where
    toSamples Proxy = [("I don't know what this is either", sample)] where
        sample = EssayNew "This is my new essay title"
                          "This is my new essay content"

instance ToSample LoginUser where
    toSamples Proxy = [("Still don't know", sample)] where
        sample = LoginUser "ᕙ(░ಥ╭͜ʖ╮ಥ░)━☆ﾟ.*･｡ﾟ"
                           "Password123"

instance ToSample LogoutPage where
    toSamples Proxy = []

instance ToSample LoginPage where
    toSamples Proxy = []

instance ToSample EditPage where
    toSamples Proxy = []

instance ToSample NewPage where
    toSamples Proxy = []

instance ToSample Single where
    toSamples Proxy = do
        s <- toSamples Proxy
        return (fst s, Single (snd s) Nothing)

instance ToSample Essay where
    toSamples Proxy = [("A standard one", sample)] where
        sample = Essay (EssayTitle "My essay title")
                       (EssaySlug "my-essay-title")
                       (EssayContent "This is a new post that I made")
                       (EssayCreatedAt (read "2016-01-10 01:20:54.244492 UTC"))

instance ToSample Homepage where
    toSamples Proxy = do
        s <- toSamples Proxy
        return (fst s, Homepage [snd s] Nothing Nothing)

instance MimeRender HTML Void where
    mimeRender _ _ = ""

instance ToSample Void where
    toSamples _ = []

instance HasDocs sub => HasDocs (WithEssay :> sub) where
    docsFor Proxy = docsFor (Proxy :: Proxy (Capture "slug" EssaySlug :> sub))

instance HasDocs sub => HasDocs (SessionVar a :> sub) where
    docsFor Proxy = docsFor (Proxy :: Proxy sub)

instance ToCapture (Capture "slug" EssaySlug) where
    toCapture _ = DocCapture "slug" "The URL slug of the essay to retrieve"
