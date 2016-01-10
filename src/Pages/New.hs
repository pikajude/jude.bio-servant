{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.New where

import API
import HTMLRendering
import Pages.Forms
import Servant.API
import Text.Hamlet

instance MimeRender HTML NewPage where
    mimeRender _ (NewPage view _) = defaultLayout $ do
        setTitle "New post"
        let form = $(hamletFile "static/html/_form.hamlet")
        render $(hamletFile "static/html/new.hamlet")
