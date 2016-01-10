{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.Edit where

import API
import Data.Monoid
import HTMLRendering
import Models
import Servant.API
import Text.Digestive.View
import Text.Hamlet

instance MimeRender HTML EditPage where
    mimeRender _ (EditPage e view) = defaultLayout $ do
        setTitle $ "Editing " <> unTitle (essayTitle e)
        let form = $(hamletFile "static/html/_form.hamlet")
        render $(hamletFile "static/html/edit.hamlet")
