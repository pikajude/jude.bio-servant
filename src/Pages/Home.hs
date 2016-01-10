{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.Home where

import API
import HTMLRendering
import Models
import Models.SessionData
import Servant.API
import Text.Hamlet

instance MimeRender HTML Homepage where
    mimeRender _ (Homepage es mu mmsg) =
        defaultLayout $ render $(hamletFile "static/html/home.hamlet")
