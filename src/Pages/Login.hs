{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.Login where

import API
import HTMLRendering
import Servant.API
import Text.Hamlet

instance MimeRender HTML LoginPage where
    mimeRender _ _ = defaultLayout $ render $(hamletFile "static/html/login.hamlet")
