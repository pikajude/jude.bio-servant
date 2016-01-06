{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.Login where

import API
import Data.ByteString.Lazy (ByteString)
import HTMLRendering
import Text.Hamlet

renderLogin :: ByteString
renderLogin = defaultLayout $ render $(hamletFile "static/html/login.hamlet")
