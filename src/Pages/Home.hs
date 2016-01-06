{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.Home where

import API
import Data.ByteString.Lazy (ByteString)
import HTMLRendering
import Models
import Text.Hamlet

renderHome :: Homepage -> Maybe UserS -> ByteString
renderHome (Homepage es) mu = defaultLayout $ render $(hamletFile "static/html/home.hamlet")
