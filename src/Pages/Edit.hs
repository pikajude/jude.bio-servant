{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.Edit where

import API
import Data.ByteString.Lazy (ByteString)
import HTMLRendering
import Models
import Text.Digestive.View
import Text.Hamlet

renderEdit :: Essay -> View String -> ByteString
renderEdit e view = defaultLayout $ render $(hamletFile "static/html/edit.hamlet")
