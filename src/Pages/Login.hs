{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Pages.Login where

import API
import Crypto.PasswordStore
import Data.FileEmbed
import Data.String
import Data.Text.Encoding
import HTMLRendering
import Models
import Servant.API
import Text.Digestive.Form
import Text.Digestive.View
import Text.Hamlet

loginForm :: (Monad m, Monoid v, IsString v)
          => Form v m LoginUser
loginForm = LoginUser
    <$> "username" .: checkNotNull (text Nothing)
    <*> "password" .: checkPw (text Nothing)
    where
        checkNotNull = check "Can't be empty!" (/= mempty)
        checkPw = check "Invalid password." $
            \ pw -> verifyPassword (encodeUtf8 pw) $(embedFile "important-secret")

instance MimeRender HTML LoginPage where
    mimeRender _ (LoginPage view) = defaultLayout $ render $(hamletFile "static/html/login.hamlet")
