{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module API.PageTypes where

import           Data.Aeson
import qualified Data.Text           as T
import           Models
import           Models.SessionData
import           Servant
import           Text.Digestive.View

-- | Homepage
data Homepage = Homepage [Essay] (Maybe User)

instance ToJSON Homepage where
    toJSON (Homepage es _) = toJSON es

-- | Read a post
data Single = Single Essay (Maybe User)

instance ToJSON Single where
    toJSON (Single e _) = toJSON e

-- | Create a post
data NewPage = NewPage (View String) User deriving Show

instance ToJSON NewPage where
    toJSON (NewPage v _) = toJSON (Errors v)

-- | Edit a post
data EditPage = EditPage Essay (View String) User

instance ToJSON EditPage where
    toJSON (EditPage e _ _) = toJSON e

-- | Auth pages
data LoginPage = LoginPage
data LogoutPage = LogoutPage

instance MimeRender PlainText LogoutPage where
    mimeRender _ _ = ""

-- | Form errors
newtype Errors = Errors { unErrors :: View String }

instance ToJSON Errors where
    toJSON (Errors v) = object
        [ "code" .= ("INVALID_INPUT" :: T.Text)
        , "message" .= ("Invalid input" :: T.Text)
        , "fields" .= errorsJson (viewErrors v)
        ]
        where
            errorsJson = map (\ (p, s) -> object
                [ "message" .= s
                , "path" .= T.intercalate "." p ])
