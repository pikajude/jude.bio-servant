{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Models.SessionData where

import Control.Monad.Reader
import Data.ByteString      (ByteString)
import Data.Proxy
import Data.Serialize
import Data.Text            (Text)
import Data.Text.Encoding
import Models

class Serialize a => SessionData a where
    sessionKey :: proxy a -> ByteString

data User = User Text deriving Show
data Message = Message Text

pattern KUser :: Proxy User
pattern KUser = Proxy

pattern KMessage :: Proxy Message
pattern KMessage = Proxy

instance Serialize User where
    put (User t) = put (encodeUtf8 t)
    get = User . decodeUtf8 <$> get

instance Serialize Message where
    put (Message t) = put (encodeUtf8 t)
    get = Message . decodeUtf8 <$> get

instance SessionData User where sessionKey _ = "user"
instance SessionData Message where sessionKey _ = "message"

fetch :: (MonadReader AppState m, MonadIO m, SessionData a)
      => proxy a -> m (Maybe a)
fetch k = do
    Just (fetch', _) <- asks appSession
    liftM (>>= em . decode) $ liftIO (fetch' $ sessionKey k)
    where
        em (Left _) = Nothing
        em (Right x) = Just x

set :: (MonadReader AppState m, MonadIO m, SessionData a)
    => proxy a -> a -> m ()
set k s = do
    Just (_, put') <- asks appSession
    liftIO $ put' (sessionKey k) (encode s)

clear :: (MonadReader AppState m, MonadIO m, SessionData a)
      => proxy a -> m ()
clear k = do
    Just (_, put') <- asks appSession
    liftIO $ put' (sessionKey k) (encode ())
