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

pattern KUser = Proxy :: Proxy User
pattern KMessage = Proxy :: Proxy Message

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

{-

-- | Session data
newtype UserS = UserS Text deriving (S.Serialize, Show)
newtype Message = Message Text deriving (S.Serialize, Show)

-- | Orphans
instance S.Serialize Text where
    put = S.put . encodeUtf8
    get = fmap decodeUtf8 S.get

set :: (S.Serialize a, MonadReader AppState m, MonadIO m)
    => ByteString -> a -> m ()
set k s = do
    Just (_, put') <- asks appSession
    liftIO $ put' k $ S.encode s

fetch :: (S.Serialize a, MonadReader AppState m, MonadIO m)
      => ByteString -> m (Maybe a)
fetch k = do
    Just (fetch', _) <- asks appSession
    liftM (>>= em . S.decode) $ liftIO (fetch' k)
    where
        em (Left _) = Nothing
        em (Right x) = Just x

-}
