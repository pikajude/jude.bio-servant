{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#define INSTANCES Data, Eq, Ord, Show
#define JSON_INSTANCES INSTANCES, ToJSON, FromJSON
#define HTML_INSTANCES JSON_INSTANCES, ToMarkup

module Models where

import           Control.Lens         (over, _head)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Acid.Core       (MethodState)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString      (ByteString)
import           Data.Char
import           Data.Data            hiding (Proxy)
import           Data.IxSet
import           Data.SafeCopy
import qualified Data.Serialize       as S
import           Data.Text            (Text)
import           Data.Time
import           Network.Wai.Session
import           Prelude              hiding (div)
import           Servant              (FromFormUrlEncoded (..), FromHttpApiData,
                                       ToFormUrlEncoded (..), ToHttpApiData)
import           Text.Blaze           (ToMarkup)
import           Web.ClientSession    (Key)

data AppState = AppState
              { appKey      :: Key
              , appDatabase :: AcidState Database
              , appSession  :: Maybe (Session IO ByteString ByteString)
              }

data Database = Database (IxSet Essay)

-- | Essaying
data Essay = Essay
           { essayTitle     :: EssayTitle
           , essaySlug      :: EssaySlug
           , essayContent   :: EssayContent
           , essayCreatedAt :: EssayCreatedAt
           } deriving (INSTANCES)

newtype EssayTitle = EssayTitle { unTitle :: Text }
                     deriving (HTML_INSTANCES)
newtype EssaySlug = EssaySlug { unSlug :: Text }
                    deriving (JSON_INSTANCES, FromHttpApiData, ToHttpApiData)
newtype EssayContent = EssayContent { unContent :: Text }
                       deriving (JSON_INSTANCES)
newtype EssayCreatedAt = EssayCreatedAt { unCreatedAt :: UTCTime }
                         deriving (INSTANCES)

-- can't use GND here, because the resulting JSON is missing quotes around
-- the date (???)
instance ToJSON EssayCreatedAt where
    toJSON (EssayCreatedAt c) = toJSON c

instance FromJSON EssayCreatedAt where
    parseJSON s = EssayCreatedAt <$> parseJSON s

deriveSafeCopy 0 'base ''Database
deriveSafeCopy 0 'base ''Essay
deriveSafeCopy 0 'base ''EssayTitle
deriveSafeCopy 0 'base ''EssaySlug
deriveSafeCopy 0 'base ''EssayContent
deriveSafeCopy 0 'base ''EssayCreatedAt

deriveJSON defaultOptions { fieldLabelModifier = over _head toLower . drop 5
                          , constructorTagModifier = map toLower
                          } ''Essay

instance Indexable Essay where
    empty = ixSet
        [ ixGen (Proxy :: Proxy EssayTitle)
        , ixGen (Proxy :: Proxy EssaySlug)
        , ixGen (Proxy :: Proxy EssayContent)
        , ixGen (Proxy :: Proxy EssayCreatedAt)
        ]

-- | User
data User = User { username :: Text, password :: Text } deriving Show

deriveJSON defaultOptions ''User

instance FromFormUrlEncoded User where
    fromFormUrlEncoded ps = User
        <$> lookupE "Missing username" "username" ps
        <*> lookupE "Missing password" "password" ps
        where lookupE s k f = maybe (Left s) Right $ lookup k f

instance ToFormUrlEncoded User where
    toFormUrlEncoded (User u p) = [("username", u), ("password", p)]

-- | Session data
newtype UserS = UserS String deriving (S.Serialize, Show)

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

-- | Running
runDB :: (MonadIO m, MonadReader AppState m, QueryEvent event, MethodState event ~ Database)
      => event -> m (EventResult event)
runDB f = do
    st <- asks appDatabase
    liftIO $ query st f

execDB :: (MonadIO m, MonadReader AppState m, UpdateEvent event, MethodState event ~ Database)
    => event -> m (EventResult event)
execDB f = do
    st <- asks appDatabase
    liftIO $ update st f

getAll :: Query Database [Essay]
getAll = do
    Database essays <- ask
    return $ toAscList (Proxy :: Proxy EssayCreatedAt) essays

selectSlug :: EssaySlug -> Query Database (Maybe Essay)
selectSlug slug = do
    Database essays <- ask
    return $ getOne $ essays @= slug

insert :: Essay -> Update Database ()
insert e = do
    Database es <- get
    put $ Database $ Data.IxSet.insert e es

makeAcidic ''Database ['getAll, 'selectSlug, 'Models.insert]
