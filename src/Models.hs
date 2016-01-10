{-# OPTIONS_GHC -fno-warn-orphans #-}
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
{-# LANGUAGE ViewPatterns               #-}

#define INSTANCES Data, Eq, Ord, Show
#define JSON_INSTANCES INSTANCES, ToJSON, FromJSON
#define HTML_INSTANCES JSON_INSTANCES, ToMarkup

module Models where

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
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.SafeCopy
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           Models.OverHead
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
                    deriving (JSON_INSTANCES, ToHttpApiData, FromHttpApiData)
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

deriveJSON defaultOptions { fieldLabelModifier = overHead toLower . drop 5
                          , constructorTagModifier = map toLower
                          } ''Essay

instance Indexable Essay where
    empty = ixSet [ ixFun indexEssay ]
        where indexEssay Essay{..} = [ essaySlug ]

updateEssay :: Essay -> (Text, Text) -> Essay
updateEssay e (newT, newC) = e { essayTitle = EssayTitle newT
                               , essayContent = EssayContent newC
                               }

data EssayUpdate = EssayUpdate { euTitle :: Maybe Text, euContent :: Maybe Text }
                  deriving (INSTANCES)

instance FromFormUrlEncoded EssayUpdate where
    fromFormUrlEncoded ps = Right $ EssayUpdate
        (lookup "essay.title" ps) (lookup "essay.content" ps)

instance ToFormUrlEncoded EssayUpdate where
    toFormUrlEncoded EssayUpdate{..} = catMaybes
        [(,) "essay.title" <$> euTitle, (,) "essay.content" <$> euContent]

deriveJSON defaultOptions { fieldLabelModifier = overHead toLower . drop 2
                          , constructorTagModifier = map toLower
                          } ''EssayUpdate

data EssayNew = EssayNew { enTitle :: Text, enContent :: Text }
              deriving (INSTANCES)

instance FromFormUrlEncoded EssayNew where
    fromFormUrlEncoded ps = EssayNew
        <$> lookupE "Missing title" "essay.title" ps
        <*> lookupE "Missing content" "essay.content" ps
        where lookupE t n s = maybe (Left t) Right (lookup n s)

instance ToFormUrlEncoded EssayNew where
    toFormUrlEncoded EssayNew{..} =
        [("essay.title", enTitle), ("essay.content", enContent)]

deriveJSON defaultOptions { fieldLabelModifier = overHead toLower . drop 2
                          , constructorTagModifier = map toLower
                          } ''EssayNew

fromNew :: EssayNew -> IO Essay
fromNew EssayNew{..} = do
    t <- getCurrentTime
    return $ Essay (EssayTitle enTitle) (EssaySlug $ mkSlug enTitle)
        (EssayContent enContent) (EssayCreatedAt t)

mkSlug :: Text -> Text
mkSlug = T.foldr (\ e m -> if (T.take 1 m, e) == ("-", '-') then m else T.cons e m) mempty
       . T.map (\ x -> if isAlphaNum x then x else '-')
       . T.toLower

-- | User
data LoginUser = LoginUser { username :: Text, password :: Text } deriving Show

deriveJSON defaultOptions ''LoginUser

instance FromFormUrlEncoded LoginUser where
    fromFormUrlEncoded ps = LoginUser
        <$> lookupE "Missing username" "login.username" ps
        <*> lookupE "Missing password" "login.password" ps
        where lookupE s k f = maybe (Left s) Right $ lookup k f

instance ToFormUrlEncoded LoginUser where
    toFormUrlEncoded LoginUser{..} =
        [("login.username", username), ("login.password", password)]

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
    return $ sortBy (comparing essayCreatedAt) $ toList essays

selectSlug :: EssaySlug -> Query Database (Maybe Essay)
selectSlug slug = do
    Database essays <- ask
    return $ getOne $ essays @= slug

replaceSlug :: EssaySlug -> Essay -> Update Database ()
replaceSlug slug e = do
    Database essays <- get
    put $ Database $ updateIx slug e essays

insert :: Essay -> Update Database (Maybe String)
insert e = do
    Database es <- get
    case getOne (es @= essaySlug e) of
        Nothing -> do
            put $ Database $ Data.IxSet.insert e es
            return Nothing
        Just _ -> return $ Just "Slug collision"

makeAcidic ''Database ['getAll, 'selectSlug, 'replaceSlug, 'Models.insert]
