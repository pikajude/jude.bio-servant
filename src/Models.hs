{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text                   (Text)
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import Prelude                     hiding (div)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Essay
    title Text sqltype=TEXT
    slug Text sqltype=TEXT
    content Text sqltype=TEXT
    createdAt UTCTime
    UniqueEssay slug
    deriving Show
|]

deriveJSON defaultOptions { fieldLabelModifier = over _head toLower . drop 5
                          , constructorTagModifier = map toLower
                          } ''Essay

runDB :: (MonadTrans t, MonadReader ConnectionPool (t m), MonadBaseControl IO m)
      => SqlPersistT m b -> t m b
runDB query = do
    pool <- ask
    lift $ runSqlPool query pool
