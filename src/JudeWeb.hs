{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module JudeWeb where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text                  (Text)
import Database.Persist
import Database.Persist.Sqlite    hiding (Single)
import Network.Wai                (Application)
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles

import API
import Models

server :: ConnectionPool -> Server API
server pool = enter' serveHome
         :<|> enter' serveSingle
         :<|> enter' serveEdit
         :<|> serveDirectory "static"
    where
        enter' = enter (runReaderTNat pool)

serveStatic :: Application
serveStatic = serveDirectory "static"

serveHome :: AppM Homepage
serveHome = do
    es <- runDB $ selectList [] [Desc EssayCreatedAt]
    return $ Homepage es

serveSingle :: Text -> AppM Single
serveSingle sl = do
    e <- runDB $ getBy (UniqueEssay sl)
    case e of
        Nothing -> lift $ left err404
        Just x -> return $ Single x

serveEdit :: EssayId -> Essay -> AppM Single
serveEdit k essay = do
    e <- runDB $ get k
    case e of
        Nothing -> lift $ left err404
        Just _ -> error $ show essay

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "test.db" 1
    runSqlPool (runMigration migrateAll) pool

    run 8081 $ serve (Proxy :: Proxy API) $ server pool
