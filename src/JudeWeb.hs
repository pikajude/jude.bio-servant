{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

module JudeWeb where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Acid
-- import           Data.FileEmbed
import           Data.IxSet                        hiding (Proxy)
import           Data.Maybe
import           Data.Proxy
import qualified Data.Vault.Lazy                   as V
import           HTMLRendering
import           Network.Wai                       (Application, vault)
import           Network.Wai.Session
import           Network.Wai.Session.ClientSession
import           Servant.API
import           Servant.Server
import           StaticFiles
import           System.Environment
import           Web.ClientSession                 hiding (Key)
import           Web.Cookie

import           API
import           Models

import           Pages.Home
import           Pages.Login
import           Pages.Single

server :: AppState -> Server API
server appState = enter' serveHome
             :<|> enter' serveSingle
             :<|> enter' serveEdit
             :<|> enter' serveNew
             :<|> (enter' serveLoginGet :<|> enter' serveLoginPost)
             :<|> serveStatic
    where
        enter' = enter (runReaderTNat appState)

serveStatic :: Application
serveStatic = serveFile

serveHome :: AppM Homepage
serveHome = do
    mu <- fetch "user"
    set "user" $ UserS "pikajude"
    es <- runDB GetAll
    let h = Homepage es
    return $ Rendered h (renderHome h mu)

serveSingle :: EssaySlug -> AppM Single
serveSingle sl = do
    e <- runDB $ SelectSlug sl
    case e of
        Nothing -> lift $ throwE err404
        Just x -> do
            let s = Single x
                rendered = Rendered s (renderSingle s Nothing)
            return rendered

serveEdit :: EssaySlug -> AppM Single :<|> (Essay -> AppM Single)
serveEdit sl = serveEditGet :<|> serveEditPatch where
    serveEditGet = do
        e <- runDB $ SelectSlug sl
        case e of
            Nothing -> lift $ throwE err404
            Just ese -> error $ show ese
    serveEditPatch ese = error (show ese)

serveNew :: Essay -> AppM Single
serveNew essay = do
    execDB $ Insert essay
    lift $ throwE (err301 { errHeaders = [("Location", "/")] })

serveLoginGet :: AppM LoginPage
serveLoginGet = return $ Rendered undefined renderLogin

serveLoginPost :: User -> AppM ()
serveLoginPost _user = do
    _key <- asks appKey
    return undefined
    {- if password user == $(embedStringFile "important-secret")
        then do
            nc <- newCookie key (SessionStore (Just (username user)) Nothing)
            lift $ throwE (err301
                 { errHeaders = [ ("Location", "/")
                                , ("Set-Cookie", nc)
                                ] })
        else do
            nc <- newCookie key (SessionStore Nothing (Just "Invalid password"))
            lift $ throwE (err301
                 { errHeaders = [ ("Location", "/in")
                                , ("Set-Cookie", nc)
                                ] }) -}

serveApp :: IO Application
serveApp = do
    stateDir <- fromMaybe "db" <$> lookupEnv "STATE_DIR"
    database <- openLocalStateFrom stateDir (Database empty)
    k <- getDefaultKey
    vk <- V.newKey
    return $ withSession (clientsessionStore k) "_SESSION" (def { setCookiePath = Just "/" }) vk
        $ \ req -> serve (Proxy :: Proxy API)
            (server (AppState k database (V.lookup vk $ vault req)))
            req
