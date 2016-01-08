{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeOperators             #-}

module JudeWeb where

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Crypto.PasswordStore                      (verifyPassword)
import           Data.Acid
import           Data.ByteString.UTF8                      (fromString)
import           Data.FileEmbed
import           Data.IxSet                                hiding (Proxy)
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text.Encoding
import qualified Data.Vault.Lazy                           as V
import           HTMLRendering
import           Models.SessionData
import           Network.Wai                               (Application, vault)
import           Network.Wai.Middleware.MethodOverridePost
import           Network.Wai.Session
import           Network.Wai.Session.ClientSession
import           Servant.API
import           Servant.Server
import           StaticFiles
import           System.Environment
import           Text.Digestive.View
import           Web.ClientSession                         hiding (Key)
import           Web.Cookie

import           API
import           Models

import           Pages.Edit
import           Pages.Forms
import           Pages.Home
import           Pages.Login
import           Pages.Single

server :: AppState -> Server API
server appState = enter' serveHome
             :<|> enter' serveSingle
             :<|> enter' serveEdit
             :<|> enter' serveNew
             :<|> (enter' serveLoginGet :<|> enter' serveLoginPost)
             :<|> enter' serveLogout
             :<|> serveStatic
    where
        enter' = enter (runReaderTNat appState)

requireAuth :: AppBare User
requireAuth = do
    mu <- fetch KUser
    case mu of
        Nothing -> lift $ left err401
        Just u -> return u

redirectTo :: URI -> AppBare a
redirectTo uri = lift $ left (err301 { errHeaders =
    [ ("Location", "/" <> fromString (show uri)) ] })

serveStatic :: Application
serveStatic = serveFile

serveHome :: AppM Homepage
serveHome = do
    mu <- fetch KUser
    es <- runDB GetAll
    let h = Homepage es
    return $ Rendered h (renderHome h mu)

serveSingle :: EssaySlug -> AppM Single
serveSingle sl = do
    mu <- fetch KUser
    e <- runDB $ SelectSlug sl
    case e of
        Nothing -> lift $ left err404
        Just x -> return $ Rendered (Single x) (renderSingle x mu)

serveEdit :: (EssaySlug -> AppM Single) :<|> (EssaySlug -> PartialEssay -> AppM Single)
serveEdit = serveEditGet :<|> serveEditPatch where
    serveEditGet sl = do
        _ <- requireAuth
        e <- runDB $ SelectSlug sl
        case e of
            Nothing -> lift $ left err404
            Just ese -> do
                fg <- getForm "essay" (essayForm $ Just ese)
                return $ Rendered (Single ese) (renderEdit ese fg)
    serveEditPatch sl part = do
        _ <- requireAuth
        e <- runDB $ SelectSlug sl
        case e of
            Nothing -> lift $ left err404
            Just ese -> do
                (_v, _me) <- postForm "essay" (essayForm $ Just ese) (efEnv part)
                case _me of
                    Nothing -> return $ Rendered (Single ese) (renderEdit ese _v)
                    Just newE -> do
                        execDB $ ReplaceSlug sl newE
                        redirectTo $ readLink (essaySlug newE)

serveNew :: Essay -> AppM Single
serveNew essay = do
    _ <- requireAuth
    execDB $ Insert essay
    lift $ left (err301 { errHeaders = [("Location", "/")] })

serveLoginGet :: AppM LoginPage
serveLoginGet = do
    mu <- fetch KUser
    case mu of
        Just _ -> redirectTo homeLink
        Nothing -> return $ Rendered undefined renderLogin

serveLoginPost :: LoginUser -> AppM ()
serveLoginPost user = do
    mu <- fetch KUser
    case mu of
        Just _ -> redirectTo homeLink
        Nothing ->
            if verifyPassword (encodeUtf8 $ password user) $(embedFile "important-secret")
                then do
                    set KUser $ User (username user)
                    redirectTo homeLink
                else do
                    set KMessage $ Message "Invalid username or password"
                    redirectTo loginLink

serveLogout :: AppBare ()
serveLogout = do
    _ <- requireAuth
    clear KUser
    redirectTo homeLink

serveApp :: IO Application
serveApp = do
    stateDir <- fromMaybe "db" <$> lookupEnv "STATE_DIR"
    database <- openLocalStateFrom stateDir (Database empty)
    k <- getDefaultKey
    vk <- V.newKey
    return $ withSession (clientsessionStore k) "_SESSION" (def { setCookiePath = Just "/" }) vk
           $ methodOverridePost
        $ \ req ->
           serve (Proxy :: Proxy API)
            (server (AppState k database (V.lookup vk $ vault req)))
            req
