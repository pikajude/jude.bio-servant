{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeOperators             #-}

module JudeWeb where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Crypto.PasswordStore                      (verifyPassword)
import           Data.Acid
import           Data.Aeson                                (encode)
import           Data.ByteString.UTF8                      (fromString)
import           Data.FileEmbed
import           Data.IxSet                                hiding (Proxy)
import           Data.Monoid
import           Data.Proxy
import           Data.Text.Encoding
import qualified Data.Vault.Lazy                           as V
import           Models.SessionData
import           Network.Wai                               (Application, vault)
import           Network.Wai.Middleware.MethodOverridePost
import           Network.Wai.Session
import           Network.Wai.Session.ClientSession
import           Servant.API
import           Servant.Server
import           StaticFiles
import           Text.Digestive.View
import           Web.ClientSession                         hiding (Key)
import           Web.Cookie

import           API
import           Models

import           Pages.Edit                                ()
import           Pages.Forms
import           Pages.Home                                ()
import           Pages.Login                               ()
import           Pages.New                                 ()
import           Pages.Single                              ()

server :: AppState -> Server API
server appState = enter' serveHome
             :<|> enter' serveSingle
             :<|> enter' serveNew
             :<|> enter' serveEdit
             :<|> (enter' serveLoginGet :<|> enter' serveLoginPost)
             :<|> enter' serveLogout
             :<|> serveStatic
    where
        enter' = enter (runReaderTNat appState)

requireAuth :: AppBare User
requireAuth = do
    mu <- fetch KUser
    case mu of
        Nothing -> lift $ throwE err401
        Just u -> return u

redirectTo :: URI -> AppBare a
redirectTo uri = lift $ throwE (err301 { errHeaders =
    [ ("Location", "/" <> fromString (show uri)) ] })

serveStatic :: Application
serveStatic = serveFile

serveHome :: AppBare Homepage
serveHome = do
    mu <- fetch KUser
    es <- runDB GetAll
    return $ Homepage es mu

serveSingle :: EssaySlug -> AppBare Single
serveSingle sl = do
    mu <- fetch KUser
    e <- runDB $ SelectSlug sl
    case e of
        Nothing -> lift $ throwE err404
        Just x -> return $ Single x mu

serveEdit :: ToFormUrlEncoded a
          => (EssaySlug -> AppBare EditPage)
        :<|> (EssaySlug -> a -> AppBare EditPage :<|> AppBare ())
serveEdit = serveEditGet :<|> serveEditPatch where
    serveEditGet sl = do
        mu <- requireAuth
        e <- runDB $ SelectSlug sl
        case e of
            Nothing -> lift $ throwE err404
            Just ese -> do
                fg <- getForm "essay" (essayForm $ Just ese)
                return $ EditPage ese fg mu

serveEditPatch :: ToFormUrlEncoded r
               => EssaySlug -> r -> AppBare EditPage :<|> AppBare ()
serveEditPatch sl part = sHtml :<|> sJson where
    common = do
        mu <- requireAuth
        e <- runDB $ SelectSlug sl
        case e of
            Nothing -> lift $ throwE err404
            Just ese -> do
                (_v, _me) <- postForm "essay" (essayForm $ Just ese) (efEnv part)
                case _me of
                    Nothing -> return $ Left $ EditPage ese _v mu
                    Just newE -> do
                        execDB $ ReplaceSlug sl newE
                        return $ Right newE
    sHtml = do
        mep <- common
        case mep of
            Left ep -> return ep
            Right e -> redirectTo (readLink (essaySlug e))
    sJson = do
        mep <- common
        case mep of
            Left ep -> lift $ throwE (err400 { errBody = encode ep })
            Right _ -> return ()

serveNew :: AppBare NewPage :<|> (EssayNew -> (AppBare NewPage :<|> AppBare ()))
serveNew = serveNewGet :<|> serveNewPatch where
    serveNewGet = do
        mu <- requireAuth
        fg <- getForm "essay" (essayForm Nothing)
        return $ NewPage fg mu

serveNewPatch :: EssayNew -> AppBare NewPage :<|> AppBare ()
serveNewPatch part = serveNewHtml :<|> serveNewJson where
    serveNewHtml = do
        res <- inserter
        case res of
            Left m -> return m
            Right (Just s) -> error s
            Right Nothing -> redirectTo homeLink
    serveNewJson = do
        res <- inserter
        case res of
            Left m -> lift $ throwE (err400 { errBody = encode m })
            Right (Just s) -> error s
            Right Nothing -> return ()
    inserter = do
        mu <- requireAuth
        (_v, _me) <- postForm "essay" (essayForm Nothing) (efEnv part)
        case _me of
            Nothing -> return $ Left $ NewPage _v mu
            Just essay -> Right <$> execDB (Insert essay)

serveLoginGet :: AppBare LoginPage
serveLoginGet = do
    mu <- fetch KUser
    case mu of
        Just _ -> redirectTo homeLink
        Nothing -> return LoginPage

serveLoginPost :: LoginUser -> AppBare LoginPage
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

serveLogout :: AppBare LogoutPage
serveLogout = do
    _ <- requireAuth
    clear KUser
    redirectTo homeLink

serveApp :: IO Application
serveApp = do
    database <- openLocalStateFrom "db" (Database empty)
    k <- getDefaultKey
    vk <- V.newKey
    return $ methodOverridePost
           $ withSession (clientsessionStore k) "_SESSION" (def { setCookiePath = Just "/" }) vk
           $ \ req -> serve (Proxy :: Proxy API)
                 (server (AppState k database (V.lookup vk $ vault req)))
                 req
