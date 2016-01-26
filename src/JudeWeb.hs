{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module JudeWeb where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Acid
import           Data.Aeson                                (encode)
import           Data.ByteString.UTF8                      (fromString)
import           Data.IxSet                                hiding (Proxy)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Vault.Lazy                           as V
import           Data.Void                                 (Void)
import           Models.SessionData
import           Network.Wai                               (Application, vault)
import           Network.Wai.Middleware.MethodOverridePost
import           Network.Wai.Session
import           Network.Wai.Session.ClientSession
import           Servant.API
import           Servant.Docs                              hiding (API)
import           Servant.Server
import           StaticFiles
import           Text.Digestive.View
import           Web.ClientSession                         hiding (Key)
import           Web.Cookie

import           API
import           API.Docs                                  ()
import           Models

import           Pages.Edit                                ()
import           Pages.Forms
import           Pages.Home                                ()
import           Pages.Login
import           Pages.New                                 ()
import           Pages.Single                              ()

type APIWithDocs = API :<|> ("docs.md" :> Get '[PlainText] String)

server :: AppState -> Server APIWithDocs
server appState = (enter' serveHome
             :<|> enter' serveSingle
             :<|> enter' serveNew
             :<|> enter' serveEdit
             :<|> enter' serveDelete
             :<|> (enter' serveLoginGet :<|> enter' serveLoginPost)
             :<|> enter' serveLogout
             :<|> serveStatic)
             :<|> return (markdown $ docs (Proxy :: Proxy API))
    where
        enter' = enter (runReaderTNat appState)

redirectTo :: URI -> AppBare a
redirectTo uri = lift $ throwE (err301 { errHeaders =
    [ ("Location", "/" <> fromString (show uri)) ] })

serveStatic :: Application
serveStatic = serveFile

serveHome :: AppBare Homepage
serveHome = do
    mu <- fetch KUser
    mm <- fetch KMessage
    clear KMessage
    es <- runDB GetAll
    return $ Homepage es mu mm

serveSingle :: Essay -> AppBare Single
serveSingle e = do
    mu <- fetch KUser
    return $ Single e mu

serveEdit :: ToFormUrlEncoded a
          => (User -> Essay -> AppBare EditPage)
        :<|> (User -> Essay -> a -> AppBare EditPage :<|> AppBare ())
serveEdit = serveEditGet :<|> serveEditPatch where
    serveEditGet _ ese = do
        fg <- getForm "essay" (essayForm $ Just ese)
        return $ EditPage ese fg

serveEditPatch :: ToFormUrlEncoded r
               => User -> Essay -> r -> AppBare EditPage :<|> AppBare ()
serveEditPatch _ ese part = sHtml :<|> sJson where
    common = do
        (_v, _me) <- postForm "essay" (essayForm $ Just ese) (efEnv part)
        case _me of
            Nothing -> return $ Left $ EditPage ese _v
            Just newE -> do
                execDB $ ReplaceSlug (essaySlug ese) newE
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

serveNew :: (User -> AppBare NewPage)
       :<|> (User -> EssayNew -> (AppBare NewPage :<|> AppBare ()))
serveNew = serveNewGet :<|> serveNewPut where
    serveNewGet mu = do
        fg <- getForm "essay" (essayForm Nothing)
        return $ NewPage fg mu

serveNewPut :: User -> EssayNew -> AppBare NewPage :<|> AppBare ()
serveNewPut mu part = serveNewHtml :<|> serveNewJson where
    serveNewHtml = do
        res <- inserter
        case res of
            Left m -> return m
            Right (_, Just s) -> error s
            Right (e, Nothing) -> redirectTo $ readLink (essaySlug e)
    serveNewJson = do
        res <- inserter
        case res of
            Left m -> lift $ throwE (err400 { errBody = encode m })
            Right (_, Just s) -> error s
            Right (_, Nothing) -> return ()
    inserter = do
        (_v, _me) <- postForm "essay" (essayForm Nothing) (efEnv part)
        case _me of
            Nothing -> return $ Left $ NewPage _v mu
            Just essay -> Right . (,) essay <$> execDB (Insert essay)

serveDelete :: User -> Essay -> (AppBare Void :<|> AppBare ())
serveDelete _ (essaySlug -> slug) =
        (do worked <- common
            if worked
                then set KMessage $ Message "Successfully deleted."
                else set KMessage $ Message "Essay not found."
            redirectTo homeLink)
   :<|> (do worked <- common
            unless worked $ lift $ throwE err404)
    where common = execDB $ Delete slug

serveLoginGet :: AppBare LoginPage
serveLoginGet = do
    mu <- fetch KUser
    case mu of
        Just _ -> redirectTo homeLink
        Nothing -> do
            v <- getForm "login" loginForm
            return $ LoginPage v

serveLoginPost :: LoginUser -> AppBare LoginPage
serveLoginPost user = do
    mu <- fetch KUser
    case mu of
        Just _ -> redirectTo homeLink
        Nothing -> do
            (v, ml) <- postForm "login" loginForm (efEnv user)
            case ml of
                Nothing -> return $ LoginPage v
                Just u -> do
                    set KUser $ User (username u)
                    redirectTo homeLink

serveLogout :: User -> AppBare LogoutPage
serveLogout _ = do
    clear KUser
    redirectTo homeLink

serveApp :: IO Application
serveApp = do
    database <- openLocalStateFrom "db" (Database empty)
    k <- getDefaultKey
    vk <- V.newKey
    let cfg = database :. vk :. EmptyConfig
    return $ methodOverridePost
           $ withSession (clientsessionStore k) "_SESSION" (def { setCookiePath = Just "/" }) vk
           $ \ req -> serve (Proxy :: Proxy APIWithDocs)
                 cfg
                 (server (AppState k database (V.lookup vk $ vault req)))
                 req
