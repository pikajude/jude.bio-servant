{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Combinators where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Acid
import           Data.ByteString         (ByteString)
import           Data.Serialize          (decode)
import qualified Data.Vault.Lazy         as V
import           Models
import           Models.SessionData
import           Network.Wai             (vault)
import           Network.Wai.Session
import           Servant
import           Servant.Server.Internal

-- | Grab an essay
data WithEssay

instance (HasConfigEntry cfg (AcidState Database), HasServer sub cfg)
        => HasServer (WithEssay :> sub) cfg where
    type ServerT (WithEssay :> sub) m = Essay -> ServerT sub m

    route Proxy cfg sub = DynamicRouter $ \ first ->
        route (Proxy :: Proxy sub)
              cfg
              (addCapture sub $ case captured captureProxy first of
                Nothing -> return $ Fail err404
                Just v -> do
                    e <- query (getConfigEntry cfg) $ SelectSlug v
                    case e of
                        Nothing -> return $ Fail err404
                        Just x -> return $ Route x)
        where
            captureProxy = Proxy :: Proxy (Capture "slug" EssaySlug)

instance HasLink sub => HasLink (WithEssay :> sub) where
    type MkLink (WithEssay :> sub) = MkLink (Capture "slug" EssaySlug :> sub)
    toLink Proxy = toLink (Proxy :: Proxy (Capture "slug" EssaySlug :> sub))

-- | Grab a user
data SessionVar a

type MySession = Session IO ByteString ByteString

type WithUser = SessionVar User

instance (HasConfigEntry cfg (V.Key MySession), HasServer sub cfg, SessionData a)
        => HasServer (SessionVar a :> sub) cfg where
    type ServerT (SessionVar a :> sub) m = a -> ServerT sub m

    route Proxy cfg sub = WithRequest $ \ req ->
        route (Proxy :: Proxy sub) cfg
            (addCapture sub $ do
                let Just (fetcher, _) = V.lookup vaultKey (vault req)
                val <- liftM (>>= em . decode) $ liftIO (fetcher (sessionKey (Proxy :: Proxy a)))
                return $ case val of
                    Nothing -> Fail err401
                    Just x -> Route x
            )
        where vaultKey = getConfigEntry cfg :: V.Key MySession
              em (Left _) = Nothing
              em (Right x) = Just x

instance HasLink sub => HasLink (SessionVar a :> sub) where
#if __GLASGOW_HASKELL__ >= 800
    type MkLink (SessionVar _ :> sub) = MkLink sub
#else
    type MkLink (SessionVar a :> sub) = MkLink sub
#endif
    toLink Proxy = toLink (Proxy :: Proxy sub)
