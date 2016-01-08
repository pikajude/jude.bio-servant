{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module API where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Text                  (Text)
import           HTMLRendering
import           Models
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant
import           System.FilePath

newtype Homepage = Homepage { unEssays :: [Essay] } deriving ToJSON
newtype Single = Single { unSingle :: Essay }
data LoginPage = LoginPage

instance ToJSON Single where
    toJSON (Single e) = toJSON e

type AppM a = AppBare (Rendered a)

type AppBare a = ReaderT AppState (ExceptT ServantErr IO) a

-----------------------------------------------------------
-- Endpoint definition
-----------------------------------------------------------

type HomeE = Get '[JSON, HTML] (Rendered Homepage)

type ReadE = "r" :> Capture "slug" EssaySlug :> Get '[JSON, HTML] (Rendered Single)

type GetEditE = "e" :> Capture "slug" EssaySlug :> Get '[HTML] (Rendered Single)
type PostEditE = "e" :> Capture "slug" EssaySlug :>
    ReqBody '[JSON, FormUrlEncoded] PartialEssay
        :> Patch '[JSON, HTML] (Rendered Single)

type NewE = "n" :> ReqBody '[JSON] Essay :> Put '[HTML] (Rendered Single)

type GetLoginE = "in" :> Get '[HTML] (Rendered LoginPage)
type PostLoginE = "in" :> ReqBody '[FormUrlEncoded, JSON] LoginUser :> Post '[HTML] (Rendered ())

type LogoutE = "out" :> Get '[HTML] ()

type StaticE = "s" :> Raw

type API = HomeE
      :<|> ReadE
      :<|> (GetEditE :<|> PostEditE)
      :<|> NewE
      :<|> (GetLoginE :<|> PostLoginE)
      :<|> LogoutE
      :<|> StaticE


-----------------------------------------------------------
-- URLs
-----------------------------------------------------------

homeLink :: URI
homeLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy HomeE)
loginLink :: URI
loginLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy GetLoginE)
readLink :: EssaySlug -> URI
readLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy ReadE)
editLink :: EssaySlug -> URI
editLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy GetEditE)
staticLink :: FilePath -> URI
staticLink t = u { uriPath = uriPath u </> t } where
    u = safeLink (Proxy :: Proxy API) (Proxy :: Proxy StaticE)

apiErr :: Text -> [HTTP.Header] -> ServantErr
apiErr body = ServantErr 422 "Unprocessable Entity" (encode $ object ["error" .= (body :: Text)])
