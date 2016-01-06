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
import           Data.ByteString.Conversion.To
import           Data.Text                     (Text)
import           HTMLRendering
import           Models
import qualified Network.HTTP.Types.Header     as HTTP
import           Servant
import           System.FilePath
import           Web.Cookie

newtype Homepage = Homepage { unEssays :: [Essay] } deriving ToJSON
newtype Single = Single { unSingle :: Essay } deriving ToJSON
data LoginPage = LoginPage

type AppM a = ReaderT AppState (ExceptT ServantErr IO) (Rendered a)

type ReturnsCookie = Headers '[Header "Set-Cookie" (Maybe SetCookie)]

instance ToByteString (Maybe SetCookie) where
    builder Nothing = mempty
    builder (Just sc) = renderSetCookie sc

-----------------------------------------------------------
-- Endpoint definition
-----------------------------------------------------------

type HomeE = Get '[JSON, HTML] (Rendered Homepage)

type ReadE = "r" :> Capture "slug" EssaySlug :> Get '[JSON, HTML] (Rendered Single)

type EditE = "e" :> Capture "slug" EssaySlug :>
    (Get '[HTML] (Rendered Single)
        :<|> ReqBody '[JSON] Essay :> Patch '[JSON, HTML] (Rendered Single))

type NewE = "n" :> ReqBody '[JSON] Essay :> Put '[HTML] (Rendered Single)

type LoginE = "in" :> (Get '[HTML] (Rendered LoginPage)
                  :<|> ReqBody '[FormUrlEncoded, JSON] User :> Post '[HTML] (Rendered ()))

type StaticE = "s" :> Raw

type API = HomeE
      :<|> ReadE
      :<|> EditE
      :<|> NewE
      :<|> LoginE
      :<|> StaticE


-----------------------------------------------------------
-- URLs
-----------------------------------------------------------

homeLink :: URI
homeLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy HomeE)
loginLink :: URI
loginLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy ("in" :> Get '[HTML] (Rendered LoginPage)))
readLink :: EssaySlug -> URI
readLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy ReadE)
staticLink :: FilePath -> URI
staticLink t = u { uriPath = uriPath u </> t } where
    u = safeLink (Proxy :: Proxy API) (Proxy :: Proxy StaticE)

apiErr :: Text -> [HTTP.Header] -> ServantErr
apiErr body = ServantErr 422 "Unprocessable Entity" (encode $ object ["error" .= (body :: Text)])
