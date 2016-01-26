{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module API (module API, module API.PageTypes) where

import           API.PageTypes
import           Combinators
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           HTMLRendering
import           Models
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant
import           System.FilePath

type AppM a = AppBare (Rendered a)

type AppBare a = ReaderT AppState (ExceptT ServantErr IO) a

-----------------------------------------------------------
-- Endpoint definition
-----------------------------------------------------------

type HomeE = Get '[JSON, HTML] Homepage

type ReadE = "r" :> WithEssay :> Get '[JSON, HTML] Single

type GetNewE = "n" :> WithUser :> Get '[HTML] NewPage
type PutNewE = "n" :> WithUser :> ReqBody '[JSON, FormUrlEncoded] EssayNew
    :> (Verb 'PUT 400 '[HTML] NewPage
   :<|> PutNoContent '[JSON] ())

type GetEditE = "e" :> WithUser :> WithEssay :> Get '[HTML] EditPage
type PatchEditE = "e" :> WithUser :> WithEssay :>
    ReqBody '[JSON, FormUrlEncoded] EssayUpdate
        :> (Verb 'PATCH 400 '[HTML] EditPage
       :<|> PatchNoContent '[JSON] ())

type DeleteE = "d" :> WithUser :> WithEssay
    :> (DeleteNoContent '[HTML] Void
   :<|> DeleteNoContent '[JSON] ())

type GetLoginE = "in" :> Get '[HTML] LoginPage
type PostLoginE = "in" :> ReqBody '[FormUrlEncoded] LoginUser
    :> Verb 'POST 400 '[HTML] LoginPage

type LogoutE = "out" :> WithUser :> Get '[PlainText] LogoutPage

type StaticE = "s" :> Raw

type API = HomeE
      :<|> ReadE
      :<|> (GetNewE :<|> PutNewE)
      :<|> (GetEditE :<|> PatchEditE)
      :<|> DeleteE
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
newLink :: URI
newLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy GetNewE)
editLink :: EssaySlug -> URI
editLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy GetEditE)
deleteLink :: EssaySlug -> URI
deleteLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy ("d" :> WithUser :> WithEssay :> DeleteNoContent '[HTML] Void))
staticLink :: FilePath -> URI
staticLink t = u { uriPath = uriPath u </> t } where
    u = safeLink (Proxy :: Proxy API) (Proxy :: Proxy StaticE)

apiErr :: Text -> [HTTP.Header] -> ServantErr
apiErr body = ServantErr 422 "Unprocessable Entity" (encode $ object ["error" .= (body :: Text)])
