{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module API where

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Text                  (Text)
import           Database.Persist.Sql       (ConnectionPool, Entity (..), Key)
import           Models
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant
import           System.FilePath
import           Web.PathPieces

newtype Homepage = Homepage { unEssays :: [Entity Essay] } deriving ToJSON
newtype Single = Single { unSingle :: Entity Essay } deriving ToJSON

instance ToJSON a => ToJSON (Entity a) where
    toJSON (Entity _ v) = toJSON v

type AppM = ReaderT ConnectionPool (EitherT ServantErr IO)

-----------------------------------------------------------
-- Endpoint definition
-----------------------------------------------------------

type HomeE = Get '[JSON] Homepage

type ReadE = "r" :> Capture "slug" Text :> Get '[JSON] Single

type EditE = "e" :> Capture "id" EssayId :> ReqBody '[JSON] Essay :> Patch '[JSON] Single

type StaticE = "s" :> Raw

type API = HomeE
      :<|> ReadE
      :<|> EditE
      :<|> StaticE


-----------------------------------------------------------
-- URLs
-----------------------------------------------------------

homeLink :: URI
homeLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy HomeE)
readLink :: Text -> URI
readLink = safeLink (Proxy :: Proxy API) (Proxy :: Proxy ReadE)
staticLink :: FilePath -> URI
staticLink t = u { uriPath = uriPath u </> t } where
    u = safeLink (Proxy :: Proxy API) (Proxy :: Proxy StaticE)

apiErr :: Text -> [HTTP.Header] -> ServantErr
apiErr body = ServantErr 422 "Unprocessable Entity" (encode $ object ["error" .= (body :: Text)])

instance FromText (Key Essay) where fromText = fromPathPiece
