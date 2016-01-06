{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Session (Session, SessionStore(..), toCookie) where

import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder
import           Data.ByteString.Conversion
import           Data.ByteString.Lazy       (toStrict)
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text)
import qualified Data.Text                  as T (length)
import           Data.Text.Encoding
import           Data.Word
import           Network.Wai
import           Servant.API                ((:>))
import           Servant.Server.Internal
import           Servant.Utils.Links
import           Web.ClientSession
import           Web.Cookie

data Session

data SessionStore = SessionStore
                  { sessionUser    :: Maybe Text
                  , sessionMessage :: Maybe Text
                  } deriving Show

defaultSessionStore :: SessionStore
defaultSessionStore = SessionStore Nothing Nothing

instance ToByteString SessionStore where
    builder SessionStore{..} = render' sessionUser <> render' sessionMessage
        where
            render' Nothing = word8 0
            render' (Just x) = word8 1 <> word16BE (fromIntegral $ T.length x) <> builder x

instance FromByteString SessionStore where
    parser = SessionStore <$> parse <*> parse where
        parse = do
            flag <- A.anyWord8
            case flag of
                0 -> pure Nothing
                1 -> do
                    len <- do
                        w1 <- A.anyWord8
                        w2 <- A.anyWord8
                        return $ fromIntegral w1 * 256 + fromIntegral w2
                    bytes <- A.take $ fromIntegral (len :: Word16)
                    return $ case decodeUtf8' bytes of
                        Left _ -> Nothing
                        Right x -> Just x
                _ -> error "unknown flag"

instance HasServer sublayout => HasServer (Session :> sublayout) where
  type ServerT (Session :> sublayout) m
    = (Key -> SessionStore) -> ServerT sublayout m
  route Proxy a = WithRequest
        $ \request -> route (Proxy :: Proxy sublayout)
        $ passToServer a (go request)
      where
        go req key = case s of
                         Nothing -> defaultSessionStore
                         Just v -> fromMaybe defaultSessionStore $ decrypt key v >>= fromByteString
            where
                s = do
                    c <- lookup "Cookie" $ requestHeaders req
                    lookup sessionCookieName $ parseCookies c

instance HasLink sublayout => HasLink (Session :> sublayout) where
    type MkLink (Session :> sublayout) = MkLink sublayout

    toLink Proxy = toLink (Proxy :: Proxy sublayout)

toCookie :: Key -> SessionStore -> IO SetCookie
toCookie k s = do
    c <- encryptIO k (toStrict $ toByteString s)
    return (def { setCookieName = sessionCookieName, setCookieValue = c, setCookiePath = Just "/" })

sessionCookieName :: ByteString
sessionCookieName = "_SESSION"
