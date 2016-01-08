{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Pages.Forms where

import           Control.Monad.IO.Class
import           Data.Char
import           Data.Maybe
import           Data.String
import qualified Data.Text                as T
import           Data.Time
import           Models
import           Servant.API.ContentTypes
import           Text.Digestive.Form
import           Text.Digestive.Types

essayForm :: (IsString v, Monoid v, MonadIO m)
          => Maybe Essay -> Form v m Essay
essayForm mEssay = fmap updateSlug $ monadic $ do
    time <- liftIO getCurrentTime
    return $ (\ t c -> Essay (EssayTitle t) (EssaySlug $ mkSlug t) (EssayContent c)
            (maybe (EssayCreatedAt time) essayCreatedAt mEssay))
        <$> "title" .: checkNotNull (text mtitle)
        <*> "content" .: checkNotNull (text mcontent)
    where
        mtitle = unTitle . essayTitle <$> mEssay
        mcontent = unContent . essayContent <$> mEssay
        checkNotNull = check "Can't be empty!" (/= mempty)
        updateSlug e@Essay{..} = e { essaySlug = EssaySlug $ mkSlug (unTitle essayTitle) }
        mkSlug = T.foldr (\ e m -> if (T.take 1 m, e) == ("-", '-') then m else T.cons e m) mempty
               . T.map (\ x -> if isAlphaNum x then x else '-')
               . T.toLower

updateE e PartialEssay{..} =
        e { essayTitle = maybe (essayTitle e) EssayTitle peTitle
          , essayContent = maybe (essayContent e) EssayContent peContent
          }

efEnv (toFormUrlEncoded -> ps) fenc = return $ \ path ->
    return $ maybeToList $ TextInput <$> lookup (fromPath path) ps
