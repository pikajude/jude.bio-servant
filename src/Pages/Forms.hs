{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Pages.Forms where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Maybe
import           Data.String
import           Data.Text                   (Text)
import           Data.Time
import           Models
import           Servant.API.ContentTypes
import qualified Text.Blaze.Html5            as H hiding (label)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive.Blaze.Html5  as H
import           Text.Digestive.Form
import           Text.Digestive.Types
import           Text.Digestive.View         (View, errors)

essayForm :: (MonadReader AppState m, MonadIO m, Monoid v, IsString v)
          => Maybe Essay -> Form v m Essay
essayForm mEssay = monadic $ do
    t <- liftIO getCurrentTime
    return $ (\ title content -> Essay
        (EssayTitle title)
        (EssaySlug $ mkSlug title)
        (EssayContent content)
        (fromMaybe (EssayCreatedAt t) (essayCreatedAt <$> mEssay)))
        <$> "title" .: validateSlug (checkNotNull (text mtitle))
        <*> "content" .: checkNotNull (text mcontent)
    where
        mtitle = unTitle . essayTitle <$> mEssay
        mcontent = unContent . essayContent <$> mEssay
        checkNotNull = check "Can't be empty!" (/= mempty)
        validateSlug
            | isJust mEssay = id
            | otherwise = validateM $ \ t -> do
                let slug = mkSlug t
                existing <- runDB $ SelectSlug (EssaySlug slug)
                case existing of
                    Nothing -> return (return t)
                    Just{} -> return $ Error "This title conflicts with an existing title"

efEnv :: (Monad m, Monad n, ToFormUrlEncoded a)
      => a -> t -> m (Path -> n [FormInput])
efEnv (toFormUrlEncoded -> ps) _fenc = return $ \ path ->
    return $ maybeToList $ TextInput <$> lookup (fromPath path) ps

foundationField :: H.ToMarkup a => String -> Text -> View a -> r -> H.Html
foundationField t r (fmap H.toHtml -> v) _ = H.div H.! A.class_ dclass $ do
    H.label r v $ do
        H.toHtml (t :: String)
        tagger r v H.! A.class_ "form-control"
    forM_ (errors r v) $ \ e ->
        H.small H.! A.class_ "error" $ e
    where
        dclass = H.toValue ("form-group" ++ (if not (null (errors r v)) then " error" else "") :: String)
        tagger "password" = H.inputPassword "password"
        tagger "content" = H.inputTextArea (Just 10) Nothing "content"
        tagger t' = H.inputText t'
