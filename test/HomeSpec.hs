{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module HomeSpec where

import JudeWeb
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with serveApp $
    describe "/" $
        it "200" $
            get "/" `shouldRespondWith` [json|[]|]
