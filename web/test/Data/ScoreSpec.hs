{-# LANGUAGE OverloadedStrings #-}

module Data.ScoreSpec
  ( spec
  ) where

import Data.Aeson
import Data.Score
import Test.Hspec

spec :: Spec
spec =
  describe "json convertions" $ do
    let repr =
          "{\"score\":130,\"username\":\"username\",\"id\":1,\"level\":\"Easy\"}"
    it "should convert to json" $
      encode (Score 1 "username" Easy 130) `shouldBe` repr
    it "should parse json" $
      decode repr `shouldBe` (Just $ Score 1 "username" Easy 130)
