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
          "{\"username\":\"username\",\"time-spent\":130,\"level\":\"Easy\"}"
    it "should convert to json" $
      encode (Score "username" Easy 130) `shouldBe` repr
    it "should parse json" $
      decode repr `shouldBe` (Just $ Score "username" Easy 130)
