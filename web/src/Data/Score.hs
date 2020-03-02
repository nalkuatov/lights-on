{-# LANGUAGE OverloadedStrings #-}

module Data.Score
  ( Score(..)
  , GameLevel(..)
  , toGameLevel
  ) where

import Data.Aeson
import Data.Text (Text, toLower)

toGameLevel :: Int -> GameLevel
toGameLevel 1 = Easy
toGameLevel 2 = Medium
toGameLevel 3 = Hard

data GameLevel
  = Easy
  | Medium
  | Hard
  deriving (Show, Eq)

data Score =
  Score
    { username :: Text
    , level :: GameLevel
    , timeSpent :: Int
    }
  deriving (Eq, Show)

instance FromJSON GameLevel where
  parseJSON (String level) =
    case toLower level of
      "easy" -> return Easy
      "medium" -> return Medium
      "hard" -> return Hard
  parseJSON _ = fail "invalid game level"

instance FromJSON Score where
  parseJSON (Object score) =
    Score <$> score .: "username" <*> score .: "level" <*> score .: "time-spent"
  parseJSON _ = fail "invalid score"

instance ToJSON Score where
  toJSON (Score username level time) =
    object ["username" .= username, "level" .= show level, "time-spent" .= time]
