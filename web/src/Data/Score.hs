{-# LANGUAGE OverloadedStrings #-}

module Data.Score
  ( Score(..)
  , GameLevel(..)
  , toGameLevel
  , toInt
  ) where

import Data.Aeson
import Data.Text (Text, toLower)

toGameLevel :: Int -> GameLevel
toGameLevel 1 = Easy
toGameLevel 2 = Medium
toGameLevel 3 = Hard

toInt :: GameLevel -> Int
toInt Easy = 1
toInt Medium = 2
toInt Hard = 3

data GameLevel
  = Easy
  | Medium
  | Hard
  deriving (Show, Eq)

data Score =
  Score
    { id :: Int
    , username :: Text
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
    Score <$> 
      score .:? "id" .!= (-1) <*> 
      score .: "username" <*> 
      score .: "level" <*>
      score .: "score"
  parseJSON _ = fail "invalid score"

instance ToJSON Score where
  toJSON (Score id username level time) =
    object ["id" .= id, "username" .= username, "level" .= show level, "score" .= time]
