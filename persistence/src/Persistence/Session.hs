{-# LANGUAGE OverloadedStrings #-}

module Persistence.Session
  ( S.run
  , S.Session
  , connection
  , byUsername
  , byLevel
  , byId
  ) where

import Data.Int
import Data.Text
import Hasql.Connection
import qualified Hasql.Session as S
import Internal.Types
import Persistence.Statement

byLevel :: Int32 -> S.Session [(ScoreId, Username, Score, Level)]
byLevel level = S.statement level findByLevel

byUsername :: Text -> S.Session [(ScoreId, Username, Score, Level)]
byUsername username = S.statement formatted findByUsername
  where
    formatted = "%" <> username <> "%"

byId :: Int32 -> S.Session (Maybe (ScoreId, Username, Score, Level))
byId id = S.statement id findById

connection =
  acquire $ settings "localhost" 5432 "user" "password" "ice-age-squirrel"
