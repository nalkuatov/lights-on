{-# LANGUAGE OverloadedStrings #-}

module Persistence.Session
  ( S.run
  , S.Session
  , connection
  , byUsername
  , byLevel
  , byId
  , insert
  ) where

import Data.Int
import Data.Text
import Hasql.Connection
import qualified Hasql.Session as S
import Internal.Types
import qualified Persistence.Statement as Statement

insert :: (ScoreId, Username, Score, Level) -> S.Session Int32
insert value = S.statement value Statement.insert

byLevel :: Int32 -> S.Session [(ScoreId, Username, Score, Level)]
byLevel level = S.statement level Statement.findByLevel

byUsername :: Text -> S.Session [(ScoreId, Username, Score, Level)]
byUsername username = S.statement formatted Statement.findByUsername
  where
    formatted = "%" <> username <> "%"

byId :: Int32 -> S.Session (Maybe (ScoreId, Username, Score, Level))
byId id = S.statement id Statement.findById

connection =
  acquire $ settings "localhost" 5432 "user" "password" "ice-age-squirrel"
