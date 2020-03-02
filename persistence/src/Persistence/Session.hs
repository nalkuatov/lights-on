{-# LANGUAGE OverloadedStrings #-}

module Persistence.Session
  ( S.run
  , S.Session
  , connection
  , byUsername
  ) where

import Data.Int
import Data.Text
import Hasql.Connection
import qualified Hasql.Session as S
import Persistence.Statement (findByUsername)

byUsername :: Text -> S.Session [(Int32, Text, Int32, Int32)]
byUsername username = S.statement formatted findByUsername
  where
    formatted = "%" <> username <> "%"

connection =
  acquire $ settings "localhost" 5432 "user" "password" "ice-age-squirrel"
