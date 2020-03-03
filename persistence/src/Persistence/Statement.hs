{-# LANGUAGE OverloadedStrings #-}

module Persistence.Statement
  ( findByUsername
  , findById
  ) where

import Data.Int
import Data.List
import Data.Text
import Internal.Types
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Statement (Statement(..))

findByUsername :: Statement Text [(ScoreId, Username, Score, Level)]
findByUsername =
  let sql =
        "select id, username, score, game_level from scores where username like $1"
      encoders = E.param (E.nonNullable E.text)
      decoders =
        D.rowList $
        (,,,) <$> D.column (D.nonNullable D.int4) <*>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.int4) <*>
        D.column (D.nonNullable D.int4)
   in Statement sql encoders decoders True

findById :: Statement Int32 (Maybe (ScoreId, Username, Score, Level))
findById =
  let sql =
        "select id, username, score, game_level from scores where id = $1"
      encoders = E.param (E.nonNullable E.int4)
      decoders =
        D.rowMaybe $
        (,,,) <$> D.column (D.nonNullable D.int4) <*>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.int4) <*>
        D.column (D.nonNullable D.int4)
   in Statement sql encoders decoders True
