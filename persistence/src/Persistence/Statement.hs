{-# LANGUAGE OverloadedStrings #-}

module Persistence.Statement
  ( findByUsername
  , findById
  , findByLevel
  , insert
  ) where

import Data.Int
import Data.List hiding (insert)
import Data.Text
import Internal.Types
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Contravariant.Extras.Contrazip (contrazip4)
import Hasql.Statement (Statement(..))

insert :: Statement (ScoreId, Username, Score, Level) Int32
insert = 
  let sql = 
        "insert into scores (id, username, score, game_level) \ 
         \ values ($1, $2, $3, $4) returning id"
      encoders = 
        contrazip4 
          (E.param (E.nonNullable E.int4))
          (E.param (E.nonNullable E.text))
          (E.param (E.nonNullable E.int4))
          (E.param (E.nonNullable E.int4))
      decoders =
        D.singleRow $ (D.column . D.nonNullable) D.int4
  in Statement sql encoders decoders True

findByLevel :: Statement Int32 [(ScoreId, Username, Score, Level)]
findByLevel =
  let sql = 
        "select id, username, score, game_level from scores where game_level = $1"
      encoders = E.param (E.nonNullable E.int4)
      decoders =
        D.rowList $
        (,,,) <$> D.column (D.nonNullable D.int4) <*>
        D.column (D.nonNullable D.text) <*>
        D.column (D.nonNullable D.int4) <*>
        D.column (D.nonNullable D.int4)
   in Statement sql encoders decoders True

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
