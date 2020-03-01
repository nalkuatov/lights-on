{-# LANGUAGE OverloadedStrings #-}

module Persistence.Session 
  ( S.run
  , S.CommandError(..)
  , S.QueryError(..)
  , connection
  , findScores
  )
  where

import qualified Hasql.Session as S
import qualified Persistence.Statement as Statement
import Data.Text
import Data.Int

import Hasql.Connection
import Data.Either

findScores :: Text -> S.Session Int32
findScores username = 
  S.statement username $ Statement.findByUsername

connection = acquire 
           $ settings "localhost" 
                      5432 
                      "user" 
                      "password" 
                      "ice-age-squirrel"
