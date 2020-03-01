{-# LANGUAGE OverloadedStrings #-}

module Persistence.Statement 
  ( findByUsername
  , dummy
  )
  where

import Data.Int
import Data.Text
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


findByUsername :: Statement Text Int32
findByUsername =
  let 
    sql = "select id from scores where username = $1"
    encoders = (E.param (E.nonNullable E.text))
    decoders = D.singleRow $ (D.column . D.nonNullable) D.int4
  in 
    Statement sql encoders decoders True

dummy :: Statement () Int64
dummy = undefined
