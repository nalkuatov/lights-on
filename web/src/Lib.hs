{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , tremendous
    ) where

import Data.Either
import Data.Score
import Web.Scotty
import Web.Routes (routes)
import qualified Persistence.Session as S

import Control.Monad.Except

someFunc :: IO ()
someFunc = do

  Right conn <- S.connection
  result <- S.run (S.findScores "tremendous") conn

  print result

tremendous :: IO ()
tremendous = do
  scotty 3000 $ 
    routes
