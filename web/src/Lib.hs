{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( app
  , sample
  ) where

import qualified Persistence.Session as S
import Web.Routes (routes)
import Web.Scotty.Trans (scottyT)

sample :: IO ()
sample = do
  Right conn <- S.connection
  result <- S.run (S.byUsername "tremendous") conn
  print result

app :: IO ()
app = scottyT 3000 runIO routes
  where
    runIO :: S.Session a -> IO a
    runIO session = do
      Right conn <- S.connection
      Right result <- S.run session conn
      return result
