{-# LANGUAGE OverloadedStrings #-}

module Web.Routes
  ( routes
  ) where

import Control.Monad.Trans (lift)
import Data.Score
import Data.Text.Lazy (Text)
import qualified Persistence.Session as S
import Web.Scotty.Trans (ScottyT, get, json, param)

routes :: ScottyT Text S.Session ()
routes = do
  let converted (_, username, score, game_level) =
        Score
          username
          (toGameLevel $ fromIntegral game_level)
          (fromIntegral score)
  get "/scores" $ do
    username <- param "username"
    result <- lift $ S.byUsername username
    json $ converted <$> result
