{-# LANGUAGE OverloadedStrings #-}

module Web.Routes
  ( routes
  ) where

import Control.Monad.Trans (lift)
import Data.Score
import Data.Text (empty)
import Data.Text.Lazy (Text)
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import qualified Persistence.Session as S
import Web.Scotty.Trans

routes :: ScottyT Text S.Session ()
routes = do

  middleware logStdoutDev

  let converted (id, username, score, game_level) =
        Score
          (fromIntegral id)
          username
          (toGameLevel $ fromIntegral game_level)
          (fromIntegral score)

  get "/scores" $ do
    username <- param "username" `rescue` (\_ -> return empty)
    result <- lift $ S.byUsername username
    json $ converted <$> result

  get "/scores/:id" $ do
    scoreId <- param "id"
    result <- lift $ S.byId scoreId
    case result of 
      Nothing -> status status404 
      Just _ -> return ()
    json $ converted <$> result
