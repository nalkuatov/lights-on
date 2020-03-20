{-# LANGUAGE OverloadedStrings #-}

module Web.Routes
  ( routes
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Maybe (isNothing)
import Data.Score
import Data.Text (empty)
import Data.Text.Lazy (Text)
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import qualified Persistence.Session as S
import Web.Scotty.Trans

routes :: ScottyT Text S.Session ()
routes = do

  middleware logStdoutDev
  middleware simpleCors

  let converted (id, username, score, game_level) =
        Score
          (fromIntegral id)
          username
          (toGameLevel $ fromIntegral game_level)
          (fromIntegral score)

      toInt "easy" = 1
      toInt "medium" = 2
      toInt "hard" = 3

  get "/scores" $ do
    level <- param "level" `rescue` (\_ -> return empty)
    liftIO $ print $ toInt level
    result <- lift $ S.byLevel $ toInt level
    json $ converted <$> result

  get "/scores/:id" $ do
    scoreId <- param "id"
    result <- lift $ S.byId scoreId
    when (isNothing result) $ 
      status status404
    json $ converted <$> result
