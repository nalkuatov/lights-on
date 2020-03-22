{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Web.Routes
  ( routes
  ) where

import Prelude hiding (id)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Maybe (isNothing)
import Data.Score
import Data.Aeson (decode)
import Data.Aeson.Types (Parser)
import Data.Text (empty, toLower)
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

  let wrapped (id, username, score, game_level) =
        Score
          (fromIntegral id)
          username
          (toGameLevel $ fromIntegral game_level)
          (fromIntegral score)

      unwrapped Score { id, username, level, timeSpent } =
        (fromIntegral id, username, (fromIntegral . toInt) level, fromIntegral timeSpent)

      lvlToInt "easy" = 1
      lvlToInt "medium" = 2
      lvlToInt "hard" = 3

  post "/scores" $ do
    jsonBody <- body
    let score = decode jsonBody :: Maybe Score
    case score of
      Nothing -> do 
        status status415
        json ("couldn't decode json to Score" :: String)
      Just value -> do
        liftIO $ print $ unwrapped value
        result <- lift $ S.insert $ unwrapped value
        json result

  get "/scores" $ do
    level <- param "level" `rescue` (\_ -> return empty)
    result <- lift $ S.byLevel $ (lvlToInt . toLower) level
    json $ wrapped <$> result

  get "/scores/:id" $ do
    scoreId <- param "id"
    result <- lift $ S.byId scoreId
    when (isNothing result) $ 
      status status404
    json $ wrapped <$> result
