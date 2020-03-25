module Lightson.Capability.Resource.Player 
  where

import Control.Monad (class Monad)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Lightson.Data.Player (Player, PlayerId)
import Lightson.Api.Endpoint
import Prelude ((<<<))

class Monad m <= ManagePlayer m where
  createPlayer :: Player -> m (Maybe PlayerId)
  getPlayers :: ScoreParams -> m (Maybe (Array Player))

instance managePlayerHalogenM 
  :: ManagePlayer m 
  => ManagePlayer (HalogenM st act slots msg m) where
  createPlayer = lift <<< createPlayer
  getPlayers = lift <<< getPlayers
