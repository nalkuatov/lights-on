module Lightson.Capability.Resource.Player 
  where

import Control.Monad (class Monad)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Lightson.Data.Player (Player)
import Lightson.Api.Endpoint
import Prelude ((<<<))

class Monad m <= ManagePlayer m where
  getPlayers :: ScoreParams -> m (Maybe (Array Player))

instance managePlayerHalogenM 
  :: ManagePlayer m 
  => ManagePlayer (HalogenM st act slots msg m) where
  getPlayers = lift <<< getPlayers
