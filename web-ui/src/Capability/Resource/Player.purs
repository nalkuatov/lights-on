module Lightson.Capability.Resource.Player 
  where

import Lightson.Data.Player (Player, Username)
import Control.Monad (class Monad)

class Monad m <= ManagePlayer m where
  getPlayers :: Username -> m (Array Player)
