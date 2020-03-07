module Lightson.Component.Util 
  where

import Data.Const (Const)
import Halogen as H
import Prelude (Void)

type OpaqueSlot = H.Slot (Const Void) Void
