module Lightson.Api.Endpoint 
  ( Endpoint(..)
  , print
  )
  where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Lightson.Data.Player (PlayerId)
import Prelude (class Show, ($))
import Routing.Duplex (RouteDuplex', root, segment, int)
import Routing.Duplex (print) as Route
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = Players
  | Player PlayerId

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint
  where show = genericShow

codec :: RouteDuplex' Endpoint
codec = root $ sum 
  { "Players": "players" / noArgs
  , "Player":  "players" / int segment
  }

print :: Endpoint -> String
print = Route.print codec
