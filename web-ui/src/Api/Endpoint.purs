module Lightson.Api.Endpoint 
  ( Endpoint(..)
  , print
  , codec
  )
  where

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Lightson.Data.Player (PlayerId)
import Prelude (class Show, ($), (<<<))
import Routing.Duplex (RouteDuplex', root, segment, int, params, optional, string)
import Routing.Duplex (print) as Route
import Routing.Duplex.Generic (sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = Scores { username :: Maybe String }
  | Score PlayerId

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint
  where show = genericShow

codec :: RouteDuplex' Endpoint
codec = root $ sum 
  { "Scores": "scores" / params { username: optional <<< string }
  , "Score":  "scores" / int segment
  }

print :: Endpoint -> String
print = Route.print codec
