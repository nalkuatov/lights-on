module Lightson.Api.Request 
  where

import Affjax (Request)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lightson.Api.Endpoint (Endpoint)
import Lightson.Api.Endpoint as Endpoint
import Lightson.Api.Request.Verb (Verb)
import Lightson.Api.Request.Verb as Verb
import Prelude ((<$>), (<>))

type BaseURL = String

mkRequest :: BaseURL -> Verb -> Endpoint -> Request Json
mkRequest url verb endpoint =
  { method: Left (Verb.method verb)
  , url: url <> Endpoint.print endpoint
  , headers: []
  , content: RequestBody.json <$> Verb.body verb
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: ResponseFormat.json
  }
