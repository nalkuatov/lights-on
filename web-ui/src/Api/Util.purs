module Lightson.Api.Util where

import Lightson.Api.Request

import Affjax (request)
import Control.Applicative (class Applicative)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Prelude (($), bind, pure)

mkRequest :: forall m. MonadAff m => RequestOptions -> m (Maybe Json)
mkRequest opts = do
  let url = "http://localhost:3001"
  response <- liftAff $ request $ defaultRequest url opts
  pure $ hush $ rmap _.body response

decode :: forall m a
  .  Applicative m 
  => DecodeJson a => Maybe Json -> m (Maybe a)
decode Nothing = pure Nothing
decode (Just value) = case decodeJson value of
  Left e -> pure Nothing
  Right r -> pure $ Just r
