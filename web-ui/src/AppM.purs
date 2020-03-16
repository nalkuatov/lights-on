module Lightson.AppM 
  ( runAppM
  , AppM
  )
  where

import Prelude

import Lightson.Api.Endpoint (Endpoint(..))

import Control.Monad.Reader (ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Lightson.Api.Request.Verb (Verb(..))
import Lightson.Api.Util (decode, mkRequest)
import Lightson.Capability.Resource.Player (class ManagePlayer)

newtype AppM a = AppM (ReaderT String Aff a) 

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

runAppM :: forall a. String -> AppM a -> Aff a
runAppM value (AppM app) = runReaderT app value

instance managePlayerAppM :: ManagePlayer AppM where
  getPlayers username = 
    mkRequest 
      { endpoint: Scores { username }
      , verb: Get 
      }
      >>= decode
