module Lightson.Component.Lights 
  ( lights 
  , component
  )
  where

import Lightson.Data.Game
import Lightson.Component.HTML.Util (css)

import Data.Array ((..))
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))
import Prelude (Void, Unit, (<$>), ($), (>>=))
import Data.Newtype (unwrap)

type State = 
  { game :: Maybe Game
  }

component
  :: forall q i m
  .  H.Component HH.HTML q i Void m
component = 
  H.mkComponent 
    { initialState: (\_ -> { game: Just levelThree })
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
    render 
      :: forall a
      . State 
      -> H.ComponentHTML a () m
    render { game } = lights $ (\g -> (unwrap g).size) <$> game

    handleAction
      :: forall action
      .  action
      -> H.HalogenM State action () Void m Unit
    handleAction _ = 
      H.modify_ (\{ game } -> { game: game >>= nextLevel})

lights 
  :: forall action props
  .  Maybe Size
  -> HH.HTML action props
lights sizeMaybe = 

  case sizeMaybe of 
    Nothing -> HH.div [] []
    Just size -> 
      HH.div [ css "container" ] 
        [ HH.div [ css "columns is-centered"] 
            [ HH.div [ css "column is-half"] 
                [ table
                ] 
            ]
        ]
      where
        table = 
          HH.table_
            [ HH.tbody_ $
                (\_ -> trs size) <$> (1 .. size)
            ]
        trs y = 
          HH.tr_ $ (\_ -> HH.td [ css "off" ] []) <$> (1 .. y)
