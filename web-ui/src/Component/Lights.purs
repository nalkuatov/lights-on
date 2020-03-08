module Lightson.Component.Lights 
  ( component
  )
  where

import Lightson.Data.Game

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lightson.Component.HTML.Util (css)
import Prelude (Void, Unit, (<$>), ($), (>>=))

type State = 
  { game :: Maybe Game
  }

-- | An action to turn on the lights
data Action = Switch (Tuple Int Int)

component
  :: forall q i m
  .  H.Component HH.HTML q i Void m
component = 
  H.mkComponent 
    { initialState: (\_ -> { game: Just levelOne })
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
    render 
      :: State 
      -> H.ComponentHTML Action () m
    render state = lights state.game

    handleAction
      :: Action
      -> H.HalogenM State Action () Void m Unit
    handleAction (Switch coordinates) = 
      H.modify_ 
        (\state -> { game: state.game >>= switchN coordinates })

lights 
  :: forall props
  .  Maybe Game
  -> HH.HTML props Action
lights gameMaybe = 

  case gameMaybe of 
    Nothing -> HH.div [] []
    Just game -> 
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
                (\rowNo -> trs rowNo) <$> (1 .. _size game)            ]

        trs rowNumber = 
          HH.tr_ $ (\colNumber -> 
                     HH.td [ css $ cellIsOn rowNumber colNumber 
                           , HE.onClick $ (\_ -> Just (Switch $ Tuple rowNumber colNumber))
                           ] 
                      []) 
                 <$> (1 .. _size game)

        cellIsOn row col = 
          if isOn (Tuple row col) game
            then "on"
            else "off"
