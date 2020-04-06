module Lightson.Component.Lights 
  ( component
  )
  where

import Lightson.Data.Game
import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (placeholder)
import Lightson.Capability.Resource.Player (class ManagePlayer, createPlayer)
import Lightson.Component.HTML.Util (css)
import Lightson.Data.Player (Username)

type State = 
  { game :: Maybe Game
  , username :: Username
  }

-- | An action to turn on the lights
data Action = Switch (Tuple Int Int)
            | NextLevel
            | Input Username

component
  :: forall q i m
  . MonadEffect m
  => ManagePlayer m
  => H.Component HH.HTML q i Void m
component = 
  H.mkComponent 
    { initialState: (\_ -> { game: Just levelOne, username: "" })
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
    handleAction action = case action of
      Switch coordinates ->
        H.modify_ 
          (\state -> { game: state.game >>= switchN coordinates, username: state.username })
      NextLevel -> do
        { game, username } <- H.get
        case username of 
          "" -> H.modify_ (\state -> { game: state.game >>= nextLevel, username })
          value -> do
            let lvlMaybe = show <<< _level <$> game
            result <- 
              createPlayer 
                { username, score: (-1)
                , level: fromMaybe "undefined" lvlMaybe 
                }
            H.modify_ (\state -> { game: state.game >>= nextLevel, username: "" })
      Input username -> do 
        H.modify_ (\state -> { game: state.game, username })

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
            [ HH.div [ css "column is-half has-text-centered"] $
                [ HH.h4 [ css "title is-4"] 
                    [ HH.text $ (show <<< _level) game <> " level"
                    ]
                , HH.hr_
                , table 
                ] <> if isWin game then [ nextButton ] else []
            ]
        ]
      where

        nextButton = 
          HH.div_ 
            [ HH.div [ css "has-text-centered" ]
                [ HH.h5 [ css "title is-5"] 
                    [ HH.text $ "Well done! "
                    ]
                , HH.hr_
                ]
            , HH.div [ css "field has-addons"] 
              [ HH.p [ css "control"] 
                  [ HH.input 
                      [ css "input is-info"
                      , placeholder "type your name"
                      , HE.onValueInput (Just <<< Input)
                      ]
                  ]
              , HH.p [ css "control"] 
                  [ HH.button [ css "button is-success", HE.onClick (const $ Just NextLevel) ] 
                      [ HH.text "Go next" ]
                  ]
              ]
            ]
          -- HH.div [ css "level" ] 
          --   [ HH.div [ css "level-item" ] 
          --       [ HH.button 
          --           [ css "button is-pulled-right is-success"
          --           , HE.onClick (\_ -> Just NextLevel)
          --           ] 
          --           [ HH.text "Go next" ]
          --       ]
          --   ]
        table = 
          HH.table_
            [ HH.tbody_ $
                (\rowNo -> trs rowNo) <$> (1 .. _size game)
            ]

        trs rowNumber = 
          HH.tr_ $ (\colNumber -> 
                     HH.td [ css $ cellIsOn rowNumber colNumber 
                           , HE.onClick (const $ Just (Switch $ Tuple rowNumber colNumber))
                           ] 
                      []) 
                 <$> (1 .. _size game)

        cellIsOn row col = 
          if isOn (Tuple row col) game
            then "on"
            else "off"
