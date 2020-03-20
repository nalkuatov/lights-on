module Lightson.Component.Ratings 
  ( component
  )
  where

import Lightson.Data.Player

import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lightson.Api.Endpoint (ScoreParams)
import Lightson.Capability.Resource.Player (class ManagePlayer, getPlayers)
import Lightson.Component.HTML.Util (css)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Prelude (Void, bind, ($), (<>), (<$>), void, (==))

type State = 
  { searchLevel :: String
  , players :: RemoteData String (Array Player)
  }

data Action
  = Initialize
  | LoadPlayers ScoreParams

component 
  :: forall q i m
  .  ManagePlayer m
  => H.Component HH.HTML q i Void m
component = 

  H.mkComponent
    { initialState: (\i -> { searchLevel: "easy", players: NotAsked } )
    , render 
    , eval: H.mkEval $ 
        H.defaultEval 
          { initialize = Just Initialize
          , handleAction = handleAction 
          }
    }

  where 

    render = panel

    handleAction =
      case _ of 
        Initialize -> do
          void $ H.fork $ handleAction $ 
            LoadPlayers { username: Nothing, level: Just "easy" }

        LoadPlayers { username, level } -> do
          { searchLevel } <- H.get
          players <- getPlayers { username: Nothing, level }
          H.modify_ _ 
            { players = RemoteData.fromMaybe players
            , searchLevel = fromMaybe "easy" level
            }

panel 
  :: forall props
  . State
  -> HH.HTML props Action
panel { searchLevel, players } = 
  HH.nav [ css "panel" ] $
    [ HH.p [ css "panel-heading" ] [ HH.text "Best results"]
    , HH.p [ css "panel-tabs" ] 
        [ HH.a [ css $ if searchLevel == "easy" then "is-active" else "" 
               , HE.onClick (\_ -> Just $ LoadPlayers { username: Nothing, level: Just "easy" })
               ]
          [ HH.text "Easy level"]
        , HH.a [ css $ if searchLevel == "medium" then "is-active" else "" 
               , HE.onClick (\_ -> Just $ LoadPlayers { username: Nothing, level: Just "medium" })
               ]
          [ HH.text "Medium level" ]
        , HH.a [ css $ if searchLevel == "hard" then "is-active" else ""
               , HE.onClick (\_ -> Just $ LoadPlayers { username: Nothing, level: Just "hard" })
               ]
          [ HH.text "Hard level" ]
        ]
    ] <> renderPlayers

    where 
      renderPlayers = 
        case players of 
          NotAsked -> 
            [ HH.a [ css "panel-block" ] 
              [ HH.text $ "Not loaded" ]
            ]

          Failure e->
            [ HH.a [ css "panel-block" ] 
              [ HH.text "Failed to connect to the server" ]
            ]

          Loading ->
            [ HH.a [ css "panel-block" ] 
              [ HH.text "Loading" ]
            ]

          Success [] -> [ renderEmpty ]

          Success items -> renderPlayer <$> items

      renderEmpty  = 
        HH.a [ css "panel-block" ] 
          [ HH.text "No one has solved this lvl yet"
          ]

      renderPlayer { username } = 
        HH.a [ css "panel-block" ] 
          [ HH.span [ css "panel-icon" ] 
              [ HH.i [ css "fas fa-user"] []]
          , HH.text username 
          -- <> " | " 
          -- , HH.span [ css "tag is-info" ]
          --     [ HH.text "123 seconds" ]
          ]
