module Lightson.Component.Ratings 
  ( component
  )
  where

import Lightson.Data.Player

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Lightson.Capability.Resource.Player (class ManagePlayer, getPlayers)
import Lightson.Component.HTML.Util (css)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Prelude (Void, bind, ($), (<>), (<$>), void)

type State = 
  { searchLevel :: String
  , players :: RemoteData String (Array Player)
  }

data Action
  = Initialize
  | LoadPlayers

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
          void $ H.fork $ handleAction LoadPlayers 

        LoadPlayers -> do
          { searchLevel } <- H.get
          players <- getPlayers Nothing
          H.modify_ _ { players = RemoteData.fromMaybe players}

panel 
  :: forall props
  . State
  -> HH.HTML props Action
panel { players } = 
  HH.nav [ css "panel" ] $
    [ HH.p [ css "panel-heading" ] [ HH.text "Best results"]
    , HH.p [ css "panel-tabs" ] 
        [ HH.a [ css "is-active" ] [ HH.text "Easy level"]
        , HH.a_ [ HH.text "Medium level" ]
        , HH.a_ [ HH.text "Hard level" ]
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

          Success items -> renderPlayer <$> items

      renderPlayer { username } = 
        HH.a [ css "panel-block" ] 
          [ HH.span [ css "panel-icon" ] 
              [ HH.i [ css "fas fa-user"] []]
          , HH.text username 
          -- <> " | " 
          -- , HH.span [ css "tag is-info" ]
          --     [ HH.text "123 seconds" ]
          ]

