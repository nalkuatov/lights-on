module Lightson.Component.Ratings 
  ( panel
  , component
  )
  where

import Halogen as H
import Halogen.HTML as HH
import Lightson.Component.HTML.Util (css)
import Prelude (Unit, Void, unit, ($))

type State = Unit

component 
  :: forall q i m
  .  H.Component HH.HTML q i Void m
component = 

  H.mkComponent
    { initialState: (\i -> unit )
    , render 
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where 

    render _ = panel

    handleAction _ =
      H.modify_ (\_ -> unit)

panel 
  :: forall action props
  .  HH.HTML action props
panel = 
  HH.nav [ css "panel" ] 
    [ HH.p [ css "panel-heading" ] [ HH.text "Best results"]
    , HH.p [ css "panel-tabs" ] 
        [ HH.a [ css "is-active" ] [ HH.text "Easy level"]
        , HH.a_ [ HH.text "Medium level" ]
        , HH.a_ [ HH.text "Hard level" ]
        ]
    , HH.a [ css "panel-block" ] 
        [ HH.span [ css "panel-icon" ] 
            [ HH.i [ css "fas fa-user"] []]
        , HH.text "odinsonthorin | " 
        , HH.span [ css "tag is-info"] 
            [ HH.text "123 seconds"]
        ]
    ]
