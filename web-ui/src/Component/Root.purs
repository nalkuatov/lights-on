module Lightson.Component.Root 
  ( component
  )
  where

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Lightson.Component.Ratings as Ratings
import Lightson.Component.Lights as Lights
import Lightson.Component.HTML.Util (css)
import Lightson.Component.Util (OpaqueSlot)
import Type.Prelude (SProxy(..))
import Prelude (($), Void, Unit, unit, absurd)

type State = Int

type ChildSlots = 
  ( ratings :: OpaqueSlot Unit
  , game :: OpaqueSlot Unit
  )

component 
  :: forall q i
  . H.Component HH.HTML q i Void Aff
component = 

  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where

    render :: forall a m. State -> HH.ComponentHTML a ChildSlots m
    render _ = 
      HH.section [ css "section" ]
        [ HH.div [ css "container" ]
            [ HH.div [ css "columns is-centered" ] 
                [ HH.div [ css "column is-7" ] 
                    [ HH.slot (SProxy :: _ "game") unit Lights.component unit absurd
                    ]
                ]
            , HH.div [ css "columns is-centered" ]
                [ HH.div [css "column is-7" ] 
                    [ HH.slot (SProxy :: _ "ratings") unit Ratings.component unit absurd
                    ]
                ]
            ]
        ]

    initialState :: i -> State
    initialState _ = 0

    handleAction 
      :: forall a
      .  a
      -> H.HalogenM State a ChildSlots Void Aff Unit
    handleAction _ = H.modify_ (\_ -> 0)
