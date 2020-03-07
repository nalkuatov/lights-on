module Lightson.Component.HTML.Util 
  ( css 
  )
  where

import Prelude ((<<<))
import Halogen.HTML (IProp, ClassName(..))
import Halogen.HTML.Properties (class_)

css :: forall r i. String -> IProp ( class :: String | r ) i
css = class_ <<< ClassName
