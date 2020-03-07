module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Lightson.Component.Root (component)

main :: Effect Unit
main = do
  log "launching missiles..."
  launchAff_ do
    body <- awaitBody
    runUI component unit body
