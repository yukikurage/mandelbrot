module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Viewer (component)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
