module Main where

import RenderState

import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, (<>), discard, bind)


greet :: String -> String
greet name = "Hello, " <> name <> "!"

main :: Effect Unit
main = do
    log (greet "World")
    runHalogenAff do
        body <- awaitBody
        runUI renderState initialState body

