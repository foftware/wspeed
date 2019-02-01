module Test.Main where

import Effect (Effect)
import Prelude (Unit)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

import Test.WordGen (wordGenSpec)


main :: Effect Unit
main = run [consoleReporter] do
    wordGenSpec
