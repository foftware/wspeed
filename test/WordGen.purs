module Test.WordGen where

import Effect.Class (liftEffect)
import Prelude (Unit, discard, void, ($), (<<<))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions.Aff (expectError)
import WordGen (randomElem)

wordGenSpec :: Spec Unit
wordGenSpec = do
    mkRandomSpec

mkRandomSpec :: Spec Unit
mkRandomSpec = describe "randomElem" do
    it "selects random element" $ test [1,2,3,4]
    it "blows up on empty array" $ expectError $ test ([] :: Array Int)
  where
    test = liftEffect <<< void <<< randomElem 
