module WordGen where

import Data.Array (index, length)
import Data.Maybe (fromJust)
import Effect
import Effect.Random
import Prelude
import Partial.Unsafe (unsafePartial)

import Dictionary (dictionary)
import Word (Word, newWord)

mkRandom :: forall a. Array a -> Effect a
mkRandom d = unsafeIndex d <$> randomInt 0 (length d - 1)
  where
    unsafeIndex a = unsafePartial $ fromJust <<< index a

randomWord :: Effect Word
randomWord = newWord <$> text <*> speed <*> vOffset
  where
    text = mkRandom dictionary
    vOffset = pure 0.0
    speed = pure 0.0
