module WordGen where

import Data.Array (index, length)
import Data.Maybe (fromJust)
import Data.Monoid (class Monoid, mempty)
import Effect (Effect)
import Effect.Random (randomInt)
import Prelude (
    pure, 
    ($), 
    (-), 
    (<$>), 
    (<*>)
)
import Partial.Unsafe (unsafePartial)

import Dictionary (dictionary)
import Word (Word, newWord)

randomElem :: forall a. Array a -> Effect a
randomElem es = unsafePartial $ fromJust <$> randomElem'
  where
    randomElem' = index es <$> randomInt 0 (length es - 1)

randomWord :: forall s o. Monoid o => Monoid s => Effect (Word s o)
randomWord = newWord <$> text <*> speed <*> vOffset
  where
    text = randomElem dictionary
    vOffset = pure mempty
    speed = pure mempty
