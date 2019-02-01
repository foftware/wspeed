module WordGen where

import Data.Array (index, length)
import Data.Maybe (Maybe, fromJust)
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

randomWord :: Effect Word
randomWord = newWord <$> text <*> speed <*> vOffset
  where
    text = randomElem dictionary
    vOffset = pure 0.0
    speed = pure 0.0
