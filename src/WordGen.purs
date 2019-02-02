module WordGen 
    ( randomElem
    , randomWord
    ) where

import Data.Foldable (class Foldable, indexl, length)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Random (randomInt)
import Prelude (
    bind,
    flip,
    pure, 
    ($), 
    (-), 
    (<$>), 
    (<*>)
)
import Partial.Unsafe (unsafePartial)

import Dictionary (dictionary)
import Word (AbsoluteWord)


randomElem :: forall f a. Foldable f => f a -> Effect a
randomElem es = unsafePartial $ fromJust <$> randomElem'
  where
    randomElem' = flip indexl es <$> randomInt 0 (length es - 1)

randomWord :: Effect AbsoluteWord
randomWord = do
    text <- getText
    pure { text: text, speed: 0 }
  where
    getText = randomElem dictionary
