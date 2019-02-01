module WordGen 
    ( randomElem
    , randomWord
    ) where

import Data.Foldable (class Foldable, indexl, length)
import Data.Maybe (fromJust)
import Data.Monoid (class Monoid, mempty)
import Effect (Effect)
import Effect.Random (randomInt)
import Prelude (
    flip,
    pure, 
    ($), 
    (-), 
    (<$>), 
    (<*>)
)
import Partial.Unsafe (unsafePartial)

import Dictionary (dictionary)
import Word (Word, newWord)


randomElem :: forall f a. Foldable f => f a -> Effect a
randomElem es = unsafePartial $ fromJust <$> randomElem'
  where
    randomElem' = flip indexl es <$> randomInt 0 (length es - 1)

randomWord :: forall s o. Monoid o => Monoid s => Effect (Word s o)
randomWord = newWord <$> text <*> speed <*> vOffset
  where
    text = randomElem dictionary
    vOffset = pure mempty
    speed = pure mempty
