module Word 
    ( AbsoluteWord
    , Word(..)
    , newWord
    )
    where

import Prelude (class Eq, class Ord, (==), (&&), compare)
import Data.Monoid (class Monoid, mempty)


newtype Word s o = Word
    { text :: String
    , speed :: s
    , vOffset :: o
    , hOffset :: o
    }

instance wordEq :: Eq o => Eq (Word s o) where
    -- Hmmm this is super tricky:
    -- How are two words equal with respect to the rendering grid
    -- whatever it is ?
    eq (Word w1) (Word w2) =
        w1.vOffset == w2.vOffset && w1.hOffset == w2.hOffset

instance wordOrd :: (Eq o, Ord o) => Ord (Word s o) where
    compare (Word w1) (Word w2) = if w1.vOffset == w2.vOffset
        then compare w1.hOffset w2.hOffset
        else compare w1.vOffset w2.vOffset

type AbsoluteWord = Word Int Int

newWord :: forall s o. Monoid o => Monoid s => String -> s -> o -> Word s o
newWord text speed vOffset = Word
    { text: text
    , speed: mempty
    , vOffset: vOffset
    , hOffset: mempty
    }
