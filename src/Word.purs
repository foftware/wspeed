module Word 
    ( AbsoluteWord
    , Word
    , newWord
    )
    where

import Data.Monoid (class Monoid, mempty)


type Word s o = 
    { text :: String
    , speed :: s
    , vOffset :: o
    , hOffset :: o
    }

type AbsoluteWord = Word Int Int

newWord :: forall s o. Monoid o => Monoid s => String -> s -> o -> Word s o
newWord text speed vOffset =
    { text: text
    , speed: mempty
    , vOffset: vOffset
    , hOffset: mempty
    }
