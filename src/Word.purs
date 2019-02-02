module Word 
    ( AbsoluteWord
    , Word
    )
    where

import Prelude (class Eq, class Ord, (==), (&&), compare)
import Data.Monoid (class Monoid, mempty)


type Word s = { text :: String, speed :: s }

type AbsoluteWord = Word Int
