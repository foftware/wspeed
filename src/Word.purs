module Word where

type Offset = Number
type Speed = Number

type Word = 
    { text :: String
    , speed :: Speed
    , vOffset :: Offset -- <0, 1)
    , hOffset :: Offset -- <0, 1)
    }

newWord :: String -> Speed -> Offset -> Word
newWord text speed vOffset =
    { text: text
    , speed: speed
    , vOffset: vOffset
    , hOffset: 0.0
    }
