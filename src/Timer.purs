module Timer
    ( TimerId
    , startTimer
    , stopTimer
    )
    where

import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Effect (Effect)
import Prelude (Unit)


newtype TimerId = TimerId Int
derive instance eqTimerId :: Eq TimerId
derive instance ordTimerId :: Ord TimerId

foreign import startTimer :: Int -> Effect Unit -> Effect TimerId
foreign import stopTimer :: TimerId -> Effect Unit
