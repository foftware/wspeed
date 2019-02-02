module RenderState where

import Control.Applicative (when)
import Data.Array (cons, sort, uncons, reverse)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.Foldable (traverse_, fold, foldr, foldl)
import Data.Function (flip)
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromJust)
import Data.Semigroup ((<>))
import Data.String.CodeUnits (length)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Halogen.Query.EventSource (SubscribeStatus(Listening), eventSource', eventSource_)
import Partial.Unsafe (unsafePartial)
import Prelude (type (~>), (<<<), (=<<), ($), (-), (+), Unit, Void, bind, const, discard, pure, void)
import Timer (startTimer)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Word (Word(..), AbsoluteWord)


type State =
    { writtenText :: String
    , words :: Array AbsoluteWord
    , resolution :: Resolution
    }

initialState :: State
initialState =
    { writtenText: ""
    , words:
        [ Word {text: "ahoj", hOffset: 0, vOffset: 0, speed: 0}
        , Word {text: "ahoj3", hOffset: 50, vOffset: 0, speed: 0}
        , Word {text: "ahoj2", hOffset: 10, vOffset: 10, speed: 0}
        ]
    , resolution: {width: 100, height: 30}
    }

render :: forall p i. State -> HH.HTML p i
render state =
    HH.div_
    [ HH.div
        [ class_ (HH.ClassName "game-div")
        ]
        $ toHtml state
--        $ renderWords ordered state.resolution
    , HH.div_ [HH.text state.writtenText]
    ]
  where
    ordered :: Array AbsoluteWord
    ordered = sort state.words

stringReplicate :: Int -> String -> String
stringReplicate n w = fold (replicate n w :: Array String)

intersperse :: forall a. a -> Array a -> Array a
intersperse a array = case uncons array of
    Just {head: x, tail: xs} -> cons x $ preprendToAll a xs
    Nothing -> []
  where
    preprendToAll :: a -> Array a -> Array a
    preprendToAll a' = foldr f []
      where
        f y b = [a', y] <> b

renderWords
    :: forall p i
    .  Array AbsoluteWord
    -> Resolution
    -> Array (HH.HTML p i)
renderWords words resolution =
    renderWords' (uncons words) (Tuple 0 0)
  where
    renderWords'
        :: Maybe {head :: AbsoluteWord, tail :: Array AbsoluteWord}
        -> Tuple Int Int
        -> Array (HH.HTML p i)
    renderWords' (Just {head: (Word w), tail}) (Tuple x y) =
        if y == (w.vOffset)
            then [HH.text $ stringReplicate (w.hOffset - x) " "]
                <> [HH.text w.text]
                <> renderWords' (uncons tail)
                    (Tuple (w.hOffset + length w.text) y)
            else replicate (w.vOffset - y) HH.br_
                <> [HH.text $ stringReplicate (w.hOffset - x) " "]
                <> [HH.text w.text]
                <> renderWords' (uncons tail)
                    (Tuple (w.hOffset + length w.text) w.vOffset)
    renderWords' Nothing (Tuple x y) = replicate (resolution.height - y) HH.br_

type ToListState =
    { lines :: Array String
    , x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }

type Resolution =
    { width :: Int
    , height :: Int
    }

initialToListState :: Resolution -> ToListState
initialToListState r =
    { lines: [""]
    , x: 0
    , y: 0
    , width: r.width
    , height: r.height
    }

toHtml :: forall p i. State -> Array (HH.HTML p i)
toHtml state = intersperse HH.br_ <<< map HH.text $ fillSpace $ reverse
    (foldl (flip toList) (initialToListState state.resolution)
    $ sort state.words).lines
  where
    fillSpace :: Array String -> Array String
    fillSpace xs = (map fillSpace' xs)
        <> replicate (state.resolution.height - A.length xs)
            (replicateSpaces state.resolution.width)
      where
        fillSpace' x = x <> replicateSpaces (state.resolution.width - length x)

toList :: AbsoluteWord -> ToListState -> ToListState
toList (Word w) state = unsafePartial $ if state.y == w.vOffset
    then let {head, tail} = fromJust $ uncons state.lines
        in state
            { lines = cons
                (head <> replicateSpaces (w.hOffset - state.x) <> w.text)
                tail
            , x = w.hOffset + length w.text
            }
    else state
        { lines =
            [replicateSpaces w.hOffset <> w.text]
            <> replicateEmptyLines (w.vOffset - state.y)
            <> state.lines
        , x = w.hOffset + length w.text
        , y = w.vOffset
        }
  where
    replicateEmptyLines n = replicate n ""

replicateSpaces :: Int -> String
replicateSpaces n = stringReplicate n " "

onKeyPress
    :: HTMLDocument
    -> (KeyboardEvent
    -> Effect Unit)
    -> Effect (Effect Unit)
onKeyPress document fn = do
    let target = toEventTarget document
    listener <- eventListener (traverse_ fn <<< fromEvent)
    addEventListener keydown listener false target
    pure $ removeEventListener keydown listener false target

data Query a
    = Init a
    | HandleKey KeyboardEvent (SubscribeStatus -> a)
    | Timeout a

type DSL = H.ComponentDSL State Query Void Aff

onTimeout :: Int -> Effect Unit -> Effect Unit
onTimeout time return = void $ startTimer time return

eval :: Query ~> DSL
eval (Init next) = do
        document <- H.liftEffect $ document =<< window
        H.subscribe $ eventSource'
            (onKeyPress document)
            (Just <<< H.request <<< HandleKey)
        H.subscribe $ eventSource_ (onTimeout 100) (Timeout Listening)
        pure next

eval (HandleKey ev reply)
    | key ev == "Enter" = do
        H.modify_ (\st -> st { writtenText = "" })
        -- TODO: Remove word from simulation!!!
        pure $ reply Listening
    | otherwise = do
        let char = key ev
        when (length char == 1) do
            H.modify_ (\st -> st { writtenText = st.writtenText <> char })
        pure $ reply Listening

eval (Timeout reply) = do
    pure $ reply

renderState :: H.Component HH.HTML Query State Void Aff
renderState = H.lifecycleComponent
    { eval
    , initializer: Just (H.action Init)
    , initialState: const initialState
    , receiver: const Nothing
    , render
    , finalizer: Nothing
    }
