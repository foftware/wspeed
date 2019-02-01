module RenderState where

import Control.Applicative (when)
import Data.Array (sort, uncons)
import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.Foldable (traverse_, fold)
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import Data.Semigroup ((<>))
import Data.String.CodeUnits (length)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Halogen.Query.EventSource (SubscribeStatus(Listening), eventSource', eventSource_)
import Prelude (type (~>), (<<<), (=<<), ($), (-), (+), Unit, Void, bind, const, discard, pure, void)
import Timer (startTimer)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Word (AbsoluteWord, Word(Word))


type State =
    { writtenText :: String
    , words :: Array AbsoluteWord
    }

initialState :: State
initialState =
    { writtenText: ""
    , words:
        [ Word {text: "ahoj", hOffset: 0, vOffset: 0, speed: 0}
        , Word {text: "ahoj3", hOffset: 50, vOffset: 0, speed: 0}
        , Word {text: "ahoj2", hOffset: 10, vOffset: 10, speed: 0}
        ]
    }

render :: forall p i. State -> HH.HTML p i
render state =
    HH.div_
    [ HH.div
        [ class_ (HH.ClassName "game-div")
        ]
        ([HH.text "-------------------------------------" , HH.br_]
        <> renderWords (uncons ordered) (Tuple 0 0)
        <> [HH.br_, HH.text "-------------------------------------"]
        )
    , HH.div_ [HH.text state.writtenText]
    ]
  where
    ordered :: Array AbsoluteWord
    ordered = sort state.words

renderWords
    :: forall p i
    .  Maybe {head :: AbsoluteWord, tail :: Array AbsoluteWord}
    -> Tuple Int Int
    -> Array (HH.HTML p i)
renderWords (Just {head: (Word w), tail}) (Tuple x y) = if y == (w.vOffset)
    then [HH.text (fold $ replicate' (w.hOffset - x) " ")]
        <> [HH.text w.text]
        <> renderWords (uncons tail) (Tuple (w.hOffset + length w.text) y)
    else replicate (w.vOffset - y) HH.br_
        <> [HH.text (fold $ replicate' (w.hOffset - x) " ")]
        <> [HH.text w.text]
        <> renderWords (uncons tail) (Tuple (w.hOffset + length w.text) w.vOffset)
renderWords Nothing (Tuple x y) = [] -- TODO: render the rest

replicate' :: forall a. Int -> a -> Array a
replicate' n a = if n < 0
    then []
    else replicate n a


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
        -- TODO: Remove word from simulation!!!
        pure $ reply Listening
    | otherwise = do
        let char = key ev
        when (length char == 1) do
            H.modify_ (\st -> st { writtenText = st.writtenText <> char })
        pure $ reply Listening

eval (Timeout reply) = do
        log "kwa"
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
