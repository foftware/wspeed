module RenderState where

import Data.Array (sortBy, uncons)
import Data.Eq ((==))
import Data.Foldable (traverse_, fold)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), compare)
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
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)


type StateWord =
    { text :: String
    , x :: Int
    , y :: Int
    }

type State =
    { writtenText :: String
    , words :: Array StateWord
    }

initialState :: State
initialState =
    { writtenText: ""
    , words:
        [ {text: "ahoj", x: 0, y: 0}
        , {text: "ahoj3", x: 50, y: 0}
        , {text: "ahoj2", x: 10, y: 10}
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
    renderWords (Just {head: w, tail: ws}) (Tuple x y) = if y == w.y
        then [HH.text (fold $ replicate' (w.x - x) " ")] <> [HH.text w.text] <> renderWords (uncons ws) (Tuple (w.x + length w.text) y)
        else replicate (w.y - y) HH.br_ <> [HH.text (fold $ replicate' (w.x - x) " ")] <> [HH.text w.text] <> renderWords (uncons ws) (Tuple (w.x + length w.text) w.y)
    renderWords Nothing (Tuple x y) = [] -- TODO: render the rest

    ordered :: Array StateWord
    ordered = sortBy sortF state.words
      where
        sortF a b = if a.y == b.y
            then compare a.x b.x
            else compare a.y b.y

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
eval = case _ of
    Init next -> do
        document <- H.liftEffect $ document =<< window
        H.subscribe $ eventSource'
            (onKeyPress document)
            (Just <<< H.request <<< HandleKey)
        H.subscribe $ eventSource_ (onTimeout 100) (Timeout Listening)
        pure next

    HandleKey ev reply -> do
        pure $ reply Listening
    Timeout reply -> do
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
