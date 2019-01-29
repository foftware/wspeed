module RenderState where

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Halogen.Query.EventSource
    (SubscribeStatus(Listening)
    , eventSource'
    , eventSource_
    )
import Prelude
    (type (~>)
    , (<<<)
    , (=<<)
    , ($)
    , Unit
    , Void
    , bind
    , const
    , discard
    , pure
    , void
    )
import Timer (startTimer)
import Web.Event.EventTarget
    ( addEventListener
    , eventListener
    , removeEventListener
    )
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent)


data StateWord = StateWord
    { text :: String
    , x :: Int
    , y :: Int
    }

data State = State
    { writtenText :: String
    , words :: [

initialState :: State
initialState = State

render :: forall p i. State -> HH.HTML p i
render _ =
    HH.div_
    [ HH.div
        [ class_ (HH.ClassName "game-div")
        ]
        [ HH.text "-------------------------------------"
        , HH.br_
        , HH.text "-------------------------------------"
        ]
    , HH.div
        [
        ]
    ]

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
