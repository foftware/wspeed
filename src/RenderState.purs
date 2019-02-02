module RenderState where

--import Control.Applicative (when)
--import Data.Array (cons, intercalate, sort, uncons, reverse)
--import Data.Array as A
import Data.Boolean (otherwise)
import Data.Eq ((==))
--import Data.Foldable (traverse_, fold, foldl)
import Data.Foldable (traverse_)
-- import Data.Function (flip)
-- import Data.Functor (map)
--import Data.Maybe (Maybe(..), fromJust)
import Data.Maybe (Maybe(..))
--import Data.Semigroup ((<>))
--import Data.String.CodeUnits (length)
--import Data.Tuple (Tuple(..))
--import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Halogen.Query.EventSource (SubscribeStatus(Listening), eventSource', eventSource_)
--import Partial.Unsafe (unsafePartial)
--import Prelude (type (~>), (<<<), (=<<), ($), (-), (+), Unit, Void, bind, const, discard, pure, void)
import Prelude (type (~>), (<<<), (=<<), ($), Unit, Void, bind, const, discard, pure, unit, void)
import Timer (startTimer)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
-- import Word (AbsoluteWord)


type State = Unit

type Resolution =
    { width :: Int
    , height :: Int
    }

initialState :: State
initialState = unit

render :: forall p i. State -> HH.HTML p i
render state =
    HH.div_
    [ HH.div
        [ class_ (HH.ClassName "game-div")
        ] $ []
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
eval (Init next) = do
        document <- H.liftEffect $ document =<< window
        H.subscribe $ eventSource'
            (onKeyPress document)
            (Just <<< H.request <<< HandleKey)
        H.subscribe $ eventSource_ (onTimeout 100) (Timeout Listening)
        pure next

eval (HandleKey ev reply)
    | key ev == "Enter" = do
        pure $ reply Listening
    | otherwise = do
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
