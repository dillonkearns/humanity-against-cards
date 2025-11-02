module Types exposing (..)

import Browser
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Browser.Navigation exposing (Key)
import Set exposing (Set)
import Url exposing (Url)


-- DECK TYPES


type alias Deck =
    { prompts : List String
    , answers : List String
    }


-- MODELS


type alias BackendModel =
    { counter : Int
    , deck : Maybe Deck
    }


type Route
    = PlayerRoute
    | AdminRoute


type alias FrontendModel =
    { counter : Int
    , clientId : Maybe ClientId
    , route : Route
    , key : Key
    , deckJsonInput : String
    , loadedDeck : Maybe Deck
    }


-- MESSAGES


type FrontendMsg
    = Increment
    | Decrement
    | FNoop
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | DeckJsonInputChanged String
    | LoadDeckClicked


type ToBackend
    = CounterIncremented
    | CounterDecremented
    | LoadDeck Deck


type BackendMsg
    = ClientConnected SessionId ClientId
    | Noop


type ToFrontend
    = CounterNewValue Int ClientId
    | DeckLoaded Deck
