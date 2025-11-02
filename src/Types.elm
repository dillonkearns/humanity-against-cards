module Types exposing (..)

import Browser
import Dict exposing (Dict)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Browser.Navigation exposing (Key)
import Set exposing (Set)
import Url exposing (Url)


-- DECK TYPES


type alias Deck =
    { prompts : List String
    , answers : List String
    }


-- PLAYER TYPES


type PlayerToken
    = PlayerToken String


type alias Player =
    { token : PlayerToken
    , name : String
    , hand : List String  -- Answer cards in player's hand
    , score : Int
    , clientId : Maybe ClientId  -- Track which client this player is connected as
    }


-- GAME STATE


type GameState
    = Lobby
    | Playing
        { currentJudge : PlayerToken
        , remainingAnswers : List String  -- Cards not yet dealt
        , remainingPrompts : List String  -- Prompts not yet used
        }
    | Ended


-- MODELS


type alias BackendModel =
    { counter : Int
    , deck : Maybe Deck
    , players : Dict String Player  -- keyed by token string
    , gameState : GameState
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
    , playerToken : Maybe PlayerToken
    , playerNameInput : String
    , joinedPlayer : Maybe Player
    , playersList : List Player  -- For admin view
    , gameState : GameState
    , myHand : List String  -- Player's current hand of cards
    , currentJudge : Maybe PlayerToken
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
    | PlayerNameInputChanged String
    | JoinGameClicked
    | GotPlayerToken PlayerToken
    | StartGameClicked


type ToBackend
    = CounterIncremented
    | CounterDecremented
    | LoadDeck Deck
    | JoinGame PlayerToken String  -- token, name
    | StartGame


type BackendMsg
    = ClientConnected SessionId ClientId
    | Noop


type ToFrontend
    = CounterNewValue Int ClientId
    | DeckLoaded Deck
    | PlayerJoined Player
    | PlayersListUpdated (List Player)
    | GameStarted
        { yourHand : List String
        , currentJudge : PlayerToken
        }
    | GameStateUpdated GameState
