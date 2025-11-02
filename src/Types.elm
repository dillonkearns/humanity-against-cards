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


type alias Submission =
    { playerToken : PlayerToken
    , card : String
    }


type RoundPhase
    = SubmissionPhase
        { prompt : String
        , submissions : List Submission
        }
    | RevealPhase
        { prompt : String
        , submissions : List Submission  -- Shuffled to hide identity
        , revealedCount : Int  -- How many cards have been revealed so far
        }
    | JudgingPhase
        { prompt : String
        , submissions : List Submission
        }
    | RoundComplete
        { winner : PlayerToken
        , winningCard : String
        }


type GameState
    = Lobby
    | Playing
        { currentJudge : PlayerToken
        , remainingAnswers : List String  -- Cards not yet dealt
        , remainingPrompts : List String  -- Prompts not yet used
        , roundPhase : Maybe RoundPhase
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
    , selectedCard : Maybe String  -- Card selected but not yet submitted
    , hasSubmitted : Bool
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
    | CardSelected String
    | SubmitCardClicked
    | RevealNextCardClicked
    | SelectWinnerClicked PlayerToken


type ToBackend
    = CounterIncremented
    | CounterDecremented
    | LoadDeck Deck
    | JoinGame PlayerToken String  -- token, name
    | StartGame
    | SubmitCard PlayerToken String  -- player token, card
    | RevealNextCard
    | SelectWinner PlayerToken  -- winner's token


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
        , initialPrompt : String
        }
    | GameStateUpdated GameState
    | RoundPhaseUpdated RoundPhase
    | CardSubmitted  -- Confirmation that your card was submitted
    | WinnerSelected
        { winner : PlayerToken
        , winningCard : String
        }
