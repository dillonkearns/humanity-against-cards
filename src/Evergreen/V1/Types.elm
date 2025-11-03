module Evergreen.V1.Types exposing (..)

import Browser
import Dict
import Effect.Browser.Navigation
import Effect.Lamdera
import Url


type Route
    = PlayerRoute
    | AdminRoute


type alias Deck =
    { prompts : List String
    , answers : List String
    }


type PlayerToken
    = PlayerToken String


type alias Player =
    { token : PlayerToken
    , name : String
    , hand : List String
    , score : Int
    , clientId : Maybe Effect.Lamdera.ClientId
    }


type alias Submission =
    { playerToken : PlayerToken
    , card : String
    }


type Reaction
    = Laugh
    | Grimace
    | MindBlown


type alias PlayerReaction =
    { playerToken : PlayerToken
    , submissionCard : String
    , reaction : Reaction
    }


type RoundPhase
    = SubmissionPhase
        { prompt : String
        , submissions : List Submission
        }
    | RevealPhase
        { prompt : String
        , submissions : List Submission
        , revealedCount : Int
        }
    | JudgingPhase
        { prompt : String
        , submissions : List Submission
        , reactions : List PlayerReaction
        }
    | RoundComplete
        { winner : PlayerToken
        , winningCard : String
        }
    | NextRound
        { nextPrompt : String
        , nextJudge : PlayerToken
        , previousSubmissions : List Submission
        , previousReactions : List PlayerReaction
        }


type GameState
    = Lobby
    | Playing
        { currentJudge : PlayerToken
        , remainingAnswers : List String
        , remainingPrompts : List String
        , roundPhase : Maybe RoundPhase
        }
    | Ended


type alias FrontendModel =
    { counter : Int
    , clientId : Maybe Effect.Lamdera.ClientId
    , route : Route
    , key : Effect.Browser.Navigation.Key
    , deckJsonInput : String
    , loadedDeck : Maybe Deck
    , playerToken : Maybe PlayerToken
    , playerNameInput : String
    , joinedPlayer : Maybe Player
    , playersList : List Player
    , gameState : GameState
    , myHand : List String
    , currentJudge : Maybe PlayerToken
    , selectedCard : Maybe String
    , hasSubmitted : Bool
    }


type alias BackendModel =
    { counter : Int
    , deck : Maybe Deck
    , players : Dict.Dict String Player
    , gameState : GameState
    }


type FrontendMsg
    = Increment
    | Decrement
    | FNoop
    | UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | DeckJsonInputChanged String
    | LoadDeckClicked
    | PlayerNameInputChanged String
    | JoinGameClicked
    | RemovePlayerClicked PlayerToken
    | StartGameClicked
    | EndGameClicked
    | CardSelected String
    | SubmitCardClicked
    | RevealNextCardClicked
    | SelectWinnerClicked PlayerToken
    | AcceptPromptClicked
    | VetoPromptClicked
    | ReactToCardClicked String Reaction


type ToBackend
    = CounterIncremented
    | CounterDecremented
    | LoadDeck Deck
    | JoinGame PlayerToken String
    | RemovePlayer PlayerToken
    | StartGame
    | EndGame
    | SubmitCard PlayerToken String
    | RevealNextCard
    | SelectWinner PlayerToken
    | AcceptPrompt
    | VetoPrompt
    | AddReaction PlayerToken String Reaction


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | Noop


type ToFrontend
    = CounterNewValue Int Effect.Lamdera.ClientId
    | DeckLoaded Deck
    | PlayerJoined Player
    | PlayerReconnected Player (List String)
    | PlayersListUpdated (List Player)
    | GameStarted
        { yourHand : List String
        , currentJudge : PlayerToken
        }
    | GameStateUpdated GameState
    | RoundPhaseUpdated RoundPhase
    | CardSubmitted
    | WinnerSelected
        { winner : PlayerToken
        , winningCard : String
        }
    | HandUpdated (List String)
