module Backend exposing (app, app_, init)

import Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription
import Lamdera
import Set exposing (Set)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        app_


app_ =
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }


init : ( Model, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { counter = 0
      , deck = Nothing
      , players = Dict.empty
      , gameState = Lobby
      }
    , Command.none
    )


update : BackendMsg -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            let
                playersList =
                    Dict.values model.players
            in
            ( model
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId <| CounterNewValue model.counter clientId
                , Effect.Lamdera.sendToFrontend clientId <| PlayersListUpdated playersList
                , Effect.Lamdera.sendToFrontend clientId <| GameStateUpdated model.gameState
                , case model.deck of
                    Just deck ->
                        Effect.Lamdera.sendToFrontend clientId <| DeckLoaded deck

                    Nothing ->
                        Command.none
                ]
            )

        Noop ->
            ( model, Command.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CounterIncremented ->
            let
                newCounter =
                    model.counter + 1
            in
            ( { model | counter = newCounter }
            , Effect.Lamdera.broadcast (CounterNewValue newCounter clientId)
            )

        CounterDecremented ->
            let
                newCounter =
                    model.counter - 1
            in
            ( { model | counter = newCounter }
            , Effect.Lamdera.broadcast (CounterNewValue newCounter clientId)
            )

        LoadDeck deck ->
            ( { model | deck = Just deck }
            , Effect.Lamdera.broadcast (DeckLoaded deck)
            )

        JoinGame (PlayerToken tokenStr) name ->
            let
                player =
                    { token = PlayerToken tokenStr
                    , name = name
                    , hand = []
                    , score = 0
                    , clientId = Just clientId
                    }

                newPlayers =
                    Dict.insert tokenStr player model.players

                playersList =
                    Dict.values newPlayers
            in
            ( { model | players = newPlayers }
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId (PlayerJoined player)
                , Effect.Lamdera.broadcast (PlayersListUpdated playersList)
                ]
            )

        SubmitCard playerToken card ->
            case model.gameState of
                Playing playingState ->
                    case playingState.roundPhase of
                        Just (SubmissionPhase phaseData) ->
                            let
                                -- Add the submission
                                newSubmission =
                                    { playerToken = playerToken, card = card }

                                newSubmissions =
                                    newSubmission :: phaseData.submissions

                                -- Count non-judge players
                                nonJudgeCount =
                                    Dict.size model.players - 1

                                -- Check if all players have submitted
                                allSubmitted =
                                    List.length newSubmissions >= nonJudgeCount

                                ( nextPhase, newGameState ) =
                                    if allSubmitted && nonJudgeCount > 0 then
                                        -- Transition to reveal phase with shuffled submissions
                                        let
                                            shuffled =
                                                shuffleSubmissions newSubmissions

                                            revealPhase =
                                                RevealPhase
                                                    { prompt = phaseData.prompt
                                                    , submissions = shuffled
                                                    , revealedCount = 0
                                                    }
                                        in
                                        ( revealPhase
                                        , Playing { playingState | roundPhase = Just revealPhase }
                                        )

                                    else
                                        -- Stay in submission phase
                                        let
                                            updatedPhase =
                                                SubmissionPhase { phaseData | submissions = newSubmissions }
                                        in
                                        ( updatedPhase
                                        , Playing { playingState | roundPhase = Just updatedPhase }
                                        )
                            in
                            ( { model | gameState = newGameState }
                            , Effect.Lamdera.broadcast (RoundPhaseUpdated nextPhase)
                            )

                        _ ->
                            -- Not in submission phase, ignore
                            ( model, Command.none )

                _ ->
                    -- Not playing, ignore
                    ( model, Command.none )

        RevealNextCard ->
            case model.gameState of
                Playing playingState ->
                    case playingState.roundPhase of
                        Just (RevealPhase phaseData) ->
                            let
                                newRevealedCount =
                                    phaseData.revealedCount + 1

                                allRevealed =
                                    newRevealedCount >= List.length phaseData.submissions

                                ( nextPhase, newGameState ) =
                                    if allRevealed then
                                        -- Transition to judging phase
                                        let
                                            judgingPhase =
                                                JudgingPhase
                                                    { prompt = phaseData.prompt
                                                    , submissions = phaseData.submissions
                                                    }
                                        in
                                        ( judgingPhase
                                        , Playing { playingState | roundPhase = Just judgingPhase }
                                        )

                                    else
                                        -- Increment reveal count
                                        let
                                            updatedPhase =
                                                RevealPhase { phaseData | revealedCount = newRevealedCount }
                                        in
                                        ( updatedPhase
                                        , Playing { playingState | roundPhase = Just updatedPhase }
                                        )
                            in
                            ( { model | gameState = newGameState }
                            , Effect.Lamdera.broadcast (RoundPhaseUpdated nextPhase)
                            )

                        _ ->
                            -- Not in reveal phase, ignore
                            ( model, Command.none )

                _ ->
                    -- Not playing, ignore
                    ( model, Command.none )

        SelectWinner winnerToken ->
            case model.gameState of
                Playing playingState ->
                    case playingState.roundPhase of
                        Just (JudgingPhase phaseData) ->
                            let
                                -- Find the winning card
                                winningCard =
                                    phaseData.submissions
                                        |> List.filter (\sub -> sub.playerToken == winnerToken)
                                        |> List.head
                                        |> Maybe.map .card
                                        |> Maybe.withDefault ""

                                -- Update winner's score
                                updatedPlayers =
                                    Dict.update (tokenToString winnerToken)
                                        (Maybe.map (\player -> { player | score = player.score + 1 }))
                                        model.players

                                -- TODO: Start next round or end game
                                -- For now, just broadcast the winner
                            in
                            ( { model | players = updatedPlayers }
                            , Effect.Lamdera.broadcast (WinnerSelected { winner = winnerToken, winningCard = winningCard })
                            )

                        _ ->
                            -- Not in judging phase, ignore
                            ( model, Command.none )

                _ ->
                    -- Not playing, ignore
                    ( model, Command.none )

        StartGame ->
            case model.deck of
                Nothing ->
                    -- Can't start without a deck
                    ( model, Command.none )

                Just deck ->
                    if Dict.isEmpty model.players then
                        -- Can't start without players
                        ( model, Command.none )

                    else
                        let
                            -- Get first player as judge
                            firstJudge =
                                Dict.values model.players
                                    |> List.head
                                    |> Maybe.map .token
                                    |> Maybe.withDefault (PlayerToken "")

                            -- Deal 10 cards to each player
                            ( playersWithHands, remainingAnswers ) =
                                dealCardsToPlayers (Dict.values model.players) deck.answers []

                            newPlayers =
                                playersWithHands
                                    |> List.map (\p -> ( tokenToString p.token, p ))
                                    |> Dict.fromList

                            -- Start first round with first prompt
                            ( firstPrompt, restPrompts ) =
                                case deck.prompts of
                                    prompt :: rest ->
                                        ( prompt, rest )

                                    [] ->
                                        ( "", [] )

                            initialRoundPhase =
                                SubmissionPhase
                                    { prompt = firstPrompt
                                    , submissions = []
                                    }

                            newGameState =
                                Playing
                                    { currentJudge = firstJudge
                                    , remainingAnswers = remainingAnswers
                                    , remainingPrompts = restPrompts
                                    , roundPhase = Just initialRoundPhase
                                    }

                            -- Send each player their hand
                            playerCommands =
                                Dict.values newPlayers
                                    |> List.filterMap
                                        (\player ->
                                            case player.clientId of
                                                Just cid ->
                                                    Just
                                                        (Effect.Lamdera.sendToFrontend cid
                                                            (GameStarted
                                                                { yourHand = player.hand
                                                                , currentJudge = firstJudge
                                                                , initialPrompt = firstPrompt
                                                                }
                                                            )
                                                        )

                                                Nothing ->
                                                    Nothing
                                        )

                            commands =
                                Command.batch playerCommands
                        in
                        ( { model
                            | players = newPlayers
                            , gameState = newGameState
                          }
                        , commands
                        )


-- HELPER FUNCTIONS


tokenToString : PlayerToken -> String
tokenToString (PlayerToken str) =
    str


shuffleSubmissions : List Submission -> List Submission
shuffleSubmissions submissions =
    -- Simple deterministic shuffle based on card content
    -- This provides anonymity while being deterministic for testing
    List.sortBy (\sub -> String.length sub.card) submissions
        |> List.reverse


dealCardsToPlayers : List Player -> List String -> List Player -> ( List Player, List String )
dealCardsToPlayers players availableCards accum =
    case players of
        [] ->
            ( List.reverse accum, availableCards )

        player :: rest ->
            let
                ( hand, remaining ) =
                    ( List.take 10 availableCards, List.drop 10 availableCards )

                updatedPlayer =
                    { player | hand = hand }
            in
            dealCardsToPlayers rest remaining (updatedPlayer :: accum)


subscriptions model =
    Subscription.batch
        [ Effect.Lamdera.onConnect ClientConnected
        ]
