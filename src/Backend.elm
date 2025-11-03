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
                sessionIdStr =
                    sessionIdToString sessionId

                playersList =
                    Dict.values model.players

                -- Check if this SessionId has an existing player
                maybeExistingPlayer =
                    Dict.get sessionIdStr model.players

                ( updatedModel, reconnectCmd ) =
                    case maybeExistingPlayer of
                        Just player ->
                            -- Player exists! Update their clientId and reconnect
                            let
                                updatedPlayer =
                                    { player | clientId = Just clientId }

                                newPlayers =
                                    Dict.insert sessionIdStr updatedPlayer model.players
                            in
                            ( { model | players = newPlayers }
                            , Effect.Lamdera.sendToFrontend clientId (PlayerReconnected updatedPlayer updatedPlayer.hand)
                            )

                        Nothing ->
                            -- New session, no existing player
                            ( model, Command.none )
            in
            ( updatedModel
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId <| CounterNewValue model.counter clientId
                , Effect.Lamdera.sendToFrontend clientId <| PlayersListUpdated playersList
                , Effect.Lamdera.sendToFrontend clientId <| GameStateUpdated model.gameState
                , case model.deck of
                    Just deck ->
                        Effect.Lamdera.sendToFrontend clientId <| DeckLoaded deck

                    Nothing ->
                        Command.none
                , reconnectCmd
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

        JoinGame _ name ->
            -- Generate PlayerToken from SessionId for stability across reconnects
            let
                sessionIdStr =
                    sessionIdToString sessionId

                token =
                    PlayerToken sessionIdStr

                player =
                    { token = token
                    , name = name
                    , hand = []
                    , score = 0
                    , clientId = Just clientId
                    }

                newPlayers =
                    Dict.insert sessionIdStr player model.players

                playersList =
                    Dict.values newPlayers
            in
            ( { model | players = newPlayers }
            , Command.batch
                [ Effect.Lamdera.sendToFrontend clientId (PlayerJoined player)
                , Effect.Lamdera.broadcast (PlayersListUpdated playersList)
                ]
            )

        RemovePlayer playerToken ->
            -- Only allow removing players in Lobby state
            case model.gameState of
                Lobby ->
                    let
                        -- Find and remove the player with this token
                        newPlayers =
                            Dict.filter (\_ player -> player.token /= playerToken) model.players

                        playersList =
                            Dict.values newPlayers
                    in
                    ( { model | players = newPlayers }
                    , Effect.Lamdera.broadcast (PlayersListUpdated playersList)
                    )

                _ ->
                    -- Can't remove players once game has started
                    ( model, Command.none )

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

                                -- Calculate next judge (rotate round-robin)
                                playersList =
                                    Dict.values updatedPlayers

                                nextJudge =
                                    getNextJudge playingState.currentJudge playersList

                                -- Get next prompt
                                ( nextPrompt, remainingPromptsAfter ) =
                                    case playingState.remainingPrompts of
                                        prompt :: rest ->
                                            ( prompt, rest )

                                        [] ->
                                            -- No more prompts, end game
                                            ( "", [] )

                                -- Transition to NextRound phase to show prompt preview
                                nextRoundPhase =
                                    if String.isEmpty nextPrompt then
                                        -- No more prompts, stay in RoundComplete
                                        RoundComplete
                                            { winner = winnerToken
                                            , winningCard = winningCard
                                            }

                                    else
                                        NextRound
                                            { nextPrompt = nextPrompt
                                            , nextJudge = nextJudge
                                            }

                                updatedGameState =
                                    Playing
                                        { playingState
                                            | roundPhase = Just nextRoundPhase
                                            , currentJudge = nextJudge
                                            , remainingPrompts = remainingPromptsAfter
                                        }
                            in
                            ( { model
                                | players = updatedPlayers
                                , gameState = updatedGameState
                              }
                            , Command.batch
                                [ Effect.Lamdera.broadcast (WinnerSelected { winner = winnerToken, winningCard = winningCard })
                                , Effect.Lamdera.broadcast (PlayersListUpdated (Dict.values updatedPlayers))
                                , Effect.Lamdera.broadcast (RoundPhaseUpdated nextRoundPhase)
                                ]
                            )

                        _ ->
                            -- Not in judging phase, ignore
                            ( model, Command.none )

                _ ->
                    -- Not playing, ignore
                    ( model, Command.none )

        AcceptPrompt ->
            -- Judge accepts the prompt, start new round
            case model.gameState of
                Playing playingState ->
                    case playingState.roundPhase of
                        Just (NextRound { nextPrompt, nextJudge }) ->
                            let
                                -- Deal replacement cards to players (1 card each)
                                ( playersWithNewCards, remainingAnswers ) =
                                    dealReplacementCards (Dict.values model.players) playingState.remainingAnswers

                                newPlayers =
                                    playersWithNewCards
                                        |> List.map (\p -> ( tokenToString p.token, p ))
                                        |> Dict.fromList

                                -- Start new submission phase
                                newRoundPhase =
                                    SubmissionPhase
                                        { prompt = nextPrompt
                                        , submissions = []
                                        }

                                updatedGameState =
                                    Playing
                                        { playingState
                                            | roundPhase = Just newRoundPhase
                                            , remainingAnswers = remainingAnswers
                                        }
                            in
                            ( { model
                                | players = newPlayers
                                , gameState = updatedGameState
                              }
                            , Effect.Lamdera.broadcast (RoundPhaseUpdated newRoundPhase)
                            )

                        _ ->
                            ( model, Command.none )

                _ ->
                    ( model, Command.none )

        VetoPrompt ->
            -- Judge vetoes the prompt, cycle to next one
            case model.gameState of
                Playing playingState ->
                    case playingState.roundPhase of
                        Just (NextRound { nextJudge }) ->
                            let
                                -- Get next prompt from remaining
                                ( newNextPrompt, newRemainingPrompts ) =
                                    case playingState.remainingPrompts of
                                        prompt :: rest ->
                                            ( prompt, rest )

                                        [] ->
                                            -- No more prompts, end game
                                            ( "", [] )

                                updatedPhase =
                                    if String.isEmpty newNextPrompt then
                                        -- No more prompts, game ends
                                        RoundComplete
                                            { winner = nextJudge
                                            , winningCard = "No more prompts"
                                            }

                                    else
                                        NextRound
                                            { nextPrompt = newNextPrompt
                                            , nextJudge = nextJudge
                                            }

                                updatedGameState =
                                    Playing
                                        { playingState
                                            | roundPhase = Just updatedPhase
                                            , remainingPrompts = newRemainingPrompts
                                        }
                            in
                            ( { model | gameState = updatedGameState }
                            , Effect.Lamdera.broadcast (RoundPhaseUpdated updatedPhase)
                            )

                        _ ->
                            ( model, Command.none )

                _ ->
                    ( model, Command.none )

        EndGame ->
            -- End the current game and return to lobby
            ( { model | gameState = Ended }
            , Effect.Lamdera.broadcast (GameStateUpdated Ended)
            )

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
                                Command.batch
                                    (Effect.Lamdera.broadcast (GameStateUpdated newGameState) :: playerCommands)
                        in
                        ( { model
                            | players = newPlayers
                            , gameState = newGameState
                          }
                        , commands
                        )


-- HELPER FUNCTIONS


sessionIdToString : SessionId -> String
sessionIdToString sessionId =
    -- Use the Effect.Lamdera function to convert SessionId to String
    Effect.Lamdera.sessionIdToString sessionId


tokenToString : PlayerToken -> String
tokenToString (PlayerToken str) =
    str


getNextJudge : PlayerToken -> List Player -> PlayerToken
getNextJudge currentJudge players =
    -- Find current judge index and rotate to next player
    let
        indexed =
            List.indexedMap Tuple.pair players

        currentIndex =
            indexed
                |> List.filter (\( _, player ) -> player.token == currentJudge)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        nextIndex =
            modBy (List.length players) (currentIndex + 1)
    in
    indexed
        |> List.filter (\( idx, _ ) -> idx == nextIndex)
        |> List.head
        |> Maybe.map (Tuple.second >> .token)
        |> Maybe.withDefault currentJudge


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


dealReplacementCards : List Player -> List String -> ( List Player, List String )
dealReplacementCards players availableCards =
    -- Deal 1 replacement card to each player
    dealReplacementCardsHelper players availableCards []


dealReplacementCardsHelper : List Player -> List String -> List Player -> ( List Player, List String )
dealReplacementCardsHelper players availableCards accum =
    case players of
        [] ->
            ( List.reverse accum, availableCards )

        player :: rest ->
            let
                -- Take 1 card for this player
                newCard =
                    List.head availableCards

                remaining =
                    List.drop 1 availableCards

                updatedPlayer =
                    case newCard of
                        Just card ->
                            { player | hand = player.hand ++ [ card ] }

                        Nothing ->
                            player
            in
            dealReplacementCardsHelper rest remaining (updatedPlayer :: accum)


subscriptions model =
    Subscription.batch
        [ Effect.Lamdera.onConnect ClientConnected
        ]
