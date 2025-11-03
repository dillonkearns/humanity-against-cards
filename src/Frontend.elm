module Frontend exposing (Model, app, app_)

import Browser
import Dict
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription
import Effect.Browser.Navigation as Navigation
import Effect.Task
import Effect.Time
import Html exposing (Html, text)
import Html.Attributes exposing (id, style, value, placeholder, disabled, class)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Lamdera
import Time
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type alias Model =
    FrontendModel


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        app_


app_ =
    { init = init
    , update = update
    , updateFromBackend = updateFromBackend
    , view =
        \model ->
            { title = "Humanity Against Cards"
            , body = [ view model ]
            }
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = UrlRequested
    }


-- ROUTING


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map PlayerRoute Parser.top
        , Parser.map AdminRoute (Parser.s "admin")
        ]


urlToRoute : Url -> Route
urlToRoute url =
    Parser.parse routeParser url
        |> Maybe.withDefault PlayerRoute


-- INIT


init : Url -> Navigation.Key -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
init url key =
    ( { counter = 0
      , clientId = Nothing
      , route = urlToRoute url
      , key = key
      , deckJsonInput = ""
      , loadedDeck = Nothing
      , playerToken = Nothing
      , playerNameInput = ""
      , joinedPlayer = Nothing
      , playersList = []
      , gameState = Lobby
      , myHand = []
      , currentJudge = Nothing
      , selectedCard = Nothing
      , hasSubmitted = False
      }
    , Command.none
    )


-- JSON DECODERS


deckDecoder : Decoder Deck
deckDecoder =
    Decode.map2 Deck
        (Decode.field "prompts" (Decode.list Decode.string))
        (Decode.field "answers" (Decode.list Decode.string))


-- UPDATE


update : FrontendMsg -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Effect.Lamdera.sendToBackend CounterIncremented )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Effect.Lamdera.sendToBackend CounterDecremented )

        FNoop ->
            ( model, Command.none )

        UrlChanged url ->
            ( { model | route = urlToRoute url }, Command.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Navigation.load url )

        DeckJsonInputChanged input ->
            ( { model | deckJsonInput = input }, Command.none )

        LoadDeckClicked ->
            case Decode.decodeString deckDecoder model.deckJsonInput of
                Ok deck ->
                    ( { model | loadedDeck = Just deck }
                    , Effect.Lamdera.sendToBackend (LoadDeck deck)
                    )

                Err _ ->
                    -- For now, just do nothing on parse error
                    -- We could add error display later
                    ( model, Command.none )

        PlayerNameInputChanged input ->
            ( { model | playerNameInput = input }, Command.none )

        JoinGameClicked ->
            let
                trimmedName =
                    String.trim model.playerNameInput
            in
            if String.isEmpty trimmedName then
                ( model, Command.none )

            else
                -- Backend will generate token from SessionId
                ( model
                , Effect.Lamdera.sendToBackend (JoinGame (PlayerToken "") trimmedName)
                )

        RemovePlayerClicked playerToken ->
            ( model, Effect.Lamdera.sendToBackend (RemovePlayer playerToken) )

        StartGameClicked ->
            ( model, Effect.Lamdera.sendToBackend StartGame )

        EndGameClicked ->
            ( model, Effect.Lamdera.sendToBackend EndGame )

        CardSelected card ->
            ( { model | selectedCard = Just card }, Command.none )

        SubmitCardClicked ->
            case ( model.playerToken, model.selectedCard ) of
                ( Just token, Just card ) ->
                    ( { model | hasSubmitted = True }
                    , Effect.Lamdera.sendToBackend (SubmitCard token card)
                    )

                _ ->
                    ( model, Command.none )

        RevealNextCardClicked ->
            ( model, Effect.Lamdera.sendToBackend RevealNextCard )

        SelectWinnerClicked winnerToken ->
            ( model, Effect.Lamdera.sendToBackend (SelectWinner winnerToken) )

        AcceptPromptClicked ->
            ( model, Effect.Lamdera.sendToBackend AcceptPrompt )

        VetoPromptClicked ->
            ( model, Effect.Lamdera.sendToBackend VetoPrompt )

        ReactToCardClicked card reaction ->
            case model.playerToken of
                Just token ->
                    ( model, Effect.Lamdera.sendToBackend (AddReaction token card reaction) )

                Nothing ->
                    ( model, Command.none )


-- UPDATE FROM BACKEND


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue clientId ->
            ( { model | counter = newValue, clientId = Just clientId }, Command.none )

        DeckLoaded deck ->
            ( { model | loadedDeck = Just deck }, Command.none )

        PlayerJoined player ->
            ( { model
                | joinedPlayer = Just player
                , playerToken = Just player.token
              }
            , Command.none
            )

        PlayerReconnected player hand ->
            -- Restore player state after reconnection
            let
                updatedGameState =
                    case model.gameState of
                        Playing playingState ->
                            -- Restore current judge info
                            Playing { playingState | currentJudge = playingState.currentJudge }

                        other ->
                            other
            in
            ( { model
                | joinedPlayer = Just player
                , playerToken = Just player.token
                , myHand = hand
                , gameState = updatedGameState
                , currentJudge =
                    case model.gameState of
                        Playing playingState ->
                            Just playingState.currentJudge

                        _ ->
                            Nothing
              }
            , Command.none
            )

        PlayersListUpdated players ->
            ( { model | playersList = players }, Command.none )

        GameStarted { yourHand, currentJudge } ->
            -- Game started, hand received. RoundPhaseUpdated will set the phase.
            ( { model
                | myHand = yourHand
                , currentJudge = Just currentJudge
                , hasSubmitted = False
              }
            , Command.none
            )

        GameStateUpdated gameState ->
            let
                -- Reset submission status when new round starts
                resetSubmission =
                    case gameState of
                        Playing playingState ->
                            case playingState.roundPhase of
                                Just (SubmissionPhase _) ->
                                    True

                                _ ->
                                    False

                        _ ->
                            False
            in
            ( { model
                | gameState = gameState
                , currentJudge =
                    case gameState of
                        Playing playingState ->
                            Just playingState.currentJudge

                        _ ->
                            model.currentJudge
                , hasSubmitted = if resetSubmission then False else model.hasSubmitted
                , selectedCard = if resetSubmission then Nothing else model.selectedCard
              }
            , Command.none
            )

        RoundPhaseUpdated roundPhase ->
            let
                updatedGameState =
                    case model.gameState of
                        Playing playingState ->
                            Playing { playingState | roundPhase = Just roundPhase }

                        other ->
                            other

                -- Reset submission status when new round starts
                resetSubmission =
                    case roundPhase of
                        SubmissionPhase _ ->
                            True

                        _ ->
                            False

                -- Update currentJudge when entering NextRound phase
                updatedCurrentJudge =
                    case roundPhase of
                        NextRound { nextJudge } ->
                            Just nextJudge

                        _ ->
                            model.currentJudge
            in
            ( { model
                | gameState = updatedGameState
                , hasSubmitted = if resetSubmission then False else model.hasSubmitted
                , selectedCard = if resetSubmission then Nothing else model.selectedCard
                , currentJudge = updatedCurrentJudge
              }
            , Command.none
            )

        CardSubmitted ->
            -- Confirmation message, already handled locally
            ( model, Command.none )

        WinnerSelected { winner, winningCard } ->
            -- Store winner info to display announcement
            let
                updatedGameState =
                    case model.gameState of
                        Playing playingState ->
                            Playing
                                { playingState
                                    | roundPhase =
                                        Just
                                            (RoundComplete
                                                { winner = winner
                                                , winningCard = winningCard
                                                }
                                            )
                                }

                        other ->
                            other
            in
            ( { model | gameState = updatedGameState }, Command.none )

        HandUpdated newHand ->
            ( { model | myHand = newHand }, Command.none )


-- VIEW


view : Model -> Html FrontendMsg
view model =
    case model.route of
        PlayerRoute ->
            viewPlayer model

        AdminRoute ->
            viewAdmin model


viewPlayer : Model -> Html FrontendMsg
viewPlayer model =
    Html.div []
        [ Html.div [ class "app-header" ]
            [ Html.h1 [ class "app-title" ] [ text "Humanity Against Cards" ]
            ]
        , Html.div [ style "padding" "30px" ]
            [ case model.joinedPlayer of
            Nothing ->
                viewJoinForm model

            Just player ->
                case model.gameState of
                    Lobby ->
                        viewPlayerLobby player

                    Playing _ ->
                        viewPlayerGame player model

                    Ended ->
                        viewGameEnded model.playersList
            ]
        ]


viewGameEnded : List Player -> Html FrontendMsg
viewGameEnded players =
    let
        sortedPlayers =
            List.sortBy (\p -> -p.score) players

        topScore =
            sortedPlayers
                |> List.head
                |> Maybe.map .score
                |> Maybe.withDefault 0

        winners =
            sortedPlayers
                |> List.filter (\p -> p.score == topScore && p.score > 0)

        isTie =
            List.length winners > 1
    in
    Html.div
        [ style "text-align" "center"
        , style "padding" "40px"
        ]
        [ Html.h2
            [ style "font-size" "36px"
            , style "margin-bottom" "20px"
            ]
            [ text "🎉 Game Over! 🎉" ]
        , if topScore > 0 then
            Html.div
                [ style "margin-bottom" "40px" ]
                [ Html.div
                    [ style "font-size" "48px"
                    , style "margin-bottom" "10px"
                    ]
                    [ text "🏆" ]
                , Html.h3
                    [ style "font-size" "28px"
                    , style "color" "#4CAF50"
                    , style "margin-bottom" "10px"
                    , id "game-winner"
                    ]
                    [ if isTie then
                        text ("It's a tie! " ++ String.join ", " (List.map .name winners))

                      else
                        case List.head winners of
                            Just w ->
                                text (w.name ++ " wins!")

                            Nothing ->
                                text ""
                    ]
                , Html.p
                    [ style "font-size" "20px"
                    , style "color" "#666"
                    ]
                    [ text (String.fromInt topScore ++ " points") ]
                ]

          else
            Html.div
                [ style "margin-bottom" "40px" ]
                [ Html.p
                    [ style "font-size" "20px"
                    , style "color" "#666"
                    ]
                    [ text "No rounds were completed" ]
                ]
        , Html.div
            [ style "max-width" "600px"
            , style "margin" "0 auto"
            ]
            [ Html.h3
                [ style "margin-bottom" "20px" ]
                [ text "Final Standings" ]
            , Html.div
                [ id "final-standings"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "gap" "10px"
                ]
                (List.indexedMap viewFinalStanding sortedPlayers)
            ]
        ]


viewFinalStanding : Int -> Player -> Html FrontendMsg
viewFinalStanding index player =
    Html.div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "space-between"
        , style "padding" "15px 20px"
        , style "background-color" (if index == 0 && player.score > 0 then "#e8f5e9" else "#f9f9f9")
        , style "border-radius" "8px"
        , style "border" (if index == 0 && player.score > 0 then "2px solid #4CAF50" else "1px solid #ddd")
        ]
        [ Html.span
            [ style "font-size" "18px"
            , style "font-weight" (if index == 0 && player.score > 0 then "bold" else "normal")
            ]
            [ text player.name ]
        , Html.div
            [ style "font-size" "20px"
            , style "font-weight" "bold"
            , style "color" (if index == 0 && player.score > 0 then "#4CAF50" else "#666")
            ]
            [ text (String.fromInt player.score ++ " pts") ]
        ]


viewJoinForm : Model -> Html FrontendMsg
viewJoinForm model =
    Html.div []
        [ Html.h2 [] [ text "Join Game" ]
        , Html.input
            [ placeholder "Enter your name..."
            , value model.playerNameInput
            , onInput PlayerNameInputChanged
            , id "player-name-input"
            , style "padding" "10px"
            , style "font-size" "16px"
            , style "margin-bottom" "10px"
            , style "width" "300px"
            ]
            []
        , Html.div []
            [ Html.button
                [ onClick JoinGameClicked
                , id "join-game-button"
                , style "padding" "10px 20px"
                , style "font-size" "16px"
                , disabled (String.isEmpty (String.trim model.playerNameInput))
                ]
                [ text "Join Game" ]
            ]
        ]


viewPlayerLobby : Player -> Html FrontendMsg
viewPlayerLobby player =
    Html.div []
        [ Html.h2 [] [ text ("Welcome, " ++ player.name ++ "!") ]
        , Html.p [ id "player-joined-message" ] [ text "Waiting for game to start..." ]
        ]


getJudgeName : Maybe PlayerToken -> List Player -> String
getJudgeName maybeJudgeToken players =
    case maybeJudgeToken of
        Just judgeToken ->
            players
                |> List.filter (\p -> p.token == judgeToken)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault "the judge"

        Nothing ->
            "the judge"


viewScoreboard : List Player -> Maybe PlayerToken -> Html FrontendMsg
viewScoreboard players currentJudge =
    let
        sortedPlayers =
            List.sortBy (\p -> -p.score) players
    in
    Html.div
        [ id "scoreboard"
        , style "margin-bottom" "20px"
        , style "padding" "15px"
        , style "background-color" "#f9f9f9"
        , style "border-radius" "8px"
        , style "border" "1px solid #ddd"
        ]
        [ Html.h3 [ style "margin-top" "0", style "margin-bottom" "10px" ] [ text "Scores" ]
        , Html.div [ style "display" "flex", style "flex-wrap" "wrap", style "gap" "10px" ]
            (List.map (viewPlayerScore currentJudge) sortedPlayers)
        ]


viewPlayerScore : Maybe PlayerToken -> Player -> Html FrontendMsg
viewPlayerScore currentJudge player =
    let
        isJudge =
            currentJudge == Just player.token
    in
    Html.div
        [ style "padding" "8px 12px"
        , style "background-color" (if isJudge then "#e3f2fd" else "white")
        , style "border-radius" "4px"
        , style "border" (if isJudge then "2px solid #2196F3" else "1px solid #ddd")
        , style "min-width" "100px"
        ]
        [ Html.div
            [ style "font-weight" "bold"
            , style "margin-bottom" "4px"
            ]
            [ text (player.name ++ if isJudge then " ⚖️" else "") ]
        , Html.div
            [ style "font-size" "20px"
            , style "color" "#4CAF50"
            ]
            [ text (String.fromInt player.score ++ " pts") ]
        ]


viewPlayerGame : Player -> Model -> Html FrontendMsg
viewPlayerGame player model =
    let
        isJudge =
            model.currentJudge == Just player.token

        roundPhase =
            case model.gameState of
                Playing playingState ->
                    playingState.roundPhase

                _ ->
                    Nothing

        headerText =
            case roundPhase of
                Just (SubmissionPhase _) ->
                    if isJudge then
                        player.name ++ " - You're judging this round"
                    else
                        let
                            judgeName =
                                getJudgeName model.currentJudge model.playersList
                        in
                        player.name ++ " - Pick your best card for " ++ judgeName

                Just (RevealPhase _) ->
                    if isJudge then
                        player.name ++ " - Reveal the answers"
                    else
                        "Watch the judge reveal..."

                Just (JudgingPhase _) ->
                    if isJudge then
                        player.name ++ " - Pick the winner"
                    else
                        "Waiting for the judge's decision..."

                Just (RoundComplete _) ->
                    "Round Complete!"

                Just (NextRound _) ->
                    if isJudge then
                        player.name ++ " - Ready for next round?"
                    else
                        "Round Complete!"

                Nothing ->
                    "Welcome, " ++ player.name ++ "!"
    in
    Html.div []
        [ Html.h2 [ class "app-subtitle" ] [ text headerText ]
        , viewScoreboard model.playersList model.currentJudge
        , case roundPhase of
            Nothing ->
                Html.p [] [ text "Round starting..." ]

            Just phase ->
                viewRoundPhase player model isJudge phase
        ]


viewRoundPhase : Player -> Model -> Bool -> RoundPhase -> Html FrontendMsg
viewRoundPhase player model isJudge roundPhase =
    case roundPhase of
        SubmissionPhase { prompt, submissions } ->
            Html.div []
                [ Html.div [ style "margin-bottom" "20px" ]
                    [ Html.h3 [ style "margin-bottom" "10px" ] [ text "Current Prompt:" ]
                    , Html.div
                        [ class "prompt-card"
                        , id "current-prompt"
                        ]
                        [ text prompt ]
                    ]
                , if isJudge then
                    Html.div []
                        [ Html.p [] [ text "You are the judge this round!" ]
                        , Html.p [] [ text ("Waiting for " ++ String.fromInt (List.length submissions) ++ " submissions...") ]
                        ]

                  else
                    viewCardSelection model
                ]

        RevealPhase { prompt, submissions, revealedCount } ->
            Html.div []
                [ Html.div [ style "margin-bottom" "20px" ]
                    [ Html.h3 [ style "margin-bottom" "10px" ] [ text "Current Prompt:" ]
                    , Html.div
                        [ class "prompt-card"
                        , id "current-prompt"
                        ]
                        [ text prompt ]
                    ]
                , if isJudge then
                    viewJudgeReveal submissions revealedCount

                  else
                    Html.div []
                        [ Html.h3 [] [ text "Revealing cards..." ]
                        , Html.p [ id "reveal-status" ]
                            [ text (String.fromInt revealedCount ++ " of " ++ String.fromInt (List.length submissions) ++ " cards revealed") ]
                        ]
                ]

        JudgingPhase { prompt, submissions, reactions } ->
            Html.div []
                [ Html.div [ style "margin-bottom" "20px" ]
                    [ Html.h3 [ style "margin-bottom" "10px" ] [ text "Current Prompt:" ]
                    , Html.div
                        [ class "prompt-card"
                        , id "current-prompt"
                        ]
                        [ text prompt ]
                    ]
                , if isJudge then
                    viewJudgeSelection prompt submissions

                  else
                    Html.div []
                        [ Html.h3 [] [ text "All cards revealed!" ]
                        , Html.p [] [ text "Waiting for judge to select winner..." ]
                        , viewSubmittedCardsWithReactions submissions reactions model.playerToken
                        ]
                ]

        RoundComplete { winner, winningCard } ->
            viewWinnerAnnouncement winner winningCard model.playersList

        NextRound { nextPrompt, nextJudge, previousSubmissions, previousReactions } ->
            Html.div []
                [ if not (List.isEmpty previousSubmissions) then
                    Html.div [ style "margin-bottom" "30px" ]
                        [ Html.h3 [ style "text-align" "center" ] [ text "Previous Round Results" ]
                        , viewSubmittedCardsWithReactionTotals previousSubmissions previousReactions
                        ]

                  else
                    Html.text ""
                , if isJudge then
                    viewJudgePromptPreview nextPrompt

                  else
                    Html.div
                        [ style "text-align" "center"
                        , style "padding" "40px"
                        ]
                        [ Html.h3 [] [ text "Round Complete!" ]
                        , Html.p
                            [ style "font-size" "18px"
                            , style "color" "#666"
                            , style "margin-top" "20px"
                            ]
                            [ text "Waiting for the next judge to start the next round..." ]
                        ]
                ]


viewJudgePromptPreview : String -> Html FrontendMsg
viewJudgePromptPreview prompt =
    Html.div
        [ style "text-align" "center"
        , style "padding" "40px"
        ]
        [ Html.h2
            [ style "margin-bottom" "30px" ]
            [ text "You're the judge for the next round!" ]
        , Html.div
            [ style "margin-bottom" "30px"
            , style "max-width" "600px"
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
            [ Html.h3
                [ style "margin-bottom" "10px" ]
                [ text "Next Prompt:" ]
            , Html.div
                [ class "prompt-card"
                , id "next-prompt-preview"
                ]
                [ text prompt ]
            ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "justify-content" "center"
            ]
            [ Html.button
                [ onClick AcceptPromptClicked
                , id "accept-prompt-button"
                , style "padding" "15px 40px"
                , style "font-size" "18px"
                , style "background-color" "#4CAF50"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "4px"
                , style "cursor" "pointer"
                ]
                [ text "✓ Accept" ]
            , Html.button
                [ onClick VetoPromptClicked
                , id "veto-prompt-button"
                , style "padding" "15px 40px"
                , style "font-size" "18px"
                , style "background-color" "#f44336"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "4px"
                , style "cursor" "pointer"
                ]
                [ text "✗ Veto" ]
            ]
        ]


viewCardSelection : Model -> Html FrontendMsg
viewCardSelection model =
    Html.div []
        [ Html.h3 [] [ text "Your Hand:" ]
        , if model.hasSubmitted then
            Html.p [ style "color" "green", id "submission-status" ]
                [ text "✓ Card submitted! Waiting for other players..." ]

          else
            Html.div []
                [ Html.ul [ id "player-hand", style "list-style" "none", style "padding" "0" ]
                    (List.indexedMap (viewCardButton model.selectedCard) model.myHand)
                , case model.selectedCard of
                    Nothing ->
                        Html.p [ style "color" "#666" ] [ text "Select a card to submit" ]

                    Just _ ->
                        Html.button
                            [ onClick SubmitCardClicked
                            , id "submit-card-button"
                            , style "padding" "10px 20px"
                            , style "font-size" "16px"
                            , style "background-color" "#4CAF50"
                            , style "color" "white"
                            , style "border" "none"
                            , style "border-radius" "4px"
                            , style "cursor" "pointer"
                            ]
                            [ text "Submit Card" ]
                ]
        ]


viewJudgeReveal : List Submission -> Int -> Html FrontendMsg
viewJudgeReveal submissions revealedCount =
    let
        revealedSubmissions =
            List.take revealedCount submissions

        hasMore =
            revealedCount < List.length submissions
    in
    Html.div []
        [ Html.h3 [] [ text "Reveal Cards One-by-One" ]
        , Html.div [ id "revealed-cards", style "margin-bottom" "20px" ]
            (List.indexedMap viewRevealedCard revealedSubmissions)
        , if hasMore then
            Html.button
                [ onClick RevealNextCardClicked
                , id "reveal-next-button"
                , style "padding" "15px 30px"
                , style "font-size" "18px"
                , style "background-color" "#2196F3"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "4px"
                , style "cursor" "pointer"
                ]
                [ text ("Reveal Card " ++ String.fromInt (revealedCount + 1)) ]

          else
            Html.p [ style "color" "green", style "font-weight" "bold" ]
                [ text "All cards revealed! Now select the winner." ]
        ]


viewRevealedCard : Int -> Submission -> Html FrontendMsg
viewRevealedCard index submission =
    Html.div
        [ style "padding" "15px"
        , style "margin-bottom" "10px"
        , style "background-color" "white"
        , style "border" "2px solid #ddd"
        , style "border-radius" "8px"
        ]
        [ Html.p [ style "font-size" "16px", style "margin" "0" ] [ text submission.card ] ]


viewJudgeSelection : String -> List Submission -> Html FrontendMsg
viewJudgeSelection prompt submissions =
    Html.div []
        [ Html.h3 [] [ text "Select the Winner!" ]
        , Html.div [ id "judging-cards" ]
            (List.indexedMap viewJudgingCard submissions)
        ]


viewJudgingCard : Int -> Submission -> Html FrontendMsg
viewJudgingCard index submission =
    Html.div
        [ style "margin-bottom" "15px" ]
        [ viewCard [] submission.card
        , Html.button
            [ onClick (SelectWinnerClicked submission.playerToken)
            , id ("select-winner-" ++ String.fromInt index)
            , style "margin-top" "10px"
            , style "padding" "10px 20px"
            , style "font-size" "14px"
            , style "background-color" "#4CAF50"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            ]
            [ text "Select Winner" ]
        ]


viewSubmittedCards : List Submission -> Html FrontendMsg
viewSubmittedCards submissions =
    Html.div [ id "submitted-cards" ]
        (List.map
            (\submission ->
                viewCard [ style "margin-bottom" "15px" ] submission.card
            )
            submissions
        )


viewSubmittedCardsWithReactions : List Submission -> List PlayerReaction -> Maybe PlayerToken -> Html FrontendMsg
viewSubmittedCardsWithReactions submissions reactions maybePlayerToken =
    Html.div [ id "submitted-cards" ]
        (List.indexedMap
            (\index submission ->
                let
                    isOwnSubmission =
                        case maybePlayerToken of
                            Just token ->
                                submission.playerToken == token

                            Nothing ->
                                False

                    playerReaction =
                        case maybePlayerToken of
                            Just token ->
                                reactions
                                    |> List.filter (\r -> r.playerToken == token && r.submissionCard == submission.card)
                                    |> List.head
                                    |> Maybe.map .reaction

                            Nothing ->
                                Nothing
                in
                Html.div
                    [ style "margin-bottom" "15px" ]
                    [ viewCard [] submission.card
                    , if isOwnSubmission then
                        Html.p
                            [ style "text-align" "center"
                            , style "color" "#999"
                            , style "font-style" "italic"
                            , style "margin-top" "10px"
                            , id ("your-card-" ++ String.fromInt index)
                            ]
                            [ text "Your card" ]

                      else
                        Html.div
                            [ style "display" "flex"
                            , style "gap" "10px"
                            , style "justify-content" "center"
                            , style "margin-top" "10px"
                            ]
                            [ viewReactionButton index submission.card Laugh "😂" (playerReaction == Just Laugh)
                            , viewReactionButton index submission.card Grimace "😬" (playerReaction == Just Grimace)
                            , viewReactionButton index submission.card MindBlown "🤯" (playerReaction == Just MindBlown)
                            ]
                    ]
            )
            submissions
        )


viewReactionButton : Int -> String -> Reaction -> String -> Bool -> Html FrontendMsg
viewReactionButton cardIndex card reaction emoji isSelected =
    let
        reactionName =
            case reaction of
                Laugh ->
                    "laugh"

                Grimace ->
                    "grimace"

                MindBlown ->
                    "mindblown"
    in
    Html.button
        [ onClick (ReactToCardClicked card reaction)
        , id ("react-" ++ reactionName ++ "-" ++ String.fromInt cardIndex)
        , style "padding" "8px 16px"
        , style "font-size" "24px"
        , style "background-color" (if isSelected then "#4CAF50" else "white")
        , style "border" (if isSelected then "2px solid #4CAF50" else "2px solid #ddd")
        , style "border-radius" "8px"
        , style "cursor" "pointer"
        ]
        [ text emoji ]


viewSubmittedCardsWithReactionTotals : List Submission -> List PlayerReaction -> Html FrontendMsg
viewSubmittedCardsWithReactionTotals submissions reactions =
    Html.div [ id "previous-round-cards" ]
        (List.map
            (\submission ->
                let
                    laughCount =
                        reactions
                            |> List.filter (\r -> r.submissionCard == submission.card && r.reaction == Laugh)
                            |> List.length

                    grimaceCount =
                        reactions
                            |> List.filter (\r -> r.submissionCard == submission.card && r.reaction == Grimace)
                            |> List.length

                    mindBlownCount =
                        reactions
                            |> List.filter (\r -> r.submissionCard == submission.card && r.reaction == MindBlown)
                            |> List.length
                in
                Html.div
                    [ style "margin-bottom" "15px" ]
                    [ viewCard [] submission.card
                    , Html.div
                        [ style "display" "flex"
                        , style "gap" "15px"
                        , style "justify-content" "center"
                        , style "font-size" "18px"
                        , style "margin-top" "10px"
                        ]
                        [ Html.span [] [ text ("😂 " ++ String.fromInt laughCount) ]
                        , Html.span [] [ text ("😬 " ++ String.fromInt grimaceCount) ]
                        , Html.span [] [ text ("🤯 " ++ String.fromInt mindBlownCount) ]
                        ]
                    ]
            )
            submissions
        )


viewWinnerAnnouncement : PlayerToken -> String -> List Player -> Html FrontendMsg
viewWinnerAnnouncement winnerToken winningCard players =
    let
        winnerName =
            players
                |> List.filter (\p -> p.token == winnerToken)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault "Unknown"
    in
    Html.div
        [ id "winner-announcement"
        , style "text-align" "center"
        , style "padding" "40px"
        ]
        [ Html.div
            [ style "font-size" "48px"
            , style "margin-bottom" "20px"
            ]
            [ text "🏆" ]
        , Html.h2
            [ style "font-size" "32px"
            , style "margin-bottom" "20px"
            , style "color" "#4CAF50"
            ]
            [ text (winnerName ++ " wins this round!") ]
        , Html.div
            [ style "padding" "20px"
            , style "background-color" "#f0f0f0"
            , style "border-radius" "8px"
            , style "margin-bottom" "30px"
            , style "max-width" "600px"
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
            [ Html.h3 [ style "margin-top" "0" ] [ text "Winning Card:" ]
            , Html.p
                [ style "font-size" "20px"
                , style "font-style" "italic"
                ]
                [ text ("\"" ++ winningCard ++ "\"") ]
            ]
        , Html.p
            [ style "font-size" "18px"
            , style "color" "#666"
            ]
            [ text "Waiting for next round..." ]
        ]


viewCard : List (Html.Attribute msg) -> String -> Html msg
viewCard attrs cardText =
    Html.div
        (class "card-content" :: attrs)
        [ text cardText ]


viewCardButton : Maybe String -> Int -> String -> Html FrontendMsg
viewCardButton selectedCard index card =
    let
        isSelected =
            selectedCard == Just card
    in
    Html.li
        [ style "margin-bottom" "10px" ]
        [ Html.button
            [ onClick (CardSelected card)
            , id ("card-" ++ String.fromInt index)
            , class "card-content"
            , style "width" "100%"
            , style "text-align" "left"
            , style "border" (if isSelected then "3px solid #4CAF50" else "2px solid #1f2937")
            , style "background-color" (if isSelected then "#e8f5e9" else "white")
            , style "cursor" "pointer"
            ]
            [ text card ]
        ]


viewAdmin : Model -> Html FrontendMsg
viewAdmin model =
    Html.div []
        [ Html.div [ class "app-header" ]
            [ Html.h1 [ class "app-title" ] [ text "Admin Console" ]
            ]
        , Html.div [ style "padding" "30px" ]
            [ viewPlayersList model.playersList
        , viewGameControls model
        , Html.h2 [] [ text "Load Deck" ]
        , Html.textarea
            [ placeholder "Paste deck JSON here..."
            , value model.deckJsonInput
            , onInput DeckJsonInputChanged
            , style "width" "100%"
            , style "min-height" "200px"
            , style "font-family" "monospace"
            , id "deck-json-input"
            ]
            []
        , Html.div []
            [ Html.button
                [ onClick LoadDeckClicked
                , id "load-deck-button"
                , style "margin-top" "10px"
                ]
                [ text "Load Deck" ]
            ]
        , case model.loadedDeck of
            Nothing ->
                Html.div [] [ text "No deck loaded yet" ]

            Just deck ->
                viewLoadedDeck deck
            ]
        ]


viewGameControls : Model -> Html FrontendMsg
viewGameControls model =
    case model.gameState of
        Lobby ->
            let
                canStart =
                    not (List.isEmpty model.playersList) && model.loadedDeck /= Nothing
            in
            Html.div [ style "margin-bottom" "30px" ]
                [ Html.h2 [] [ text "Game Controls" ]
                , Html.button
                    [ onClick StartGameClicked
                    , id "start-game-button"
                    , style "padding" "10px 20px"
                    , style "font-size" "16px"
                    , disabled (not canStart)
                    ]
                    [ text "Start Game" ]
                , if not canStart then
                    Html.p [ style "color" "#666" ]
                        [ text "Need at least one player and a loaded deck to start" ]

                  else
                    Html.text ""
                ]

        Playing _ ->
            Html.div [ style "margin-bottom" "30px" ]
                [ Html.h2 [] [ text "Game Controls" ]
                , Html.p [ style "margin-bottom" "10px" ] [ text "Game in progress..." ]
                , Html.button
                    [ onClick EndGameClicked
                    , id "end-game-button"
                    , style "padding" "10px 20px"
                    , style "font-size" "16px"
                    , style "background-color" "#f44336"
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "4px"
                    , style "cursor" "pointer"
                    ]
                    [ text "End Game" ]
                ]

        Ended ->
            Html.div [ style "margin-bottom" "30px" ]
                [ Html.h2 [] [ text "Game Controls" ]
                , Html.p [] [ text "Game ended" ]
                ]


viewPlayersList : List Player -> Html FrontendMsg
viewPlayersList players =
    Html.div [ style "margin-bottom" "30px" ]
        [ Html.h2 [] [ text "Joined Players" ]
        , if List.isEmpty players then
            Html.p [] [ text "No players yet" ]

          else
            Html.ul [ id "players-list", style "list-style" "none", style "padding" "0" ]
                (List.map viewPlayerListItem players)
        ]


viewPlayerListItem : Player -> Html FrontendMsg
viewPlayerListItem player =
    Html.li
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "10px"
        , style "margin-bottom" "8px"
        ]
        [ Html.span [ style "flex" "1" ] [ text player.name ]
        , Html.button
            [ onClick (RemovePlayerClicked player.token)
            , id ("remove-player-" ++ tokenToString player.token)
            , style "padding" "5px 10px"
            , style "font-size" "12px"
            , style "background-color" "#f44336"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            ]
            [ text "Remove" ]
        ]


viewLoadedDeck : Deck -> Html FrontendMsg
viewLoadedDeck deck =
    Html.div [ style "margin-top" "30px" ]
        [ Html.h3 [] [ text "Loaded Deck" ]
        , Html.h4 [] [ text "Prompts:" ]
        , Html.ul [ id "prompts-list" ]
            (List.map (\prompt -> Html.li [] [ text prompt ]) deck.prompts)
        , Html.h4 [] [ text "Answers:" ]
        , Html.ul [ id "answers-list" ]
            (List.map (\answer -> Html.li [] [ text answer ]) deck.answers)
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Subscription.Subscription FrontendOnly FrontendMsg
subscriptions model =
    Subscription.none


-- HELPER FUNCTIONS


tokenToString : PlayerToken -> String
tokenToString (PlayerToken str) =
    str
